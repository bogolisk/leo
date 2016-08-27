;; leo.el -- Lexical Emacs Objects -*- lexical-binding: t -*-

(require 'gv)
(require 'bytecomp)

(defconst leo--macro-current-action "doing nothing")

;; a leo object is
;; (name
;;  (class superclass ...) <- types list
;;  [closure1 closure2 closure3] <- vtab
;;  [getter1 getter2 getter3]
;;  [setter1 setter2 setter3]
;;  )

(define-inline leo-aget (key alist)
  (inline-letevals (key alist)
    (inline-quote (cdr (assq ,key ,alist)))))

(defalias 'leo-arglist-signature 'byte-compile-arglist-signature)
(defalias 'leo-arglist-vars 'byte-compile-arglist-vars)

(define-inline leo-unoptional-args (arglist)
  ;; make all args "used" so they can be forwarded from a stub
  (inline-letevals (arglist)
    (inline-quote
     (mapcar (lambda (arg)
	       (let ((arg-string (symbol-name arg)))
		 (if (= (aref arg-string 0) ?_)
		     ;; specified as unused argument
		     (intern (substring arg-string 1))
		   arg)))
	     ,arglist))))

(defun has-duplicates-p (lst)
  (and (consp lst)
       (if  (memq (car lst) (cdr lst))
	   (car lst)
	 (has-duplicates-p (cdr lst)))))

(define-inline leo-p (leo)
  (inline-letevals (leo)
    (inline-quote (and (consp ,leo) (eq (cadr ,leo) :leo)))))

(define-inline leo-ident (leo) (inline-letevals (leo) (inline-quote (car ,leo))))
(define-inline leo-types-list (leo) (inline-letevals (leo) (inline-quote (nth 2 ,leo))))
(define-inline leo-class (leo) (inline-letevals (leo) (inline-quote (car (leo-types-list ,leo)))))
(define-inline leo-vtab (leo) (inline-letevals (leo) (inline-quote (nth 3 ,leo))))
(define-inline leo-getters-vec (leo) (inline-letevals (leo) (inline-quote (nth 4 ,leo))))
(define-inline leo-setters-vec (leo) (inline-letevals (leo) (inline-quote (nth 5 ,leo))))

(define-inline leo-superclass (leo) 
  (inline-letevals (leo) 
    (inline-quote (cadr (leo-types-list ,leo)))))

(define-inline leo-vtab-method (leo vtab-no) 
  (inline-letevals (leo vtab-no) 
    (inline-quote (aref (leo-vtab ,leo) ,vtab-no))))

(define-inline leo-is-kind-of (leo class-sym)
  (inline-letevals (leo class-sym)
    (inline-quote
     (and (leo-p ,leo) (memq ,class-sym (leo-types-list ,leo))))))


(defsubst leo--macro-make-getter-form (class-sym leo-form slot-sym slot-idx)
  `(if (leo-is-kind-of ,leo-form ',class-sym)
       (funcall (aref (leo-getters-vec ,leo-form) ,slot-idx))
     (leo--runtime-getter-error ,leo-form ',class-sym ',slot-sym)))

(defsubst leo--macro-make-setter-form (class-sym leo-form slot-sym slot-idx value-sym)
  `(if (leo-is-kind-of ,leo-form ',class-sym)
       (funcall (aref (leo-setters-vec ,leo-form) ,slot-idx) ,value-sym)
     (leo--runtime-setter-error ,leo-form ',class-sym ',slot-sym)))

(defsubst leo-class-types (def) (leo-aget :types def))
(defsubst leo-class-super (def) (cadr (leo-aget :types def)))
(defsubst leo-class-slots (def &optional all) (leo-aget (if all :all-slots :slots) def))
(defsubst leo-class-getters (def) (leo-aget :getters def))
(defsubst leo-class-setters (def) (leo-aget :setters def))

(define-inline leo-class-vtab-specs (def)
  (inline-letevals (def)
    (inline-quote (leo-aget :vtab-specs ,def))))

(defsubst leo-class-new-methods (def) (leo-aget :new-methods def))
(defsubst leo-class-preserved-methods (def) (leo-aget :preserved-methods def))
(defsubst leo-class-distinct-methods (def) (leo-aget :distinct-methods def))
(defsubst leo-class-methods (def) (leo-aget :methods def))
(defsubst leo-class-all-methods (def) (mapcar #'car (leo-class-vtab-specs def)))
(defsubst leo-class-ctor-name (def) (leo-aget :ctor def))

(define-inline leo-class-externals (def)
  (inline-letevals (def)
    (inline-quote (leo-aget :externals ,def))))

(defsubst leo-class-method-spec (def method-sym) (assq method-sym (leo-class-vtab-specs def)))

(defun leo-class-available-methods (def)
  (nconc (copy-sequence (leo-class-new-methods def)) ;; normal methods
	 ;; distinc method names
	 (mapcar #'cdr (leo-class-distinct-methods def))
	 ;; will be preserved by superclasses, so available in this class
	 (delq nil (mapcar (lambda (method-class-pair)
			     (intern (format "%s::%s"
					     (cdr method-class-pair) (car method-class-pair))))
			   (leo-class-preserved-methods def)))
	 ;; inherited available methods from super
	 (when (leo-class-super def)
	   (leo-class-available-methods (symbol-value (leo-class-super def))))))

(defsubst leo-vtab-spec-method-name (mspec) (car mspec))
(defsubst leo-vtab-spec-method-args-spec (mspec) (cadr mspec))

(define-inline leo-vtab-spec-vtab-no (mspec)
  (inline-letevals (mspec)
    (inline-quote (nth 2 ,mspec))))

(defsubst leo-class-method-is-external (class-def method-sym)
  (aref (leo-class-externals class-def)
	(leo-vtab-spec-vtab-no (assq method-sym (leo-class-vtab-specs class-def)))))

(defsubst leo-mdef-method-name (mdef) (car mdef))
(defun leo-mdef-callable-form (mdef) 
  (let* ((callable-form (cadr mdef))
	 (body (when (eq (car callable-form) 'lambda)
		 (cdr callable-form)))
	 (arglist (when body (car body))))
    ;; remove &optional when instantiate lambda form
    (when (consp body)
      (setq callable-form (nconc (list 'lambda (leo-arglist-vars arglist))
				 (cdr body))))
    callable-form))
(defsubst leo-mdef-method-args (mdef) (nth 2 mdef))
(defsubst leo-mdef-method-doc (mdef) (nth 3 mdef))

(defun leo-class-find-method (class-def method-sym)
  (let ((super (leo-class-super class-def)))
    (or (when super
	  (leo-class-find-method (symbol-value super) method-sym))
	(assq method-sym (leo-class-methods class-def)))))

(defun leo-class-find-method-last-scope (class-def method-sym class-sym)
  (if (assq method-sym (leo-class-methods class-def))
      class-sym
    (let ((super (leo-class-super class-def)))
      (when super
	(leo-class-find-method-last-scope (symbol-value super) method-sym super)))))


(define-inline leo-method-bound-p (leo method-sym)
  (inline-letevals (leo method-sym)
    (inline-quote
     (let* ((class (and (leo-p ,leo) (leo-class ,leo)))
	    (class-def (and class (symbol-value class)))
	    (vtab-no (and class-def 
			  (leo-vtab-spec-vtab-no 
			   (assq ,method-sym (leo-class-vtab-specs class-def)))))
	    (method (and vtab-no (leo-vtab-method ,leo vtab-no))))
       (and (functionp method)
	    (not (eq method #'ignore)))))))

;; 
;; Defnition of a class
;;
;; ((:types (class-name superclass-name...))
;;  (:slots (slot-a slot-b...))
;;  (:all-slots (slot-a slot-b... inherited-slot-from-super...))
  ;;; (:getters ((slot-c . 2) (slot-b . 1) (slot-a . 0)...))
  ;;; (:setters ((slot-c . 1)  (slot-a . 0)...))
;;  (:vtab-specs
;;   ((method1-sym arglist-spec vtab-no)
;;    (method1-sym arglist-spec vtab-no)
;;    ...))
;;  (:new-methods
;;   1st-method-defined-by-this-class 2nd-method-defined-by-this-class...)
;;  (:methods
;;   (method1-sym callable-form (arg1 arg2...) "method document")
;;   (method1-sym callable-form (arg1 arg2...) "method document"))
;;  (:preserved-methods
;;   (inherited-method-to-be-preserved1 inherited-method-to-be-preserved2...))
;;  (:distinct-methods
;;   ((method-sym1 . distinct-name1) (method-sym2 . distinct-name2) ...))
;;  (:externals external-flags-bool-vector)
;;  (:ctor . ctor-function-name))
(defun leo--macro-build-class-definition (class-sym super-sym body)
  "Build the class definition for class CLASS-SYM.
SUPER-SYM if non-nil is the name of the superclass.
BODY is a list of slots and/or methods."
  (let ((leo--macro-current-action (format "parsing class %s" class-sym))
	super-def super-vtab-specs super-types super-all-method-syms super-all-slots
	super-getters super-setters super-external-flags
	external-flags new-methods new-method-syms
	slots-list setters-alist getters-alist all-slots method-definitions vtab-specs
	preserved-methods distinct-methods-alist

	(slot-access-level :public)
	(setter-idx 0)
	(getter-idx 0))

    (when super-sym
      (unless (and (boundp super-sym) 
		   (setq super-def (symbol-value super-sym))) 
	(error "While %s, superclass %s was not defined" 
	       leo--macro-current-action super-sym))

      (setq super-types (leo-class-types super-def))
      (setq super-vtab-specs (leo-class-vtab-specs super-def))
      (setq super-all-slots (leo-class-slots super-def t))
      (setq super-all-method-syms (leo-class-all-methods super-def))
      (setq super-external-flags (leo-class-externals super-def))

      (setq super-getters (leo-class-getters super-def))
      (setq super-setters (leo-class-setters super-def))
      (setq getter-idx (length super-getters))
      (setq setter-idx (length super-setters)))

    (let ((vtab-no (length super-vtab-specs))
	  item)

      (setq external-flags (and super-external-flags (copy-sequence super-external-flags)))

      (while body
	(setq item (pop body))
	(cond ((memq item '(:public :visible :protected)) (setq slot-access-level item))
	      ((symbolp item)
	       (push item slots-list)
	       (when (memq slot-access-level '(:public :visible))
		 (push (cons item getter-idx) getters-alist)
		 (setq getter-idx (1+ getter-idx)))
	       (when (eq slot-access-level :public)
		 (push (cons item setter-idx) setters-alist)
		 (setq setter-idx (1+ setter-idx))))
	      ((and (consp item) (memq (car item) '(def-method def-method*)))
	       (let* ((method-sym (nth 1 item)) ;; (def-method method-sym ...)
		      (method-args-list (nth 2 item)) ;; (def-method method-sym (...)...)
		      (method-args-spec (leo-arglist-signature method-args-list))
		      method-body method-doc method-external distinct-name must-preserve-parent)

		 (unless (symbolp method-sym)
		   (error "Invalid method name %s in %s's definition" method-sym class-sym))
		 (unless (listp method-args-list)
		   (error "Invalid arguments list in method %s::%s definition: %s"
			  method-args-list class-sym method-sym))

		 (when (memq '&rest method-args-list)
		   (error "While %s, can't handle &rest in method %'s args yet!"
			  leo--macro-current-action method-sym))

		 (when (eq (car item) 'def-method*)
		   (unless super-vtab-specs
		     (error "can't preserve inherited method %s when class %s has no superclass"
			    method-sym class-sym))
		   (setq must-preserve-parent t))

		 (setq item (nthcdr 3 item))
		 (while (not (or method-body method-external))
		   ;; 1st pass:
		   ;; nil                       -> no doc, empty
		   ;; ("doc")           	  -> doc, emtpy
		   ;; (form...) 		  -> no doc, body
		   ;; (:override name form...)  -> 2 names
		   ;; ("doc" form...) 	  -> doc, body
		   ;; external 		  -> no doc, external
		   ;; ("doc" . external)        -> doc, external

		   ;; 2nd pass:
		   ;; nil
		   ;; (form...)
		   ;; external
		   (cond ((not (consp item))
			  ;; nil      -> no doc, empty
			  ;; external -> no doc, external
			  (setq method-external (or item 'ignore)
				;; next pass: out because method-external is now set
				item nil))
			 ((and (consp item) (stringp (car item)))
			  ;; ("doc")            -> doc, emtpy
			  ;; ("doc" form...) 	  -> doc, body
			  ;; ("doc" . external) -> doc, external
			  (setq method-doc (car item)
				;; next pass: item = nil | (form...) | external
				item (cdr item)))
			 ((and (consp item) 
			       ;; override name body
			       (eq (car item) :override)
			       (symbolp (cadr item))
			       ;; unique definition can't be empty
			       (nthcdr 2 item))
			  (setq distinct-name method-sym
				method-sym (cadr item)
				;; next pass: item = (form...) | external
				item (nthcdr 2 item)))
			 ((consp item)
			  ;; (form...)
			  (setq method-body item
				;; next pass: out because method body is now set
				item nil))
			 (t (error "Invalid definition of %s::%s" class-sym method-sym))))

		 (when (assq method-sym method-definitions)
		   (error "Method %s was already defined in class %s" method-sym class-sym))

		 (when distinct-name
		   (when (memq distinct-name super-all-method-syms)
		     (error "In %s defintion, distinct method name collision: %s"
			    class-sym distinct-name))
		   (when (assq method-sym distinct-methods-alist)
		     (error "Distinct name of %s::%s was defined as %s, now %s"
			    class-sym method-sym
			    (cdr (assq method-sym distinct-methods-alist)) distinct-name))
		   (push (cons method-sym distinct-name) distinct-methods-alist))

		 (when must-preserve-parent
		   (when distinct-name
		     (error "Distinct method can't be defined with def-method*"))
		   (push method-sym preserved-methods))

		 ;;
		 ;; method defintions
		 ;;
		 (push (list method-sym
			     (if method-body
				 ;; not empty body
				 `(lambda ,method-args-list ,@method-body)
			       `(function ,method-external))
			     method-args-list
			     method-doc)
		       ;; method-definition: (method-sym callable-form args-list doc)
		       method-definitions)

		 ;; if this is a overriding definition of an existing method
		 (if (memq method-sym super-all-method-syms)
		     ;;
		     ;; overriding super's method, num args must match
		     ;;

		     (let* ((super-method-spec (assq method-sym super-vtab-specs))
			    (super-method-args-spec 
			     (and super-method-spec 
				  (leo-vtab-spec-method-args-spec super-method-spec))))

		       ;;
		       ;; overriding must have matching args list
		       ;;
		       (unless (equal method-args-spec super-method-args-spec)
			 (let* ((original-mdef (leo-class-find-method super-def method-sym))
				(original-args (and original-mdef 
						    (leo-mdef-method-args original-mdef))))
			   (error "While %s, overriding %s%S with %s%S"
				  leo--macro-current-action method-sym original-args
				  method-sym method-args-list)))		       
		       ;;
		       ;; this method was defined by super, update it to refect the fact
		       ;; that's it's now an external defined method
		       ;;
		       (aset external-flags (leo-vtab-spec-vtab-no super-method-spec)
			     (not method-body)))
		   ;;
		   ;; introducing new method
		   ;;
		   (push (list method-sym (not method-body) vtab-no) new-methods) ;; temp only
		   ;; vtab-specs's entry:
		   ;;    (method-sym args-spec vtab-no)
		   (push (list method-sym method-args-spec vtab-no) vtab-specs)

		   (setq vtab-no (1+ vtab-no))))) ;; def-method
	      (t
	       (error "While %s, malformed slot/method definition: %s"
		      leo--macro-current-action item)))))

    (setq new-methods (nreverse new-methods))

    ;; set external flags from info saved in new-methods
    (setq external-flags 
	  (apply #'bool-vector (append external-flags (mapcar #'cadr new-methods))))

    (setq slots-list (nreverse slots-list))
    (setq all-slots (append slots-list super-all-slots))
    (setq method-definitions (nreverse method-definitions))
    (setq new-method-syms (mapcar #'car new-methods)) ;; keep only name of new methods
    (setq vtab-specs (nconc (nreverse vtab-specs) super-vtab-specs))

    (setq setters-alist (nconc setters-alist super-setters))
    (setq getters-alist (nconc getters-alist super-getters))

    (setq distinct-methods-alist (nreverse distinct-methods-alist))

    ;; make the list of preserved methods into an alist of
    ;; ((preserved-method-sym . preserved-method-fqn))
    (when preserved-methods
      (setq preserved-methods
	    (mapcar (lambda (preserved-sym)
		      (cons preserved-sym 
			    (leo-class-find-method-last-scope super-def preserved-sym
							      super-sym)))
		    preserved-methods)))

    (list (cons :types (cons class-sym super-types))
	  (cons :getters getters-alist)
	  (cons :setters setters-alist)
	  (cons :slots slots-list)
	  (cons :all-slots all-slots)
	  (cons :vtab-specs vtab-specs)
	  (cons :new-methods new-method-syms)
	  (cons :methods method-definitions)
	  (cons :preserved-methods preserved-methods)
	  (cons :distinct-methods distinct-methods-alist)
	  (cons :ctor (intern (format "leo-%s-class" class-sym)))
	  (cons :externals external-flags))))

(defun leo--macro-make-closure-form (class-sym class-def &optional 
					       child-form 
					       overridden-methods
					       child-preserved-methods-alist)
  (let ((leo--macro-current-action (format "building class %s's form" class-sym))
	(slots-list (leo-class-slots class-def))
	(getters-alist (leo-class-getters class-def))
	(setters-alist (leo-class-setters class-def))
	(method-definitions (leo-class-methods class-def))
	(preserved-methods-alist (copy-sequence child-preserved-methods-alist))
	(distinct-methods-alist (leo-class-distinct-methods class-def))
	(to-be-preserved-methods-alist (leo-class-preserved-methods class-def))
	(vtab-specs (leo-class-vtab-specs class-def))
	(super-sym (leo-class-super class-def))
	(methods-added-by-class (leo-class-new-methods class-def)))

    (let ( ;; ((slot1 (leo-aget 'slot1 init-alist)) 
	  ;;  (slot2 (leo-aget 'slot2 init-alist))
					;   ...)
	  (slot-forms (mapcar (lambda (slot-sym)
				;; init-alist is the argument of the ctor
				`(,slot-sym (leo-aget ',slot-sym init-alist)))
			      slots-list))
	  ;; in the closure, the method symbol is also a local variable bound
	  ;;    to its callable form
	  ;; (method1 method2 method3...)
	  (method-vars-forms methods-added-by-class)
	  (available-methods (leo-class-available-methods class-def))
	  getter-forms setter-forms
	  method-definitions-forms this-class-form)

      ;; getter-forms:
      ;; ((aset leo--getters 2 (lambda () slot-c))
      ;;  (aset leo--getters 1 (lambda () slot-b))
      ;;  (aset leo--getters 0 (lambda () slot-a))
      ;;  ...)
      ;;
      ;; setter-forms:
      ;; ((aset leo--setters 2 (lambda (new-value) (setq slot-d new-value)))
      ;;  (aset leo--setters 1 (lambda (new-value) (setq slot-c new-value)))
      ;;  (aset leo--setters 0 (lambda (new-value) (setq slot-a new-value)))
      ;;  ...)
      (dolist (slot slots-list)
	(let (idx)
	  (when (setq idx (leo-aget slot getters-alist))
	    (push `(aset leo--getters ,idx (lambda () ,slot)) getter-forms))
	  (when (setq idx (leo-aget slot setters-alist))
	    (push `(aset leo--setters ,idx (lambda (new-value) (setq ,slot new-value)))
		  setter-forms))))

      (setq getter-forms (nreverse getter-forms)
	    setter-forms (nreverse setter-forms))

      ;; ((progn
      ;;     (setq method1 (lambda (arg1 arg2) ...))
      ;;     (aset (leo-vtab self) 2 method1))
      ;;  (progn
      ;;     (setq method2 (lambda (arg1 arg2) ...))
      ;;     (aset (leo-vtab self) 6 method2)) ... )
      ;;
      (dolist (mdef method-definitions)
	(let* ((method-sym (leo-mdef-method-name mdef))
	       (method-callablle-form (leo-mdef-callable-form mdef))
	       (vtab-no (leo-vtab-spec-vtab-no (assq method-sym vtab-specs)))
	       (is-overidden (memq method-sym overridden-methods))
	       (is-preserved (assq method-sym preserved-methods-alist))
	       (distinct-name (leo-aget method-sym distinct-methods-alist))
	       (preserved-method-sym (and is-preserved 
					  (intern (format "%s::%s" class-sym method-sym)))))

	  ;; this method is asked to be preserved by subclass
	  (when is-preserved
	    ;; for let form's local vars pointing to methods
	    (push preserved-method-sym method-vars-forms)
	    ;; this class's version of the method is preserved, but not versions
	    ;; from this class's superclasses
	    (setq preserved-methods-alist
		  (assq-delete-all method-sym preserved-methods-alist)))

	  (when distinct-name
	    ;; make this method-definition available under a distinct name
	    (push distinct-name method-vars-forms))

	  (cond ((and is-overidden is-preserved distinct-name)
		 ;; method is:
		 ;; - overriden by a subclass
		 ;; - asked to be preserved by a subclass
		 ;; - availabe under a distinct name
		 ;; 
		 ;; Even if this method was overridden, keep its definition under
		 ;;   its full name and a distinct name (SAME callable form)
		 (push `(let-when-compile
			    ((leo--method ',(intern (format "%s::%s" class-sym distinct-name))))
			  (setq ,preserved-method-sym ,method-callablle-form
				,distinct-name ,preserved-method-sym))
		       method-definitions-forms))

		((and is-overidden is-preserved (null distinct-name))
		 ;; method is:
		 ;; - overriden by a subclass
		 ;; - asked to be preserved by a subclass
		 ;; 
		 ;; Even if this method was overridden, keep its definition under its full name
		 (push `(let-when-compile ((leo--method ',preserved-method-sym))
			  (setq ,preserved-method-sym ,method-callablle-form))
		       method-definitions-forms))

		((and is-overidden (not is-preserved) distinct-name)
		 ;; method is:
		 ;; - overriden by a subclass
		 ;; - availabe under a distinct name
		 ;; 
		 ;; Even if this method was overridden, keep its definition under a distinct name
		 (push `(let-when-compile
			    ((leo--method ',(intern (format "%s::%s" class-sym distinct-name))))
			  (setq ,distinct-name ,method-callablle-form))
		       method-definitions-forms))

		((and is-overidden (not is-preserved) (null distinct-name))
		 ;; method is overriden by a subclass and not preserved nor distinct
		 ;; don't bother to set the vtab
		 nil)

		((and (not is-overidden) is-preserved distinct-name)
		 ;; method is:
		 ;; - its latest implementation for this instance
		 ;; - asked to be preserved by a subclass
		 ;; - availabe under a distinct name
		 ;; 
		 ;; set vtab, short, full and distinct symbols to the SAME callable form
		 (push `(let-when-compile 
			    ((leo--method ',(intern (format "%s::%s" class-sym distinct-name))))
			  (setq ,method-sym ,method-callablle-form
				,preserved-method-sym ,method-sym
				,distinct-name ,method-sym)
			  (aset leo--vtab ,vtab-no ,method-sym))
		       method-definitions-forms))

		((and (not is-overidden) is-preserved (null distinct-name))
		 ;; method is:
		 ;; - its latest implementation for this instance
		 ;; - asked to be preserved by a subclass
		 ;; 
		 ;; set vtab, short and full symbols to the SAME callable form
		 (push `(let-when-compile ((leo--method ',preserved-method-sym))
			  (setq ,method-sym ,method-callablle-form
				,preserved-method-sym ,method-sym)
			  (aset leo--vtab ,vtab-no ,method-sym))
		       method-definitions-forms))

		((and (not is-overidden) (not is-preserved) distinct-name)
		 ;; method is:
		 ;; - its latest implementation for this instance
		 ;; - availabe under a distinct name
		 ;; 
		 ;; set vtab, short and distinct names to the SAME callable form
		 (push `(let-when-compile 
			    ((leo--method ',(intern (format "%s::%s" class-sym distinct-name))))
			  (setq ,method-sym ,method-callablle-form
				,distinct-name ,method-sym)
			  (aset leo--vtab ,vtab-no ,method-sym))
		       method-definitions-forms))
		((and (not is-overidden) (not is-preserved) (null distinct-name))
		 ;; method is:
		 ;; - its latest implementation for this instance
		 ;; 
		 ;; set vtab and name to the SAME callable form
		 (push `(let-when-compile
			    ((leo--method ',(intern (format "%s::%s" class-sym method-sym))))
			  (setq ,method-sym ,method-callablle-form)
			  (aset leo--vtab ,vtab-no ,method-sym))
		       method-definitions-forms))

		(t (error "Unexpected internal logic error when defining method %s::%s"
			  class-sym method-sym)))))

      (setq method-definitions-forms (nreverse method-definitions-forms))

      ;; the closure form is a let form which fill the vtab,getters and setters vectors
      (setq this-class-form 
	    `(let (,@slot-forms ,@method-vars-forms)
	       (let-when-compile ((leo--class ',class-sym)
				  (leo--defined-methods ',available-methods))		 
		 ,@method-definitions-forms)
	       ,@getter-forms
	       ,@setter-forms

	       ;; (let ....)
	       ,child-form))
      (if super-sym
	  (leo--macro-make-closure-form super-sym (symbol-value super-sym)
					this-class-form
					;; methods defined here and methods
					;; defined by subclasses
					(nconc (mapcar #'car method-definitions)
					       overridden-methods)
					;; methods to be preserved for subclasses and
					;; methods to be preserved for this class
					(nconc preserved-methods-alist 
					       to-be-preserved-methods-alist))
	this-class-form))))

(defmacro leo--context-assert-avail-method (sym)
  `(eval-when-compile
     (unless (memq ,sym leo--defined-methods)
       (error "In %s, %s is not a valid method (avail:%S)" leo--method  ,sym leo--defined-methods))))

(defmacro leo--context-method-is-external (vtab-no)
  `(eval-when-compile
     (aref leo--self-external-flags ,vtab-no)))
;;
;; For every method do-something declared in a class T,
;; make the global
;;    (defsubst do-something (...) ...) to invoke the method do-something as on an
;; objectof type T. Also make a global
;;    (defmacro do-something@ (...) ...) to make direct call to the method do-something@
;; from a another method of type T.
;;

;; `(defsubst ,method-sym (leo ,@method-args)
;; 	 ,(or method-doc (format "%s's method %s" class-sym method-sym))
;; 	 (leo--runtime-call-method leo ',class-sym ',method-sym ,vtab-no 
;; 				   (list ,@arglist-vars)))


(defun leo--macro-make-call-site-form (class-sym method-sym vtab-no method-args call-args
						 &optional method-doc)
  ;; (leo--macro-make-call-site-form 'class-sym 'method-sym 10 '(a b &optional c) '(a b c) "doc")
  ;; (define-inline method-sym (leo a b &optional c)
  ;;   "doc" 
  ;;   (inline-letevals (leo a b c) 
  ;;     (inline-quote 
  ;;      (leo--runtime-call-method ,leo 'class-sym 'method-sym 10 (list ,a ,b ,c)))))
  (list 'define-inline
	method-sym
	`(leo ,@method-args)
	(or method-doc (format "%s::%s[%d]" class-sym method-sym vtab-no))
	(list 'inline-letevals `(leo ,@call-args)
	      (list 'inline-quote
		    (append '(leo--runtime-call-method ,leo)
			    `(',class-sym ',method-sym ,vtab-no)
			    (list (cons 'list
					(mapcar (lambda (arg-sym)
						  (list '\, arg-sym))
						call-args))))))))

(defun leo--macro-make-method-calling-sites-list (class-sym class-def)
  "For "
  (let ((leo--macro-current-action (format "building call-sites for class %s's methods"
					   class-sym))
	(vtab-specs (leo-class-vtab-specs class-def))
	(method-definitions (leo-class-methods class-def))
	(added-methods-list (leo-class-new-methods class-def))
	(distinct-methods-alist (leo-class-distinct-methods class-def))
	(externals (leo-class-externals class-def))
	call-sites-list)

    ;; build calling-site stubs for the newly defined methods.
    ;; overridden methods already have stubs built by superclass(es).

    (mapc (lambda (method-sym)
	    ;; method-definition: (method-sym callable-form args-list doc)
	    (let* ((mdef (assq method-sym method-definitions))
		   (method-args (leo-unoptional-args (leo-mdef-method-args mdef)))
		   (arglist-vars (leo-arglist-vars method-args))
		   (method-doc (leo-mdef-method-doc mdef))
		   (intra-method-sym (intern (concat (symbol-name method-sym) "@")))
		   (vtab-no (leo-vtab-spec-vtab-no (assq method-sym vtab-specs))))
	      ;; validate position in method jump table
	      (unless (numberp vtab-no)
		(error "While %s, internal error: failed to bind method %s to %s's vtab"
		       leo--macro-current-action method-sym class-sym))

	      ;; the stub: (foo ....)
	      (push (leo--macro-make-call-site-form class-sym method-sym vtab-no
						    method-args arglist-vars method-doc)
		    ;; `(defsubst ,method-sym (leo ,@method-args)
		    ;; 	 ,(or method-doc (format "%s's method %s" class-sym method-sym))
		    ;; 	 (leo--runtime-call-method leo ',class-sym ',method-sym ,vtab-no 
		    ;; 				   (list ,@arglist-vars)))

		    call-sites-list)
	      ;; the internal call (foo@ ...)
	      (push `(defmacro ,intra-method-sym ,method-args
		       (let ((method-sym ',method-sym)
			     (arglist-vars (list ,@arglist-vars))
			     (vtab-no ,vtab-no))
			 `(progn
			   (leo--context-assert-avail-method ',method-sym)
			   ;;
			   ;; if method-def was a symbol
			   ;;     -> external method definition
			   ;;     -> pass the object as the 1st parameter
			   ;; else
			   ;;     -> internal method definition
			   ;;     -> call directly, the object is the closure of the method
			   ;;
			   (if (leo--context-method-is-external ,vtab-no)
			       (funcall ,method-sym self ,@arglist-vars)
			     (funcall ,method-sym ,@arglist-vars)))))
		    call-sites-list)))
	  added-methods-list)

    (mapc (lambda (mdef)
	    ;; method-definition: (method-sym callable-form args-list doc)
	    (let* ((method-sym (car mdef))
		   (method-args (leo-unoptional-args (leo-mdef-method-args mdef)))
		   (arglist-vars (leo-arglist-vars method-args))
		   (fully-qualified-name (intern (format "%s::%s" class-sym method-sym)))
		   (internal-fqn (intern (format "%s::%s@" class-sym method-sym)))
		   (vtab-no (leo-vtab-spec-vtab-no (assq method-sym vtab-specs)))
		   (is-external (aref externals vtab-no)))

	      ;; the stub:
	      (push `(defmacro ,internal-fqn ,method-args
		       (let ((fully-qualified-name ',fully-qualified-name)
			     (arglist-vars (list ,@arglist-vars))
			     (is-external ,is-external))
			 ;;
			 ;; if method-def was a symbol
			 ;;     -> external method definition
			 ;;     -> pass the object as the 1st parameter
			 ;; else
					;      -> internal method definition
			 ;;     -> call directly, the object is the closure of the method
			 ;;
			 `(progn
			    (leo--context-assert-avail-method ',fully-qualified-name)
			    (if ,is-external
				(funcall ,fully-qualified-name self ,@arglist-vars)
			      (funcall ,fully-qualified-name ,@arglist-vars)))))
		    call-sites-list)))
	  method-definitions)

    (mapc (lambda (method-distinct-pair)
	    ;; method-definition: (method-sym callable-form args-list doc)
	    (let* ((method-sym (car method-distinct-pair))
		   (distinct-name (cdr method-distinct-pair))
		   (mdef (assq method-sym method-definitions))
		   (method-args (leo-unoptional-args (leo-mdef-method-args mdef)))
		   (arglist-vars (leo-arglist-vars method-args))
		   (internal-name (intern (concat (symbol-name distinct-name) "@")))
		   (vtab-no (leo-vtab-spec-vtab-no (assq method-sym vtab-specs)))
		   (is-external (aref externals vtab-no)))

	      ;; the stub:
	      (push `(defmacro ,internal-name ,method-args
		       (let ((distinct-name ',distinct-name)
			     (arglist-vars (list ,@arglist-vars))
			     (is-external ,is-external))
			 `(progn
			    (leo--context-assert-avail-method ',distinct-name)
			    ;;
			    ;; if method-def was a symbol
			    ;;     -> external method definition
			    ;;     -> pass the object as the 1st parameter
			    ;; else
					;      -> internal method definition
			    ;;     -> call directly, the object is the closure of the method
			    ;;
			    (if ,is-external
				(funcall ,distinct-name self ,@arglist-vars)
			      (funcall ,distinct-name ,@arglist-vars)))))
		    call-sites-list)))
	  distinct-methods-alist)
    (nreverse call-sites-list)))

(defun leo--macro-make-class-instantiation-form (class-sym instance-name ctor-init-alist)
  "Build an init alist ((slot1 . form) (slot2 . form)...) from
a list ((slot2 form) (slot1 form))"
  ;; ctor-init-alist: '((a 1) (b 2) (c (foo 7 "hello")) ...)
  (let* ((leo--macro-current-action (format "constructing a %s object" class-sym))
	 (class-def (symbol-value class-sym))
	 (ctor-sym (leo-class-ctor-name class-def))
	 (all-slots (leo-class-slots class-def t))
	 (slots-with-init (mapcar 'car ctor-init-alist))
	 init-alist)
    (mapc (lambda (sym-init-list)
	    (unless (memq (car sym-init-list) all-slots)
	      (error "While %s, unrecognized slot %s in init form %S"
		     leo--macro-current-action (car sym-init-list)
		     sym-init-list)))
	  ctor-init-alist)
    (setq init-alist
	  (mapcar (lambda (sym)
		    `(cons ',sym ,sym))
		  slots-with-init))
    `(let ,ctor-init-alist
       (,ctor-sym ,instance-name (list ,@init-alist)))))

(defun leo--runtime-getter-error (leo class-sym slot-sym)
  (if (leo-p leo)
      (error "Attempted to access %s::%s on object %s of type %S" class-sym slot-sym
	     (leo-ident leo) (leo-types-list leo))
    (error "Attempted to access %s::%s on object: %s" class-sym slot-sym leo)))

(defun leo--runtime-setter-error (leo class-sym slot-sym)
  (if (leo-p leo)
      (error "Attempted to set %s::%s on object %s of type %S" class-sym slot-sym
	     (leo-ident leo) (leo-types-list leo))
    (error "Attempted to set %s::%s on object: %s" class-sym slot-sym leo)))

(defun leo--runtime-method-error (leo class-sym method-sym)
  (if (leo-p leo)
      (error "Attempted to invoke %s::%s on object %s of type %S" class-sym method-sym
	     (leo-ident leo) (leo-types-list leo))
    (error "Attempted to invoke %s::%s on object: %s" class-sym method-sym leo)))

(define-inline leo--runtime-call-method (leo class-sym method-sym vtab-no args)
  (inline-letevals (leo class-sym method-sym vtab-no args)
    (inline-quote
     (if (leo-is-kind-of ,leo ,class-sym)
	 (let ((object-runtime-method (leo-vtab-method ,leo ,vtab-no)))
	   (if (not (symbolp object-runtime-method))
	       ;; method is internally defined
	       (apply object-runtime-method ,args)
	     ;; method is bound to an externally defined function
	     (apply object-runtime-method ,leo ,args)))
       (leo--runtime-method-error ,leo ,class-sym ,method-sym)))))

(define-inline leo-create (leo-class instance-name &optional init-alist)
  (inline-letevals (leo-class instance-name init-alist)
    (inline-quote
     (let ((ctor (and (consp ,leo-class) (leo-aget ,leo-class :ctor))))
       (if (functionp ctor)
	   (funcall ctor ,instance-name ,init-alist)
	 (error "%s is not a leo class" ,leo-class))))))

(defmacro define-leo-class (name super-spec &rest body)
  (let* ((leo--macro-current-action
	  (prog1 (format "defining leo class %s" name)
	    (unless (symbolp name)
	      (error "Invalid class name: %s" name))
	    (unless (and (listp super-spec)
			 (symbolp (car super-spec))
			 (null (cdr super-spec)))
	      (error "Invalid superclass spec: %s, expected: (superclass)" super-spec))))
	 (super-sym (car super-spec))
	 (type-def (leo--macro-build-class-definition name super-sym body))
	 (all-types (leo-aget :types type-def))
	 (all-slots (leo-aget :all-slots type-def))
	 (vtab-specs (leo-aget :vtab-specs type-def))
	 (getters-alist (leo-aget :getters type-def))
	 (setters-alist (leo-aget :setters type-def))
	 (externals (leo-aget :externals type-def))
	 (ctor-sym (leo-aget :ctor type-def))
	 layout)

    (set name type-def)
    (setq layout (leo--macro-make-closure-form name type-def))
    `(progn
       (defconst ,name ',type-def)

       ;; define calling sites. these are defsubst so they're must be
       ;; instantiated before the constructor.
       ,@(leo--macro-make-method-calling-sites-list name type-def)

       (defmacro ,name (arg1 &rest args) 
	 (cond ((null args) (error "Invalid use of class %s: (%s %s)" ',name ',name arg1))
	       ((null arg1) (error "Instance of class %s can't be nil" ',name))

	       ;; (class-name arg1 args)

	       ((and (eq arg1 :create) (car args))
		;; object construction: (class :create "my-object" (a 1) (b 2) ...)
		;;    arg1:       :create
		;;    args:  	  (instance-name (a 1) (b 2) ...)
		(leo--macro-make-class-instantiation-form ',name (car args) (cdr args)))

	       ((and (not (eq arg1 :create)) (cdr args))
		;; (class-name object slot-sym garbage...)
		(error "Invalid access from of class %s: %S" ',name
		       (append (list ',name arg1) args)))

	       ((assq (car args) ',getters-alist)
		;; getter form : (class object slot)
		;;     arg1: form returning an object at run-time
		;;     args: name of the slot
		(leo--macro-make-getter-form ',name arg1 (car args)
					     (leo-aget (car args) ',getters-alist)))
	       ((and arg1 (null (cdr args)) (memq (car args) ',all-slots))
		(error "%s::%s is not externally visible" ',name (car args)))
	       (t (error "Class %s has no slot %s" ',name (car args)))))

       (gv-define-expander ,name 
	 (lambda (do var slot)
	   (unless (memq slot ',all-slots) (error "Class %s has no slot: %s" ',name slot))
	   (unless (assq slot ',setters-alist) 
	     (error "%s::%s is not externally settable" ',name slot))
	   (funcall do 
		    (leo--macro-make-getter-form ',name var slot
						 (leo-aget slot ',getters-alist))
		    (lambda (value)
		      (leo--macro-make-setter-form ',name var slot
						   (leo-aget slot ',setters-alist) value)))))

       (defun ,ctor-sym (ident ,@(when all-slots '(&optional init-alist)))
	 (let-when-compile ((leo--self-types ',all-types)
			    (leo--self-external-flags ',externals))
	   (let* ((leo--vtab (make-vector ,(length vtab-specs) 'umapped-method))
		  (leo--getters (make-vector ,(length getters-alist) 'unmapped-getter))
		  (leo--setters (make-vector ,(length setters-alist) 'unmapped-setter))
		  ;; object layout:
		  ;;  (name :leo
		  ;;    (class superclass supersuperclass supersupersuperclass...)
		  ;;    [callable-form-1 callable-form-2 ...] ;; vtab
		  ;;    [getter1 getter2...]		      ;; getters-vec
		  ;;    [setter1 setter2...]		      ;; setters-vec
		  ;;    )
		  (self (list ident :leo
			      '(,@all-types)
			      leo--vtab
			      leo--getters
			      leo--setters)))
	     ,layout
	     self))
	 )
       )))

;; interface: (:interface (name base basebase basebasebase...) dispatch-table)
;;          dispatch-table: [(method-sym args-spec args-list)...]
(define-inline leo-interace-p (leo-i)
  (inline-letevals (leo-i)
    (inline-quote (and (consp ,leo-i) (eq (car ,leo-i) :interface)))))

;; helpers for inteface
(defsubst leo-interface-idents-list (leo-i) (cadr leo-i))
(defsubst leo-interface-dispatch-table (leo-i) (nth 2 leo-i))
(defsubst leo-interface-num-methods (leo-i) (length (leo-interface-dispatch-table leo-i)))
(defsubst leo-interface-method-names-list (leo-i)
  (mapcar 'car (leo-interface-dispatch-table leo-i)))

;; binding-definition:
;;     (:interface-binding (class-sym interface-sym1 interface-sym2...) mapping-defs)
  ;;;             mapping-defs: [vtab-no vtab-no vtab-no...]

;; helpers for interface binding definition
(define-inline leo-ibdef-bindings (ibdef)
  (inline-letevals (ibdef)
    (inline-quote (cadr ,ibdef))))
(define-inline leo-ibdef-mapping (ibdef)
  (inline-letevals (ibdef)
    (inline-quote (nth 2 ,ibdef))))
(define-inline leo-ibdef-class (ibdef)
  (inline-letevals (ibdef)
    (inline-quote (car (leo-ibdef-bindings ,ibdef)))))

(define-inline leo-ibdef-intefaces (ibdef)
  (inline-letevals (ibdef)
    (inline-quote (cdr (leo-ibdef-bindings ,ibdef)))))

;; binder: (target-name :interface-binder binding-name mapping-vec)
;;    mapping-vec: [closure1 closure2 closure3... ]
;;    forward-action: (lambda (binder dispatch-no args))

(define-inline leo-binder-binding (binder) 
  (inline-letevals (binder)
    (inline-quote (nth 2 ,binder))))
(define-inline leo-binder-mapping-vector (binder)
  (inline-letevals (binder)
    (inline-quote (nth 3 ,binder))))
(define-inline leo-binder-mapped-method (binder vtab-no)
  (inline-letevals (binder vtab-no)
    (inline-quote
     (aref (leo-binder-mapping-vector ,binder) ,vtab-no))))

;; helpers for binder
(define-inline leo-binder-p (binder)
  (inline-letevals (binder)
    (inline-quote (and (consp ,binder) (eq (cadr ,binder) :interface-binder)))))

(define-inline leo-binder-bound-interaces (binder)
  ;; binder : (:interface-binder binding-name mapping-vec)
  (inline-letevals (binder)
    (inline-quote
     (and (leo-binder-p ,binder)
	  (leo-ibdef-intefaces (symbol-value (leo-binder-binding ,binder)))))))

(define-inline leo-binder-of-interface-p (binder interface-sym)
  (inline-letevals (binder interface-sym)
    (inline-quote
     (memq ,interface-sym (leo-binder-bound-interaces ,binder)))))

(defun leo--macro-validate-interface-declarations (method-declarations &optional parent)
  (when parent
    (let ((parent-method-names 
	   (when parent (leo-interface-method-names-list (symbol-value parent)))))
      (mapc (lambda (declaration)
	      (let* ((method-sym (car declaration))
		     (method-args (cadr declaration))
		     (method-doc (nth 2 declaration)))

		(when (or (nth 3 declaration) 
			  (if method-doc
			      (unless (stringp method-doc)
				(setq method-doc nil)
				t)
			    (setq method-doc "Undocumented")
			    nil))
		  (error "When %s, method declaration %s cannot have body"
			 leo--macro-current-action method-sym))
		(unless (listp method-args)
		  (error "When %s, args of method declaration %s is mailformed: %s"
			 leo--macro-current-action method-sym method-args))
		(when (memq '&rest method-args)
		  (error "When %s, can't handle &rest in method %s's arguments list"
			 leo--macro-current-action method-sym))
		(when (memq method-sym parent-method-names)
		  (error "When %s, method %s already defined in interface %s"
			 leo--macro-current-action method-sym parent))))
	    method-declarations))))

(defun leo--macro-make-interface-dispatching-tab (interface-sym method-declarations parent)
  (let* ((parent-tab (when parent 
		       (leo-interface-dispatch-table (symbol-value parent))))
	 (tab (vconcat parent-tab (make-list (length method-declarations) 'unmapped)))
	 (i (if (vectorp parent-tab) (length parent-tab) 0)))
    (dolist (method method-declarations)
      (aset tab i 
	    ;; (method-sym method-args-spec method-args trigger-name)
	    (list (car method)
		  (leo-arglist-signature (cadr method))
		  (cadr method)
		  (intern (format "%s:%s" interface-sym (car method)))))
      (setq i (1+ i)))
    tab))

(defun leo--macro-make-dispatch-form (interface-sym call-sym vtab-no method-sym method-args call-args
						    &optional method-doc)

  ;; `(defsubst ,interface-method-sym (binder ,@method-args)
  ;;    (leo--runtime-dispatch-call binder ',interface-sym ,i
  ;; 				 ',method-sym (list ,@arglist-vars)))

  ;; `(define-inline ,call-sym method-args
  ;;    ,(format "%s[%d]" call-sym vtab-no)
  ;;    (inline-letevals (binder ,@call-args)
  ;;      (inline-quote
  ;; 	(leo--runtime-dispatch-call binder ',interface-sym ,vtab-no ',method-sym
  ;; 				    (list ,@arglist-vars)))))

  (list 'define-inline call-sym	`(obj ,@method-args)
	(or method-doc (format "%s[%d]" call-sym vtab-no))
	(list 'inline-letevals `(obj ,@call-args)
	      (list 'inline-quote
		    (append '(leo--runtime-dispatch-call ,obj)
			    `(',interface-sym ,vtab-no ',method-sym)
			    (list (cons 'list
					(mapcar (lambda (arg-sym)
						  (list '\, arg-sym))
						call-args))))))))

(defun leo--macro-make-interface-dispatching-sites (interface-sym dispatching-tab parent)
  (let ((parent-method-names 
	 (when parent
	   (leo-interface-method-names-list (symbol-value parent))))
	sites-list)
    (dotimes (i (length dispatching-tab))
      (let* ((spec (aref dispatching-tab i))
	     (method-sym (car spec))
	     (method-args (nth 2 spec))
	     (arglist-vars (leo-arglist-vars method-args))
	     (interface-method-sym (nth 3 spec)))

	(unless (memq method-sym parent-method-names)
	  ;; binder : (:interface-binder (interface-sym . class-sym) mapping-vec)
	  (push (leo--macro-make-dispatch-form interface-sym interface-method-sym i method-sym
					       method-args arglist-vars)
		sites-list)	  
	  ;; (push `(defsubst ,interface-method-sym (binder ,@method-args)
	  ;; 	   (leo--runtime-dispatch-call binder ',interface-sym ,i
	  ;; 				       ',method-sym (list ,@arglist-vars)))
	  ;; 	sites-list)
	  (push `(eval-when-compile
		   (put ',interface-method-sym 'leo-interface ',interface-sym))
		sites-list))))
    (nreverse sites-list)))

(defun leo--runtime-dispatch-error (binder interface-sym method-sym)
  (if (leo-binder-p binder)
      (error "Attempt to dispatch %s::%s on binder of type %s"
	     interface-sym method-sym (leo-binder-binding binder))
    (error "Attempt to call %s::%s on non-binder: %s"
	   interface-sym method-sym binder)))

(define-inline leo--runtime-dispatch-call (binder interface-sym dispatch-no method-sym args)
  (inline-letevals (binder interface-sym dispatch-no method-sym args)
    (inline-quote
     (if (leo-binder-of-interface-p ,binder ,interface-sym)
	 (apply (leo-binder-mapped-method ,binder ,dispatch-no) ,args)
       (leo--runtime-dispatch-error ,binder ,interface-sym ,method-sym)))))

(defmacro define-leo-interface (name base-spec &rest method-declarations)
  (unless (symbolp name)
    (error "Invalid name for Interface defintion: %s" name))
  (unless (or (null base-spec)
	      (and (consp base-spec) (null (cdr base-spec)) (symbolp (car base-spec))))
    (error "Invalid base interface spec: %s, must be (base-interface)" base-spec))
  (unless method-declarations
    (error "Interface %s has no method definitions" name))
  (let ((leo--macro-current-action (format "defining interface %s" name))
	(base (car base-spec)))
    (leo--macro-validate-interface-declarations method-declarations base)
    (let* ((dispatching-tab 
	    (leo--macro-make-interface-dispatching-tab name method-declarations base))
	   (dispatching-sites 
	    (leo--macro-make-interface-dispatching-sites name dispatching-tab base))
	   ;; interface:
	   ;; (:interface
	   ;;    (base grand-parent great-grand-parent...)
	   ;;    [(method-sym args-spec args-list trigger-sym) ...] 
	   ;;    (trigger-sym1 trigger-sym2 ...))
	   (interface-def (list :interface 
				(cons name (when base
					     (leo-interface-idents-list 
					      (symbol-value base))))
				dispatching-tab
				(mapcar (lambda (spec) (nth 3 spec)) dispatching-tab))))
      (set name interface-def)
      `(progn
	 (defconst ,name '(,@interface-def))
	 ,@dispatching-sites))))

(defconst leo--binding-debug nil)

(defun leo--macro-bind-methods (interface-sym class-sym
					      find-target-function
					      &optional sloppy-mapping)
  (let* ((interface-def (symbol-value interface-sym))
	 (class-def (symbol-value class-sym))

	 (dispatch-tab 
	  ;; [(method-sym args-spec method-args trigger-sym) ...]
	  (and interface-def (leo-interface-dispatch-table interface-def)))

	 (vtab-specs 
	  ;; ((method-sym args-spec vtab-index).. )
	  (and class-def (leo-aget :vtab-specs class-def)))
	 (mapping-tab (make-vector (length dispatch-tab) 'ignore)))

    (unless (and (vectorp mapping-tab)
		 (= (length mapping-tab) (length dispatch-tab)))
      (error "Internal error: bad mapping tab in function leo--macro-bind-methods (%s->%s)"
	     interface-sym class-sym))

    (dotimes (i (length mapping-tab))
      (aset mapping-tab i
	    (let* ((src (aref dispatch-tab i))
		   (src-sym (car src))
		   (src-args-spec (cadr src))
		   (dst-sym (funcall find-target-function src-sym))
		   (dst (assq dst-sym vtab-specs))
		   (dst-args-spec (and dst (leo-vtab-spec-method-args-spec dst)))
		   (dst-vtab-no (and dst (leo-vtab-spec-vtab-no dst))))
	      (unless (or dst sloppy-mapping)
		(error "Failed to map %s to %s: %s not found" interface-sym class-sym src-sym))
	      (when (and (null dst) leo--binding-debug)
		(warn "%s -> %s: not mapped %s" src-sym))
	      (when dst
		(when leo--binding-debug
		  (if (equal src-sym dst-sym)
		      (warn "%s -> %s: mapped %s")
		    (warn "%s -> %s: mapped %s to %s")))
		(unless (equal dst-args-spec src-args-spec)
		  (let* ((dst-mdef (leo-class-find-method class-def src-sym))
			 (dst-args (and dst-mdef (leo-mdef-method-args dst-mdef)))
			 (src-args (nth 2 src)))
		    (error "Failed to map %s::%s%S to %s::%s%S"
			   interface-sym src-sym src-args class-sym dst-sym dst-args))))
	      dst-vtab-no)))

    (list :interface-binding
	  (cons class-sym (leo-interface-idents-list interface-def))
	  mapping-tab)))

(defsubst leo--macro-make-default-bindings (interface-sym class-sym)
  (leo--macro-bind-methods interface-sym class-sym #'identity))

(defun leo--macro-make-method-bindings (interface-sym class-sym method-bindings)
  (leo--macro-bind-methods interface-sym class-sym
			   (lambda (src-sym)
			     (or (leo-aget src-sym method-bindings) src-sym))
			   t))

(defun leo--runtime-make-binder (leo binding-name binding-def)
  (let* ((mapping-tab (leo-ibdef-mapping binding-def))
	 (num-mapped (length mapping-tab))
	 (binding-vtab (make-vector num-mapped 'unmapped))
	 (class-sym (leo-ibdef-class binding-def))
	 (externals (leo-class-externals (symbol-value (leo-class leo)))))

    (unless (leo-is-kind-of leo class-sym)
      (error "Incompatible object types %S for binding %s, expected: %s"
	     (leo-types-list leo) binding-name class-sym))

    (dotimes (i (length binding-vtab))
      (let ((mapped-no (aref mapping-tab i)))
	(aset binding-vtab i 
	      (if (numberp mapped-no)
		  (if (aref externals mapped-no)
		      (lambda (&rest args) 
			(apply (leo-vtab-method leo mapped-no) leo args))
		    (leo-vtab-method leo mapped-no))
		#'ignore))))

    (list (car leo) :interface-binder binding-name binding-vtab)))

(defmacro define-leo-binding (name binding &rest method-bindings)
  (let* ((interface-sym (car binding))
	 (class-sym (cdr binding))
	 (binding-def (if method-bindings
			  (leo--macro-make-method-bindings interface-sym class-sym method-bindings)
			(leo--macro-make-default-bindings interface-sym class-sym))))
    (set name binding-def)
    `(progn
       (defconst ,name ',binding-def)
       (put ',interface-sym 'interface-binding
	    (cons '(,class-sym . ,name)
		  (assq-delete-all ',class-sym (get ',interface-sym 'interface-binding))))
       (defun ,name (leo)
	 (leo--runtime-make-binder leo ',name ,name)))))


(font-lock-add-keywords 
 'emacs-lisp-mode
 '(("(\\(define-leo-\\(?:class\\|binding\\|interface\\)\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
   ("(\\(def-method\\*?\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))))

(provide 'leo)
