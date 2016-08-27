;;; -*- lexical-binding: t -*-
(require 'leo)

(defmacro with-self (&rest body)
  `(progn ,@body))

(define-leo-class person ()
  first-name
  last-name
  (def-method full-name () (concat first-name " " last-name))
  (def-method nothing ()
    "Just slacking around")
  (def-method say (&optional word) 
    "Say the WORD"
    (unless word (setq word "hello"))
    (message "I'm a person, my name is %s %s. I'm saying: %s" first-name last-name word))
  (def-method walk (num-steps)
    (with-self
     (say@ "I'm walking")
     ;;(work@ 4)
     (nothing@)
     ;;(leo-self-call work 4)
     (message "%s %s walks %d steps" first-name last-name num-steps)))

  )

(setq john (person :create "Jonh" (first-name "John") (last-name "Smith")))

;;(nothing john)

(define-leo-class employee (person)
  :visible id-no
  (def-method nothing ()
    "No slacking"
    (message "employee always work"))
  (def-method id () id-no)
  (def-method* say (&optional word)
    (unless word (setq word "hello"))
    (if nil
  	(person::say@ word)
      (message "I'm employee %d, my name is %s %s. I'm saying: %s" 
  	       (id@) first-name last-name word)))
  (def-method work (&optional hours)
    "Working hard" . employee-work))



(defun employee-work (self &optional hours)
  (message "%s works for %d hours!" (employee self first-name) (or hours 0)))

(define-leo-class manager (employee)
  director
  (def-method* id ()
    (+ (employee::id@) 1000))
  (def-method think (&optional hours) :override work
    (if (numberp hours)
  	(message "manager %s %s is thinking for %d hour(s)" first-name last-name hours)
      (message "manager %s %s is thinking" first-name last-name)))
  (def-method manage (employee-list) 
    (think@ 1)
    (mapc
     (lambda (e)
       (message "I, %s %s ,manage employee %d"
  		first-name last-name (employee e id-no)))
     employee-list)
    nil)
  )



(setq jane (employee :create (concat "Jane" "D") (first-name "Jane") (id-no 77) (last-name "Didi")))
(setq boss (manager :create  "BigBoss" (first-name "Big") (id-no 1) (last-name "Boss")))


;;(disassemble (aref (nth 2 john) 2))
;;(disassemble (aref (nth 2 jane) 2))
;;(disassemble (aref (nth 2 boss) 2))

;;(disassemble (byte-compile '(memq 'foo '(foo bar))))


(say jane)

(ignore (manage boss (list jane)))

(person john first-name)
(person jane first-name)
(employee jane last-name)

(define-leo-interface walker nil
  (walk (steps)))

(define-leo-interface sayer (walker)
  (say (&optional something)))

(define-leo-binding person-sayer (sayer . person))


(define-leo-interface worker (sayer)
  (work (&optional hours)))

(define-leo-binding worker-employee (worker . employee)
  nil)


(setq b-john (person-sayer john))
(setq b-jane (person-sayer jane))

(worker:work (worker-employee jane) 3)

(setf (employee jane last-name) "Doe")

(say john "Hello")
(say jane "Hello")
(say boss "Hello")
(walk john 3)
(walk jane 7)

(work jane 40)
(work boss 3)
(work jane)
;;(work '(1 2 3) 4)
(nothing jane)
(nothing john)
(nothing boss)


(walker:walk b-john 7)
(walker:walk b-jane 11)

(sayer:say b-john "haha")
(sayer:say b-jane "hihi")


(manage boss (list jane))


