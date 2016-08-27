# Lexical Elisp Objects

This is an attempt to implement objects in elisp using closure. Unfortunately closures ain't objects.

## Caveat Utilitor
This is immature code thus its API can change and it's unstable.

## Description

* Leo ain't no CLOS, it's more similar to C-with-Classes then CLOS.
* Leo supports polymorphism, methods are dispatched via run-time v-table lookup.
* Leo supports single inheritance but objects can adhere to unlimited number of interfaces.
* Leo use closures to implement objects so lexical-binding is required.
* Compiled code using leo is fairly time-efficient but not very space-efficient.
* Leo use instance-based protection (vs type-based protection)

## Examples

```lisp

(define-leo-class person () ;; new class person with no superclass
  :visible		    ;; all slots defined after this are
  			    ;; externaly-accessible but not
  			    ;; externaly-settable.
  first-name		    ;; a data slot
  last-name		    ;; a data slot
  :public
  driver-license-no	    ;; public data slot
  :protected		    ;; this is the default
  social-insurance-no       ;; protected data slot

  (def-method full-name () ;; method full-name, methods are always accessible
    (concat first-name " " last-name))

  (def-method say (&optional something)
    (message "%s says%s%s"
	     (full-name@)		;; call method using internal name
	     				;; faster than:
	     				;;   (full-name self)
	     (if something ": " " ") 
	     (or something "nothing")))

  (def-method slack ()
    (message "%s surfs the web" first-name)))


;; create a new person object with it's first-name initialized to "John"
(setq john (person :create "john" (first-name "John")))

;; create a new person object
(setq john-smith (person :create "johnsmith" (first-name "John") (last-name "Smith")))

;; slot access
(message "this is %s" (person john first-name))

;; call method
(message "this is %s" (full-name john-smith))

;; set public slot
(setf (person john-smith driver-license-no) "D730-3431-11Z7-M14")


(define-leo-class employee (person)	;; new class employtee inheriting class person
  id		  	   		;; new pbulic slot
  (def-method slack ()
    (message "employees never slack"))

  (def-method say (&optional something)
    (message "employee %d (%s) says%s%s"
	     id
	     first-name
	     (if something ": " " ") 
	     (or something "nothing"))))
	
```
