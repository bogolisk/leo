# Lexical Elisp Objects

This is an attempt to implement objects in elisp using closure. Unfortunately closures ain't objects.

## Caveat Utilitor
* This is immature code thus its API can change
* This is immature code thus it _might_ have some major design flaws
* This is immature code thus it _might_ be unstable
* Method names clash between classes. I.e., different classes cannot have a same method name.
* I don't know how to make it work with _edebug_.

## Description

* Leo ain't no CLOS, it's more similar to C-with-Classes then CLOS.
* Leo supports polymorphism, methods are dispatched via run-time v-table look-up.
* Leo supports single inheritance but objects can adhere to unlimited number of interfaces.
* Leo use closures to implement objects so lexical-binding is required.
* Compiled code using leo is fairly time-efficient but not very space-efficient.
* Leo use instance-based protection (vs type-based protection)

### Synopsis

```lisp

(define-leo-class A ()
  ;; default protection is :public
  a b
  :visible
  c d
  :protected
  e f g
  :public
  h i
  
  ;; methods are all accessible
  (def-method f1 (x y)
    (- a x y))
	
  (def-method f2 (x y)
    (+ b c x (f1@ (+ x y) (- y x)))))

(define-leo-class B (A)
  j k l
  ;; override A::f2
  (def-method f2 (x y)
    (- c x y)))

(define-leo-class C (B)
  (def-method* f1 (x y)
    (+ (A::f1@ x y) e f g j k l)))
	
(setq b (B :create "an instance of B" (k 11) (c 7) (f 0)))
(setq c (C :create "an instance of C" (b 11) (a 0)))

(f1 b 1 2)
(f2 c 3 55)
(f1 c (B b i) 4)
(setf (C c f) 100)
(f2 c (f1 b 11 0) 77)

(define-leo-interface I1 ()
  (f1 (a b))
  (f2 (c d)))

(define-leo-binding I1-to-A (I1 . A))

(setq proxy-for-c (I1-to-A c))
(setq proxy-for-b (I1-to-A b))
(I1:f1 proxy-for-c)
(I1:f2 proxy-for-b)


```

## API

### Class Definition

* class-definition := (**define-leo-class** class-name ([super-class]) _specs_...)
* specs := _slots-specs_ | _methods-specs_
* slot-specs := [**:protected**|**:visible**|**:public**] slot-name...
* method-specs := _method-definition_...
* method-definition := (**def-method[*]** method-name (lambda-args) [doc|_method-options_] lambda-body)
* method-options := **:override** inherited-method-name

### Object Instantiation

* object-instantiation := (class-name **:create** instance-name-string _slot-init_...)
* slot-init := (slot-name init-value-form)

### Special names

* **self**: inside a method, this variable is the instance itself. Do **not** set it.
* **ident**: inside a method, this variable is the name of the instance specified at creation time.
  Do **not* set it.

### Slot access

* external access: `(class-name instance-form slot-name)`
* exernal modification: `(setf (class-name instance-form slot-name) new-value)`
* internal access: `slot-name`
* internal modification: `(setq slot-name new-value)`

### Method invokation

To invoke a method, call the method name as a function with the
instance as the first argument. Inside a method, the form
`(method-name@)` shall be used (without the instance as the first
argument.) This internal calling form is exactly the same as
`(method-name self arg...)`, but it's faster because there's no need
for runtime type checking nor virtual table look-up.

* external invokation: (method-name instance-form args...)
* internal invokation: (method-name**@** args) same effect as (method-name self args...)

### Calling overridden inherited method

Method can call the overridden implementation that it overrode using
the `def-method*` keyword:

```lisp

(define-leo-class A1 ()
  a
  (def-method f1 (x) (+ (/ a x) 17))
  (def-method f2 (m n) (+ (f1@ m) n)))

(define-leo-class A2 (A1)
  (def-method f3 () (+ 11 a)))

(define-leo-class A3 (A2)
  ;;
  (def-method* f1 (x) (* (A1::f1@ x) 2)))
```

### Method alternative static name

Methods can be overridden by subclasses. When overriding an method,
the new implementation can have an alternatie name so it won't be
overridden by subclasses. Thus, that specific implementation of a
method has two names: the official one which can be overridden by
subclasses and the alternative name which is... static and always
available.

```lisp

(define-leo-class A1 ()
  a
  (def-method f1 (x) (+ (/ a x) 17))
  (def-method f2 (m n) (+ (f1@ m) n)))

(define-leo-class A2 (A1)
  (def-method f1_with_10_offset (x) :override f1 (+ (/ a x) 10))
  (def-method f2a (m n)
    (+ 
      (f1_with_10_offset@ n) ;; static, predictable results
      11
      ))
  (def-method f2b (m n)
    (+ 
      (f1@ n) ;; can be overridden, unpredictable results
      11
      )))

(define-leo-class A3 (A2)
  (def-method f1 (_x)
    0 ;; override A2::f1 but not A2::f1_with_10_offset
    ))

```


#### More Class Examples

```lisp

;; new class person with no super-class
(define-leo-class person ()
  :visible
  ;; all slots defined after this are externally-accessible but not externally-settable.
  first-name ;; a data slot
  last-name ;; a data slot
  :public
  ;; all slots defined after this are externally-accessible and externally-settable.
  ;; this is the default protection level for slots
  driver-license-no
  :protected
  ;; all slots defined after this can only be read/written from methods BY THE INSTANCE.
  social-insurance-no       ;; protected data slot

 ;; method full-name, methods are always accessible
  (def-method full-name ()
    (concat first-name " " last-name))

  (def-method say (&optional something)
    (message "%s says%s%s"
		 ;; call method using internal name faster than: (full-name self)
	     (full-name@)
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


(define-leo-class employee (person)	;; new class employee inheriting class person
  id		  	   		;; new public slot
  (def-method slack ()
    (message "employees never slack"))

  (def-method say (&optional something)
    (message "employee %d (%s) says%s%s"
	     id
	     first-name
	     (if something ": " " ") 
	     (or something "nothing"))))
	
```
