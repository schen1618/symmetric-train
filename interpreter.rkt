#lang racket
(require "classParser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sherry Chen and Chris Toomey
;; EECS 345 Interpreter Part 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda
      (file classname)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-class-list (parser file) (newworld) (string->symbol classname) return
                              (lambda (env) (myerror "Break used outside of loop"))
                              (lambda (env) (myerror "Continue used outside of loop"))
                              (lambda (v env) (myerror "Uncaught exception thrown"))))))))
    
; Places the classes in class closures and then runs main when there are no more classes
(define interpret-class-list
  (lambda (statement-list world classname return break continue throw)
    (cond
      ((null? statement-list) (evaluate-main classname world return break continue throw))
      (else (interpret-class-list (rest-of statement-list) (eval-class (first statement-list) world return break continue throw) classname return break continue throw)))))

(define first car)
(define rest-of cdr)

; Interprets the functions in the global environment and stores them in the bottom layer
;;(define interpret-functions-global-list
  ;;(lambda
  ;    (statement-list environment world return break continue throw)
  ;  (cond
  ;    ((null? statement-list) (evaluate-main environment world return break continue throw))
  ;    (else                   (interpret-functions-global-list (rest-of-statement-list statement-list)
  ;                                       (interpret-statement (first-statement statement-list)
  ;                                                            world environment return break continue throw)
   ;                                      return break continue throw)))))

;; Creates a list of the input function with the name as the variable and the parameters and body as the value
(define add-function-to-env
  (lambda
      (statement class environment)
    (insert (name statement) (list (cons 'this (params statement)) (body statement) class) environment)))

;; Wrapper for a function that creates the params and list of statements
(define interpret-function
  (lambda
      (statement class environment return break continue throw)
    (cond
      ((null? (func statement)) environment)
      (else   (add-function-to-env statement class environment)))))

;; Once everything that is global is in the bottom layer, this function evaluates the main method if it exists
(define evaluate-main
  (lambda
      (classname world return break continue throw)
    (interpret-statement-list (cadr (find-main classname world return break continue throw)) classname world
                                                                   (find-main-env classname world)
                                                                   return break continue throw)))

;; Helper function to find the environment of the main method
(define find-main-env
  (lambda (classname world)
      (push-frame (main-environment (lookup-in-world classname world)))))

(define main-environment caddr)

;; Takes in a list of arguments and binds them to the parameters for the function
(define bind-arguments
  (lambda
      (params-list args-list environment throw)
    (cond
      ((and (null? params-list) (null? args-list))        environment)
      ((and (not (null? params-list)) (null? args-list)) (myerror "Error: missing input arguments"))
      ((and (null? params-list) (not (null? args-list))) (myerror "Error: too many input arguments"))
      (else (bind-arguments (rest-of-list params-list)   (rest-of-list args-list) (insert (first-param params-list)
                                                                                        (eval-expression (first-param args-list)
                                                                                                         (pop-frame environment) throw)
                                                                                        environment) throw)))))

;; Finds the parameters of an input function
(define find-params
  (lambda
      (name environment)
    (params-list (lookup name environment))))

;; Finds the list of statements for an input function
(define find-function-closure
  (lambda
      (name environment)
    (body-list (lookup name environment))))

;; Evaluates the list of statements in a function
(define interpret-function-statement-list
  (lambda
      (statement-list class world environment return break continue throw)
    (cond
      ((null? statement-list) (pop-frame environment))
      (else                   (interpret-function-statement-list (rest-of-list statement-list)
                                               (interpret-statement (first-statement statement-list) class world environment
                                                                    return break continue throw)
                                               return break continue throw)))))

;; Evaluates a function that is being called in the main method with the input arguments
(define interpret-funcall
  (lambda
      (funcall class world environment throw)
    (call/cc
     (lambda (function-return)
       (cond
         ((not (exists? (function-name funcall)
                        environment))          (myerror "Error: function does not exist"))
         
         ((null? (parameters funcall))         (interpret-function-statement-list
                                                (statement-list-of-function (find-function-in-world (function-name funcall) class world throw))
                                                (push-frame (pop-frame environment)) function-return breakError continueError throw))
         (else                                 (interpret-function-statement-list (find-function-in-world (function-name funcall) class world throw)
                                                                                  (bind-arguments (find-params (car funcall) environment)
                                                                                                  (parameters funcall) (push-frame environment)
                                                                                                  throw) function-return breakError
                                                                                                         continueError throw)))))))

;; Returns the environment after a function is evaluated
(define create-funcall-environment
  (lambda (statement-list class world environment return break continue throw)
    (cond
      ((null? statement-list) environment)
      ((not (list? (call/cc
                    (lambda (return)
                      (create-funcall-environment (rest-of-statement-list statement-list)
                                                             (interpret-statement (first-statement statement-list) class world
                                                                                  environment return break continue throw)
                                                             return break continue throw)))))
                             environment)
      (else                  environment))))

; Interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda
      (statement-list class world environment return break continue throw)
    (cond
      ((null? statement-list) environment)
      (else                   (interpret-statement-list (cdr statement-list) class world
                                                        (interpret-statement (car statement-list)
                                                                             class world environment return break continue throw)
                                                        return break continue throw)))))

; Interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda
      (statement class world environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement class world environment return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement class world environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement class world environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement class world environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement class world environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement class world environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement class world environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement class world environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function (without-function-identifier statement) class environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (create-funcall-environment (function-statement-list (lookup (function-name (without-function-identifier statement)) environment))
                                                                             class world
                                                                             (bind-arguments (get-parameters (lookup (function-name (without-function-identifier statement)) environment))
                                                                                                           (parameters (without-function-identifier statement)) (push-frame environment) throw)
                                                                            return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

(define without-function-identifier cdr)
(define function-name car)
(define get-parameters car)
(define function-statement-list cadr)
(define parameters cdr)

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda
      (statement class world environment return throw)
    (return (eval-expression (get-expr statement) class world environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda
      (statement class world environment throw)
    (cond
      ((exists-declare-value? statement) (insert (get-declare-var statement) (eval-expression (get-declare-value statement) class world environment throw) environment))
      (else (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda
      (statement class world environment throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) class world environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda
      (statement class world environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) class world environment throw) (interpret-statement (get-then statement) class world environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) class world environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda
      (statement class world environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition class world environment throw)
                            (loop condition body (interpret-statement body class world environment return break (lambda
                                                                                                        (env)
                                                                                                      (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda
      (statement class world environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         class world
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda
      (statement class world environment throw)
    (throw (eval-expression (get-expr statement) class world environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
; Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda
      (catch-statement class world environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env)
                                 (throw ex (interpret-block finally-block env return break continue throw))))
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list
                                                 (get-body catch-statement)
                                                 class world
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return
                                                 (lambda (env2) (break (pop-frame env2)))
                                                 (lambda (env2) (continue (pop-frame env2)))
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda
      (statement class world environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) class environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; Helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda
      (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda
      (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Looks up the class closure for a given class name
(define lookup-in-world
  (lambda (name world)
    (lookup-in-frame name world)))

; Find the main method in the world
(define find-main
  (lambda (classname world return break continue throw)
    (cond
      ((null? (first-statement world)) ('error "No main method found"))
      ((eq?  (main-function-name (main-environment (lookup-in-world classname world))) 'main) (lookup-in-env
                                                                                               (main-function-name (main-environment
                                                                                                                    (lookup-in-world classname world)))
                                                                                               (main-environment (lookup-in-world (functions-list world) world))))
      (else (find-main classname (cdar world) return break continue throw)))))

(define first-statement car)
(define main-function-name caaar)
(define functions-list caar)

; Evaluates the classes from class list
(define eval-class
  (lambda (statement world return break continue throw)
    (cond
      ((null? statement) world)
      ((and (null? (super statement)) (eq? (class-identifier statement) 'class)) (add-to-frame (class-name statement) (create-class-closure (class-name statement) '()
                                                                                         (class-body statement) return break continue throw) world))
      ((and (eq? (class-identifier statement) 'class) (eq? (extends-identifier statement) 'extends)) (add-to-frame (class-name statement) (create-class-closure
                                                                                                            (class-name statement)
                                                                                                            (parent statement)
                                                                                                            (class-body statement) return break
                                                                                                                                 continue throw) world))
      (else 'error "invalid class"))))

(define class-identifier car)
(define super caddr)
(define extends-identifier caaddr)
(define class-name cadr)
(define class-body cdddr)
(define parent
  (lambda (statement)
  (cadr (caddr statement))))

; Adds the methods in a class to the class closure
(define add-class-methods
  (lambda (name body environment return break continue throw)
    (cond
      ((null? body) environment)
      ((or (eq? (function-identifier body) 'function) (eq? (function-identifier body) 'static-function)) (add-class-methods name (rest-of-class body)
                                                                                               (interpret-function (function-body body)
                                                                                                                   name environment return break continue throw)
                                                                                               return break continue throw))
      (else (add-class-methods name (rest-of-class body) environment return break continue throw)))))

(define function-identifier caaar)
(define rest-of-class cdr)
(define function-body cdaar)

; Finds the fields in a class
(define add-class-fields
  (lambda (name body frame)
    (cond
      ((null? body) frame)
      ((and (eq? (variable-identifier body) 'var) (null? (val-of-var body))) (add-class-fields name (rest-of-class body) (add-to-frame (variable-name body) 'novalue frame)))
      ((eq? (variable-identifier body) 'var) (add-class-fields name (rest-of-class body) (add-to-frame (variable-name body) (val-of-var body) frame)))
      (else (add-class-fields name (rest-of-class body) frame)))))

(define variable-identifier caaar)
(define variable-name caadar)

(define val-of-var
  (lambda (statement)
      (caddr (caar statement))))

; Creates a class closure which holds the super class, methods, and instance fields of a class
(define create-class-closure
  (lambda (name parent body return break continue throw)
    (cond
      ((null? parent) (combine-class-elements '() (list (add-class-fields name body (newframe)) (add-class-methods name body
                                                                                                                     (newenvironment) return break continue throw))))
      (else (combine-class-elements parent (list (add-class-fields name body (newframe)) (add-class-methods name body (newenvironment) return break continue throw)))))))

(define combine-class-elements cons)

; If there is a 'new statement, creates a closure of the type and the instance field values
(define interpret-new
  (lambda (type world environment throw)
    (cond
      ((null? type) ('error "No new type"))
      (else (list type (instance-field-values (lookup-in-world type world)))))))

(define instance-field-values cadr)

(define find-dot-instance
  (lambda (statement class world environment throw)
    (cond
      ((null? (car statement)) ('error "No type"))
      ((null? (cadr statement)) ('error "No variable/method"))
      (else (eval-expression (cadr statement) (append (lookup-in-env (car statement) environment) environment) throw)))))

(define find-function-in-world
  (lambda (name class world throw)
    (cond
      ((null? (first-statement world)) ('error "No main method found"))
      ((eq?  (main-function-name (main-environment (lookup-in-world class world))) name) (lookup-in-env
                                                                                               (main-function-name (main-environment
                                                                                                                    (lookup-in-world class world)))
                                                                                               (main-environment (lookup-in-world (functions-list world) world))))
      (else (find-function-in-world name class (cdar world) throw)))))


; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda
      (expr class world environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr class world environment throw)))))


; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda
      (expr class world environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) class world environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) class world environment throw)))
      ((eq? 'new (operator expr)) (interpret-new (without-new-identifier expr) world environment throw))
      ((eq? 'dot (operator expr)) (find-dot-instance (without-dot-identifier expr) class world environment throw))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) class world environment throw) class world environment throw)))))

(define without-dot-identifier cdr)
(define without-new-identifier cadr)

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda
      (expr op1value class world environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) class world environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) class world environment throw)))
      ((eq? 'funcall (operator expr)) (interpret-funcall (statement-without-funcall expr) world environment throw))
      (else (myerror "Unknown operator:" (operator expr))))))

(define statement-without-funcall cdr)

;; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda
      (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

;; Returns an error if there is a break statement outside of a loop
(define breakError
  (lambda
      (v)
    (myerror "Break used outside loop")))

;; Returns an error if there is a continue statement outside of a loop
(define continueError
  (lambda
      (v)
    (myerror "Continue used outside of loop")))

;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the different parts of a function
(define rest-of-statement-list cdr)
(define name car)
(define params cadr)
(define body caddr)
(define func cdr)
(define rest-of-list cdr)
(define first-param car)
(define params-list car)
(define body-list cadr)
(define statement-list-of-function cadr)

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda
      (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda
      (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda
      (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; creates a new world that holds all the classes
(define newworld
  (lambda ()
    '(() ())))

; create a new empty environment
(define newenvironment
  (lambda
      ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda
      ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda
      (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda
      (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda
      (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda
      (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda
      (var environment)
    (lookup-variable var environment)))
 
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda
      (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

(define lookup-env
  (lambda (name world)
    (caddr (lookup-in-frame name world))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda
      (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (unbox (get-value (indexof var (variables frame)) (store frame))))))))

; Get the location of a name in a list of names
(define indexof
  (lambda
      (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda
      (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda
      (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda
      (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Adds the class to a global state
(define add-to-state
  (lambda (name closure state)
    (cons name (car (cons closure (cadr state)))))) 

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda
      (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda
      (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (begin (set-box! (car vallist) (scheme->language val)) vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))
; Returns the list of variables from a frame
(define variables
  (lambda
      (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda
      (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda
      (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda
      (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))
