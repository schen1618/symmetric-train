; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
#lang racket
<<<<<<< HEAD
(require "functionParser.rkt")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (parser file) (newenvironment) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
=======
(provide (all-defined-out))
(require "simpleParser.rkt")
;;;; ******************************************************************************************
;;;;   Intepreter, part 2
;;;;   EECS 345
;;;;   Sherry Chen, Chris Toomey
;;;;   *TO USE: (interpret "filename")
;;;; ******************************************************************************************

(define init-state '((() ())))
(define empty-layer '(() ()))

;; takes an expression of numbers/variables and operators and returns the value
;; The operators are +, -, *, /, % and division is integer division
(define m-value
  (lambda (exp state return break continue)
    (cond
      [(null? exp)                    (error 'undefined "undefined expression")]
      [(number? exp)                  exp]
      [(equal? (get exp state)
               'novalue)              (error 'error "variable not assigned")]
      [(and (not (list? exp))
            (equal? 'notfound
                    (get exp state))) (error 'error "variable not declared")]
      [(instate? exp state)           (get exp state)]
      [(eq? (operator exp) '+)        (+ (m-value (left-operand exp) state return break continue)
                                         (m-value (right-operand exp) state return break continue))]
      [(and (eq? (operator exp) '-)
            (null? (cddr exp)))       (* (m-value (left-operand exp) state return break continue) -1)]
      [(eq? (operator exp) '-)        (- (m-value (left-operand exp) state return break continue)
                                         (m-value (right-operand exp) state return break continue))]
      [(eq? (operator exp) '*)        (* (m-value (left-operand exp) state return break continue)
                                         (m-value (right-operand exp) state return break continue))]
      [(eq? (operator exp) '/)        (quotient (m-value (left-operand exp) state return break continue)
                                                (m-value (right-operand exp) state return break continue))]
      [(eq? (operator exp) '%)        (remainder (m-value (left-operand exp) state return break continue)
                                                 (m-value (right-operand exp) state return break continue))])))

(define operator car)
(define left-operand cadr)
(define right-operand caddr)

;; takes an expression and evealuates whether it is true or false
;; the different comparison operators are ==, !=, >, >=, <, <=, &&, ||, !
(define m-bool
  (lambda (exp state return break continue)
>>>>>>> 2ea59df29b0f69b0a11c8cc5f864a2da5524b7a2
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
<<<<<<< HEAD
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw))))
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list
                                                 (get-body catch-statement)
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return
                                                 (lambda (env2) (break (pop-frame env2)))
                                                 (lambda (env2) (continue (pop-frame env2)))
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
=======
      [(null? statement)           (error 'error "undefined expression")]
      [(null? (dec-exp statement)) (add (dec-var statement) 'novalue state)]
      [(list? (dec-exp statement)) (add (dec-var statement)
                                        (m-eval (dec-value statement) state return break continue)
                                        state)]
      [else                        (add (dec-var statement) (dec-value statement) state)])))

(define dec-exp cddr)
(define dec-var cadr)
(define dec-value caddr)

;; assigns a value to a variable for a layer
(define m-assign
  (lambda (statement state return break continue)
    (cond
      [(null? statement)            (error 'error "undefined expression")]
      [(instate? (ass-var statement)
                 state)             (update (ass-var statement) (m-eval (ass-value statement) state return break continue) state)]
      [else                         (error 'error "variable not declared")])))

(define equal-sign car)
(define ass-var cadr)
(define ass-value caddr)

;; returns an evaluated expression
(define m-return
  (lambda (statement state return break continue)
>>>>>>> 2ea59df29b0f69b0a11c8cc5f864a2da5524b7a2
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment)
    (cond
<<<<<<< HEAD
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

;; Creates the global environment
(define global-environment
  (lambda
      (statement environment return break continue throw)
    (set-box! (box newenvironment))
    (cond
      ((null? statement) environment)
      ((eq? (statement-type statement) 'var) (interpret-declare statement environment))
      ((eq? (statement-type statement) '=) (interpret-assign statement environment))
      ((eq? (statement-type statement) 'function) (function-environment (car statement) environment)))))

;; Returns the environment of a function
(define function-environment
  (lambda (statement environment)
  (add-to-frame (cadr statement) (function-evaluation (caddr statement) (cadddr statement) environment return break continue throw) environment)))

;; Evaluates a function
(define function-evaluation
  (lambda
      (parameters body environment return break continue throw)
    (set-box! box newenvironment)
    (interpret-statement-list body (add-params-to-scope parameters environment) return break continue throw)))

;; Adds the parameters of the input function to the current scope
(define add-params-to-scope
  (lambda
      (parameters environment)
    (cond
      ((null? (car parameters)) environment)
      ((exists-in-list? (car parameters) environment) (add-params-to-scope (cdr parameters) (insert (car environment) (cadr environment) environment)))
      (else (add-params-to-scope (cdr parameters) environment)))))
=======
      [(null? statement)                           (error 'error "undefined statement")]
      [(eq? #t (m-bool (if-cond statement) state return break continue)) (m-state (then-statement statement) state return break continue)]
      [(not (null? (else statement)))              (m-state (else-statement statement) state return break continue)]
      [else                                        state])))

(define if-cond cadr)
(define then-statement caddr)
(define else cdddr)
(define else-statement cadddr)

;; loop that evaluates the statement until the condition is no longer true
(define m-while
  (lambda (statement state return break continue)
    (call/cc
     (lambda (break)
    (cond
      [(null? statement)              (error 'error "undefined statement")]
      [(eq? #t (m-bool
                (while-cond statement)
                state return break continue))               (m-while statement (call/cc (lambda (continue) (m-state (while-statement statement) state return break continue)))
                                                                     return break continue)]
      [else                           state])))))

(define while-cond cadr)
(define while-statement caddr)

;; evaluates the block of statements between the brackets
(define m-block
  (lambda (statement state return break continue)
    (delete-layer (interpret-state-list (cdr statement) (add-layer state) return break continue))))

;; tries to evaluate the block of code, and if there is a throw condition in the block, immediately exists to catch,
;; and either way evaluates the finally block afterward
(define try
  (lambda (state statement return continue break throw)
    (return)))

;; m-state calls the appropriate function to evaluate the statement
(define m-state
  (lambda (statement state return break continue)
    (cond
      [(eq? (identifier statement) 'var)    (m-declare statement state return break continue)]
      [(eq? (identifier statement) '=)      (m-assign statement state return break continue)]
      [(eq? (identifier statement) 'return) (m-return statement state return break continue)]
      [(eq? (identifier statement) 'break)  (break (delete-layer state))]
      [(eq? (identifier statement) 'continue) (continue (delete-layer state))]
      [(eq? (identifier statement) 'if)     (m-if statement state return break continue)]
      [(eq? (identifier statement) 'while)  (m-while statement state return break continue)]
      [(eq? (identifier statement) 'begin)  (m-block statement state return break continue)]
      [else                                 (error 'error "undefined expression")])))

(define identifier car)

;; state-list takes a list of statements (lists) as an argument
;; and returns the evaluated collection of statements
(define interpret-state-list
  (lambda (tree state return break continue)
    (cond
      [(null? tree) state]
      [else (interpret-state-list (cdr tree) (m-state (car tree) state return break continue) return break continue)])))


;; interpret takes a filename and runs it through state-list
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (interpret-state-list (parser filename) init-state return 'nobreak 'nocontinue)))))

>>>>>>> 2ea59df29b0f69b0a11c8cc5f864a2da5524b7a2

;-----------------
; HELPER FUNCTIONS
;-----------------

<<<<<<< HEAD
; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
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
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
=======
;; adds a varaible/value pair to a layer
(define add-to-layer
  (lambda (var value layer)
    (cond
      [(instate-layer var layer) (error 'error "variable already defined")]
      [else                 (list (cons var (add-var layer)) (cons value (add-value layer)))])))

(define add-var car)
(define add-value cadr)

;; adds variable/value pair to the state
(define add
  (lambda (var value state)
    (cons (add-to-layer var value (car state)) (cdr state))))

;; removes variable/value pair from a layer, wrapper for remove-acc
(define remove-from-layer
  (lambda (var layer)
    (remove-acc var layer empty-layer)))

;; removes variable/state pair from state
(define remove-acc
  (lambda (var state acc)
    (cond
      [(null? (variable-list state)) acc]
      [(eq? var (variable state))    (remove-acc var (list (rest-of-variable-list state)
                                                           (rest-of-value-list state)) acc)]
      [else                          (remove-acc var (list (rest-of-variable-list state)
                                                           (rest-of-value-list state))
                                                 (list (cons (variable state) (variable-list acc))
                                                       (cons (var-value state) (values acc))))])))

(define values cadr)

;; determines if the variable is in state in that layer
(define instate-layer
  (lambda (var layer)
    (cond
      [(null? (variable-list layer)) #f]
      [(eq? var (variable layer))    #t]
      [else                          (instate-layer var (cons (rest-of-variable-list layer)
                                                         (value-list layer)))])))

(define variable-list car)
(define variable caar)
(define rest-of-variable-list cdar)
(define value-list cdr)

;; determines if the variable has been declared in the program
(define instate?
  (lambda (var state)
>>>>>>> 2ea59df29b0f69b0a11c8cc5f864a2da5524b7a2
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
 
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

<<<<<<< HEAD
; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
=======
(define update
  (lambda (var val state)
    (cond
      [(null? state) (error 'error "Variable does not exist")]
      [(instate-layer var (car state)) (cons (add-to-layer var val (remove-from-layer var (car state))) (cdr state))]
      [else (cons (car state) (update var val (cdr state)))])))

;; evaluates an expression
(define m-eval
  (lambda (exp state return break continue)
>>>>>>> 2ea59df29b0f69b0a11c8cc5f864a2da5524b7a2
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
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