#lang racket
(provide (all-defined-out))
(require "simpleParser.rkt")
;;;; ******************************************************************************************
;;;;   Intepreter, part 2
;;;;   EECS 345
;;;;   Sherry Chen, Chris Toomey
;;;;   *TO USE: (interpret "filename")
;;;; ******************************************************************************************


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
(define init-state '((() ())))
(define empty-layer '(() ()))

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

;; removes variable/value pair from the top layer
(define remove
  (lambda (var state)
    (cons (remove-from-layer var (car state)) (cdr state))))


;; takes an expression and evealuates whether it is true or false
;; the different comparison operators are ==, !=, >, >=, <, <=, &&, ||, !
(define m-bool
  (lambda (exp state return break continue)
    (cond
      [(null? exp)                    (error 'undefined "undefined expression")]
      [(number? exp)                  exp]
      [(instate? exp state)           (get exp state)]
      [(eq? exp #t)                   #t]
      [(eq? exp #f)                   #f]
      [(equal? exp 'true)             #t]
      [(equal? exp 'false)            #f]
      [(equal? (get exp state)
               'novalue)              (error 'error "variable not assigned")]
      [(and (not (list? exp))
            (equal? 'notfound
                    (get exp state))) (error 'error "variable not declared")]
      [(eq? (operator exp) '==)       (equal? (m-eval (left-operand exp) state return break continue)
                                              (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '!=)       (not (equal? (m-eval (left-operand exp) state return break continue)
                                                   (m-eval (right-operand exp) state return break continue)))]
      [(eq? (operator exp) '>)        (> (m-eval (left-operand exp) state return break continue)
                                         (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '>=)       (>= (m-eval (left-operand exp) state return break continue)
                                          (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '<)        (< (m-eval (left-operand exp) state return break continue)
                                         (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '<=)       (<= (m-eval (left-operand exp) state return break continue)
                                          (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '&&)       (and (m-eval (left-operand exp) state return break continue)
                                           (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '||)       (or (m-eval (left-operand exp) state return break continue)
                                          (m-eval (right-operand exp) state return break continue))]
      [(eq? (operator exp) '!)        (not (m-eval (left-operand exp) state return break continue))]
      [else                           'invalid_expression])))


;; declares a variable for a layer
(define m-declare
  (lambda (statement state return break continue)
    (cond
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
                 state)             (m-update (ass-var statement) (m-eval (ass-value statement) state return break continue) state)]
      [else                         (error 'error "variable not declared")])))

(define equal-sign car)
(define ass-var cadr)
(define ass-value caddr)

;; returns an evaluated expression
(define m-return
  (lambda (statement state return break continue)
    (cond
      [(null? statement)                           (error 'error "undefined expression")]
      [(eq? #t (m-eval (ret-exp statement) state return break continue)) (return 'true)]
      [(eq? #f (m-eval (ret-exp statement) state return break continue)) (return 'false)]
      [else                                        (return (m-eval (ret-exp statement) state return break continue))])))

(define ret-exp cadr)

;; returns the evaluated first statement if true or the second statement if false,
;; if it exists
(define m-if
  (lambda (statement state return break continue)
    (cond
      [(null? statement)                           (error 'error "undefined statement")]
      [(eq? #t (m-bool (if-cond statement) state return break continue)) (m-state (then-statement statement) state return break continue)]
      [(not (null? (else statement)))              (m-state (else-statement statement) state return break continue)]
      [else                                        state])))

(define if-cond cadr)
(define then-statement caddr)
(define else cdddr)
(define else-statement cadddr)

;; loop that evaluates the statement until the condition is no longer true
(define m-while-helper
  (lambda (statement state return break continue)
    (cond
      [(null? statement)              (error 'error "undefined statement")]
      [(eq? #t (m-bool
                (while-cond statement)
                state return break continue))               (m-while statement (m-state (while-statement statement) state return break continue)
                                                                     return break continue)]
      [else                           state])))

(define m-while
  (lambda (statement state return break continue)
    (call/cc
     (lambda (break)
       (m-while-helper statement state return break continue)))))

(define while-cond cadr)
(define while-statement caddr)

;; evaluates the block of statements between the brackets
(define m-block
  (lambda (statement state return break continue)
    (delete-layer (interpret-state-list (cdr statement) (add-layer state) return break continue))))

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
       (interpret-state-list (parser filename) init-state return 'x 'x)))))


;;;; ******************************************************************************************
;;;;   helper methods
;;;; ******************************************************************************************

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
    (cond
      [(null? state) #f]
      [(eq? #t (instate-layer var (car state))) #t]
      [else (instate? var (cdr state))])))


;; gets value of a variable from state
(define get-layer
  (lambda (var layer)
    (cond
      [
       (null? (variable-list layer)) 'notfound]
      [(eq? var (variable layer))    (var-value layer)]
      [else                          (get-layer var (list (rest-of-variable-list layer)
                                                    (rest-of-value-list layer)))])))

(define var-value caadr)
(define rest-of-value-list cdadr)

;; gets a value from the closest layer
(define get
  (lambda (var state)
    (cond
      [(null? state)                           'notfound]
      [(not (eq? 'notfound
                 (get-layer var (car state)))) (get-layer var (car state))]
      [else                                    (get var (cdr state))])))

(define m-update
  (lambda (var val state)
    (cond
      [(null? state) (error 'error "Variable does not exist")]
      [(instate-layer var (car state)) (cons (add-to-layer var val (remove-from-layer var (car state))) (cdr state))]
      [else (cons (car state) (m-update var val (cdr state)))])))


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

;; evaluates an expression
(define m-eval
  (lambda (exp state return break continue)
    (cond
      [(equal? 'invalid_expression
               (m-bool exp state return break continue)) (m-value exp state return break continue)]
      [else                        (m-bool exp state return break continue)])))

;; deletes the top layer of the state
(define delete-layer
  (lambda (state)
    (next state)))

(define next cdr)

;; adds a layer to the state
(define add-layer
  (lambda (state)
    (add-list empty-layer state)))

(define add-list cons)
