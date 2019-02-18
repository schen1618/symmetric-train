#lang racket
(require "simpleParser.rkt")
;;;; ******************************************************************************************
;;;;   Intepreter, part 1
;;;;   EECS 345
;;;;   Sherry Chen, Chris Toomey
;;;;   *TO USE: (interpret "filename")
;;;; ******************************************************************************************

    
;; takes an expression of numbers/variables and operators and returns the value
;; The operators are +, -, *, /, % and division is integer division
(define m-value
  (lambda (exp state)
    (cond
      [(null? exp)                    (error 'undefined "undefined expression")]
      [(number? exp)                  exp]
      [(equal? (get exp state)
               'novalue)              (error 'error "variable not assigned")]
      [(and (not (list? exp))
            (equal? 'notfound
                    (get exp state))) (error 'error "variable not declared")]
      [(instate? exp state)           (get exp state)]
      [(eq? (operator exp) '+)        (+ (m-value (left-operand exp) state)
                                         (m-value (right-operand exp) state))]
      [(and (eq? (operator exp) '-)
            (null? (cddr exp)))       (* (m-value (left-operand exp) state) -1)]
      [(eq? (operator exp) '-)        (- (m-value (left-operand exp) state)
                                         (m-value (right-operand exp) state))]
      [(eq? (operator exp) '*)        (* (m-value (left-operand exp) state)
                                         (m-value (right-operand exp) state))]
      [(eq? (operator exp) '/)        (quotient (m-value (left-operand exp) state)
                                                (m-value (right-operand exp) state))]
      [(eq? (operator exp) '%)        (remainder (m-value (left-operand exp) state)
                                                 (m-value (right-operand exp) state))])))

(define operator car)
(define left-operand cadr)
(define right-operand caddr)
(define init-state '(() ()))

;; adds variable/value pair to state
(define add
  (lambda (var value state)
    (cond
      [(instate? var state) (error 'error "variable already defined")]
      [else                 (list (cons var (add-variable state)) (cons value (add-value state)))])))

(define add-variable car)
(define add-value cadr)

;; removes variable/value pair from state, wrapper for remove-acc
(define remove
  (lambda (var state)
    (remove-acc var state init-state)))


;; error if there is no expression to evaluate
;;takes an expression and evealuates whether it is true or false
;;the different comparison operators are ==, !=, >, >=, <, <=, &&, ||, !
;;the operator, left-operand, and right-operand are the same as used in m-value
(define m-bool
  (lambda (exp state)
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
      [(eq? (operator exp) '==)       (equal? (m-eval (left-operand exp) state)
                                              (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '!=)       (not (equal? (m-eval (left-operand exp) state)
                                                   (m-eval (right-operand exp) state)))]
      [(eq? (operator exp) '>)        (> (m-eval (left-operand exp) state)
                                         (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '>=)       (>= (m-eval (left-operand exp) state)
                                          (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '<)        (< (m-eval (left-operand exp) state)
                                         (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '<=)       (<= (m-eval (left-operand exp) state)
                                          (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '&&)       (and (m-eval (left-operand exp) state)
                                           (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '||)       (or (m-eval (left-operand exp) state)
                                          (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '!)        (not (m-eval (left-operand exp) state))]
      [else                           'invalid_expression])))


;; declares a variable
(define m-declare
  (lambda (statement state)
    (cond
      [(null? statement)           (error 'error "undefined expression")]
      [(null? (dec-exp statement)) (add (dec-variable statement) 'novalue state)]
      [(list? (dec-exp statement)) (add (dec-variable statement) (m-eval (dec-value statement) state) state)]
      [else                        (add (dec-variable statement) (dec-value statement) state)])))

(define dec-exp cddr)
(define dec-variable cadr)
(define dec-value caddr)

;; assigns a value to a variable
(define m-assign
  (lambda (statement state)
    (cond
      [(null? statement)                         (error 'error "undefined expression")]
      [(instate? (ass-variable statement) state) (m-declare (list (equal-sign statement)
                                                                  (ass-variable statement)
                                                                  (m-eval (ass-value statement) state))
                                                            (remove (ass-variable statement) state))]
      [else                                      (error 'error "variable not declared")])))

(define equal-sign car)
(define ass-variable cadr)
(define ass-value caddr)
  
;; returns an evaluated expression
(define m-return
  (lambda (statement state)
    (cond
      [(null? statement)                           (error 'error "undefined expression")]
      [(eq? #t (m-eval (ret-exp statement) state)) (add 'return 'true state)]
      [(eq? #f (m-eval (ret-exp statement) state)) (add 'return 'false state)]
      [else                                        (add 'return (m-eval (ret-exp statement) state) state)])))

(define ret-exp cadr)

;; if statement that returns the evaluated first statement if true or the second statement if false, if it exists
(define m-if
  (lambda (statement state)
    (cond
      [(null? statement)                           (error 'error "undefined statement")]
      [(eq? #t (m-bool (if-cond statement) state)) (m-state (then-statement statement) state)]
      [(not (null? (else statement)))              (m-state (else-statement statement) state)]
      [else                                        state])))

(define if-cond cadr)
(define then-statement caddr)
(define else cdddr)
(define else-statement cadddr)

;; while loop that evaluates the statement until the condition is no longer true
(define m-while
  (lambda (statement state)
    (cond
      [(null? statement)                              (error 'error "undefined statement")]
      [(eq? #t (m-bool (while-cond statement) state)) (m-while statement (m-state (while-statement statement) state))]
      [else                                           state])))

(define while-cond cadr)
(define while-statement caddr)

;; m-state calls the appropriate function to evaluate the statement
(define m-state
  (lambda (statement state)
    (cond
      [(eq? (identifier statement) 'var)    (m-declare statement state)]
      [(eq? (identifier statement) '=)      (m-assign statement state)]
      [(eq? (identifier statement) 'return) (m-return statement state)]
      [(eq? (identifier statement) 'if)     (m-if statement state)]
      [(eq? (identifier statement) 'while)  (m-while statement state)]
      [else                                 (error 'error "undefined expression")])))

(define identifier car)

;; interpret-tree takes a list of statements (lists) as an argument
;; and returns the evaluated collection of statements
(define interpret-tree
  (lambda (tree state)
    (cond
      [(null? tree) (get 'return state)]
      [(pair? tree) (interpret-tree (cdr tree) (m-state (car tree) state))])))
      

;; interpret takes a filename and runs it through interpret-tree
(define interpret
  (lambda (filename)
    (interpret-tree (parser filename) init-state)))


;;;; ******************************************************************************************
;;;;   helper methods
;;;; ******************************************************************************************

;; determines if the variable is in state
(define instate?
  (lambda (var state)
    (cond
      [(null? (variable-list state)) #f]
      [(eq? var (variable state))    #t]
      [else                          (instate? var (cons (rest-of-variable-list state)
                                                         (value-list state)))])))

(define variable-list car)
(define variable caar)
(define rest-of-variable-list cdar)
(define value-list cdr)

;; gets value of a variable from state
(define get
  (lambda (var state)
    (cond
      [(null? (variable-list state)) 'notfound]
      [(eq? var (variable state))    (variable-value state)]
      [else                          (get var (list (rest-of-variable-list state)
                                                    (rest-of-value-list state)))])))

(define variable-value caadr)
(define rest-of-value-list cdadr)

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
                                                       (cons (variable-value state) (values acc))))])))

(define values cadr)

;; evaluates an expression
(define m-eval
  (lambda (exp state)
    (cond
      [(equal? 'invalid_expression (m-bool exp state)) (m-value exp state)]
      [else                                            (m-bool exp state)])))        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
