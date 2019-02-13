#lang racket
(require "simpleParser.rkt")
;;;; ******************************************************************************************
;;;;   Intepreter, part 1
;;;;   EECS 345
;;;;   Sherry Chen, Chris Toomey
;;;; ******************************************************************************************

;;;; ******************************************************************************************
;;;;   helper methods
;;;; ******************************************************************************************

;; determines if the variable is in state
(define instate?
  (lambda (var state)
    (cond
      [(null? (car state))    #f]
      [(eq? var (caar state)) #t]
      [else                   (instate? var (cons (cdar state) (cdr state)))])))

;; gets value of a variable from state
(define get
  (lambda (var state)
    (cond
      [(null? (car state))    'notfound]
      [(eq? var (caar state)) (caadr state)]
      [else                   (get var (list (cdar state) (cdadr state)))])))

;; removes variable/state pair from state
(define remove-acc
  (lambda (var state acc)
    (cond
      [(null?                 (car state)) acc]
      [(eq? var (caar state)) (remove-acc var (list (cdar state) (cdadr state)) acc)]
      [else                   (remove-acc var (list (cdar state) (cdadr state)) (list (cons (caar state) (car acc)) (cons (caadr state) (cadr acc))))])))

;; evaluates an expression
(define m-eval
  (lambda (exp state)
    (cond
      [(equal? 'invalid_expression (m-bool exp state)) (m-value exp state)]
      [else                                            (m-bool exp state)])))

;;;; ******************************************************************************************
;;;;   end of helper methods
;;;; ******************************************************************************************

;; error for when variable is not found in state?       
;; takes an expression of numbers/variables and operators and returns the value
;; The operators are +, -, *, /, % and division is integer division
(define m-value
  (lambda (exp state)
    (cond
      [(null? exp)             (error 'undefined "undefined expression")]
      [(number? exp)           exp]
      [(instate? exp state)    (get exp state)]
      [(eq? (operator exp) '+) (+ (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '-) (- (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '*) (* (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '/) (quotient (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '%) (remainder (m-value (left-operand exp) state) (m-value (right-operand exp) state))])))

(define operator car)
(define left-operand cadr)
(define right-operand caddr)
(define init-state '(() ()))

;; adds variable/value pair to state
(define add
  (lambda (var value state)
    (cond
      [(instate? var state) (error 'error "already in state")]
      [else                 (list (cons var (car state)) (cons value (cadr state)))])))

;; removes variable/value pair from state, wrapper for remove-acc
(define remove
  (lambda (var state)
    (remove-acc var state init-state)))

;; Chris please test all conditions thanks
(define m-bool
  (lambda (exp state)
    (cond
      [(null? exp)              (error 'undefined "undefined expression")]
      [(number? exp)            exp]
      [(instate? exp state)     (get exp state)]
      [(eq? exp #t)             #t]
      [(eq? exp #f)             #f]
      [(list? (cadr exp))       (m-value (cadr exp) state)]
      [(eq? (operator exp) '==) (equal? (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '!=) (not (equal? (m-bool (left-operand exp) state) (m-bool (right-operand exp) state)))]
      [(eq? (operator exp) '>)  (> (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '>=) (>= (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '<)  (< (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '<=) (<= (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '&&) (and (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '||) (or (m-bool (left-operand exp) state) (m-bool (right-operand exp) state))]
      [(eq? (operator exp) '!)  (not (m-bool (left-operand exp) state))]
      [else                     'invalid_expression])))
     
;; declares a variable
(define m-declare
  (lambda (exp state)
    (cond
      [(null? exp)          (error 'error "undefined expression")]
      [(null? (caddr exp))  (add (cadr exp) 'novalue state)]
      [else (add (cadr exp) (caddr exp) state)])))

;; Chris please test
(define m-assign
  (lambda (exp state)
    (cond
      [(null? exp)                 (error 'error "undefined expression")]
      [(instate? (cadr exp) state) (m-declare (list (car exp) (cadr exp) (m-eval (caddr exp) state)) (remove (cadr exp) state))]
      [else                        (m-declare (list (car exp) (cadr exp) (m-eval (caddr exp) state)) state)])))

;;
(define m-return
  (lambda (exp state)
    (cond
      [(null? exp) (error 'error "undefined expression")]
      [else        (m-eval (cadr exp) state)])))


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
            