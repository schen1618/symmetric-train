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

;;;; ******************************************************************************************
;;;;   end of helper methods
;;;; ******************************************************************************************

       
;; Code a function that can take an expression of numbers and operators and return the value
;; '(3 + (6 / 2))
;; '((4 + 5) - (7 * 10))
;; '(1 + 2)
;; The operators are +, -, *, /, % and division is integer division
(define m-value
  (lambda (exp)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(number? exp) exp]
      [(eq? (operator exp) '+) (+ (m-value (left-operand exp)) (m-value (right-operand exp)))]
      [(eq? (operator exp) '-) (- (m-value (left-operand exp)) (m-value (right-operand exp)))]
      [(eq? (operator exp) '*) (* (m-value (left-operand exp)) (m-value (right-operand exp)))]
      [(eq? (operator exp) '/) (quotient (m-value (left-operand exp)) (m-value (right-operand exp)))]
      [(eq? (operator exp) '%) (remainder (m-value (left-operand exp)) (m-value (right-operand exp)))])))

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

(define remove-acc
  (lambda (var state acc)
    (cond
      [(null?                 (car state)) acc]
      [(eq? var (caar state)) (remove-acc var (list (cdar state) (cdadr state)) acc)]
      [else                   (remove-acc var (list (cdar state) (cdadr state)) (list (cons (caar state) (car acc)) (cons (caadr state) (cadr acc))))])))

;; NOT DONE
(define m-name
  (lambda (exp)
    (cond
      [(null? exp) (error 'error "no expression")]
      [else (m-value exp)])))

;; NOT DONE
(define m-bool
  (lambda (exp)
    (cond
      [(null? exp) (error 'error "no expression")]
      [else 0])))

;; gets value of a variable from state
(define get
  (lambda (var state)
    (cond
      [(null? (car state))    'valuenotfound]
      [(eq? var (caar state)) (caadr state)]
      [else                   (get var (list (cdar state) (cdadr state)))])))



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
            