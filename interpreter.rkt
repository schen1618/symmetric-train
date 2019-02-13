#lang racket
(require "simpleParser.rkt")


;;;; ******************************************************************************************
;;;;   helper methods
;;;; ******************************************************************************************

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

(define add
  (lambda (name value state)
    (cond
      [(instate? name state) (error 'error "already in state")]
      [else                  (list (cons name (car state)) (cons value (cadr state)))])))

(define remove
  (lambda (name state)
    (remove-acc name state init-state)))

(define remove-acc
  (lambda (name state acc)
    (cond
      [(null?                  (car state)) acc]
      [(eq? name (caar state)) (remove-acc name (list (cdar state) (cdadr state)) acc)]
      [else                    (remove-acc name (list (cdar state) (cdadr state)) (list (cons (caar state) (car acc)) (cons (caadr state) (cadr acc))))])))
       
(define m-name
  (lambda (exp)
    (cond
      [(null? exp) (error 'error "no expression")]
      [else (m-value exp)])))

(define m-bool
  (lambda (exp)
    (cond
      [(null? exp) (error 'error "no expression")]
      [else 0])))

(define get
  (lambda (var state)
    (cond
      [(null? (car state))    'valuenotfound]
      [(eq? var (caar state)) (caadr state)]
      [else                   (get var (list (cdar state) (cdadr state)))])))



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
            