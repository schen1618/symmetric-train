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

;; removes variable/state pair from state
(define remove-acc
  (lambda (var state acc)
    (cond
      [(null?                 (car state)) acc]
      [(eq? var (caar state)) (remove-acc var (list (cdar state) (cdadr state)) acc)]
      [else                   (remove-acc var (list (cdar state) (cdadr state)) (list (cons (caar state) (car acc)) (cons (caadr state) (cadr acc))))])))

;;;; ******************************************************************************************
;;;;   end of helper methods
;;;; ******************************************************************************************

       
;; takes an expression of numbers/variables and operators and returns the value
;; The operators are +, -, *, /, % and division is integer division
(define m-value
  (lambda (exp state)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(number? exp) exp]
      [(instate? exp state) (get exp state)]
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

;; NOT DONE
(define m-bool
  (lambda (exp state)
    (cond
      [(null? exp) (error 'error "undefined expression")]
      [else 0])))

;; gets value of a variable from state
(define get
  (lambda (var state)
    (cond
      [(null? (car state))    'valuenotfound]
      [(eq? var (caar state)) (caadr state)]
      [else                   (get var (list (cdar state) (cdadr state)))])))



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
            