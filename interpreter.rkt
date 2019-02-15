#lang racket
(require "simpleParser.rkt")
;;;; ******************************************************************************************
;;;;   Intepreter, part 1
;;;;   EECS 345
;;;;   Sherry Chen, Chris Toomey
;;;; ******************************************************************************************


;; error for when variable is not found in state?       
;; takes an expression of numbers/variables and operators and returns the value
;; The operators are +, -, *, /, % and division is integer division
(define m-value
  (lambda (exp state)
    (cond
      [(null? exp)              (error 'undefined "undefined expression")]
      [(number? exp)            exp]
      [(equal? (get exp state)
               'novalue)        (error 'error "variable not assigned")]
      [(and (not (list? exp))
            (equal? 'notfound (get exp state))) (error 'error "variable not declared")]
      [(instate? exp state)     (get exp state)]
      [(eq? (operator exp) '+)  (+ (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(and (eq? (operator exp) '-)
            (null? (cddr exp))) (* (m-value (left-operand exp) state) -1)]
      [(eq? (operator exp) '-)  (- (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '*)  (* (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '/)  (quotient (m-value (left-operand exp) state) (m-value (right-operand exp) state))]
      [(eq? (operator exp) '%)  (remainder (m-value (left-operand exp) state) (m-value (right-operand exp) state))])))

(define operator car)
(define left-operand cadr)
(define right-operand caddr)
(define init-state '(() ()))

;; adds variable/value pair to state
(define add
  (lambda (var value state)
    (cond
      [(instate? var state) (error 'error "variable already defined")]
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
      [(equal? exp 'true)       #t]
      [(equal? exp 'false)      #f]
      [(equal? (get exp state)
               'novalue)        (error 'error "variable not assigned")]
      [(and (not (list? exp))
            (equal? 'notfound (get exp state))) (error 'error "variable not declared")]
      [(eq? (operator exp) '==) (equal? (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '!=) (not (equal? (m-eval (left-operand exp) state) (m-eval (right-operand exp) state)))]
      [(eq? (operator exp) '>)  (> (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '>=) (>= (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '<)  (< (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '<=) (<= (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '&&) (and (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '||) (or (m-eval (left-operand exp) state) (m-eval (right-operand exp) state))]
      [(eq? (operator exp) '!)  (not (m-eval (left-operand exp) state))]
      [else                     'invalid_expression])))
     
;; declares a variable
(define m-declare
  (lambda (exp state)
    (cond
      [(null? exp)          (error 'error "undefined expression")]
      [(null? (cddr exp))   (add (cadr exp) 'novalue state)]
      [(list? (cddr exp))   (add (cadr exp) (m-eval (caddr exp) state) state)]
      [else                 (add (cadr exp) (caddr exp) state)])))

;; Chris please test
(define m-assign
  (lambda (exp state)
    (cond
      [(null? exp)                 (error 'error "undefined expression")]
      [(instate? (cadr exp) state) (m-declare (list (car exp) (cadr exp) (m-eval (caddr exp) state)) (remove (cadr exp) state))]
      [else                        (error 'error "variable not declared")])))

;; returns an expression, Chris please test but I think it works
(define m-return
  (lambda (exp state)
    (cond
      [(null? exp) (error 'error "undefined expression")]
      [(eq? #t (m-eval (cadr exp) state)) (add 'return 'true state)]
      [(eq? #f (m-eval (cadr exp) state)) (add 'return 'false state)]
      [else        (add 'return (m-eval (cadr exp) state) state)])))

;; if statement
(define m-if
  (lambda (statement state)
    (cond
      [(null? statement)                        (error 'error "undefined statement")]
      [(eq? #t (m-bool (cadr statement) state)) (m-state (caddr statement) state)]
      [(not (null? (cdddr statement)))          (m-state (cadddr statement) state)]
      [else                                     state])))

;; while statement
(define m-while
  (lambda (statement state)
    (cond
      [(null? statement)                        (error 'error "undefined statement")]
      [(eq? #t (m-bool (cadr statement) state)) (m-while statement (m-state (caddr statement) state))]
      [else                                     state])))

;; m-state 😊
(define m-state
  (lambda (statement state)
    (cond
      [(eq? (car statement) 'var)    (m-declare statement state)]
      [(eq? (car statement) '=)      (m-assign statement state)]
      [(eq? (car statement) 'return) (m-return statement state)]
      [(eq? (car statement) 'if)     (m-if statement state)]
      [(eq? (car statement) 'while)  (m-while statement state)]
      [else                          error 'error "undefined expression"])))

;; interpret tree
(define interpret-tree
  (lambda (tree state)
    (cond
      [(null? tree) (get 'return state)]
      [(pair? tree) (interpret-tree (cdr tree) (m-state (car tree) state))])))
      

; interpret
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