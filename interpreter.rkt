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
      [else                 (list (cons var (addvar state)) (cons value (addval state)))])))

(define addvar car)
(define addval cadr)

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
      [(null? (decexp exp))   (add (decvar exp) 'novalue state)]
      [(list? (decexp exp))   (add (decvar exp) (m-eval (decval exp) state) state)]
      [else                 (add (decvar exp) (decval exp) state)])))

(define decexp cddr)
(define decvar cadr)
(define decval caddr)

;; assigns a value to a variable
(define m-assign
  (lambda (exp state)
    (cond
      [(null? exp)                 (error 'error "undefined expression")]
      [(instate? (assvar exp) state) (m-declare (list (car exp) (assvar exp) (m-eval (assval exp) state)) (remove (assvar exp) state))]
      [else                        (error 'error "variable not declared")])))

(define assvar cadr)
(define assval caddr)
  
;; returns an evaluated expression
(define m-return
  (lambda (exp state)
    (cond
      [(null? exp) (error 'error "undefined expression")]
      [(eq? #t (m-eval (retval exp) state)) (add 'return 'true state)]
      [(eq? #f (m-eval (retval exp) state)) (add 'return 'false state)]
      [else        (add 'return (m-eval (retval exp) state) state)])))

(define retval cadr)

;; if statement that returns the evaluated first statement if true or the second statement if false, if it exists
(define m-if
  (lambda (statement state)
    (cond
      [(null? statement)                        (error 'error "undefined statement")]
      [(eq? #t (m-bool (ifexp statement) state)) (m-state (ifval statement) state)]
      [(not (null? (elseexp statement)))          (m-state (elseval statement) state)]
      [else                                     state])))

(define ifexp cadr)
(define ifval caddr)
(define elseexp cdddr)
(define elseval cadddr)

;; while loop that evaluates the statement until the condition is no longer true
(define m-while
  (lambda (statement state)
    (cond
      [(null? statement)                        (error 'error "undefined statement")]
      [(eq? #t (m-bool (whileexp statement) state)) (m-while statement (m-state (whileval statement) state))]
      [else                                     state])))

(define whileexp cadr)
(define whileval caddr)

;; m-state tracks the next expression to be evaluated
(define m-state
  (lambda (statement state)
    (cond
      [(eq? (identifier statement) 'var)    (m-declare statement state)]
      [(eq? (identifier statement) '=)      (m-assign statement state)]
      [(eq? (identifier statement) 'return) (m-return statement state)]
      [(eq? (identifier statement) 'if)     (m-if statement state)]
      [(eq? (identifier statement) 'while)  (m-while statement state)]
      [else                          error 'error "undefined expression"])))

(define identifier car)

;; interpret tree takes a tree as an argument and returns the evaluated colleciton of statements
(define interpret-tree
  (lambda (tree state)
    (cond
      [(null? tree) (get 'return state)]
      [(pair? tree) (interpret-tree (cdr tree) (m-state (car tree) state))])))
      

; interpret takes a filename and runs it through interpret-tree
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
      [(null? (nullexp state))    #f]
      [(eq? var (varin state)) #t]
      [else                   (instate? var (cons (instatevar state) (instatestate state)))])))

(define nullexp car)
(define varin caar)
(define instatevar cdar)
(define  instatestate cdr)

;; gets value of a variable from state
;; nullexp and varin are same as with instate
(define get
  (lambda (var state)
    (cond
      [(null? (nullexp state))    'notfound]
      [(eq? var (varin state)) (varreturn state)]
      [else                   (get var (list (getvar state) (getstate state)))])))

(define varreturn caadr)
(define getvar cdar)
(define getstate cdadr)

;; removes variable/state pair from state
;; nullexp and varin are same as with instate
(define remove-acc
  (lambda (var state acc)
    (cond
      [(null?                 (nullexp state)) acc]
      [(eq? var (varin state)) (remove-acc var (list (removevar state) (removestate state)) acc)]
      [else                   (remove-acc var (list (removevar state) (removestate state)) (list (cons (caar state) (car acc)) (cons (caadr state) (cadr acc))))])))

(define removevar cdar)
(define removestate cdadr)

;; evaluates an expression
(define m-eval
  (lambda (exp state)
    (cond
      [(equal? 'invalid_expression (m-bool exp state)) (m-value exp state)]
      [else                                            (m-bool exp state)])))        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
