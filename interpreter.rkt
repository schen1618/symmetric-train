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
(define init-state '(() ()))

;; adds variable/value pair to state
(define add
  (lambda (var value state return break continue)
    (cond
      [(instate? var state) (error 'error "variable already defined")]
      [else                 (list (cons var (add-var state)) (cons value (add-value state)))])))

(define add-var car)
(define add-value cadr)

;; removes variable/value pair from state, wrapper for remove-acc
(define remove
  (lambda (var state return break continue)
    (remove-acc var state init-state)))


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


;; declares a variable
(define m-declare
  (lambda (statement state return break continue)
    (cond
      [(null? statement)           (error 'error "undefined expression")]
      [(null? (dec-exp statement)) (add (dec-var statement) 'novalue state return break continue)]
      [(list? (dec-exp statement)) (add (dec-var statement)
                                        (m-eval (dec-value statement) state return break continue)
                                        state return break continue)]
      [else                        (add (dec-var statement) (dec-value statement) state return break continue)])))

(define dec-exp cddr)
(define dec-var cadr)
(define dec-value caddr)

;; assigns a value to a variable
(define m-assign
  (lambda (statement state return break continue)
    (cond
      [(null? statement)            (error 'error "undefined expression")]
      [(instate? (ass-var statement)
                 state)             (m-declare (list (equal-sign statement)
                                                     (ass-var statement)
                                                     (m-eval (ass-value statement) state return break continue))
                                               (remove (ass-var statement) state return break continue) return break continue)]
      [else                         (error 'error "variable not declared")])))

(define equal-sign car)
(define ass-var cadr)
(define ass-value caddr)
  
;; returns an evaluated expression
(define m-return
  (lambda (statement state return break continue)
    (cond
      [(null? statement)                           (error 'error "undefined expression")]
      [(eq? #t (m-eval (ret-exp statement) state)) (return 'true)]
      [(eq? #f (m-eval (ret-exp statement) state)) (return 'false)]
      [else                                        (return (m-eval (ret-exp statement) state return break continue))])))

(define ret-exp cadr)

;; if statement that returns the evaluated first statement if true or the second statement if false,
;; if it exists
(define m-if
  (lambda (statement state return break continue)
    (cond
      [(null? statement)                           (error 'error "undefined statement")]
      [(eq? #t (m-bool (if-cond statement) state)) (m-state (then-statement statement) state return break continue)]
      [(not (null? (else statement)))              (m-state (else-statement statement) state return break continue)]
      [else                                        state])))

(define if-cond cadr)
(define then-statement caddr)
(define else cdddr)
(define else-statement cadddr)

;; while loop that evaluates the statement until the condition is no longer true
(define m-while-helper
  (lambda (statement state return break continue)
    (cond
      [(null? statement)              (error 'error "undefined statement")]
      [(eq? #t (m-bool
                (while-cond statement)
                state))               (m-while statement (m-state (while-statement statement) state return break continue) return break continue)]
      [else                           state])))

(define m-while
  (lambda (statement state)
    (call/cc
     (lambda (break)
       (m-while-helper statement state return break continue)))))

(define while-cond cadr)
(define while-statement caddr)

;; evaluates the block of statements between the brackets
(define block
  (lambda (statement-list state return break continue)
    (cond
      ((null? statement-list) (return state))
      (else (deleteList (state-list (cdr statement-list) (addList(state))) return break continue)))))

;; m-state calls the appropriate function to evaluate the statement
(define m-state
  (lambda (statement state return break continue)
    (cond
      [(eq? (identifier statement) 'var)    (m-declare statement state return break continue)]
      [(eq? (identifier statement) '=)      (m-assign statement state return break continue)]
      [(eq? (identifier statement) 'return) (m-return statement state return break continue)]
      [(eq? (identifier statement) 'break)  (break state)]
      [(eq? (identifier statement) 'if)     (m-if statement state return break continue)]
      [(eq? (identifier statement) 'while)  (m-while statement state return break continue)]
      [else                                 (error 'error "undefined expression")])))

(define identifier car)

;; state-list takes a list of statements (lists) as an argument
;; and returns the evaluated collection of statements
(define state-list
  (lambda (tree state return break continue)
    (cond
      [(null? tree) (get 'return state)]
      [(pair? tree) (state-list (cdr tree) (m-state (car tree)) (lambda (v) v))])))
      

;; interpret takes a filename and runs it through state-list
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
    (state-list (parser filename) init-state return break continue)))


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
      [(eq? var (variable state))    (var-value state)]
      [else                          (get var (list (rest-of-variable-list state)
                                                    (rest-of-value-list state)))])))

(define var-value caadr)
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
                                                       (cons (var-value state) (values acc))))])))

(define values cadr)

;; evaluates an expression
(define m-eval
  (lambda (exp state return break continue)
    (cond
      [(equal? 'invalid_expression
               (m-bool exp state return break continue)) (m-value exp state return break continue)]
      [else                        (m-bool exp state return break continue)])))     

;; deletes the top layer of the input state
(define deleteList
  (lambda (state)
    ((cdr state))))


;; adds athe top layer of the input state
(define addList
  (lambda (state)
    (cond
      ((null? (cdr state)) (cons (car state)))
      (else (cons (cadr state) state)))))