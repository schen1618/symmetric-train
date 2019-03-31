#lang racket
(require "interpreter.rkt")

(print "e3.")
(equal? (interpret "tests_2\\e3.txt") 'error)

(print 1. )
(equal? (interpret "tests_2\\1.txt") 20)

(print 2. )
(equal? (interpret "tests_2\\2.txt") 164)

(print 3. )
(equal? (interpret "tests_2\\3.txt") 32)

(print 4. )
(equal? (interpret "tests_2\\4.txt") 2)

;(print 5. )
;(equal? (interpret "tests_2\\5.txt") 'error)

(print "e1" )
(equal? (interpret "tests_2\\e1.txt") 12)

(print "e2" )
(equal? (interpret "tests_2\\e2.txt") 2)

(print 6. )
(equal? (interpret "tests_2\\6.txt") 25)

(print 7. )
(equal? (interpret "tests_2\\7.txt") 21)

(print 8. )
(equal? (interpret "tests_2\\8.txt") 6)

(print 9. )
(equal? (interpret "tests_2\\9.txt") -1)

(print 10. )
(equal? (interpret "tests_2\\10.txt") 789)

;(print 11. )
;(equal? (interpret "tests_2\\11.txt") 'Error)

;(print 12. )
;(equal? (interpret "tests_2\\12.txt") 'Error)

;(print 13. )
;(equal? (interpret "tests_2\\13.txt") 'Error)

(print 14. )
(equal? (interpret "tests_2\\14.txt") 12)

(print 15. )
(equal? (interpret "tests_2\\15.txt") 125)

(print 16. )
(equal? (interpret "tests_2\\16.txt") 110)

(print 17. )
(equal? (interpret "tests_2\\17.txt") 2000400)

(print 18. )
(equal? (interpret "tests_2\\18.txt") 101)

;(print 19. )
;(equal? (interpret "tests_2\\19.txt") 'error)

(print "e4.")
(equal? (interpret "tests_2\\e4.txt") 0)

(print "e5.")
(equal? (interpret "tests_2\\e5.txt") 0)

(print "e6.")
(equal? (interpret "tests_2\\e6.txt") 12)