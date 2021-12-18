(import (scheme base)
        (scheme write)
        (gauche base)
        (gauche test)
        (lib amb))

(test-start "amb")

(define (call/test-section describe body)
  (test-section describe)
  (body))

(call/test-section "amb" (lambda ()
  (init-amb)
  (test* "not found" 'not-found (amb))
  (init-amb)
  (test* "2 amb operator - success"
    '(5 7)
    (call/cc (lambda (return)
      (let ((a (amb 2 3 5 7 11)) (b (amb 2 3 5 7 11)))
        (if (amb-not-found? a b) (return #f))
        (amb-assert (= (* a b) 35))
        (return (list a b))))))
  (init-amb)
  (test* "2 amb operator - fail"
    #f
    (call/cc (lambda (return)
      (let ((a (amb 2 3 5 7 11)) (b (amb 2 3 5 7 11)))
        (if (amb-not-found? a b) (return #f))
        (amb-assert (= (* a b) 36))
        (return (list a b))))))
  (init-amb)
  (test* "amb-collect"
    '((5 7) (7 5))
    (amb-collect (lambda ()
      (let ((a (amb 2 3 5 7 11)) (b (amb 2 3 5 7 11)))
        (amb-assert (= (* a b) 35))
        (list a b)))))
  ))

(test-end)
