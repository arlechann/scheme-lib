(define-library (lib coroutine)
  (import (scheme base))
  (export make-generator
          coroutine-yield
          make-coroutine
          )

  (begin
    (define (make-generator proc)
      (let ((return #f) (restart #f))
        (lambda args
          (call/cc (lambda (return-cont)
            (set! return return-cont)
            (if restart
                (apply restart args)
                (begin
                  (apply proc
                         (lambda (x)
                           (call/cc (lambda (restart-cont)
                             (set! restart (lambda args
                               (restart-cont args)))
                             (return x))))
                         args)
                  (call/cc (lambda (eof-cont) (set! restart eof-cont)))
                  (eof-object))))))))

    (define gtest
            (make-generator
              (lambda (yield . args)
                (let rec ((command (car args)) (args (cdr args)))
                  (let ((args (yield
                                (case command
                                  ((show) (display args) (newline))
                                  ((sum) (apply + args))
                                  ((product) (apply * args))))))
                    (rec (car args) (cdr args)))))))

    (define *continue* #f)

    (define (coroutine-yield x)
      (*continue* x))

    (define (make-coroutine proc)
      (let ((saved-continue #f)
            (restart #f)
            (return #f))
        (letrec ((push-continue
                   (lambda (c)
                     (set! saved-continue *continue*)
                     (set! *continue* c)))
                 (pop-continue
                   (lambda ()
                     (let ((c *continue*))
                       (set! *continue* saved-continue)
                       c)))
                 (yield
                   (lambda (x)
                     (call/cc (lambda (restart-cont)
                       (pop-continue)
                       (set! restart (lambda args
                         (push-continue yield)
                         (restart-cont args)))
                       (return x))))))
          (lambda args
            (call/cc (lambda (return-cont)
              (set! return return-cont)
                (if restart
                    (apply restart args)
                    (begin
                      (push-continue yield)
                      (apply proc args)))))))))
  ))

