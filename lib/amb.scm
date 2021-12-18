(define-library (lib amb)
  (import (scheme base)
          (scheme list))
  (export init-amb
          amb-found?
          amb-not-found?
          amb-fail
          amb
          amb-assert
          amb-collect
          )

  (begin
    (define *amb-fail* #f)

    (define (init-amb . args)
      (set! *amb-fail*
            (if (null? args)
                (lambda () 'not-found)
                (car args))))

    (define (amb-found-1? arg)
      (not (eq? arg 'not-found)))

    (define (amb-found? . args)
      (every amb-found-1? args))

    (define (amb-not-found? . args)
      (not (apply amb-found? args)))

    (define (amb-fail)
      (*amb-fail*))

    (define (amb . args)
      (if (null? args)
          (amb-fail)
          (let ((saved-fail *amb-fail*))
            (call/cc (lambda (succ-cont)
              (for-each
                (lambda (x)
                  (call/cc (lambda (fail-cont)
                    (set! *amb-fail*
                          (lambda ()
                            (set! *amb-fail* saved-fail)
                            (fail-cont #f)))
                    (succ-cont x))))
                args)
              (amb-fail))))))

    (define (amb-assert p)
      (if (not p) (amb-fail)))

    (define (amb-collect proc)
      (let ((saved-fail *amb-fail*)
            (results '()))
        (if (call/cc (lambda (cont)
              (set! *amb-fail* (lambda () (cont #f)))
              (set! results (cons (proc) results))
              (cont #t)))
            (amb-fail))
        (set! *amb-fail* saved-fail)
        (reverse! results)))
    ))
