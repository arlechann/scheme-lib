(define-library (lib micro-thread)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme list-queue)
          (lib coroutine)
          )
  (export fork
          main-loop
          )

  (begin
    (define *processes* (make-list-queue '()))

    (define enqueue! list-queue-add-back!)
    (define dequeue! list-queue-remove-front!)

    (define (fork proc)
      (enqueue! *processes* (make-generator (lambda (yield) (not (eof-object? (proc yield)))))))

    (define (main-loop . args)
      (for-each (lambda (proc) (fork proc)) args)
      (let loop ()
        (unless (list-queue-empty? *processes*)
          (let ((p (dequeue! *processes*)))
            (if (not (p))
                (enqueue! *processes* p))
            (loop)))))
    ))
