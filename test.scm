(driver-loop)

(define (parity x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(define (map f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))))

(define (range n)
  (if (< n 0)
    '()
    (append (range (- n 1)) (list n))))

(range 10)
(map parity (range 10))
