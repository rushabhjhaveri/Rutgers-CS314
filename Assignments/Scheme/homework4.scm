;;; Homework 4 - Scheme
;;; Author - Rushabh Jhaveri
;;; NetID rrj28
;;; RUID 171008850

;;; Define (i.e. write) the function (echo lst). This function doubles each top-level element of
;;; list lst. E.g., (echo '(a b c)) returns (a a b b c c). (echo '(a (b c))) returns (a a (b c) (b c))

(define (echo lst) ;Top level definition
  (if (null? lst) ;Base case - return empty list
      '()
      (append (list (car lst) (car lst))
              (echo (cdr lst)))))

;;; Define the function (echo-lots lst n). (echo-lots '(a (b c)) 3) returns (a a a (b c) (b c) (b c)),
;;; that is, it is like echo but it repeats each element of lst n times.

(define (echo-lots lst n)
  (if (null? lst) '()
      (let loop ((i (- n 1)) (x (car lst)))
        (if (zero? i)
            (cons x (echo-lots (cdr lst) n))
            (cons x (loop (- i 1) x))))))

;;; Define the function (echo-all lst) which is a deep version of echo.
;;; (echo-all '(a (b c))) should return (a a (b b c c)(b b c c)).
(define (echo-all lst)
  (cond((null? lst) '())
       ((not(pair?(car lst)))
        (cons (car lst)
              (cons (car lst)
                    (echo-all(cdr lst)))))
       (else (cons (echo-all(car lst))
                   (echo-all (cdr lst))))))


;;; Define the function nth. (nth i lst) returns the ith element of lst.
;;; E.g., (nth 0 '(a b c)) returns a, and (nth 1 '(a (b c) d) returns (b c).
;;; You may assume that 0 ≤ i < (length lst).
;;; You may not use the functions list-tail or list-ref in defining nth.
(define (nth i lst)
  (cond((null? lst) "Empty List.")
       ((or(> i (length lst)) (< i 0)) ("Index Error"))
       ((= i 0) (car lst))
       (else(nth (- i 1) (cdr lst)))))
       
                          