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
                               