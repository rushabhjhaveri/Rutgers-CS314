;;; Homework 4 - Scheme
;;; Author - Rushabh Jhaveri
;;; NetID rrj28
;;; RUID 171008850

(define (echo lst) ;Top level definition
  (if (null? lst) ;Base case - return empty list
      '()
      (append (list (car lst) (car lst))
              (echo (cdr lst)))))
                               