;;; Homework 4 - Scheme
;;; Author - Rushabh Jhaveri
;;; NetID rrj28
;;; RUID 171008850

(define echo
  (lambda (l)
    (cond ((null? l) '())
          ((not (pair? (car l)))
           (cons (car l)
                 (cons (car l)
                       (echo (cdr l)))))
          (else (cons (echo (car l))
                      (echo (cdr l)))))))
                               