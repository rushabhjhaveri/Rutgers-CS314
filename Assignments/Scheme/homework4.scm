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

;;; Define a scheme function (assoc-all keys a-list) where keys is a list of symbols and
;;; a-list is an assoc-list. (An assoc-list is a list ((<key1> <value1>)(<key2> <value2>) ...)
;;; whose elements are two-element lists (<keyi> <valuei>),  whose first element is a key and
;;; whose second element is the associated value.) assoc-all returns a list of the data
;;; associated with elements of keys by a-list.
;;; E.g. (assoc-all '(a d c d) '((a apple)(b boy)(c (cat cow))(d dog)))
;;; returns (apple dog (cat cow) dog). Use map. Note that you can't simply use assoc as
;;; one of the arguments to map; you need to use a lambda expression.
(define (assoc-all keys a-list)
  (map (lambda (key) (cadr (assoc key a-list))) ; Iterate over keys list and use assoc to retrieve the first matching element in a-list
       keys))

;;; Define a scheme function filter which takes two arguments:
;;; a function fn and a list lst.
;;; Filter should return a list consisting of those elements of lst for which
;;; the fn returns true.
;;; E.g., the value of (filter even? '(3 4 6 7 8)) should be (4 6 8))
;;; (The function even? is a built-in function
;;; in scheme which returns #t if its argument is even and #f if odd.)
(define (filter fn lst)
  (cond ((null? (cdr lst)) '())
        (cons
         (fn (car lst) (car (cdr lst)))
         (filter fn (cdr lst)))))
                          