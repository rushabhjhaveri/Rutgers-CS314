;;; Author: Rushabh Jhaveri
;;; NetID: rrj28

;;; [I]
;;; Write a scheme macro (firstNon0 <expr1> <expr2> <expr3>), which evaluates the <exprs> one by one until one of them evaluates to a non-0 value. As soon as it finds an <expr> which evaluates to a non-zero value, firstNon0 stops and returns that value.
;;; If all of the <expr>s evaluate to 0, firstNon0 should return 0.
;;; If you prefer, your macro can take more than three <expr>s.

(define-syntax firsNon0
  (syntax-rules ()
    ((_expr1 expr2 expr3)
     (if(not (number? expr1)) (display expr1)
        (let ((val expr1))
          (if (not (= val 0)) expr1
              (let ((val expr2))
                (if(not(number? expr2)) (display expr2)
                   (if(not (= val 0)) expr2
                      (let ((val expr3))
                        (if(not(number? expr3)) (display expr3)
                           (if(not(= val 0)) expr3
                              (display 0))))))))))
     )))

;;; [II]
;;; Write a scheme function (tribonacci n).
;;; The tribonacci numbers Ti are defined as T0 = T1 = 0, T2 = 1, and TN = TN-1 + TN-2 + TN-3. (tribonacci n) should return Tn.
;;; tribonacci and any helper functions must be tail recursive.

(define (tribonacci n)
  (tribonacci-tail n 0 0 1)
  ) ; end define tribonacci 

(define (tribonacci-tail n a b c)
  (cond ((or (= n 0) (= n 1)) 0) ; Base case: if n = 0 or n = 1, return 0.
        ((= n 2) c) ; Second base case: if n = 2, return c [= 1].
        (else
         (let ((m (- n 1)))
           ; Tail-recursion.
           ; Recursively call tribonacci-tail,
           ; passing (n-1), and b, c, and (a + b + c).
           ; Reasoning:
           ; For example, consider n = 4. 
           ; tribonacci-tail will fall into the else condition
           ; recursive call passing (3, 0, 1, 1)
           ; Now, n = 3
           ; recursive call passing (2, 1, 1, 2)
           ; Now, n = 2
           ; returns 2 = Tribonacci[4]
           (tribonacci-tail m b c (+ a b c))))
        ) ; end cond 
  ) ; end tribonacci-tail

;(display (tribonacci 6))
