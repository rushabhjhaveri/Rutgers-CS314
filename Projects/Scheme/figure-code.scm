;;;; This file is project 1 for CS 314 for Spring 2018, taught by Prof. Steinberg
;;;; The assignment is to fill in the definitions below, adding your code where ever
;;;; you see the comment 
;;  replace this line
;;;; to make each function do what its comments say it should do.  You
;;;; may replace such a line with as many lines as you want to.  You may
;;;; also add your own functions, as long as each function has a
;;;; comment like the ones below.  You may not make any other changes
;;;; to this code.

;;;; see end of file for some examples that will run ONCE THE FUNCTION add-check
;;;; IS FILLED IN

;;;; See the assignment on Sakai for more examples (these may require
;;;; more than add-check to be filled in) and further information,
;;;; including due date.



;;;; code for a program to create closures that generate figures out
;;;; of characters.

;;; figures: a figure is represented by a list of 3 elements: func,
;;; numrows, and numcols where:
;;;   func is a function (ie a closure) of two parameters: row, column
;;;     that returns the character at the given row and column of the
;;;     figure.  If row is out of bounds ie row<0 or row>=numrows, or
;;;     similarly for col, func returns the character #\. (a period
;;;     character)
;;;   numrows is the number of rows in the figure
;;;   numcols is the number of columns in the figure

;;; The following are functions to create and access a figure. Note that
;;; make-figure adds bounds checking to func
(define (make-figure func numrows numcols)
  (list (add-check func numrows numcols) numrows numcols))
(define (figure-func figure) (car figure))
(define (figure-numrows figure)(cadr figure))
(define (figure-numcols figure)(caddr figure))

;;; forn takes three arguments:  start and stop, which are numbers, and func 
;;; which is a function of one argument.
;;; forn calls func several times, first with the argument start, then with start+1
;;; then ... finally with stop.  It returns a list of the values of the calls. 
;;; If start>stop, forn simply returns the empty list without doing any calls to func.

(define (forn start stop func)
  (if (> start stop) '()
      (let ((first-value (func start)))
        (cons first-value (forn (+ 1 start) stop func)))))
  
;;; range-check takes 4 arguments:  row, numrows, col, numcols It checks if
;;;  0 <= row < numrows and similarly for col and numcols.  If both row and col are in 
;;;  range, range-check returns #t.  If either are out of range, rangecheck  returns #f
(define (range-check row numrows col numcols)
  (not (or (< row 0) (< col 0) (>= row numrows) (>= col numcols))))

;;; add-check takes 3 arguments: func, numrows and numcols.  func is a
;;; function of two numbers, row and col.  add-check returns a new
;;; function, which we will refer to here as func2.  Like func, func2 takes
;;; a row number and a column number as arguments. func2 first calls
;;; range-check to do a range check on these numbers against numrows
;;; and numcols. If row or col is out of range func2 returns #\.,
;;; otherwise it returns the result of (func row col).  You can think of
;;; func2 as a "safe" version of func, like the function returned by
;;; null-safe in Resources > Scheme > null-safe.scm except that a
;;; "bad", i.e. out of range, argument to func here will not necessarily
;;; crash scheme the way (car '( )) would.
(define (add-check func numrows numcols)
  (lambda (row col)
    (if (range-check row numrows col numcols)
        (func row col) ; If the condition evaluates to true, it returns the result of (func row col)
        #\.) ; If the condition evaluates to false, it returns #\.
    )
  )
 
;;; display-window prints out the characters that make up a rectangular segment of the figure
;;;    startrow and endrow are the first and last rows to print, similarly for startcol and endcol
;;; The last thing display-window does is to call (newline) to print a blank line under the figure.
(define (display-window start-row stop-row start-col stop-col figure)
  (forn start-row stop-row 
        (lambda (r)
          (forn start-col stop-col 
                (lambda (c)
                  (display ((figure-func figure) r c))))
          (newline)))
  (newline))

;;; charfig take one argument, a character, and returns a 1-row, 1-column figure consisting of that character
(define (charfig char)
  (make-figure (lambda (row col)
                 char)
               1 1))

;;; sw-corner returns a figure that is a size x size square, in which
;;; the top-left to bottom-right diagonal and everything under it is
;;; the chracter * and everything above the diagonal is the space
;;; character
(define (sw-corner size)
  (make-figure (lambda (row col)
                 (if (>= row col)
                     #\*
                     #\space))
               size
               size))


;;; repeat-cols returns a figure made up of nrepeat copies of
;;; figure, appended horizontally (left and right of each other)
(define (repeat-cols nrepeat figure)
  (make-figure (lambda (row col)
                 ((figure-func figure) row (modulo col (figure-numcols figure))))
               (figure-numrows figure) 
               (* nrepeat (figure-numcols figure)) 
               ;; the function just calls the function that repeat-cols received, but 
               ;; uses modulo to select the right position.
               ))

;;; repeat-rows returns a figure made up of nrepeat copies
;;; of a figure, appended vertically (above and below each other)
(define (repeat-rows nrepeat figure)
  (make-figure (lambda (row col)
                 ((figure-func figure) (modulo row (figure-numrows figure)) col))
               (figure-numcols figure)
               (* nrepeat (figure-numrows figure))
               ;; repeat-rows calls the function that it [repeat-rows] received
               ;; uses modulo to select the right position. 
               )
  )

;;; append cols returns the figure made by appending figureb to the
;;; right of figurea the number of rows in the resulting figure is the
;;; smaller of the number of rows in figurea and figureb
(define (append-cols figurea figureb)
  (make-figure (lambda (row col)
                 (if(< col (figure-numcols figurea)) ; If the current col is < the # of cols in figure a
                    ((figure-func figurea) row col) ; Compute figure a closure at specified row / col
                    ((figure-func figureb) row (- col (figure-numcols figurea))) ; Figure a completed, append
                    ; figure b by computing its closure at specified row / col.
                    ))
               (if(> (figure-numrows figurea) (figure-numrows figureb)) ; If the # of rows in figure a > # of rows in figure b
                  (figure-numrows figureb) ; Return figure b [the one with less # of rows].
                  (figure-numrows figurea)) ; Else, return figure a.
               (+ (figure-numcols figurea)(figure-numcols figureb)) ; Add # of cols of both figures to get total # of cols
               )
  )

;;; append-rows returns the figure made by appending figureb below figurea
;;; the number of columns in the resulting figure is the smaller of the number of columns in figurea
;;; and figureb
(define (append-rows figurea figureb)
  (make-figure (lambda (row col)
                 (if(< row (figure-numrows figurea)) ; If the current row is < the # of rows in figure a
                    ((figure-func figurea) row col) ; Compute figure a closure at specified row / col
                    ((figure-func figureb) (- row (figure-numrows figurea)) col) ; Figure a completed, append
                    ; figure b by computing its closure at specified row / col.
                    ))
               (+ (figure-numrows figurea)(figure-numrows figureb)) ; Add # of rows of both figures to get total # of cols
               (if(> (figure-numcols figurea) (figure-numcols figureb)) ; If the # of cols in figure a > # of cols in figure b
                  (figure-numcols figureb) ; Return figure b [the one with less # of cols].
                  (figure-numcols figurea)) ; Else, return figure a.
               )
  )
;;; flip-cols returns a figure that is the left-right mirror image of figure
(define (flip-cols figure)
  ; Remember - make-figure receives 3 args - <func, numrows, numcols> - IN THAT ORDER
  ; lambda syntax : (lambda (arg-id ... ) body)
  (make-figure(lambda (row col) ; arg-ids (row col)
                ; body
                ; Lambda function that returns a function.
                ; (figure-func figure) applies the given function that defines the figure
                ; Returns the character and the specified row and col.
                ; We pass col as (total # of cols in figure) - (col + 1) because
                ; e.g.
                ;   0 1 2 3     0 1 2 3 
                ; 0 *                 *
                ; 1 * *             * *
                ; 2 * * *         * * *
                ; 3 * * * *     * * * *
                ; since we need a left-right mirror image, character at figure[0][0]
                ; will be replicated at figure[3][0]
                ; and (3, 0) = (0, 4 - (0 + 1)) = (row, (numcols - (col + 1)) and so on.
                 ((figure-func figure) row (- (figure-numcols figure) (+ 1 col))
               ))
              (figure-numrows figure)
              (figure-numcols figure))
  )

;;; flip-rows returns a figure that is the up-down mirror image of figure
(define (flip-rows figure)
  ; Remember - make-figure receives 3 args - <func, numrows, numcols> - IN THAT ORDER
  ; lambda syntax : (lambda (arg-id ... ) body)
  (make-figure (lambda (row col) ; arg-ids (row col)
                ; body
                ; Lambda function that returns a function.
                ; (figure-func figure) applies the given function that defines the figure
                ; Returns the character and the specified row and col.
                ; We pass row as (total # of rows in figure) - (row + 1) because
                ; e.g.
                ;   0 1 2 3
                ; 0 *
                ; 1 * * 
                ; 2 * * *
                ; 3 * * * *
                ;  
                ;   0 1 2 3 
                ; 0 * * * *
                ; 1 * * *
                ; 2 * *
                ; 3 *
                ; since we need an up-down mirror image, character at figure[0][0]
                ; will be replicated at figure[3][0]
                ; and (3, 0) = ((4 - (0 + 1)), 0) = ((numrows - (row + 1)), col) and so on. 
                 ((figure-func figure) (- (figure-numrows figure) (+ 1 row)) col))
               (figure-numrows figure)
               (figure-numcols figure)
               )
  )


;;;; some examples thst should work after just add-check is filled in
;;;; above.  (Remove the ;'s at the start of the lines below.)
;(define fig1 (sw-corner 4))
;(display "Figure 1")(newline)
;(display-window 0 3 0 3 fig1)
;(define fig2 (repeat-cols 3 fig1))
;(display "Figure 2")(newline)
;(display-window 0 4
;                0 12 fig2)

; Test cases after completing the code. 
(define ca (charfig #\a))
(define cb (charfig #\b))
(define ab (append-cols ca cb))
(define cde (append-cols (charfig #\c)
                         (append-cols (charfig #\d)
                                      (charfig #\e))))
(define abcd (append-rows ab cde))
(display-window 0 0 0 0 ca)
(display-window 0 0 0 1 ca)
(display-window 0 1 0 1 ca)
(display-window 0 1 0 1 ab)
(display-window 0 2 0 2 abcd)
(display-window 0 1 0 2 cde)
(display-window 0 3 0 3 (sw-corner 4))
(display-window 0 3 0 3 (flip-rows(sw-corner 4)))
(display-window 0 3 0 3 (flip-cols(sw-corner 4)))
(let ((f1 (append-rows ab (flip-cols ab))))
(display-window 0 2 0 4 (append-cols f1 (flip-rows f1))))