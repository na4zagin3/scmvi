#! /usr/bin/gosh

(use srfi-1)
(use util.match)
(use c-wrapper)
(c-load-library "libcurses.so")
(c-include "curses.h")

(define (draw-line window y x line)
  (if (null? line)
      #f
      (match (car line)
        (()
         #f)
        (('word-str str)
         (mvwprintw window y x "%s" str)
         (draw-line window y (+ x (string-length str)) (cdr line)))
        (('word-space)
         (draw-line window y (+ x 2) (cdr line)))
        )))

(define (real-cursor-line-pos window y0 x0 lines cursor)
  (define (real-cursor-line-pos-sub y x line pos)
      (match (car line)
        (() #f)
        (('word-str str)
         (if (= pos (cursor-element cursor))
             (cons y (+ x (cursor-char cursor)))
             (real-cursor-line-pos-sub y (+ x (string-length str)) (cdr line) (+ pos 1))))
        (('word-space)
         (if (= pos (cursor-element cursor))
             (cons y (+ x 1))
             (real-cursor-line-pos-sub y (+ x 2) (cdr line) (+ pos 1))))
        ))
  (real-cursor-line-pos-sub y0 x0 lines 0)
  )

(define (real-cursor-pos window y0 x0 lines cursor)
  (real-cursor-line-pos window (+ y0 (cursor-line cursor)) x0 (text-object-ref 'line lines cursor) cursor))

(define (draw-cursor window y x cursor lines)
  (let ((realpos (real-cursor-pos window y x lines cursor)))
    (wmove window (car realpos) (cdr realpos)))
  )

(define (draw-window window y x lines)
  (map
   (lambda (lineno line)
     (draw-line window (+ y lineno) x line))
   (iota (length lines))
   lines)
  )

(define (cursor-line cursor)
  (list-ref cursor 0 0))

(define (cursor-element cursor)
  (list-ref cursor 1 0))

(define (cursor-char cursor)
  (list-ref cursor 2 0))

(define (element-length elem)
  (match elem
   (('word-space)
    1)
   (('word-str str)
    (string-length str))))
  
(define (text-object-ref unit lines cursor)
  (match unit
   ('line
    (list-ref lines (cursor-line cursor) #f))
   ('element
    (list-ref
     (text-object-ref 'line lines cursor)
     (cursor-element cursor)))
   ('char
    #f)))
 
(define (move-cursor unit x modifier lines cursor)
  (match unit
   ('line
    (let ((new-line (+ (cursor-line cursor) x))
          (line-length (length lines)))
      (cond
       ((< new-line 0) cursor)
       ((>= new-line line-length) cursor)
       (else
        (match modifier
         (#f (list new-line))
         ('start (list new-line))
         ('subelem-end (list new-line (- line-length 1)))
         )))))
   ('element
    (let ((new-line (cursor-line cursor))
          (new-element (+ (cursor-element cursor) x)))
      (cond
       ((< new-element 0) cursor)
       ((>= new-element (length (text-object-ref 'line lines cursor))) cursor)
       (else
        (match modifier
         (#f (list new-line new-element))
         ('start (list new-line new-element))
         ('subelem-end
          (list
           new-line
           new-element
           (- (element-length
               (text-object-ref 'element lines (list new-line new-element))) 1)))
           ))
         )))
   ('char
    (let ((new-line (cursor-line cursor))
          (new-element (cursor-element cursor))
          (new-char (+ (cursor-char cursor) x))
          (elem (text-object-ref 'element lines cursor)))
      (cond
       ((< new-char 0) 
        (let* ((cursor (move-cursor 'element -1 'subelem-end lines cursor))
               (cursor (move-cursor 'char (+ new-char 1) #f lines cursor)))
          cursor))
       ((>= new-char (element-length (text-object-ref 'element lines cursor)))
        (let* ((cursor (move-cursor 'element 1 'start lines cursor))
               (cursor (move-cursor 'char (- new-char (element-length elem)) #f lines cursor)))
          cursor))
       (else
        (list
         new-line
         new-element
         new-char))))))
  )

(define (cursor-rel rel unit-elem)
  (define (cursor-rel-sub c1 c2)
    (cond
     ((and (null? c1) (null? c2)) #t)
     ((null? c1) (rel c1 unit))
     ((null? c2) (rel unit c2))
     (else
      (if-let1 result (rel c1 c2)
       result
       (cursor-rel-sub (cdr c1) (cdr c1))))))
  cursor-rel-sub)

(define sample-buffer
  '(
    ((word-str "1st") (word-space) (word-str "line"))
    ((word-str "2nd") (word-space) (word-str "line")
     (word-space)
     (word-str "2nd") (word-space) (word-str "line"))
    ((word-str "3rd") (word-space) (word-str "line"))
  ))

(define (main args)
  
(initscr)
(cbreak)
(noecho)

(let ((hello  " [Simple Vi-like editor] ")
      (help " [h]:left [j]:down [k]:up [l]:right [c]:clear [q]: quit "))
  (let* ((line (x->number LINES))
         (maxy (- line 2))
         (cols (x->number COLS))
         (maxx (- cols 2))
         (win (newwin (- line 2) (- cols 2) 1 1)))
    (define (init)
      (wclear win)
      (box win (x->number #\|) (x->number #\-))
      (mvwprintw win 0 (x->integer (/ (- cols (string-length hello)) 2)) hello)
      (mvwprintw win (- line 3) (- cols (string-length help) 5) help)
      (wrefresh win))

    (init)

    ;; main loop
    (let lp ((cursor ())
             (buffer sample-buffer))


;      (draw-line win 0 0 (list y x z) sample-buffer)
      (wrefresh win)
      (let* ((elem (text-object-ref 'element buffer cursor))
             (elem-string (format #f "~a" elem))
             (realpos (real-cursor-pos win 1 1 buffer cursor))
             (position-string (format #f "~a ~a"
                                      realpos
                                      cursor
                                      )))
        (mvwprintw win (- line 5) (- cols (string-length elem-string) 5) elem-string)
        (mvwprintw win (- line 4) (- cols (string-length position-string) 5) position-string))
      (draw-window win 1 1 buffer)
      (draw-cursor win 1 1 cursor buffer)
      (wrefresh win)
      (case (read-char)
        ((#\k) (lp (move-cursor 'line -1 #f buffer cursor) buffer))
        ((#\j) (lp (move-cursor 'line  1 #f buffer cursor) buffer))
        ((#\h) (lp (move-cursor 'char -1 #f buffer cursor) buffer))
        ((#\l) (lp (move-cursor 'char  1 #f buffer cursor) buffer))
        ((#\e) (lp (move-cursor 'element  1 'subelem-end buffer cursor) buffer))
        ((#\w) (lp (move-cursor 'element  1 #f buffer cursor) buffer))
        ((#\b) (lp (move-cursor 'element -1 #f buffer cursor) buffer))
        ((#\c) (init) (lp cursor buffer))
        ((#\q) (endwin) (exit 0))
        (else (lp cursor buffer)))

      )))
)
