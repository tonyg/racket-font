#lang racket/base
;; Copyright (c) 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(require racket/match)
(require racket/list)
(require "font.rkt")
(require "cache.rkt")

;; TODO: kerning, both between and within StyledGlyphs.

;; Positions name *boundaries* between characters.
;;
;;    +---+---+---+---+---+
;;    | H | e | l | l | o |
;;    +---+---+---+---+---+
;;      0   1   2   3   4    = character indexes
;;    0   1   2   3   4   5  = positions

;; An AtomicGlyph is a ListOf<Glyph>, representing a selectable atomic
;; piece of text. The reason it's a list rather than a single Glyph is
;; because of combining characters.

;; A TextStyle is a font, color, size and line-spacing combination.
(struct text-style (color face size line-spacing) #:transparent)

;; A StyledGlyph is an AtomicGlyph corresponding to some original
;; Character, to be rendered using a given TextStyle.
(struct styled-glyph (style char glyphs) #:transparent)

;; A TypeRow is a ListOf<StyledGlyph>, representing a sequence of
;; styled AtomicGlyphs in REVERSE ORDER, that is with the rightmost
;; glyph first.

;; A ComposedRow is a (composed-row Number Number
;; VectorOf<StyledGlyph> VectorOf<Number>), representing a sequence of
;; StyledGlyphs in NON-REVERSE ORDER, combined with a y-coordinate for
;; the baseline of the whole row, the number of the first position in
;; the row, and a vector containing x-coordinates for each subsequent
;; position in the row. The `position-offsets` vector will be the same
;; length as the `styled-glyphs` vector, and the `n`th element of
;; `position-offsets` will correspond to position `(+ first-position n
;; 1)`.
(struct composed-row (baseline-offset
		      first-position
		      styled-glyphs
		      position-offsets) #:transparent)

;; A Paragraph is a ListOf<Pair<TextStyle,String>>.

(define current-dpi (make-parameter 135)) ;; MacBook Air Late 2010

(define (points->pixels p)
  (* p (/ (current-dpi) 72)))

;; How far to move the caret for the given StyledGlyph.
(define (styled-glyph-advance sglyph)
  (define style (styled-glyph-style sglyph))
  (define face (text-style-face style))
  (define glyphs (styled-glyph-glyphs sglyph))
  (* (foldl + 0 (map glyph-horizontal-advance glyphs))
     (points->pixels (text-style-size style))))

;; The number of affected pixel-widths in the rendering of the given
;; StyledGlyph.
(define (styled-glyph-width sglyph)
  (cond
   [(char-whitespace? (styled-glyph-char sglyph)) 0]
   [else
    (define style (styled-glyph-style sglyph))
    (define face (text-style-face style))
    (define glyphs (styled-glyph-glyphs sglyph))
    (do ((offset 0 (+ offset (glyph-horizontal-advance (car glyphs))))
	 (glyphs glyphs (cdr glyphs))
	 (rightmost 0 (max (+ offset (glyph-width (car glyphs))) rightmost)))
	((null? glyphs) (* rightmost (points->pixels (text-style-size style)))))]))

(define (styled-glyph-height sglyph)
  (define style (styled-glyph-style sglyph))
  (* (font-face-height (text-style-face style))
     (points->pixels (text-style-size style))
     (text-style-line-spacing style)))

(define (type-row-advance r)
  (foldl + 0 (map styled-glyph-advance r)))

(define (type-row-height r)
  (foldl max 0 (map styled-glyph-height r)))

(define (type-row-glyph-count r)
  (length r))

(define (type-row-positions r)
  (if (null? r)
      '()
      (let ((rest (type-row-positions (cdr r))))
	(cons (+ (styled-glyph-advance (car r))
		 (if (null? rest) 0 (car rest)))
	      rest))))

(define (character->styled-glyph* style c)
  ;; TODO: Better decomposition, fallback to compatibility-decomposition etc.
  (define face (text-style-face style))
  (define (c->g ch) (character->glyph face ch))
  (define g (c->g c))
  (define gs (if (missing-glyph? g)
		 (begin
		   (when (not (char-whitespace? c))
		     (write `(decomposing ,c ,(string-normalize-nfd (string c))))
		     (newline))
		   (map c->g (string->list (string-normalize-nfd (string c)))))
		 (list g)))
  (styled-glyph style c gs))

(define GENERATIONS 5)

(define STYLE-CACHE (make-cache hasheq GENERATIONS))
(define (character->styled-glyph style c)
  (cache-lookup/insert! (cache-lookup/insert! STYLE-CACHE style
					      (lambda () (make-cache hasheq GENERATIONS)))
			c
			(lambda () (character->styled-glyph* style c))))

(define (age-style-cache!)
  (cache-age! STYLE-CACHE)
  (log-info "STYLE-CACHE occupancy: ~v" (cache-stats STYLE-CACHE))
  (for ([(style c) (in-cache STYLE-CACHE)])
    (cache-age! c)
    (log-info "  occupancy for ~v/~v/~v/~v: ~v"
	      (font-face-family (text-style-face style))
	      (font-face-style (text-style-face style))
	      (text-style-color style)
	      (text-style-size style)
	      (cache-stats c))))

;; Paragraph Number -> ListOf<ComposedRow>
(define (compose paragraph max-width)
  (define paragraph-glyphs (append-map (lambda (style-and-string)
					 (define style (car style-and-string))
					 (define string (cdr style-and-string))
					 (map (lambda (c) (character->styled-glyph style c))
					      (string->list string)))
				       paragraph))
  (let loop ((paragraph-glyphs paragraph-glyphs) ;; ListOf<StyledGlyph>, remaining text
	     (rows-rev '()) ;; ListOf<ComposedRow>, finished rows of type
	     (row '()) ;; TypeRow, current row of type
	     (remaining-width max-width) ;; Number, pixels remaining in current row
	     (row-at-break '()) ;; TypeRow, row as it was just before the most recent word break
	     (glyphs-at-break paragraph-glyphs) ;; ListOf<StyledGlyph>, text after most recent break
	     )
    ;; (write `(loop ,(length paragraph-glyphs)
    ;; 		  ,(if (pair? paragraph-glyphs)
    ;; 		       (list (styled-glyph-char (car paragraph-glyphs))
    ;; 			     (styled-glyph-width (car paragraph-glyphs)))
    ;; 		       '--)
    ;; 		  ,(length rows-rev)
    ;; 		  ,remaining-width
    ;; 		  ,(length row-at-break)
    ;; 		  ,(length glyphs-at-break)
    ;; 		  ,(map styled-glyph-char row)
    ;; 		  ,(map styled-glyph-char row-at-break)
    ;; 		  )) (newline)
    (define (baseline-offset)
      (if (null? rows-rev) 0 (composed-row-baseline-offset (car rows-rev))))
    (define (first-position)
      (if (null? rows-rev)
	  0
	  (+ (composed-row-first-position (car rows-rev))
	     (vector-length (composed-row-styled-glyphs (car rows-rev))))))
    (define (finish-row row min-height)
      (cons (composed-row (+ (baseline-offset) (max min-height (type-row-height row)))
			  (first-position)
			  (list->vector (reverse row))
			  (list->vector (reverse (type-row-positions row))))
	    rows-rev))
    (if (null? paragraph-glyphs)
	(reverse (finish-row row 0))
	(let ((sg (car paragraph-glyphs)))
	  (cond
	   [(and (> (styled-glyph-width sg) remaining-width)
		 (pair? row))
	    ;; Only start a new row if we both don't have room on
	    ;; the current one *and* the current one is non-empty.
	    (define-values (row-to-finish remaining-glyphs)
	      (if (null? row-at-break)
		  (values row paragraph-glyphs)
		  (values row-at-break glyphs-at-break)))
	    (loop remaining-glyphs
		  (finish-row row-to-finish 0)
		  '()
		  max-width
		  '()
		  remaining-glyphs)]
	   [(eqv? (styled-glyph-char sg) #\newline)
	    ;; Explicit newline found.
	    (loop (cdr paragraph-glyphs)
		  (finish-row row (styled-glyph-height sg))
		  '()
		  max-width
		  '()
		  (cdr paragraph-glyphs))]
	   [else
	    (define is-breaking
	      (or (char-whitespace? (styled-glyph-char sg))
		  (char-punctuation? (styled-glyph-char sg))))
	    (loop (cdr paragraph-glyphs)
		  rows-rev
		  (cons sg row)
		  (- remaining-width (styled-glyph-advance sg))
		  (if is-breaking (cons sg row) row-at-break)
		  (if is-breaking (cdr paragraph-glyphs) glyphs-at-break))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require profile)

(require racket/gui/base)
(require "font-directory.rkt")
(require "font-render.rkt")
(require racket/class)
(require racket/file)

(let ()
  (register-faces! (time (read-faces-from-file "gentium-basic-regular.gz")))
  ;;(register-faces! (time (read-faces-from-file "andale-mono.gz")))
  ;;(register-faces! (time (read-faces-from-file "times-new-roman.gz")))
  (define text (file->string "test-text.txt"))
  (define style (text-style "black" (font-face-matching "" "") 11 1))
  (define h1 (text-style "blue" (font-face-matching "" "") 24 1))
  (define para (list 
		(cons h1 "Sample Text \u1E69\uFB01s\u0323\u0307\n\n")
		(cons style text)))

  (define (render-composition composition max-height dc)
    (define original-transformation (send dc get-transformation))
    (define-values (base-x base-y) (send dc get-origin))
    (call/ec
     (lambda (escape)
       (for-each (lambda (row)
		   (match-define (composed-row offset-y _ styled-glyphs position-offsets) row)
		   (when (positive? (vector-length styled-glyphs))
		     (send dc set-pen "black" 0 'transparent)
		     (send dc set-origin base-x (+ base-y offset-y))
		     (do ((i 0 (+ i 1))
			  (offset-x 0 (vector-ref position-offsets i)))
			 ((= i (vector-length styled-glyphs)))
		       (let ()
			 (match-define (styled-glyph (text-style color face points _)
						     original-char
						     glyphs)
				       (vector-ref styled-glyphs i))
			 (define size (points->pixels points))
			 (send dc set-origin (+ base-x offset-x) (+ base-y offset-y))
			 (send dc set-brush color 'solid)
			 (draw-glyphs face glyphs size dc)
			 )))
		   (when (> offset-y max-height)
		     (escape (void))))
		 composition)))
    (send dc set-transformation original-transformation))

  (define frame (new frame%
                   [label "Example"]
                   [width 500]
                   [height 500]))
  (define identity-transformation (vector (vector 1 0 0 1 0 0) 0 0 1 1 0))
  (new canvas% [parent frame]
       [paint-callback
	(lambda (canvas dc)
	  (define-values (width height) (send canvas get-virtual-size))
	  (send dc set-transformation identity-transformation)
	  (send dc set-smoothing 'smoothed)
	  (send dc set-pen "red" 2 'solid)
	  (send dc draw-line 50 50 50 height)
	  (send dc draw-line (- width 50) 50 (- width 50) height)
	  (send dc set-origin 50 50)
	  (write 'composing) (newline)
	  ;; (profile-thunk
	  ;;  (lambda ()
	  ;;    (render-composition (compose para (- width 100)) height dc)
	  ;;    )
	  ;;  #:repeat 3
	  ;;  #:delay 0.001)
	  (time (render-composition (time (compose para (- width 100))) height dc))
	  (age-style-cache!)
	  (age-font-caches!)
	  )])
  (send frame show #t))
