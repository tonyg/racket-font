#lang racket/base

(require racket/match)
(require racket/class)
(require racket/gui/base)

(require "font.rkt")
(require "font-render.rkt")
(require "font-directory.rkt")

(define (cartoon-glyph glyph dc)
  (send dc set-brush "blue" 'solid)
  (send dc set-alpha 0.2)
  (send dc draw-rectangle
	(glyph-horizontal-bearing-x glyph) (- (glyph-horizontal-bearing-y glyph))
	(glyph-width glyph) (glyph-height glyph))
  (send dc set-alpha 1)
  (send dc set-pen "yellow" 2/72 'solid)
  (send dc draw-line -1 0 1 0)
  (send dc draw-line 0 (- -1 (glyph-height glyph)) 0 1)
  (send dc set-pen "green" 2/72 'solid)
  (send dc draw-line
	(glyph-horizontal-advance glyph) (- -1 (glyph-height glyph))
	(glyph-horizontal-advance glyph) 1))

(define (cartoon-glyphs face glyphs dc)
  (define advancer (make-horizontal-glyph-advancer face))
  (for-each (lambda (g)
	      (match-define (cons kx ky) (advancer g))
	      (send dc translate kx (- ky))
	      (cartoon-glyph g dc)
	      (send dc translate (- kx) ky))
	    glyphs))

(register-faces! (read-faces-from-file "gentium-basic-regular.gz"))

(define face (font-face-matching "")) ;; any old face

(define frame (new frame%
                   [label "Example"]
                   [width 500]
                   [height 300]))

(define identity-transformation
  (vector (vector 1 0
		  0 1
		  0 0)
	  0 0
	  1 1
	  0))

(define label-text "Racket")

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
		(send dc set-transformation identity-transformation)
		(send dc set-smoothing 'smoothed)
		(send dc translate 100 200)
                (send dc scale 72 72)
		(cartoon-glyphs face (string->glyphs face label-text) dc)
                (send dc set-brush "black" 'solid)
                (send dc set-pen "black" 1/72 'transparent)
		(draw-glyphs face (string->glyphs face label-text) dc)
		)])

(send frame show #t)
