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
(require racket/class)
(require racket/gui/base)

(require "font.rkt")
(require "font-render.rkt")
(require "font-directory.rkt")

(define (cartoon-glyph glyph size dc)
  (define-values (sx sy) (send dc get-scale))
  (send dc set-scale size size)
  (send dc set-brush "blue" 'solid)
  (send dc set-alpha 0.2)
  (send dc draw-rectangle
	(glyph-horizontal-bearing-x glyph) (- (glyph-horizontal-bearing-y glyph))
	(glyph-width glyph) (glyph-height glyph))
  (send dc set-alpha 1)
  (send dc set-pen "yellow" (/ 2 size) 'solid)
  (send dc draw-line -1 0 1 0)
  (send dc draw-line 0 (- -1 (glyph-height glyph)) 0 1)
  (send dc set-pen "green" (/ 2 size) 'solid)
  (send dc draw-line
	(glyph-horizontal-advance glyph) (- -1 (glyph-height glyph))
	(glyph-horizontal-advance glyph) 1)
  (send dc set-scale sx sy))

(define (cartoon-glyphs face glyphs size dc)
  (define advancer (make-horizontal-glyph-advancer face))
  (for-each (lambda (g)
	      (match-define (cons kx ky) (advancer g))
	      (send dc translate (* size kx) (* size (- ky)))
	      (cartoon-glyph g size dc)
	      (send dc translate (* size (- kx)) (* size ky)))
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
(define label-size 72)

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
		(send dc set-transformation identity-transformation)
		(send dc set-smoothing 'smoothed)
		(send dc translate 100 200)
		(cartoon-glyphs face (string->glyphs face label-text) label-size dc)
                (send dc set-brush "black" 'solid)
                (send dc set-pen "black" 0 'transparent)
		(draw-glyphs face (string->glyphs face label-text) label-size dc)
		)])

(send frame show #t)
