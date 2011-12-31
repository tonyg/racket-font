#lang racket/base

(require racket/match)
(require racket/class)
(require racket/draw)

(require "font.rkt")

(provide glyph->path
	 draw-glyph
	 string->glyphs
	 glyphs->path
	 draw-glyphs
	 make-horizontal-glyph-advancer)

(define (glyph->path glyph)
  (define path (new dc-path%))
  (for-each (lambda (instruction)
	      (match instruction
		[(list 'move-to x y)
		 (send path move-to x (- y))]
		[(list 'line-to x y)
		 (send path line-to x (- y))]
		[(list 'curve-to x1 y1 x2 y2 x3 y3)
		 (send path curve-to x1 (- y1) x2 (- y2) x3 (- y3))]
		[(list 'close-path)
		 (send path close)]))
	    (glyph-path glyph))
  path)

(define (draw-glyph glyph dc)
  (send dc draw-path (glyph->path glyph)))

(define (string->glyphs face str [low 0] [high (string-length str)])
  (do ((i (- high 1) (- i 1))
       (glyphs '() (cons (character->glyph face (string-ref str i)) glyphs)))
      ((< i low) glyphs)))

(define (glyphs->path face glyphs)
  (define advancer (make-horizontal-glyph-advancer face))
  (define path (new dc-path%))
  (for-each (lambda (g)
	      (define gp (glyph->path g))
	      (match-define (cons kx ky) (advancer g))
	      (send gp translate kx (- ky))
	      (send path append gp)
	      (send path close))
	    glyphs)
  path)

(define (draw-glyphs face glyphs dc)
  (send dc draw-path (glyphs->path face glyphs)))

(define (make-horizontal-glyph-advancer face)
  (define offset 0)
  (define previous-glyph #f)
  (lambda (g)
    (define p previous-glyph)
    (set! previous-glyph g)
    (match-define (cons kx ky) (if p (kerning face p g) (cons 0 0)))
    (set! offset (+ offset kx))
    (define rx offset)
    (set! offset (+ offset (glyph-horizontal-advance g)))
    (cons rx ky)))
