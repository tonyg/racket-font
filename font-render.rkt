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
(require racket/draw)

(require "font.rkt")
(require "cache.rkt")

(provide glyph->path
	 draw-glyph
	 string->glyphs
	 glyphs->path
	 draw-glyphs
	 make-horizontal-glyph-advancer
	 age-font-caches!)

(define GENERATIONS 5)

(define GLYPH-CACHE (make-cache hasheq GENERATIONS))

(define (glyph->path glyph)
  (cache-lookup/insert! GLYPH-CACHE glyph
   (lambda ()
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
     path)))

(define (draw-glyph glyph dc)
  (send dc draw-path (glyph->path glyph)))

(define (string->glyphs face str [low 0] [high (string-length str)])
  (do ((i (- high 1) (- i 1))
       (glyphs '() (cons (character->glyph face (string-ref str i)) glyphs)))
      ((< i low) glyphs)))

(define (glyphs->path* face glyphs)
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

(define FACE-CACHE (make-cache hasheq GENERATIONS))
(define (glyphs->path face glyphs)
  (define glyphs-cache (cache-lookup/insert! FACE-CACHE face
					     (lambda () (make-cache hasheq GENERATIONS))))
  (cache-lookup/insert! glyphs-cache glyphs (lambda () (glyphs->path* face glyphs))))

;; (define cache (make-hash))
;; (define cache-accesses 0)
;; (define cache-misses 0)
;; (define bitmap-supersample 1)
;; (struct cache-entry (bitmap left top width height) #:transparent)
;; (define (draw-glyphs face glyphs size dc)
;;   (define pen (send dc get-pen))
;;   (define brush (send dc get-brush))
;;   (define key (list (font-face-family face)
;;  		    (font-face-style face)
;;  		    (map glyph-number glyphs)
;; 		    pen
;; 		    brush))
;;   (set! cache-accesses (+ cache-accesses 1))
;;   (match-define (cache-entry bitmap left top width height)
;;     (hash-ref cache key
;; 	      (lambda ()
;; 		(define path (glyphs->path face glyphs))
;; 		(define-values (left top width height)
;; 		  (send path get-bounding-box))
;; 		(define bitmap
;; 		  (make-object bitmap%
;; 			       (max 1 (inexact->exact (ceiling (* width size bitmap-supersample))))
;; 			       (max 1 (inexact->exact (ceiling (* height size bitmap-supersample))))
;; 			       #f #t))
;; 		(define bdc (new bitmap-dc% [bitmap bitmap]))
;; 		(send bdc set-smoothing 'smoothed)
;; 		(send bdc set-pen pen)
;; 		(send bdc set-brush brush)
;; 		(send bdc set-scale (* bitmap-supersample size) (* bitmap-supersample size))
;; 		(send bdc draw-path path (- left) (- top))
;; 		(send bdc flush)
;; 		(define entry (cache-entry bitmap
;; 					   (* bitmap-supersample size left)
;; 					   (* bitmap-supersample size top)
;; 					   (* bitmap-supersample size width)
;; 					   (* bitmap-supersample size height)))
;; 		(hash-set! cache key entry)
;; 		(set! cache-misses (+ cache-misses 1))
;; 		(write `(cache count ,(hash-count cache)
;; 			       accesses ,cache-accesses
;; 			       hits ,(- cache-accesses cache-misses)
;; 			       misses ,cache-misses
;; 			       )) (newline)
;; 		entry)))
;;   ;;(send dc set-scale (/ bitmap-supersample) (/ bitmap-supersample))
;;   (send dc draw-bitmap bitmap left top)
;;   ;;(send dc set-scale 1 1)
;;   )

;; (define cache (make-hash))
;; (define cache-accesses 0)
;; (define cache-misses 0)
;; (define (draw-glyphs face glyphs size dc)
;;   (define key (list (font-face-family face)
;; 		    (font-face-style face)
;; 		    (map glyph-number glyphs)))
;;   (set! cache-accesses (+ cache-accesses 1))
;;   (define path (hash-ref cache key (lambda ()
;; 				     (define path (glyphs->path face glyphs))
;; 				     (hash-set! cache key path)
;; 				     (set! cache-misses (+ cache-misses 1))
;; 				     (write `(cache count ,(hash-count cache)
;; 						    accesses ,cache-accesses
;; 						    hits ,(- cache-accesses cache-misses)
;; 						    misses ,cache-misses
;; 						    )) (newline)
;; 				     path)))
;;   (define-values (sx sy) (send dc get-scale))
;;   (send dc set-scale size size)
;;   (send dc draw-path path)
;;   (send dc set-scale sx sy))

;; SIDE EFFECT: alters the scale of the dc!
(define (draw-glyphs face glyphs size dc)
  (define-values (sx sy) (send dc get-scale))
  (when (not (= sx sy size))
    (send dc set-scale size size))
  (send dc draw-path (glyphs->path face glyphs)))

;; (define (draw-glyphs face glyphs size dc)
;;   (void))

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

(define (age-font-caches!)
  (cache-age! GLYPH-CACHE)
  (log-info "GLYPH-CACHE occupancy: ~v" (cache-stats GLYPH-CACHE))
  (cache-age! FACE-CACHE)
  (log-info "FACE-CACHE occupancy: ~v" (cache-stats FACE-CACHE))
  (for ([(face glyphs-cache) (in-cache FACE-CACHE)])
    (cache-age! glyphs-cache)
    (log-info "  occupancy for ~v/~v: ~v"
	      (font-face-family face)
	      (font-face-style face)
	      (cache-stats glyphs-cache))))
