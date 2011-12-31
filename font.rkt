#lang racket/base

(require racket/match)
(require racket/port)
(require file/gunzip)
(require srfi/2)

(provide (struct-out font-face)
	 (struct-out glyph)

	 missing-glyph
	 missing-glyph?

	 read-faces-from-file
	 read-faces
	 read-face

	 character->glyph
	 kerning)

(struct font-face (family
		   style
		   ascender
		   descender
		   height
		   max-advance-width
		   max-advance-height
		   underline-position
		   underline-thickness
		   character-map
		   kerning
		   glyphs)
	#:transparent)

(struct glyph (number
	       width
	       height
	       horizontal-bearing-x
	       horizontal-bearing-y
	       horizontal-advance
	       vertical-bearing-x
	       vertical-bearing-y
	       vertical-advance
	       path)
	#:transparent)

(define missing-glyph
  (glyph #f 1 1 0 1 1 -0.5 0 1 '((move-to 0.1 0.1)
				 (line-to 0.9 0.1)
				 (line-to 0.9 0.9)
				 (line-to 0.1 0.9)
				 (line-to 0.1 0.1)
				 (close-path)
				 (move-to 0.2 0.2)
				 (line-to 0.8 0.2)
				 (line-to 0.8 0.8)
				 (line-to 0.2 0.8)
				 (line-to 0.2 0.2)
				 (close-path))))

(define (missing-glyph? g)
  (eq? (glyph-number g) #f))

(define gunzip-pipe-buffer (make-parameter 65536))

(define (gunzip-pipe in)
  (define-values (pin pout) (make-pipe (gunzip-pipe-buffer)))
  (thread (lambda ()
	    (gunzip-through-ports in pout)
	    (close-output-port pout)))
  pin)

(define (read-faces-from-file filename)
  (call-with-input-file filename
    (lambda (p) (read-faces (gunzip-pipe p)))))

(define (read-faces in)
  (define f (read-face in))
  (if f
      (cons f (read-faces in))
      '()))

(define (read-face in)
  (and-let* ((f0 (read-face-header in))
	     (character-map (read-unicode-map in))
	     (kerning-map (read-kerning-map in))
	     (glyphs (read-glyphs in (hash))))
    (struct-copy font-face f0
		 [character-map character-map]
		 [kerning kerning-map]
		 [glyphs glyphs])))

(define (next-line in)
  (match (read-line in)
    [(? eof-object?) ""]
    [line line]))

(define (port-prefix? str in)
  (match (peek-string (string-length str) 0 in)
    [(? eof-object?) #f]
    [val (string=? str val)]))

(define font-face-setters
  (hash
   "familyName" (lambda (f v) (struct-copy font-face f [family (unescape-string v)]))
   "styleName" (lambda (f v) (struct-copy font-face f [style (unescape-string v)]))
   "ascender" (lambda (f v) (struct-copy font-face f [ascender (unscale-string v)]))
   "descender" (lambda (f v) (struct-copy font-face f [descender (unscale-string v)]))
   "height" (lambda (f v) (struct-copy font-face f [height (unscale-string v)]))
   "maxAdvanceWidth" (lambda (f v) (struct-copy font-face f
						[max-advance-width (unscale-string v)]))
   "maxAdvanceHeight" (lambda (f v) (struct-copy font-face f
						 [max-advance-height (unscale-string v)]))
   "underlinePosition" (lambda (f v) (struct-copy font-face f
						  [underline-position (unscale-string v)]))
   "underlineThickness" (lambda (f v) (struct-copy font-face f
						   [underline-thickness (unscale-string v)]))))

(define (read-face-header in)
  (and (port-prefix? "Face " in)
       (begin (next-line in)
	      (read-face-headers in (font-face #f #f #f #f #f
					       #f #f #f #f
					       #f #f #f)))))

(define (read-face-headers in f)
  (match (regexp-split ": *" (next-line in))
    [(list key val)
     (read-face-headers in ((hash-ref font-face-setters key) f val))]
    [(list "") ;; no instances of the separator found. See regexp-split docs
     f]
    [_
     #f]))

(define (unescape-string str)
  (substring str 1 (- (string-length str) 1)))

(define (unscale-string str)
  (/ (string->number str) 65536.0))

(define (read-unicode-map in)
  (and (string=? "Unicode" (next-line in))
       (read-unicode-map-entries in (hash))))

(define (read-unicode-map-entries in base)
  (match (regexp-split ":" (next-line in))
    [(list ustr gstr)
     (read-unicode-map-entries in (hash-set base
					    (string->number ustr)
					    (string->number gstr)))]
    [(list "")
     base]
    [_
     #f]))

(define (read-kerning-map in)
  (and (string=? "Kerning" (next-line in))
       (read-kerning-map-entries in (hash))))

(define (read-kerning-map-entries in base)
  (match (regexp-split ":" (next-line in))
    [(list g1 g2 xoff yoff)
     (read-kerning-map-entries in (hash-set base
					    (cons (string->number g1)
						  (string->number g2))
					    (cons (unscale-string xoff)
						  (unscale-string yoff))))]
    [(list "")
     base]
    [_
     #f]))

(define (read-glyphs in base)
  (define g (read-glyph in))
  (if g
      (read-glyphs in (hash-set base (glyph-number g) g))
      base))

(define glyph-setters
  (hash
   "width" (lambda (g v) (struct-copy glyph g [width (unscale-string v)]))
   "height" (lambda (g v) (struct-copy glyph g [height (unscale-string v)]))
   "horiBearingX" (lambda (g v) (struct-copy glyph g [horizontal-bearing-x (unscale-string v)]))
   "horiBearingY" (lambda (g v) (struct-copy glyph g [horizontal-bearing-y (unscale-string v)]))
   "horiAdvance" (lambda (g v) (struct-copy glyph g [horizontal-advance (unscale-string v)]))
   "vertBearingX" (lambda (g v) (struct-copy glyph g [vertical-bearing-x (unscale-string v)]))
   "vertBearingY" (lambda (g v) (struct-copy glyph g [vertical-bearing-y (unscale-string v)]))
   "vertAdvance" (lambda (g v) (struct-copy glyph g [vertical-advance (unscale-string v)]))
   "path" (lambda (g v) (struct-copy glyph g [path (compile-path v)]))))

(define (read-glyph in)
  (and (port-prefix? "Glyph " in)
       (let ((label (next-line in)))
	 (read-glyph-headers in (glyph (string->number (substring label 6))
				       #f #f
				       #f #f #f
				       #f #f #f
				       #f)))))

(define (read-glyph-headers in g)
  (match (regexp-split ": *" (next-line in))
    [(list key val)
     (read-glyph-headers in ((hash-ref glyph-setters key) g val))]
    [(list "")
     g]
    [_
     #f]))

(define (compile-path path-str)
  (define pieces (filter (lambda (p) (positive? (string-length p)))
			 (regexp-split " +" path-str)))
  (let compile ((remainder pieces)
		(instructions-rev '()))
    (match remainder
      ['() (reverse instructions-rev)]
      [(list-rest "M" x y rest)
       (compile rest (cons (list 'move-to (unscale-string x) (unscale-string y))
			   instructions-rev))]
      [(list-rest "L" x y rest)
       (compile rest (cons (list 'line-to (unscale-string x) (unscale-string y))
			   instructions-rev))]
      [(list-rest "C" x1 y1 x2 y2 x3 y3 rest)
       (compile rest (cons (list 'curve-to
				 (unscale-string x1) (unscale-string y1)
				 (unscale-string x2) (unscale-string y2)
				 (unscale-string x3) (unscale-string y3))
			   instructions-rev))]
      [(list-rest "O" rest)
       (compile rest (cons (list 'close-path)
			   instructions-rev))])))

(define (character->glyph face ch)
  (define glyph-number (hash-ref (font-face-character-map face) (char->integer ch) #f))
  (if glyph-number
      (hash-ref (font-face-glyphs face) glyph-number missing-glyph)
      missing-glyph))

(define (kerning face glyph1 glyph2)
  (hash-ref (font-face-kerning face)
	    (cons (glyph-number glyph1)
		  (glyph-number glyph2))
	    (cons 0 0)))
