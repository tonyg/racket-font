#lang racket/base

(require racket/set)
(require racket/match)

(require "font.rkt")

(provide current-font-directory
	 register-faces!
	 register-face!
	 font-faces-matching
	 font-face-matching)

(define current-font-directory (make-parameter (set)))

(define (register-faces! faces)
  (for-each register-face! faces))

(define (register-face! face)
  (current-font-directory (set-add (current-font-directory) face)))

(define (font-faces-matching family-pattern [style-pattern ""])
  (filter (lambda (f)
	    (and (regexp-match family-pattern (font-face-family f))
		 (regexp-match style-pattern (font-face-style f))))
	  (set->list (current-font-directory))))

(define (font-face-matching family-pattern [style-pattern ""])
  ;; Just some random element
  (match (font-faces-matching family-pattern style-pattern)
    [(cons f _) f]
    ['() #f]))
