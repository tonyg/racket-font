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
