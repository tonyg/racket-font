#lang racket/base
;; Generational cache, after https://gist.github.com/lukego/4706097

(require racket/list)
(require racket/set)
(require racket/generator)

(provide make-cache
	 default-cache-generations
	 cache?
	 cache-clear!
	 cache-insert!
	 cache-remove!
	 cache-has-key?
	 cache-lookup
	 cache-lookup/insert!
	 cache-age!
	 cache-age!/finalization
	 cache-keys
	 cache->hash
	 cache-stats
	 in-cache)

(struct cache (new olds hash-maker) #:mutable #:transparent)

(define (make-cache [hash-maker hash] [n-olds (default-cache-generations)])
  (cache (hash-maker) (for/list ([n (in-range n-olds)]) (hash-maker)) hash-maker))

(define default-cache-generations (make-parameter 1))

(define (cache-clear! c)
  (set-cache-new! c ((cache-hash-maker c)))
  (set-cache-olds! c (map (lambda (_) ((cache-hash-maker c))) (cache-olds c)))
  c)

(define (cache-insert! c k v)
  (set-cache-new! c (hash-set (cache-new c) k v))
  v)

(define (cache-remove! c k)
  (set-cache-new! c (hash-remove (cache-new c) k))
  (set-cache-olds! c (map (lambda (h) (hash-remove h k)) (cache-olds c)))
  c)

(define (cache-has-key? c k)
  (or (hash-has-key? (cache-new c) k)
      (ormap (lambda (t) (hash-has-key? t k)) (cache-olds c))))

(define (cache-lookup c k [missing (lambda () #f)])
  (hash-ref (cache-new c)
            k
            (lambda ()
	      (let loop ((olds (cache-olds c)))
		(cond
		 [(null? olds)
		  (if (procedure? missing)
		      (missing)
		      missing)]
		 [(hash-has-key? (car olds) k)
                  (cache-insert! c k (hash-ref (car olds) k))]
		 [else
		  (loop (cdr olds))])))))

(define (cache-lookup/insert! c k missing)
  (cache-lookup c k
		(lambda ()
		  (cache-insert! c k (if (procedure? missing)
					 (missing)
					 missing)))))

(define (cache-age! c)
  (set-cache-olds! c (cons (cache-new c) (drop-right (cache-olds c) 1)))
  (set-cache-new! c ((cache-hash-maker c))))

(define (cache-age!/finalization c)
  (define oldest (last (cache-olds c)))
  (cache-age! c)
  (for/fold ([dying ((cache-hash-maker c))])
      ([(k v) (in-hash oldest)])
    (if (cache-has-key? c k)
	dying
	(hash-set dying k v))))

(define (cache-keys c)
  (for/fold ([keys (list->set (hash-keys (cache-new c)))])
      [(o (in-list (cache-olds c)))]
    (set-union keys (list->set (hash-keys o)))))

(define (cache->hash c)
  (for/fold ([h (cache-new c)])
      [(o (in-list (cache-olds c)))]
    (for/fold ([h h])
	[((k v) (in-hash o))]
      (if (hash-has-key? h k)
	  h
	  (hash-set h k v)))))

(define (cache-stats c)
  (cons (hash-count (cache-new c))
	(map hash-count (cache-olds c))))

(define (in-cache c)
  (define seen-keys ((cache-hash-maker c)))
  (in-generator
   #:arity 2
   (for ([(k v) (in-hash (cache-new c))]) (yield k v))
   (for ([o (in-list (cache-olds c))])
     (for ([(k v) (in-hash o)])
       (when (not (hash-has-key? seen-keys k))
	 (set! seen-keys (hash-set seen-keys k #t))
	 (yield k v))))))
