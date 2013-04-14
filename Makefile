all:
	raco make layout.rkt

run: all
	racket layout.rkt

clean:
	find . -name compiled -type d | xargs rm -rf
