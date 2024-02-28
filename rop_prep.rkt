#lang racket

; Task:
;   We would like a probabilistic function called `generate` that tends 
;   to generate sequences of tokens that "look like" data that is given
;   (e.g., the empirical distribution). It is not clear what "look like"
;   means---we don't want to "overfit" to the data, and it's okay to
;   generate sequences that aren't in the empirical data, but we want to
;   use the empirical distribution to guide the distribution that
;   `generate` produces.

;   Here's an example of what DATA could look like:
(define DATA 
  '((< < > < > > EOS) ; sequence #1
    (< < > < > > EOS) ; sequence #2 from the data distribution
    (< < < > < > > > EOS) ; ...
    (< < < < > < > > > > EOS) ; ...
    (< < < > < > > > EOS)
    (< > < > EOS)))

;   This is a highly open-ended task, and you are not expected to provide
;   a complete and sound solution. This task is meant to get an idea of
;   how you think and whether you can make progress in these kinds of
;   ambiguous situations. A good first step is to break the problem into
;   parts, and see if there are parts or sub-problems that are approachable.

;   To provide you with an example of the type of Racket programming that
;   we will likely do in this ROP, here is some code that generates
;   "random" sequences (without regard to the empirical data).

(define EOS 'EOS) ; the "end of sequence" token
(define TOKENS '(< > EOS))

; The functions gen-token-* generate the next token in the sequence,
; given the previous tokens seq.

(define (gen-token-random seq)
  ; Return a random token from our list TOKENS without considering
  ; the tokens in seq at all. Each token has equal probability of
  ; being selected.
  (list-ref TOKENS (random 3)))

(define ((gen-token-stateless prob) seq)
  ; Return a random token from our list TOKENS without considering
  ; the tokens in seq at all. The probability that each token is
  ; selected is given by `prob`: a vector of cumulative probabilities.
  ; For example, prob = '#(0.3 0.5 1) corresponding to the probabilities
  ;       P(<) = 0.3
  ;       P(>) = 0.5-0.3 = 0.2
  ;       P(EOS) = 1-0.5
  (let ((r (random)))
    (let loop ((i 0))
      (if (> (vector-ref prob i) r)
          (list-ref TOKENS i)
          (loop (+ i 1))))))

; Generates a sequence of tokens given a token generating function
;      genfn - token generating function
;      seq   - currently generated sequence so far
(define ((generate genfn) seq)
  (let* ((next (genfn seq))
         (seq^ (append seq (list next))))
    (if (equal? next EOS)
        seq^
        ((generate genfn) seq^))))


; generate and print 20 sequences, where each token comes from the uniform
; random distribution
(pretty-write (map (generate gen-token-random)
                   (make-list 20 '())))

; generate and print 20 sequences, where each token comes from the 
; distribution P(<) = P(>) = 0.4 and P(EOS) = 0.2
(pretty-write (map (generate (gen-token-stateless '#(0.4 0.8 1)))
                   (make-list 20 '())))

