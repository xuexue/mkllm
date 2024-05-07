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

; Function to tally transitions from one token to another
(define (tally-transitions data)
  (let ([table (make-hash)])
    (for ([seq data])
      (for ([i (in-range 1 (length seq))])
        (let* ([prev (list-ref seq (sub1 i))]
               [next (list-ref seq i)])
          (hash-update! table (cons prev next) add1 0))))
    table))

; Function to calculate transitional probabilities based on transition counts
(define (transitional-probabilities data)
  (let* ([counts (tally-transitions data)]
         [totals (make-hash)])
    (hash-for-each
     counts
     (lambda (pair count)
       (hash-update! totals (car pair) add1 count)))
    (hash-map
     counts
     (lambda (pair count)
       (let ([total (hash-ref totals (car pair))])
         (cons pair (/ count total)))))))

(define transition-probs (transitional-probabilities DATA))

; Function to generate sequences based on the given transitional probabilities (list of pairs)
(define (generate-sequence prob-pairs)
  (letrec ([gen-next-token (lambda (current-token)
                             (let* ([filtered-pairs (filter (lambda (pair) (eq? (car (car pair)) current-token))
                                                            prob-pairs)]
                                    [total (apply + (map cdr filtered-pairs))]
                                    [rand-value (* (random) total)]
                                    [sum 0]
                                    [result 'EOS])
                               (for ([pair (in-list filtered-pairs)]
                                     #:break (>= sum rand-value))
                                 (set! sum (+ sum (cdr pair)))
                                 (set! result (cdr (car pair))))
                               result))]
           [generate (lambda (sequence)
                       (let ([next-token (gen-next-token (last sequence))])
                         (if (eq? next-token 'EOS)
                             sequence
                             (generate (append sequence (list next-token))))))])
    (generate (list (list-ref TOKENS (random (length TOKENS)))))))

(define generated-sequences (for/list ([i (in-range 10)])
                                     (generate-sequence transition-probs)))
(for-each pretty-print generated-sequences)


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

#|
; generate and print 20 sequences, where each token comes from the uniform
; random distribution
(pretty-write (map (generate gen-token-random)
                   (make-list 20 '())))

; generate and print 20 sequences, where each token comes from the 
; distribution P(<) = P(>) = 0.4 and P(EOS) = 0.2
(pretty-write (map (generate (gen-token-stateless '#(0.4 0.8 1)))
                   (make-list 20 '())))
|#