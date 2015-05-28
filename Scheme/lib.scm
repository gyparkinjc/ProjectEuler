;; Delayed Stream Construction
(define-syntax _cons
  (syntax-rules ()
    ((_cons a b)
     (cons a (delay b)))))
(define (_car stream) (car stream))
(define (_cdr stream) (force (cdr stream)))

;; Util Functions
(define (divisible? a b)
  (= (remainder a b) 0))

(define (is_palindrome? n)
  (let ((l (string->list (number->string n))))
    (equal? l (reverse l))))

(define (prime-factor n)
  (let loop ((num n)
             (rest primes))
    (cond ((= num 1)
           '())
          ((not (divisible? num (_car rest)))
           (loop num (_cdr rest)))
          (else
           (cons (_car rest)
                  (loop (/ num (_car rest))
                        rest))))))

;; List Utils Functions
(define (take n list)
  (if (or (= n 0) (null? list))
      '()
      (cons (car list)
            (take (- n 1)
                  (cdr list)))))

(define (drop n list)
  (if (or (= n 0) (null? list))
      list
      (drop (- n 1)
            (cdr list))))

(define (repeat n cnt)
  (if (= cnt 0)
      '()
      (cons n (repeat n (- cnt 1)))))

;; Stream Util Functions
(define (_map f stream)
  (if (null? stream)
      '()
      (_cons (f (_car stream))
             (_map f (_cdr stream)))))

(define (_filter pred stream)
  (cond ((null? stream)
         '())
        ((pred (_car stream))         
         (_cons (_car stream)
                (_filter pred (_cdr stream))))
        (else 
         (_filter pred (_cdr stream)))))

(define (_fold f acc stream)
  (if (null? stream)
      acc
      (_fold f
             (f acc (_car stream))
             (_cdr stream))))

(define (_take n stream)
  (if (or (= n 0) (null? stream))
      '()
      (_cons (_car stream)
             (_take (- n 1)
                    (_cdr stream)))))

(define (_takewhile pred stream)
  (if (or (null? stream) (not (pred (_car stream))))
      '()
      (_cons (_car stream)
             (_takewhile pred (_cdr stream)))))

(define (_ref n stream)
  (if (= n 0)
      (_car stream)
      (_ref (- n 1) (_cdr stream))))  

(define (_reverse stream)
  (let loop ((s stream)
             (result '()))
    (if (null? s)
        result
        (loop (_cdr s)
              (_cons (_car s) result)))))

(define (_append . lists)
  (define (_merge a b)
    (if (null? a)
        b
        (_cons (_car a)
               (_merge (_cdr a) b))))
  
  (let loop ((rest lists))
    (if (null? (_cdr rest))
        (_car rest)
        (_merge (_car rest)
                (loop (_cdr rest))))))

(define (_flat streams)
  (cond ((null? streams)
         '())
        ((not (pair? (_car streams)))
         (_cons (_car streams)
                (_flat (_cdr streams))))
        (else
         (_append (_flat (_car streams))
                  (_flat (_cdr streams))))))

(define (_concat streams)
  (if (null? (_cdr streams))
      (_car streams)
      (_append (_car streams) 
               (_concat (_cdr streams)))))

(define (_flatmap proc streams)
  (_concat (_map proc streams)))

(define (_max streams)
  (_fold
   (lambda (acc x)
     (if (> acc x)
         acc
         x))
   0
   streams))

(define (stream->list streams)
  (cond ((null? streams)
         '())
        ((not (pair? (_car streams)))
         (cons (_car streams)
               (stream->list (_cdr streams))))
        (else
         (cons (stream->list (_car streams))
               (stream->list (_cdr streams))))))

(define (_print stream)
  (begin
    (display "[ ")
    (let loop ((s stream))
      (cond ((null? s)
             (display "]"))
            ((not (pair? (_car s)))
             (begin
               (display (_car s))
               (display " ")
               (loop (_cdr s))))
            (else
             (begin
               (_print (_car s))
               (loop (_cdr s))))))))

;; Several Streams
(define (_range begin end step)
  (if (>= begin end)
      '()
      (_cons begin
             (_range (+ begin step)
                     end
                     step))))

(define (_seq n step)
  (_cons n
         (_seq (+ n step)
                 step)))

(define fibs
  (let loop ((a 1) (b 1))
    (_cons a (loop (+ a b) a))))

;(define primes
;  (let sieve ((stream (_seq 2 1)))
;    (_cons (_car stream)
;           (sieve (_filter
;                   (lambda (x) 
;                     (not (divisible? x (_car stream))))
;                   (_cdr stream))))))
(define primes
  (_cons
   2
   (_filter prime? (_seq 3 2))))

(define (prime? n)
  (let loop ((ps primes))
    (cond ((> (sqr (_car ps)) n) #t)
          ((divisible? n (_car ps)) #f)
          (else (loop (_cdr ps))))))

(define tri-numbers
  (let loop ((n 1) (cnt 2))
    (_cons n (loop (+ n cnt)
                   (+ cnt 1)))))           