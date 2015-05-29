(load "lib.scm")

;;---------------------
;; Solution1
;;---------------------
(define (solution1)
  (define (pred x)
    (or (= (remainder x 3) 0)
        (= (remainder x 5) 0)))
  
  (_fold + 0 (_filter pred (_range 1 1000 1))))
;;---------------------
;; Solution2
;;---------------------
(define (solution2)  
  (_fold + 0 (_takewhile (lambda (x) (< x 4000000)) (_filter even? fibs))))
;;---------------------
;; Solution3
;;---------------------
(define (solution3)  
  (_car (_reverse (prime-factor 600851475143))))
;;---------------------
;; Solution4
;;---------------------
(define (solution4)
  (define products
    (_flatmap 
     (lambda (x)
       (_map (lambda (y) (* x y)) (_range x 1000 1)))
     (_range 100 1000 1)))
  
  (_max (_filter is_palindrome? products)))
;;---------------------
;; Solution5
;;---------------------
(define (solution5)
  (_fold lcm 1 (_range 2 21 1)))
;;---------------------
;; Solution6
;;---------------------
(define (solution6)
  (define sum-of-sqr
    (_fold + 0 (_map sqr (_range 1 101 1))))
  
  (define sqr-of-sum
    (sqr (_fold + 0 (_range 1 101 1))))
  
  (- sqr-of-sum sum-of-sqr))
;;---------------------
;; Solution7
;;---------------------
(define (solution7)
  (_ref 10000 primes))
;;---------------------
;; Solution8
;;---------------------
(define (solution8)
  (define 1000-digit
    (call-with-input-file "../data/number_8.txt"
      (lambda (in)
        (let loop ((value (read-line in)))
          (if (equal? value eof)
              '()
              (append
               (filter (lambda (x) (not (equal? x #\return))) (string->list value))
               (loop (read-line in))))))))
  
  (define digit-list   
    (map (lambda (c) (string->number (string c))) 1000-digit))

  (define (product-list list)
    (if (null? list)
        '()
        (cons (foldl * 1 (take 13 list))
              (product-list (cdr list)))))
        
  (foldl max 0 (product-list digit-list)))
;;---------------------
;; Solution9
;;---------------------
(define (solution9)
  (define 1000-pairs
    (_flatmap (lambda (x)
                (_map (lambda (y)
                        (let ((z (- 1000 (+ x y))))
                          (list x y z)))
                      (_range x 1000 1)))
              (_range 2 1000 1)))
  
  (define pytha-pair
    (_filter
     (lambda (pair)
       (let ((a (sqr (car pair)))
             (b (sqr (cadr pair)))
             (c (sqr (caddr pair))))
         (cond ((= a (+ b c)) #t)
               ((= b (+ a c)) #t)
               ((= c (+ a b)) #t)
               (else #f))))
     1000-pairs))
  
  (foldl * 1 (_car pytha-pair)))
;;---------------------
;; Solution10
;;---------------------
(define (solution10)
  (_fold + 0 (_takewhile (lambda (x) (< x 2000000)) primes)))
;;---------------------
;; Solution11
;;---------------------
(define (solution11)
  (define num-list
    (call-with-input-file "../data/20x20.txt"
      (lambda (in)
        (let loop ((value (read in)))
          (if (equal? value eof)
              '()
              (cons value
                    (loop (read in))))))))
  
  (define grid
    (append
     (let loop ((rest num-list))
       (if (null? rest)
           '()
           (cons (append '(1 1 1) (take 20 rest) '(1 1 1))
                 (loop (drop 20 rest)))))
     (list (repeat 1 26) (repeat 1 26) (repeat 1 26))))  
  
  (define (maximum i j grid)
    (define (index a b table)
      (list-ref
       (list-ref table a)
       b))
    
    (foldl
     max
     0
     (list
      (foldl * 1 (map (lambda (k) (index i (+ j k) grid)) '(0 1 2 3)))
      (foldl * 1 (map (lambda (k) (index (+ i k) j grid)) '(0 1 2 3)))
      (foldl * 1 (map (lambda (k) (index (+ i k) (+ j k) grid)) '(0 1 2 3)))
      (foldl * 1 (map (lambda (k) (index (+ i k) (- j k) grid)) '(0 1 2 3))))))
  
  (define pair
    (_flatmap (lambda (i)
                (_map (lambda (j)
                        (list i j))
                      (_range 3 22 1)))
              (_range 0 20 1)))
  
  (_fold
   max
   0
   (_map (lambda (p)
           (maximum (_car p)
                    (_car (_cdr p))
                    grid))
         pair)))
;;---------------------
;; Solution12
;;---------------------
(define (solution12)
  (define (f-pair factor)
    (let ((pair-list (map (lambda (x) 
                            (list x 1)) 
                          factor)))
      (foldl 
       (lambda (p acc)
         (if (or (null? acc) 
                 (not (= (car p) (caar acc))))
             (cons p acc)             
             (cons (list (car p)
                         (+ (cadar acc) 1))
                   (cdr acc))))
       '()
       pair-list)))
  
  (define (num-of-divisor n)
    (let ((pair-list (f-pair (prime-factor n))))
      (let ((exp-list (map (lambda (p) (+ (cadr p) 1)) 
                           pair-list)))
        (foldl * 1 exp-list))))
  
  (_car (_filter (lambda (p) 
                   (> (num-of-divisor p) 500))
                 tri-numbers)))
;;---------------------
;; Solution13
;;---------------------
(define (solution13)
  (define num-list
    (call-with-input-file "../data/numbers.txt"
      (lambda (in)
        (let loop ((value (read in)))
          (if (equal? value eof)
              '()
              (cons value
                    (loop (read in))))))))
  (string->number
   (list->string
    (take 10 
          (string->list 
           (number->string 
            (foldl + 0 num-list)))))))
;;---------------------
;; Solution14
;;---------------------
(define (solution14)
  (define (collatz n)
    (cond ((= n 1) '(1))
          ((odd? n) (cons n (collatz (+ (* 3 n) 1))))
          (else (cons n (collatz (/ n 2))))))
  
  (_car (_fold (lambda (acc pair)
                 (if (>= (cadr acc) (cadr pair))
                     acc
                     pair))
               '(1 1)
               (_map (lambda (n)
                       (list n
                             (length (collatz n))))
                     (_range 1 1000000 1)))))
;;---------------------
;; Solution15
;;---------------------
(define (solution15)
  (define path
    (memoize (lambda (pair)
               (let ((x (car pair))
                     (y (cdr pair)))
                 (if (or (= x 0) (= y 0))
                     1
                     (+ (path (cons (- x 1) y))
                        (path (cons x (- y 1)))))))))
  
  (path '(20 . 20)))

;;---------------------
;; Solution Framework
;;---------------------
(define sMap
  (list 
   (list 1 solution1)
   (list 2 solution2)
   (list 3 solution3)
   (list 4 solution4)
   (list 5 solution5)
   (list 6 solution6)
   (list 7 solution7)
   (list 8 solution8)
   (list 9 solution9)
   (list 10 solution10)
   (list 11 solution11)
   (list 12 solution12)
   (list 13 solution13)
   (list 14 solution14)
   (list 15 solution15)
  )
)

(define (lookup n table)
  (list-ref
   (list-ref table (- n 1))
   1))

(define (solution proj answer)  
  (let ((func (lookup proj sMap))
        (ans (lookup proj answer)))
    (let ((result (func)))
      (printf "[Proj ~a] : ~a  ~a" proj (equal? result ans) result))))

(define (solutionAll answer)
  (define (evalAll pair)
    (let ((proj (car pair)))
      (begin
        (solution proj answer)
        (newline))))
    
  (map evalAll sMap))

(define (main)
  (begin
    (let ((answer (call-with-input-file "../data/answer"
                    (lambda (in)
                      (let loop ((n 1)
                                 (value (read in)))
                        (if (equal? value eof)
                            '()
                            (cons (list n value)
                                  (loop (+ n 1) (read in))))))))) 
      (newline)
      (display "Input Number: ") 
      (let ((proj (read)))
        (if (= proj 0)
            (solutionAll answer)
            (solution proj answer))))))

(main)