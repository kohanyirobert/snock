(import (rnrs (6)))

(define n
  (case-lambda
    ((a) (n a (lambda (l) l)))
    ((a r)
     (cond
       ((list? a)
        (let ((l (length a)))
          (cond
            ((equal? l 0) (raise 1))
            ((equal? l 1) (r (car a)))
            (else
              (let ((t (cond
                         ((equal? l 2) (cadr a))
                         (else (cdr a)))))
                (n (car a)
                   (lambda (p) (n t
                                  (lambda (q) (r (cons p q)))))))))))
       ((fixnum? a) (r a))
       (else (raise 2))))))

(define (wut a)
  (cond
    ((fixnum? a) 1)
    (else 0)))

(define (lus a)
  (cond
    ((equal? (wut a) 0) (raise 3))
    (else (+ 1 a))))

(define (tis a)
  (cond
    ((equal? (wut a) 1) (raise 4))
    ((equal? (car a) (cdr a)) 0)
    (else 1)))

(define fas
  (case-lambda
    ((a) (fas a (lambda (l) l)))
    ((a r)
     (cond
       ((equal? (wut a) 1) (raise 5))
       (else
         (let ((h (car a))
               (t (cdr a)))
           (cond
             ((not (fixnum? h)) (raise 6))
             ((equal? h 0) (raise 7))
             ((equal? h 1) (r t))
             ((fixnum? t) (raise 8))
             ((equal? h 2) (r (car t)))
             ((equal? h 3) (r (cdr t)))
             (else
               (fas (cons (div h 2) t)
                    (lambda (p) (fas (cons (+ 2 (mod h 2)) p)
                                     (lambda (q) (r q)))))))))))))

(define tar
  (case-lambda
    ((a) (tar a (lambda (l) l)))
    ((a r)
     (cond
       ((equal? (wut a) 1) (raise 9))
       (else
         (let ((s (car a))
               (f (cdr a)))
           (cond
             ((equal? (wut f) 1) (raise 10))
             (else
               (let ((o (car f))
                     (v (cdr f)))
                 (cond
                   ((equal? (wut o) 0) (tar (cons s o)
                                            (lambda (p) (tar (cons s v)
                                                             (lambda (q) (r (cons p q)))))))
                   ((equal? o 0) (r (fas (cons v s))))
                   ((equal? o 1) (r v))
                   ((equal? o 3) (tar (cons s v)
                                      (lambda (p) (r (wut p)))))
                   ((equal? o 4) (tar (cons s v)
                                      (lambda (p) (r (lus p)))))
                   ((equal? o 5) (tar (cons s v)
                                      (lambda (p) (r (tis p)))))
                   ((equal? (wut v) 1) (raise 11))
                   (else
                     (let ((x (car v))
                           (y (cdr v)))
                       (cond
                         ((equal? o 2) (tar (cons s x)
                                            (lambda (p) (tar (cons s y)
                                                             (lambda (q) (tar (cons p q)
                                                                              (lambda (u) (r u))))))))
                         ((equal? o 7) (tar (n (list (list s) 2 (list x) 1 (list y)))
                                            (lambda (p) (r p))))
                         ((equal? o 8) (tar (n (list (list s) 7 (list (list 7 (list 0 1) (list x)) 0 1) (list y)))
                                            (lambda (p) (r p))))
                         ((equal? o 9) (tar (n (list (list s) 7 (list y) 2 (list 0 1) 0 (list x)))
                                            (lambda (p) (r p))))
                         ((equal? o 10) (cond
                                          ((equal? (wut x) 1) (tar (cons s y)
                                                                   (lambda (p) (r p))))
                                          (else (tar (n (list (list s) 8 (list (cdr x)) 7 (list 0 3) (list y)))
                                                     (lambda (p) (r p))))))
                         ((equal? (wut y) 1) (raise 12))
                         ((equal? o 6) (tar (n (list
                                                 (list s)
                                                 2
                                                 (list 0 1)
                                                 2
                                                 (list 1 (list (car y)) (list (cdr y)))
                                                 (list 1 0)
                                                 2
                                                 (list 1 2 3)
                                                 (list 1 0)
                                                 4
                                                 4
                                                 (list x)))
                                            (lambda (p) (r p))))
                         (else (raise 13)))))))))))))))

(define (ok? a b)
  (assert (equal? a b)))

(ok? (guard (e (e)) (n '())) 1)
(ok? (guard (e (e)) (n 'a)) 2)
(ok? (n 1) 1)
(ok? (n '(1)) 1)
(ok? (n '(1 2)) '(1 . 2))
(ok? (n '(1 2 3)) '(1 2 . 3))
(ok? (n '(1 (2 3))) '(1 2 . 3))
(ok? (n '((1 2) 3)) '((1 . 2) . 3))
(ok? (n '((1 2) 3)) '((1 . 2) . 3))
(ok? (n '(1 2 3 4)) '(1 2 3 . 4))
(ok? (n '(1 2 (3 4))) '(1 2 3 . 4))
(ok? (n '(1 (2 3 4))) '(1 2 3 . 4))
(ok? (n '((1 2) 3 4)) '((1 . 2) . (3 . 4)))
(ok? (n '((1 2) (3 4))) '((1 . 2) . (3 . 4)))
(ok? (n '((1 2 3) 4)) '((1 2 . 3) . 4))
(ok? (n '(1 (2 3) 4)) '(1 (2 . 3) . 4))
(ok? (n '(1 2 3 4 5)) '(1 2 3 4 . 5))
(ok? (n '((1 2) 3 (4 5))) '((1 . 2) 3 . (4 . 5)))
(ok? (n '(1 (2 3) (4 5))) '(1 (2 . 3) . (4 . 5)))
(ok? (n '(1 2 3 4 5 6 7 8 9 10)) '(1 2 3 4 5 6 7 8 9 . 10))
(ok? (n '(((((((((1 2) 3) 4) 5) 6) 7) 8) 9) 10)) '(((((((((1 . 2) . 3) . 4) . 5) . 6) . 7) . 8) . 9) . 10))

(ok? (wut (n '(1 2))) 0)
(ok? (wut (n 1)) 1)
(ok? (lus (n 1)) 2)
(ok? (tis (n '(1 1))) 0)
(ok? (tis (n '(1 2))) 1)
(ok? (fas (n '(1 2))) 2)
(ok? (fas (n '(2 3 4))) 3)
(ok? (fas (n '(3 4 5))) 5)

(define x '((4 5) (6 14 15)))
(ok? (guard (e (e)) (fas (n (list 0 x)))) 7)
(ok? (fas (n (list 1 x))) '((4 . 5) . (6 . (14 . 15))))
(ok? (fas (n (list 2 x))) '(4 . 5))
(ok? (fas (n (list 3 x))) '(6 . (14 . 15)))
(ok? (fas (n (list 4 x))) 4)
(ok? (fas (n (list 5 x))) 5)
(ok? (fas (n (list 6 x))) 6)
(ok? (fas (n (list 7 x))) '(14 . 15))
(ok? (guard (e (e)) (fas (n (list 8 x)))) 8)
(ok? (guard (e (e)) (fas (n (list 9 x)))) 8)
(ok? (guard (e (e)) (fas (n (list 10 x)))) 8)
(ok? (guard (e (e)) (fas (n (list 11 x)))) 8)
(ok? (guard (e (e)) (fas (n (list 12 x)))) 8)
(ok? (guard (e (e)) (fas (n (list 13 x)))) 8)
(ok? (fas (n (list 14 x))) 14)
(ok? (fas (n (list 15 x))) 15)
(ok? (guard (e (e)) (fas (n (list 16 x)))) 8)

(ok? (tar (n '(2 0 1))) 2)
(ok? (tar (n '((2 3) 0 1))) '(2 . 3))
(ok? (tar (n '((2 3) 0 2))) 2)
(ok? (tar (n '((2 3) 0 3))) 3)

(ok? (tar (n '(1 (1 2)))) 2)
(ok? (tar (n '(1 (1 (2 3))))) '(2 . 3))

(ok? (tar (n '(1 (2 (1 2) (1 1 3 4))))) '(3 . 4))
(ok? (tar (n '(77 (2 (1 42) (1 1 18 77))))) '(18 . 77))
(ok? (tar (n '(1 (2 (4 0 1) (1 4 0 1))))) 3)
(ok? (tar (n '((18 (1 77)) 2 (0 1) (0 3)))) 77)

(define x '(((1 2) 3) (4 4)))
(ok? (guard (e (e)) (tar (n (list x '(3 0))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(3 1))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(3 2))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(3 3))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(3 4))))) 10)
(ok? (tar (n (list x '(3 (0 1))))) 0)
(ok? (tar (n (list x '(3 (0 2))))) 0)
(ok? (tar (n (list x '(3 (0 3))))) 0)
(ok? (tar (n (list x '(3 (0 4))))) 0)
(ok? (tar (n (list x '(3 (0 5))))) 1)

(ok? (guard (e (e)) (tar (n (list x '(4 0))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(4 1))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(4 2))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(4 3))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(4 4))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(4 (0 1)))))) 3)
(ok? (guard (e (e)) (tar (n (list x '(4 (0 2)))))) 3)
(ok? (guard (e (e)) (tar (n (list x '(4 (0 3)))))) 3)
(ok? (guard (e (e)) (tar (n (list x '(4 (0 4)))))) 3)
(ok? (tar (n (list x '(4 (0 5))))) 4)

(ok? (guard (e (e)) (tar (n (list x '(5 0))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(5 1))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(5 2))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(5 3))))) 10)
(ok? (guard (e (e)) (tar (n (list x '(5 4))))) 10)
(ok? (tar (n (list x '(5 (0 1))))) 1)
(ok? (tar (n (list x '(5 (0 2))))) 1)
(ok? (tar (n (list x '(5 (0 3))))) 0)
(ok? (tar (n (list x '(5 (0 4))))) 1)
(ok? (guard (e (e)) (tar (n (list x '(5 (0 5)))))) 4)
(ok? (guard (e (e)) (tar (n (list x '(5 (0 6)))))) 4)
(ok? (guard (e (e)) (tar (n (list x '(5 (0 7)))))) 4)

(ok? (tar (n '(1 (6 (1 0) (1 2) (1 3))))) 2)
(ok? (tar (n '(1 (6 (1 1) (1 2) (1 3))))) 3)

(ok? (tar (n '(1 7 (0 1) (0 1)))) 1)
(ok? (tar (n '((1 2) 7 (0 1) (0 1)))) '(1 . 2))
(ok? (tar (n '(1 7 (4 0 1) (4 0 1)))) 3)
(ok? (tar (n '((1 2) 7 (0 3) 7 (4 0 1) (4 0 1)))) 4)
(ok? (tar (n '(42 7 (1 (77 (1 1 18))) (0 1)))) '(77 . (1 . (1 . 18))))

(ok? (tar (n '(1 (8 (4 0 1) (0 1))))) '(2 . 1))
(ok? (tar (n '(1 (8 (4 0 1) (4 0 2))))) 3)
(ok? (tar (n '(1 (8 (4 0 1) (4 0 3))))) 2)
(ok? (tar (n '((1 2) 8 ((4 0 2) (4 0 3)) (0 1)))) '((2 . 3) . (1 . 2)))
(ok? (tar (n '(1 (8 (4 0 1) 7 (0 3) (0 1))))) 1)

(ok? (tar (n '(42 9 3 (1 (18 (1 77)))))) 77)
(ok? (tar (n '(42 9 3 (1 (18 (4 0 2)))))) 19)

(ok? (tar (n '(1 10 18 (4 0 1)))) 2)
(ok? (tar (n '(1 10 (18 (0 1)) (4 0 1)))) 2)
(ok? (tar (n '(1 10 (18 (4 0 1)) (4 0 1)))) 2)
(ok? (tar (n '((1 2) 10 (0 (4 0 2)) (4 0 3)))) 3)
(ok? (tar (n '((1 2) 10 18 (4 0 3)))) 3)
(ok? (tar (n '((1 2) 10 (18 (4 0 3)) (4 0 3)))) 3)

(ok? (guard (e (e)) (lus (n '(1 2)))) 3)
(ok? (guard (e (e)) (tis (n 1))) 4)
(ok? (guard (e (e)) (fas (n 1))) 5)
(ok? (guard (e (e)) (fas (n '((1 2) 3)))) 6)
(ok? (guard (e (e)) (tar (n 1))) 9)

(define x
  '((0
     (6
      ; builds a cell like [100 x] where x is the subject's head atom
      ; compares the cell's atoms with = (=[100 x])
      (8 ((1 100) (0 2)) (5 (0 2)))
      ; if the comparison evaluates to 0 the subject's head atom is returned
      (0 2)
      ; otherwise a new subject is built where the original subject's head atom
      ; is replaced with its incremented value, then the same formula will be
      ; invoked on the new subject resulting in a recursion
      (9 3 ((7 (0 2) (4 0 1)) (0 3)))))
    (9 3 (0 1))))
(ok? (tar (n x)) 100)

(display (tar (n (read (current-input-port)))))
(newline)
