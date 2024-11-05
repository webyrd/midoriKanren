(load "load.scm")

(test "need-project-1a"
  (run* (x)
    (evalo
     `(run* (q)
        (fresh (l)
          (== '(cat dog mouse) l)
          (project (l)
            (== (car l) q))))
     x))
  '((cat)))


(test "need-project-1b"
  (run* (x)
    (evalo
     `(run* (q)
        (fresh (l)
          (== '(cat dog mouse) l)
          (== (car l) q)))
     x))
  '())




(test "test from the readme"
  (run* (x)
    (evalo
     `(let ((l (list ==)))
        (let ((ans (run* (q) ((car l) q 4))))
          (if (null? ans)
              'doh
	      (car ans))))
     x))
  '(4))

(test "simple foldl test"
  (time
   (run* (q)
     (evalo
      `(letrec ((foldl (lambda (proc ls start)
                         (if (null? ls)
                             start
                             (foldl proc
                                    (cdr ls)
                                    (proc (car ls) start))))))
         (foldl cons '(1 2) '()))
      q)))
  '((2 1)))

(test "simple foldlo test"
  (time
   (run* (q)
     (evalo
      `(letrec ((foldlo (lambda (rel ls start end)
                         (conde
                           ((== '() ls) (== start end))
                           ((fresh (a d res)
                              (== (cons a d) ls)
                              (rel a start res)
                              (foldlo rel d res end)))))))
         (let ((conso (lambda (a d pr) (== (cons a d) pr))))
           (run* (z) (foldlo conso '(1 2) '() z))))
      q)))
  '(((2 1))))

(test "simple foldl/foldoo test"
  (time
   (run* (q)
     (evalo
      `(letrec ((foldl (lambda (proc ls start)
                         (if (null? ls)
                             start
                             (foldl proc
                                    (cdr ls)
                                    (proc (car ls) start))))))
         (letrec ((foldlo (lambda (rel ls start end)
                            (conde
                              ((== '() ls) (== start end))
                              ((fresh (a d res)
                                 (== (cons a d) ls)
                                 (rel a start res)
                                 (foldlo rel d res end)))))))
           (let ((conso (lambda (a d pr) (== (cons a d) pr))))
             (let ((v1 (foldl cons '(1 2) '())))
               (let ((v2 (run* (z) (foldlo conso '(1 2) '() z))))
                 (list
                   v1
                   v2
                   (equal? (car v2) v1)))))))
      q)))
  '(((2 1) ((2 1)) #t)))

(test "pair?-0a"
  (run* (q)
    (evalo
     `(pair? ',q)
     #f))
  '(#f
    #t
    ()
    (_.0 (num _.0))
    (_.0 (=/= ((_.0 closure)) ((_.0 lvar)) ((_.0 prim))) (sym _.0))))

(test "pair?-0b"
  (run* (q)
    (evalo
     `(pair? ',q)
     #t))
  '(((_.0 . _.1)
     (absento
      (closure _.0)
      (closure _.1)
      (lvar _.0)
      (lvar _.1)
      (prim _.0)
      (prim _.1)))))

(test "pair?-1"
  (run* (q)
    (evalo
     '(pair? 5)
     q))
  '(#f))

(test "pair?-2"
  (run* (q)
    (evalo
     '(pair? #f)
     q))
  '(#f))

(test "pair?-3"
  (run* (q)
    (evalo
     '(pair? #t)
     q))
  '(#f))

(test "pair?-4"
  (run* (q)
    (evalo
     '(pair? 'cat)
     q))
  '(#f))

(test "pair?-5"
  (run* (q)
    (evalo
     '(pair? '())
     q))
  '(#f))

(test "pair?-6"
  (run* (q)
    (evalo
     '(pair? cons)
     q))
  '(#f))

(test "pair?-7"
  (run* (q)
    (evalo
     '(pair? (lambda (x) x))
     q))
  '(#f))

(test "pair?-8"
  (run* (q)
    (evalo
     '(pair? (cons 3 4))
     q))
  '(#t))


(test "procedure?-0a"
  (run* (q)
    (evalo
     `(procedure? ',q)
     #f))
  '(#f
    #t
    ()
    (_.0 (num _.0))
    (_.0 (=/= ((_.0 closure)) ((_.0 lvar)) ((_.0 prim))) (sym _.0))
    ((_.0 . _.1) (absento (closure _.0) (closure _.1) (lvar _.0) (lvar _.1) (prim _.0) (prim _.1)))))

(test "procedure?-0b"
  (run* (q)
    (evalo
     `(procedure? ',q)
     #t))
  '())

(test "procedure?-0c"
  (let ((procs
         (run 70 (q)
           (evalo
            `(procedure? ,q)
            #t))))
    (and (member '== procs)
         (member '((lambda _.0 _.1) (sym _.0)) procs)
         #t))
  #t)

(test "procedure?-1"
  (run* (q)
    (evalo
     '(procedure? 5)
     q))
  '(#f))

(test "procedure?-2"
  (run* (q)
    (evalo
     '(procedure? #f)
     q))
  '(#f))

(test "procedure?-3"
  (run* (q)
    (evalo
     '(procedure? #t)
     q))
  '(#f))

(test "procedure?-4"
  (run* (q)
    (evalo
     '(procedure? 'cat)
     q))
  '(#f))

(test "procedure?-5"
  (run* (q)
    (evalo
     '(procedure? '())
     q))
  '(#f))

(test "procedure?-6"
  (run* (q)
    (evalo
     '(procedure? (cons 3 4))
     q))
  '(#f))

(test "procedure?-7"
  (run* (q)
    (evalo
     '(procedure? cons)
     q))
  '(#t))

(test "procedure?-8"
  (run* (q)
    (evalo
     '(procedure? (lambda (x) x))
     q))
  '(#t))

(test "procedure?-9"
  (run* (q)
    (evalo `(procedure? (walk* car '())) q))
  '(#t))

(test "procedure?-10"
  (run* (q)
    (evalo `(procedure? (walk* (lambda (x) (cons x x)) '())) q))
  '(#t))


(test "let-1"
  (run* (q)
    (evalo
     '(let ((z (cons 3 4)))
        (car z))
     q))
  '(3))

(test "let-2"
  (run* (q)
    (evalo
     '(let ((z (cons 3 4)))
        (let ((z (car z)))
          (list z z)))
     q))
  '((3 3)))

(test "let-3"
  (run* (q)
    (evalo
     '(let ((f (lambda (w) (cons w w))))
        (f (cons 3 4)))
     q))
  '(((3 . 4) . (3 . 4))))

(test "let-4"
  (run* (q)
    (evalo
     '(let ((f (lambda (w) (f w))))
        (f (cons 3 4)))
     q))
  '())


(test "map-1"
  (run* (q)
    (evalo
     '(map car '((a b) (c d e) (f)))
     q))
  '((a c f)))

(test "map-2"
  (run* (q)
    (evalo
     '(map (lambda (x) (null? x)) (list (list) #f '() (cons 3 4)))
     q))
  '((#t #f #t #f)))


(test "length-0"
  (run* (q)
    (evalo
     '(length '())
     q))
  '(()))

(test "length-1"
  (run* (q)
    (evalo
     '(length (cons 'a '()))
     q))
  '((())))

(test "length-2"
  (run* (q)
    (evalo
     '(length (list 'cat 'dog 'mouse))
     q))
  '((((())))))

(test "var?-1"
  (run* (q)
    (evalo
     '(var? (var ',(peano 0)))
     q))
  '(#t))

(test "var-1"
  (run* (q)
    (evalo
     '(var '())
     q))
  '((lvar)))

(test "var-2"
  (run* (q)
    (evalo
     `(var ',(peano 0))
     q))
  '((lvar)))

(test "var?-2"
  (run* (q)
    (evalo
     `(var? (var ',(peano 0)))
     q))
  '(#t))

(test "var?-3"
  (run* (q)
    (evalo
     `(var? (var ',(peano 0)))
     q))
  '(#t))

(test "var=?-1"
  (run* (q)
    (evalo
     `(var=? (var ',(peano 0)) (var ',(peano 0)))
     q))
  '(#t))

(test "var=?-2"
  (run* (q)
    (evalo
     `(var=? (var ',(peano 1)) (var ',(peano 0)))
     q))
  '(#f))

(test "var=?-3"
  (run* (q)
    (evalo
     `(var=? (var ',(peano 1)) (var ',(peano 1)))
     q))
  '(#t))


(test "evalo-walk-pair-1"
  (run* (q)
    (evalo
     `(let ((lv0 (var ',(peano 0))))
        (let ((lv1 (var ',(peano 1))))
          (let ((lv2 (var ',(peano 2))))
            (let ((lv3 (var ',(peano 3))))
              (walk
               lv0
               (list
                 (cons lv3 4)
                 (cons lv2 3)
                 (cons lv1 (cons lv2 lv3))
                 (cons lv0 lv1)))))))
     q))
  '(((lvar . ((()))) . (lvar . (((())))))))

#|
(test "evalo-walk*-pair-1"
  (run* (q)
    (evalo
     `(walk*
       '(lvar ())
       '(((lvar . (((())))) . 4)
         ((lvar . ((()))) . 3)
         ((lvar . (())) . ((lvar . ((()))) . (lvar . (((()))))))
         ((lvar . ()) . (lvar . (())))))
     q))
  '((3 . 4)))
|#

(test "evalo-lambda/cons-1"
  (run* (q)
    (evalo
     '(let ((g (lambda (z) (cons z z))))
        (g (cons 3 4)))
     q))
  '(((3 . 4) . (3 . 4))))

(test "evalo-let/==/list-1"
  (car
   (car
    (car
     (run* (q)
       (evalo
        '(let ((g (== 3 4)))
           (list g))
        q)))))
  'closure)

(test "evalo-let/==/disj-1"
  (car
   (car
    (run* (q)
      (evalo
       '(let ((g1 (== 3 4)))
          (let ((g2 (== 5 5)))
            (disj g1 g2)))
       q))))
  'closure)

(test "evalo-let/==/conj-1"
  (car
   (car
    (run* (q)
      (evalo
       '(let ((g1 (== 3 4)))
          (let ((g2 (== 5 5)))
            (conj g1 g2)))
       q))))
  'closure)

(test "evalo-call/fresh/lambda/==-1"
  (car
   (car
    (run* (q)
      (evalo
       '(call/fresh (lambda (z) (== z 5)))
       q))))
  'closure)

(test "evalo-assp-1"
  (run 1 (p l q)
    (evalo
     `(assp ,p ,l)
     q))
  '(((_.0 '() #f) (num _.0))))

(test "evalo-assp-2"
  (run 2 (p l q)
    (evalo
     `(assp ,p ,l)
     q))
  '(((_.0 (quote ()) #f) (num _.0))
    (((quote _.0) (quote ()) #f) (absento (closure _.0) (lvar _.0) (prim _.0)))))

(test "evalo-assp-3"
  (run* (q)
    (evalo
     `(assp 'genny '())
     q))
  '(#f))

(test "evalo-assp-4"
  (run* (q)
    (evalo
     `(assp (lambda (y) (equal? y 'z)) '((z . 5)))
     q))
  '((z . 5)))

(test "evalo-assp-5"
  (run* (q)
    (evalo
     `(assp (lambda (y) (equal? y 'z)) '((a . 4) (b . 5) (c . 6) (b . 7)))
     q))
  '(#f))

(test "evalo-assp-6"
  (run* (q)
    (evalo
     `(assp (lambda (y) (equal? y 'b)) '((a . 4) (b . 5) (c . 6) (b . 7)))
     q))
  '((b . 5)))

#|
;; WEB -- about 20 seconds
(test "evalo-assp-7"
  (time
   (run 1 (p)
     (fresh (b)
       (== `(lambda (y) ,b) p))
     (evalo
      `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
      '(b . 5))))
  '((lambda (y) (equal? y 'b))))
|#

#|
;; WEB too slow--didn't come back after a couple of minutes
(test "evalo-assp-8"
  (run 1 (p)
    (evalo
     `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
     '(b . 5)))
  '())
|#

(test "evalo-fresh/==/assp-1"
  (time
   (run 1 (p)
     (fresh (b)
       (== `(lambda (y) (equal? . ,b)) p))
     (evalo
      `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
      '(b . 5))))
  '((lambda (y) (equal? 'b y))))

(test "evalo-fresh/==/assp-2"
  (time
   (run 1 (p)
     (fresh (x)
       (== `(lambda (y) (equal? y ,x)) p))
     (evalo
      `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
      '(b . 5))))
  '((lambda (y) (equal? y 'b))))

#|
(test "evalo-walk-1"
  (run* (q)
    (evalo
     `(walk '(lvar ()) '(((lvar (())) . 4) ((lvar ()) . 5)))
     q))
  '(5))

(test "evalo-walk-2"
  (run* (q)
    (evalo
     `(walk '(lvar ((()))) '(((lvar (())) . 4) ((lvar ()) . 5)))
     q))
  '((lvar ((())))))

(test "evalo-walk-3"
  (run* (q)
    (evalo
     `(walk 'cat '(((lvar (())) . 4) ((lvar ()) . 5)))
     q))
  '(cat))

(test "evalo-walk-4"
  (run* (q)
    (evalo
     `(walk '(lvar ()) '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(4))

(test "evalo-unify-1"
  (run* (q)
    (evalo
     `(unify '4 '4 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar (())) . 4) ((lvar ()) . (lvar (()))))))

(test "evalo-unify-2"
  (run* (q)
    (evalo
     `(unify '4 '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(#f))

(test "evalo-unify-3"
  (run* (q)
    (evalo
     `(unify '(lvar (())) '4 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar (())) . 4) ((lvar ()) . (lvar (()))))))

(test "evalo-unify-4"
  (run* (q)
    (evalo
     `(unify '(lvar (())) '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(#f))

(test "evalo-unify-5"
  (run* (q)
    (evalo
     `(unify '(lvar ()) '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(#f))

(test "evalo-unify-6"
  (run* (q)
    (evalo
     `(unify '(lvar ()) '4 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar (())) . 4) ((lvar ()) lvar (())))))

(test "evalo-unify-7"
  (run* (q)
    (evalo
     `(unify '(lvar ()) '5 '())
     q))
  '((((lvar ()) . 5))))

(test "evalo-unify-8"
  (run* (q)
    (evalo
     `(unify '5 '(lvar ()) '())
     q))
  '((((lvar ()) . 5))))

(test "evalo-unify-9"
  (run* (q)
    (evalo
     `(unify '(lvar ((()))) '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar ((()))) . 5) ((lvar (())) . 4) ((lvar ()) lvar (())))))
|#


(test "evalo-let/==/empty-state-1"
  (run* (q)
    (evalo
     `(let ((empty-s '()))
        (let ((c0 '()))
          (let ((empty-state (cons empty-s c0)))
            ((== 'cat 'cat) empty-state))))
     q))
  '(((() . ()))))

(test "evalo-==/empty-state-1"
  (run* (q)
    (evalo
     `((== 'cat 'cat) empty-state)
     q))
  '(((() . ()))))

(test "evalo-==/call-goal-1"
  (run* (q)
    (evalo
     `(call/goal (== 'cat 'cat))
     q))
  '(((() . ()))))

#|
(test "evalo-==/lvar/call/goal-1"
  (run* (q)
    (evalo
     `(call/goal (== '(lvar . ()) 'cat))
     q))
  '((((((lvar . ()) . cat)) . ()))))
|#

(test "evalo-==/var/call/goal-1"
  (run* (q)
    (evalo
     `(call/goal (== (var '()) 'cat))
     q))
  '((((((lvar . ()) . cat)) . ()))))

(test "evalo-==/var/call/goal-2"
  (run* (q)
    (evalo
     `(call/goal (== (var ',(peano 0)) 'cat))
     q))
  '((((((lvar . ()) . cat)) . ()))))

(test "evalo-call/fresh/lambda/call/goal-1"
  (run* (q)
    (evalo
     `(call/goal
       (call/fresh
        (lambda (my-var)
          (== my-var 'cat))))
     q))
  '((((((lvar . ()) . cat)) . (())))))

(test "evalo-call/fresh/lambda/disj/==-1"
  (time
   (run* (q)
     (evalo
      `(call/goal
        (call/fresh
         (lambda (my-var)
           (disj
            (== my-var 'cat)
            (== my-var 'dog)))))
      q)))
  '((((((lvar . ()) . cat)) . (())) ((((lvar . ()) . dog)) . (())))))

(test "evalo-call/fresh/lambda/conj/==-1"
  (time
   (run* (q)
     (evalo
      `(call/goal
        (call/fresh
         (lambda (x1)
           (call/fresh
            (lambda (x2)
              (conj
               (== x1 'cat)
               (== x2 'dog)))))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (())))))

(test "evalo-take-all/fresh/==-1"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x)
           (== x 'cat))))
      q)))
  '((((((lvar . ()) . cat)) ()))))

(test "evalo-take-all/fresh/==-2"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x y)
           (== x 'cat)
           (== 'dog y))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (())))))

(test "evalo-take-all/fresh/conde/==-1"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'cat)
              (== 'dog y))
             ((== 'rat y)
              (== x 'bat))))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (()))
     ((((lvar . ()) . bat) ((lvar . (())) . rat)) (())))))

(test "evalo-take-all/fresh/conde/==-2"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '((((((lvar . (())) . 1) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 1) ((lvar . ()) . b)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . b)) (())))))




(test "evalo-take/fresh/==-1a"
  (time
   (run* (q)
     (evalo
      `(take '(())
        (call/goal
         (fresh (x)
           (== x 'cat))))
      q)))
  '((((((lvar . ()) . cat)) ()))))

(test "evalo-take/fresh/==-1b"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 1)
        (call/goal
         (fresh (x)
           (== x 'cat))))
      q)))
  '((((((lvar . ()) . cat)) ()))))

(test "evalo-take/fresh/==-2"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 1)
        (call/goal
         (fresh (x y)
           (== x 'cat)
           (== 'dog y))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (())))))

(test "evalo-take/fresh/conde/==-1"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 2)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'cat)
              (== 'dog y))
             ((== 'rat y)
              (== x 'bat))))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (()))
     ((((lvar . ()) . bat) ((lvar . (())) . rat)) (())))))

(test "evalo-take/fresh/conde/==-2"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 4)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '((((((lvar . (())) . 1) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 1) ((lvar . ()) . b)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . b)) (())))))

(test "evalo-take/fresh/conde/==-2b"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 3)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '((((((lvar . (())) . 1) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 1) ((lvar . ()) . b)) (())))))

(test "evalo-take/fresh/conde/==-2c"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 0)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '(()))


(test "evalo-reify-name-1"
  (time
   (run* (q)
     (evalo
      `(reify-name ',(peano 2))
      q)))
  '((__ ((())))))

(test "evalo-reify-s-1"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '5
        '())
      q)))
  '(()))

#|
(test "evalo-reify-s-2"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '(lvar . ())
        '())
      q)))
  '((((lvar . ()) . (__ ())))))
|#

#|
(test "evalo-reify-s-3"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '(lvar . ((((())))))
        '())
      q)))
  '((((lvar . ((((()))))) . (__ ())))))
|#

#|
(test "evalo-reify-s-4"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '((lvar . (())) 5 (lvar . ()) cat (lvar . (())))
        '())
      q)))
  '((((lvar . ()) . (__ (()))) ((lvar . (())) . (__ ())))))
|#

(test "evalo-reify-1st-1"
  (time
   (run* (q)
     (evalo
      `(map
        reify-1st
        (take-all
         (call/goal
          (fresh (x)
            (== x 'cat)))))
      q)))
  '((cat)))

(test "evalo-reify-1st-2"
  (time
   (run* (q)
     (evalo
      `(map
        reify-1st
        (take-all
         (call/goal
          (fresh (x)
            (conde
              ((== x 'cat))
              ((== x 'dog)))))))
      q)))
  '((cat dog)))


(test "evalo-multiple-run*-1"
  (time
   (run* (q)
     (evalo
      `(let ((l1
              (run* (x)
                (conde
                  ((== 'cat x))
                  ((== 'dog x))))))
         (list l1 (run* (w) (== 'mouse w)) l1))
      q)))
  '(((cat dog) (mouse) (cat dog))))

(test "evalo-multiple-run*-2"
  (time
   (run* (q)
     (evalo
      `(list (run* (x)
               (conde
                 ((== 'cat x))
                 ((== 'dog x))))
             (run* (w) (== 'mouse w)))
      q)))
  '(((cat dog) (mouse))))

(test "evalo-multiple-run*-3"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x v) (== v x))))
         (list (run* (y) (f y 'cat))
               (run* (z) (f z 'dog))))
      q)))
  '(((cat) (dog))))

(test "evalo-multiple-run*-4"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x v) (== v x))))
         (letrec ((append
                   (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
           (append (run* (y) (f y 'cat))
                   (run* (z) (f z 'dog)))))
      q)))
  '((cat dog)))

(test "evalo-multiple-run*-5"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x v) (== v x))))
         (letrec ((append
                   (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
           (append (run* (y) (f y 'cat))
                   (run* (y) (f y 'dog)))))
      q)))
  '((cat dog)))


(test "evalo-unify-with-prim-1"
  (time
   (run* (q)
     (evalo
      `(unify car (var ',(peano 0)) '())
      q)))
  '((((lvar . ()) . (prim . car)))))

(test "evalo-unify-with-prim-2"
  (time
   (run* (q)
     (evalo
      `(cdr (car (unify car (var ',(peano 0)) '())))
      q)))
  '((prim . car)))

(test "evalo-unify-with-prim-3"
  (time
   (run* (q)
     (evalo
      `(procedure? (cdr (car (unify car (var ',(peano 0)) '()))))
      q)))
  '(#t))

(test "evalo-unify-with-prim-3"
  (time
   (run* (q)
     (evalo
      `(let ((x (var ',(peano 0))))
         (unify car x '()))
      q)))
  '((((lvar . ()) . (prim . car)))))

(test "evalo-unify-with-prim-4"
  (time
   (run* (q)
     (evalo
      `(let ((x (var ',(peano 0))))
         (walk x (unify car x '())))
      q)))
  '((prim . car)))

(test "evalo-unify-with-prim-5"
  (time
   (run* (q)
     (evalo
      `(let ((x (var ',(peano 0))))
         (walk* x (unify car x '())))
      q)))
  '((prim . car)))

(test "evalo-unify-with-prim-6"
  (time
   (run* (q)
     (evalo
      `((call/fresh
         (lambda (x)
           (== car x)))
        empty-state)
      q)))
  '((((((lvar . ()) . (prim . car))) . (())))))

(test "evalo-unify-with-prim-7b"
  (time
   (run* (q)
     (evalo
      `(procedure?
        (call/goal
         (fresh (x)
           (== car x))))
      q)))
  '(#t))

(test "evalo-unify-with-prim-7c"
  (time
   (run* (q)
     (evalo
      `((call/goal
         (fresh (x)
           (== car x))))
      q)))
  '((((((lvar . ()) . (prim . car))) . (())))))

(test "evalo-unify-with-prim-7d"
  (time
   (run* (q)
     (evalo
      `(procedure?
        ((call/goal
          (fresh (x)
            (== car x)))))
      q)))
  '(#f))

(test "evalo-unify-with-prim-8"
  (run* (q)
    (evalo
     `(pull
       (call/goal
        (fresh (x)
          (== car x))))
     q))
  '((((((lvar . ()) . (prim . car))) . (())))))

(test "evalo-unify-with-prim-9"
  (run* (q)
    (evalo
     `(pull
       ((call/fresh
         (lambda (x)
           (== car x)))
        empty-state))
     q))
  '((((((lvar . ()) . (prim . car))) . (())))))

(test "evalo-unify-with-prim-10"
  (run* (q)
    (evalo
     `(take-all
       ((call/fresh
         (lambda (x)
           (== car x)))
        empty-state))
     q))
  '((((((lvar . ()) . (prim . car))) . (())))))

(test "evalo-unify-with-prim-12"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x)
           (== car x))))
      q)))
  '((((((lvar . ()) . (prim . car))) . (())))))


(test "evalo-unify-with-lambda-1"
  (time
   (caar
    (run* (q)
      (evalo
       `(cdr (car (unify (lambda (w) (cons w w)) (var ',(peano 0)) '())))
       q))))
  'closure)

(test "evalo-unify-with-lambda-2"
  (time
   (run* (q)
      (evalo
       `(procedure? (cdr (car (unify (lambda (w) (cons w w)) (var ',(peano 0)) '()))))
       q)))
  '(#t))


(test "evalo-run*-and-application-000"
  (time
   (run* (q)
     (evalo
      `(run* (x)
         (fresh (y)
           (== car y)))
      q)))
  '(((__ ()))))

(test "evalo-run*-and-application-0000"
  (time
   (run* (q)
     (evalo
      `(run* (x)
         (fresh (y)
           (== (lambda (y) (cons x y)) y)))
      q)))
  '(((__ ()))))

(test "evalo-run*-and-application-0"
  (time
   (run* (q)
     (evalo
      `(procedure? (car (run* (x) (== car x))))
      q)))
  '(#t))

(test "evalo-run*-and-application-00"
  (time
   (run* (q)
     (evalo
      `(procedure? (car (run* (x) (== (lambda (z) (cons z z)) x))))
      q)))
  '(#t))

(test "evalo-run*-and-application-1"
  (time
   (run* (q)
     (evalo
      `(run* (x) (== car x))
      q)))
  '(((prim . car))))

(test "evalo-run*-and-application-1b"
  (time
   (run* (q)
     (evalo
      `(run* (x) (== x car))
      q)))
  '(((prim . car))))

(test "evalo-run*-and-application-3"
  (time
   (run* (q)
     (evalo
      `((car (run* (x) (== car x))) '(a b c))
      q)))
  '(a))

(test "evalo-run*-and-application-3b"
  (time
   (run* (q)
     (evalo
      `((car (run* (x) (== x car))) '(a b c))
      q)))
  '(a))

(test "evalo-run*-and-application-3c"
  (time
   (run* (q)
     (evalo
      `(map
        (car (run* (x) (== x cdr)))
        '((1 2) (3 4 5) (6)))
      q)))
  '(((2) (4 5) ())))


(test "evalo-run*-and-application-4"
  (time
   (run* (q)
     (evalo
      `(map
        (car (run* (x) (== x (lambda (x) (cons x x)))))
        '((1 2) (3 4 5) (6)))
      q)))
  '((((1 2) . (1 2)) ((3 4 5) . (3 4 5)) ((6) . (6)))))


(test "evalo-run*-and-application-n"
  (time
   (run* (q)
     (evalo
      `(let ((l '((a b) (c d))))
         (let ((procs (run* (x)
                        (conde
                          ((== car x))
                          ((== cdr x))))))
           (map (lambda (proc) (proc l)) procs)))
      q)))
  '(((a b) ((c d)))))

(test "evalo-run*-and-application-n2"
  (time
   (run* (q)
     (evalo
      `(let ((l '((a b) (c d))))
         (let ((procs (run* (x)
                        (conde
                          ((== car x))
                          ((== cdr x))))))
           (map (lambda (proc) (map proc l)) procs)))
      q)))
  '(((a c) ((b) (d)))))

(test "evalo-run*/project-1"
  (time
   (run* (q)
     (evalo
      `(run* (x)
         (fresh (y)
           (== y '(cat))
           (== (car y) x)))
      q)))
  '())

(test "evalo-run*/project-2"
  (time
   (run* (q)
     (evalo
      `(run* (x)
         (fresh (y)
           (== y '(cat))
           (project (y)
             (== (car y) x))))
      q)))
  '((cat)))



#|
Examples from the first edition of The Reasoned Schemer, showing
mixing and matching of Scheme and miniKanren code.
|#

(test "trs1-1a"
  ;; from frame 60 on page 15 of TRS1
  (run* (q)
    (let ((a (== #t q))
          (b (== #f q)))
      b))
  '(#f))

(test "trs1-1c"
  ;; adapted from frame 60 on page 15 of TRS1
  (run* (x)
    (evalo
     `(run* (q)
        (let ((a (== #t q)))
          (let ((b (== #f q)))
            b)))
     x))
  '((#f)))

(test "trs1-1d"
  ;; adapted from frame 60 on page 15 of TRS1
  (run* (x)
    (evalo
     `(run* (q)
        (let ((b (== #f q)))
          b))
     x))
  '((#f)))



(test "evalo-run*/let/==-1"
  ;; adapted from frame 60 on page 15 of TRS1
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (let ((a (== 'cat q)))
           (let ((b (== 'dog q)))
             b)))
      q)))
  '((dog)))


(test "evalo-run*-1"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (== q 'cat))
      q)))
  '((cat)))

(test "evalo-run*-2"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (conde
           ((== q 'cat))
           ((== q 'dog))))
      q)))
  '((cat dog)))

(test "evalo-run*-3"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (== 'cat 'cat))
      q)))
  '(((__ ()))))

(test "evalo-run*-4"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse) (dog mouse))))

(test "evalo-run-1a"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 1) (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse))))

(test "evalo-run-1b"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 2) (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse) (dog mouse))))

(test "evalo-run-1c"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 3) (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse) (dog mouse))))

(test "evalo-run*-let-1"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (let ((g (== 'cat q)))
           g))
      q)))
  '((cat)))

(test "evalo-run*-let-2"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x) (== x 'cat))))
         (run* (y)
           (let ((g (f y)))
             g)))
      q)))
  '((cat)))

(test "evalo-run*-let-3"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x) (cons x x))))
         (run* (y)
           (== (f 'cat) y)))
      q)))
  '(((cat . cat))))

(test "evalo-run*-appendo-1"
  (time
   (run* (q)
     (evalo
      `(letrec ((appendo (lambda (l s out)
                           (conde
                             ((== '() l) (== s out))
                             ((fresh (a d res)
                                (== (cons a d) l)
                                (== (cons a res) out)
                                (appendo d s res)))))))
         (run* (q)
           (appendo '(a) '(b) q)))
      q)))
  '(((a b))))

(test "evalo-run-appendo-1"
  (time
   (run* (q)
     (evalo
      `(letrec ((appendo (lambda (l s out)
                           (conde
                             ((== '() l) (== s out))
                             ((fresh (a d res)
                                (== (cons a d) l)
                                (== (cons a res) out)
                                (appendo d s res)))))))
         (run ',(peano 1) (x)
           (appendo '(a) x '(a b))))
      q)))
  '(((b))))

(test "evalo-run-appendo-2"
  (time
   (run* (q)
     (evalo
      `(letrec ((appendo (lambda (l s out)
                           (conde
                             ((== '() l) (== s out))
                             ((fresh (a d res)
                                (== (cons a d) l)
                                (== (cons a res) out)
                                (appendo d s res)))))))
         (run ',(peano 3) (x)
           (fresh (v w)
             (== (list v w) x)
             (appendo v w '(a b)))))
      q)))
  '(((() (a b))
     ((a) (b))
     ((a b) ()))))

(test "evalo-append-1"
  (time
    (run* (q)
      (evalo
       `(letrec ((append
                  (lambda (l s)
                    (if (null? l)
                        s
                        (cons (car l) (append (cdr l) s))))))
          (append '(a) '(b)))
       q)))
  '((a b)))


(test "evalo-append-appendo-1"
  (time
    (run* (q)
      (evalo
       `(run* (z)
          (letrec ((append
                    (lambda (l s)
                      (if (null? l)
                          s
                          (cons (car l) (append (cdr l) s))))))
            (letrec ((appendo
                      (lambda (l1 l2 l)
                        (conde
                          ((== '() l1) (== l2 l))
                          ((fresh (a d l3)
                             (== (cons a d) l1)
                             (== (cons a l3) l)
                             (appendo d l2 l3)))))))
              (== (append '(a) '(b)) z))))
       q)))
  '(((a b))))

(test "evalo-append-appendo-2"
  (time
    (run* (q)
      (evalo
       `(run* (z)
          (letrec ((append
                    (lambda (l s)
                      (if (null? l)
                          s
                          (cons (car l) (append (cdr l) s))))))
            (letrec ((appendo
                      (lambda (l1 l2 l)
                        (conde
                          ((== '() l1) (== l2 l))
                          ((fresh (a d l3)
                             (== (cons a d) l1)
                             (== (cons a l3) l)
                             (appendo d l2 l3)))))))
              (fresh (y)
                (appendo '(a) '(b) y)
                (== y z)))))
       q)))
  '(((a b))))

(test "evalo-append-appendo-2b"
  (time
    (run* (q)
      (evalo
       `(run* (z)
          (letrec ((append
                    (lambda (l s)
                      (if (null? l)
                          s
                          (cons (car l) (append (cdr l) s))))))
            (letrec ((appendo
                      (lambda (l1 l2 l)
                        (conde
                          ((== '() l1) (== l2 l))
                          ((fresh (a d l3)
                             (== (cons a d) l1)
                             (== (cons a l3) l)
                             (appendo d l2 l3)))))))
              (fresh (y)
                (== y z)
                (appendo '(a) '(b) y)))))
       q)))
  '(((a b))))

(test "evalo-append-appendo-3"
  (time
    (run* (q)
      (evalo
       `(run* (z)
          (letrec ((append
                    (lambda (l s)
                      (if (null? l)
                          s
                          (cons (car l) (append (cdr l) s))))))
            (letrec ((appendo
                      (lambda (l1 l2 l)
                        (conde
                          ((== '() l1) (== l2 l))
                          ((fresh (a d l3)
                             (== (cons a d) l1)
                             (== (cons a l3) l)
                             (appendo d l2 l3)))))))
              (fresh (y)
                (appendo '(a) '(b) y)
                (== (append '(a b) '(c d)) z)))))
       q)))
  '(((a b c d))))


(test "evalo-append-appendo-n"
  (time
    (run* (q)
      (evalo
       `(run* (z)
          (letrec ((append
                    (lambda (l s)
                      (if (null? l)
                          s
                          (cons (car l) (append (cdr l) s))))))
            (letrec ((appendo
                      (lambda (l1 l2 l)
                        (conde
                          ((== '() l1) (== l2 l))
                          ((fresh (a d l3)
                             (== (cons a d) l1)
                             (== (cons a l3) l)
                             (appendo d l2 l3)))))))
              (fresh (y)
                (appendo '(a) '(b) y)
                (project (y)
                  (== (append y '(c d)) z))))))
       q)))
  '(((a b c d))))

(test "12-from-midoriKanren-a"
  (time
   (run* (q)
     (evalo
      `(run* (z)
         (letrec ((append (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l) (append (cdr l) s))))))
           (letrec ((appendo (lambda (l1 l2 l)
                               (conde
                                 ((== '() l1) (== l2 l))
                                 ((fresh (a d l3)
                                    (== (cons a d) l1)
                                    (== (cons a l3) l)
                                    (appendo d l2 l3)))))))
             (fresh ()
               (appendo '(a) '(b) '(a b))
               (== (append '(a) '(b)) z)))))
      q)))
  '(((a b))))

(test "12-from-midoriKanren-b"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 1) (z)
         (letrec ((append (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l) (append (cdr l) s))))))
           (letrec ((appendo (lambda (l1 l2 l)
                               (conde
                                 ((== '() l1) (== l2 l))
                                 ((fresh (a d l3)
                                    (== (cons a d) l1)
                                    (== (cons a l3) l)
                                    (appendo d l2 l3)))))))
             (fresh ()
               (appendo '(a) '(b) '(a b))
               (== (append '(a) '(b)) z)))))
      q)))
  '(((a b))))

#|
;; WEB too slow or diverges
(test "12-from-midoriKanren-c"
  (time
   (run* (x y)
     (evalo
      `(run ',(peano 1) (z)
         (letrec ((append (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l) (append (cdr l) s))))))
           (letrec ((appendo (lambda (l1 l2 l)
                               (conde
                                 ((== '() l1) (== l2 l))
                                 ((fresh (a d l3)
                                    (== (cons a d) l1)
                                    (== (cons a l3) l)
                                    (appendo d l2 l3)))))))
             (fresh ()
               (appendo '(a) '(b) '(a b))
               (== (append ',x '(d)) z)))))
      '((c d)))))
  '???)
|#

#|
;;; WEB too slow or diverges
(test "12-from-midoriKanren"
  (time
   (run* (x y)
     (evalo
      `(run ',(peano 1) (z)
         (letrec ((append (lambda (l s)
                            (if (null? l)
                                s
                                (cons (car l) (append (cdr l) s))))))
           (letrec ((appendo (lambda (l1 l2 l)
                               (conde
                                 ((== '() l1) (== l2 l))
                                 ((fresh (a d l3)
                                    (== (cons a d) l1)
                                    (== (cons a l3) l)
                                    (appendo d l2 l3)))))))
             (fresh ()
               (appendo ',x ',y '(a b c d e))
               (== (append ',x '(c d)) z)))))
      '((a b c d)))))
  '(((a b) (c d e))))
|#
