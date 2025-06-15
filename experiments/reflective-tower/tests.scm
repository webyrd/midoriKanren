(load "../../faster-miniKanren/test-check.scm")
(load "midoriKanren.scm")

(test "1"
  (run* (x)
    (eval-programo
     `(run* (z)
        (conde
          ((== 'cat z))
          ((== 'dog z))))
     x))
  '((cat dog)))

(test "2"
  (run 1 (x)
    (eval-programo
     `(run* (z)
        (conde
          ((== ',x z))
          ((== 'dog z))))
     '(cat dog)))
  '(cat))

(test "3"
  (run 3 (x)
    (eval-programo
     `(run* (z)
        (conde
          ((== ,x z))
          ((== 'dog z))))
     '(cat dog)))
  '((quote cat)
    ((car (quote (cat . _.0)))
     (absento (closr _.0) (mu-reifier _.0) (var _.0)))
    ((cdr (quote (_.0 . cat)))
     (absento (closr _.0) (mu-reifier _.0) (var _.0)))))

(test "4"
  (run 3 (x)
    (eval-programo
     `(run* (z)
        (conde
          ((== . ,x))
          ((== 'dog z))))
     '(cat dog)))
  '((z (quote cat))
    ((quote cat) z)
    ((z (car (quote (cat . _.0))))
     (absento (closr _.0) (mu-reifier _.0) (var _.0)))))

(test "5"
  (run* (x)
    (eval-programo
     `(run* (z)
        (,x
         ((== 'cat z))
         ((== 'dog z))))
     '(cat dog)))
  '(conde))

(test "6"
  (run* (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                       (conde
                         ((== '() l1) (== l2 l))
                         ((fresh (a d l3)
                            (== (cons a d) l1)
                            (== (cons a l3) l)
                            (appendo d l2 l3))))))
          (appendo '(1 2) '(3 4) z)))
     x))
  '(((1 2 3 4))))

(test "7"
  (run 1 (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                       (conde
                         ((== . ,x) (== l2 l))
                         ((fresh (a d l3)
                            (== (cons a d) l1)
                            (== (cons a l3) l)
                            (appendo d l2 l3))))))
          (appendo '() '() '())
          (appendo '(a) '(b) '(a b))
          (appendo '(1 2) '(3 4) z)))
     '((1 2 3 4))))
  '((() l1)))

(test "8"
  (run 5 (x y)
    (eval-programo
     `(run* (z)
        (letrec-func ((append (l s)
                              (if (null? l)
                                  s
                                  (cons (car l) (append (cdr l) s)))))
          (== (append '(a b c) '(d e)) z)
          (== (append ',x ',y) '(1 2 3 4))))
     '((a b c d e))))
  '((() (1 2 3 4))
    ((1 2 3 4) ())
    ((1) (2 3 4))
    ((1 2) (3 4))
    ((1 2 3) (4))))

(test "9"
  (run* (x)
    (eval-programo
     `(run* (z)
        (let ((y (cons 'cat 'dog)))
          (== 5 z)))
     x))
  '((5)))

(test "10"
  (run* (x)
    (eval-programo
     `(run* (z)
        (let ((y (cons 'cat 'dog)))
          (== y z)))
     x))
  '(((cat . dog))))

(test "11"
  (run* (x)
    (eval-programo
     `(run* (z)
        (letrec-func ((append (l s)
                        (if (null? l)
                            s
                            (cons (car l) (append (cdr l) s)))))
          (letrec-rel ((appendo (l1 l2 l)
                         (conde
                           ((== '() l1) (== l2 l))
                           ((fresh (a d l3)
                              (== (cons a d) l1)
                              (== (cons a l3) l)
                              (appendo d l2 l3))))))
            (appendo '(a b c) '(d e) z))))
     x))
  '(((a b c d e))))

(test "12"
  (time
    (run* (x y)
      (eval-programo
       `(run* (z)
          (letrec-func ((append (l s)
                          (if (null? l)
                              s
                              (cons (car l) (append (cdr l) s)))))
            (letrec-rel ((appendo (l1 l2 l)
                           (conde
                             ((== '() l1) (== l2 l))
                             ((fresh (a d l3)
                                (== (cons a d) l1)
                                (== (cons a l3) l)
                                (appendo d l2 l3))))))
              (appendo ',x ',y '(a b c d e))
              (== (append ',x '(c d)) z))))
       '((a b c d)))))
  '(((a b) (c d e))))

(test "13-EZ-000"
  (time
   (run 1 (y)
     (eval-programo
      `(run* (z)
         (letrec-func ((append (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s)))))
           (letrec-rel ((appendo (l1 l2 l)
                          (conde
                            ((== '() l1) (== l2 l))
                            ((fresh (a d l3)
                               (== (cons a d) l1)
                               (== (cons a l3) l)
                               (appendo d l2 l3))))))
             (appendo (append '(cat) '(dog))
                      ((mu (e r)
                         (cons
                           (car (car (cdr (car e))))
                           (cons (car (car (cdr (car (cdr e)))))
                                 '())))
                       '(fish) '(rat)) z))))
      y)))
  '(((cat dog fish rat))))

(test "13-EZ-00"
  (time
   (run 1 (y)
     (eval-programo
      `(run* (z)
         (letrec-func ((append (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s)))))
           (letrec-rel ((appendo (l1 l2 l)
                          (conde
                            ((== '() l1) (== l2 l))
                            ((fresh (a d l3)
                               (== (cons a d) l1)
                               (== (cons a l3) l)
                               (appendo d l2 l3))))))
             (appendo (append '(cat) '(dog)) ((mu (e r) (cons 'fish (cons 'rat '()))) '(fish) '(rat)) z))))
      y)))
  '(((cat dog fish rat))))

(test "13-EZ-0"
  (time
   (run 1 (y)
     (eval-programo
      `(run* (z)
         (letrec-func ((append (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s)))))
           (letrec-rel ((appendo (l1 l2 l)
                          (conde
                            ((== '() l1) (== l2 l))
                            ((fresh (a d l3)
                               (== (cons a d) l1)
                               (== (cons a l3) l)
                               (appendo d l2 l3))))))
             (appendo (append '(cat) '(dog)) ((mu (e r) (cons 'fish (cons 'rat '()))) '(fish) '(rat)) z))))
      '((cat dog fish rat)))))
  '(_.0))

(test "13-EZ-0"
  (time
   (run 1 (y)
     (eval-programo
      `(run* (z)
         (letrec-func ((append (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s)))))
           (letrec-rel ((appendo (l1 l2 l)
                          (conde
                            ((== '() l1) (== l2 l))
                            ((fresh (a d l3)
                               (== (cons a d) l1)
                               (== (cons a l3) l)
                               (appendo d l2 l3))))))
             (appendo (append '(cat) '(dog)) ((mu (a b) '(fish rat)) '(fish) '(rat)) z))))
      '((cat dog fish rat)))))
  '(_.0))

(test "13-EZ-1"
  (time
   (run 2 (x y)
     (eval-programo
      `(run* (z)
         (letrec-func ((append (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s)))))
           (letrec-rel ((appendo (l1 l2 l)
                          (conde
                            ((== '() l1) (== l2 l))
                            ((fresh (a d l3)
                               (== (cons a d) l1)
                               (== (cons a l3) l)
                               (appendo d l2 l3))))))
             (,x (append '(cat) '(dog)) (,y '(fish) '(rat)) z))))
      '((cat dog fish rat)))))
  '(((appendo (mu (_.0 _.1) '(fish rat)))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (sym _.0 _.1))
    (appendo append)))

;; aqua tests

(test "refl-1"
  (run* (q)
    (eval-programo `(run* (z)
                      (== (meaning 1 'error) z))
                   q))
  '((1)))

(test "refl-2"
  (run* (q)
    (eval-programo `(run* (z)
                      (== (mu (e r) 1) z))
                   q))
  '(((mu-reifier (e r) 1))))

(test "refl-3"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e r) 1)) z))
                   q))
  '((1)))

(test "refl-4"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e r) (meaning 1 r))) z))
                   q))
  '((1)))

(test "refl-5"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e r) (meaning (car e) r)) 1) z))
                   q))
  '((1)))

(test "refl-6b"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e r) (meaning (car e) r)) (car (cons 1 '()))) z))
                   q))
  '((1)))

(test "refl-7"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e1 r1) ((mu (e2 r2) (meaning 1 r2))))) z))
                   q))
  '((1)))

(test "refl-8"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e1 r1) ((mu (e2 r2) ((mu (e3 r3) (meaning 'level r3))))))) z))
                   q))
  '(((s z))))

(test "refl-8b"
  (run* (q)
    (eval-programo `(run (()) (z)
                      (== ((mu (e1 r1) ((mu (e2 r2) ((mu (e3 r3) (meaning 'level r3))))))) z))
                   q))
  '(((s z))))

#!eof

#|

---------------------

mu        scheme->scheme
(mu (e r) <scheme value expression>)

muo       mk->mk
(muo (ges) <goal expression>)

muos      mk->scheme
(muos (s) <scheme value expression>)

muso      scheme->mk
(muso (x) <goal expression>)

---------------------

meaning   in Scheme context
(meaning <scheme value expression> r)

meaningo  in miniKanren context
(meaningo <goal expression>)

---------------------

TODO: what do we want to reify apart from the goal expressions? Maybe the state? Maybe the extension to the stream? Maybe can you tell if a goal fails (not relational!)? Maybe collecting semantics?

TODO: consider a version that uses CPS/continuations, as in Blond.  (e r) becomes (e r k) for mu and meaning.  What about for the miniKanren versions?  (e r sk fk) perhaps?  (e r bind) or (e r mplus) or (e r bind mplus) or (e r $) or something like that?  Or keep it (e r k)?  Or perhaps represent a continuation as a list of goals to be run still (or goal expressions)?  Does the mk version need a constraint store as well as an r?

|#

(test "refl-muo-1"
  (run* (q)
    (eval-programo `(run* (z)
                      ((muo (ges)
                            ;; TODO: what do we want to reify apart from the goal expressions? Maybe the state? Maybe the extension to the stream? Maybe can you tell if a goal fails (not relational!)? Maybe collecting semantics?
                         (fresh (ge1 ge2)
                           (== ges (cons ge1 (cons ge2 '())))
                           (meaningo ge2)))
                       (== 1 0) (== 1 1)))
                   q))
  '((_.0)))

(test "refl-muos-1"
  (run* (q)
    (eval-programo `(run* (z)
                      ;; basically, a lambda escape in regular miniKanren
                      (muos (s)
                        (log 'z (walk* z s)))
                      )
                   q))
  '((_.0)))

(test "refl-muso-1"
  (run* (q)
    (eval-programo `(run* (z)
                      ;; like a nested run
                      (==
                       (muso (x) (== x 1))
                       1))
                   q))
  '((_.0)))

#!eof

;; running these backwards seems super slow, at best

(test "refl-8-backwards-0"
  (run 1 (q)
    (eval-programo `(run (()) (z)
                      (== ((mu (e1 r1) ((mu (e2 r2) ((mu (e3 r3) (meaning 'level ,q))))))) z))
                   q))
  '(((quote (s z)) level)))

(test "refl-8-backwards-1"
  (run 2 (q)
    (eval-programo `(run* (z)
                      (== ((mu (e1 r1) ((mu (e2 r2) ((mu (e3 r3) (meaning ',q r3))))))) z))
                   q))
  '(((quote (s z)) level)))

(test "refl-8-backwards-2"
  (run 2 (q)
    (eval-programo `(run* (z)
                      (== ((mu (e1 r1) ((mu (e2 r2) ((mu (e3 r3) (meaning 'level ,q))))))) z))
                   q))
  '((r3
     ('((level val s z) . _.0)
      (absento (closure _.0)
               (mu-reifier _.0)
               (prim _.0))))))

#!eof

;; this test used to run to completion, but now appears to be too slow
;; to come back in a tolerable time
(test "13"
  (time
   (run 1 (v w x y)
     (eval-programo
      `(run* (z)
         (letrec-func ((append (l s)
                         (if (null? l)
                             s
                             (cons (car ,v) (append (cdr l) s)))))
           (letrec-rel ((appendo (l1 l2 l)
                          (conde
                            ((== '() l1) (== l2 l))
                            ((fresh (a d l3)
                               (== (,w a d) l1)
                               (== (cons a l3) l)
                               (appendo d l2 l3))))))
             (,x (append '(cat) '(dog)) (,y '(fish) '(rat)) z))))
      '((cat dog fish rat)))))
  '((l cons appendo append)))

;; this aqua test doesn't work, since midoriKanren doesn't support lambda
(test "refl-6"
  (run* (q)
    (eval-programo `(run* (z)
                      (== ((mu (e r) (meaning (car e) r)) ((lambda (x) x) 1)) z))
                   q))
  '((1)))
