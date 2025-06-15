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
     (absento (closr _.0) (var _.0)))
    ((cdr (quote (_.0 . cat)))
     (absento (closr _.0) (var _.0)))))

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
     (absento (closr _.0) (var _.0)))))

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
