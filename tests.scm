(load "faster-miniKanren/test-check.scm")
(load "midoriKanren.scm")

(test "copy-term-lookupo-1"
  (run* (var var^ var/var^-store)
    (== `(var . ()) var)
    (== '() var/var^-store)
    (copy-term-lookupo var var/var^-store var^))
  '(((var . ()) #f ())))

(test "copy-term-lookupo-2"
  (run* (var var^ var/var^-store)
    (== `(var . ()) var)
    (== `(((var . ()) . (var . (())))) var/var^-store)
    (copy-term-lookupo var var/var^-store var^))
  '(((var . ()) (var . (())) (((var . ()) . (var . (())))))))

(test "copy-term-lookupo-3"
  (run* (var var^ var/var^-store)
    (== `(var . ()) var)
    (== `(((var . (())) . (var . ((()))))) var/var^-store)
    (copy-term-lookupo var var/var^-store var^))
  '(((var . ()) #f (((var . (())) . (var . ((()))))))))

(test "copy-term-lookupo-4"
  (run* (var var^ var/var^-store)
    (== `(var . ((()))) var)
    (== `(((var . ()) . (var . (()))) ((var . ((()))) . (var . (((())))))) var/var^-store)
    (copy-term-lookupo var var/var^-store var^))
  '(((var . ((()))) (var . (((())))) (((var . ()) . (var . (()))) ((var . ((()))) . (var . (((())))))))))

(test "copy-term-auxo-0a"
  (run* (t1-copy t1-copy^ c^ c^^ var/var^-store^ var/var^-store^^)
    (copy-term-auxo `(var . ()) t1-copy '() '(()) c^ '() var/var^-store^)
    (copy-term-auxo `(var . ()) t1-copy^ '() c^ c^^ var/var^-store^ var/var^-store^^))
  '(((var . (())) ;; t1-copy 
     (var . (())) ;; t1-copy^ 
     ((())) ;; c^ 
     ((())) ;; c^^ 
     (((var . ()) . (var . (())))) ;; var/var^-store^ 
     (((var . ()) . (var . (())))) ;; var/var^-store^^
     )))

(test "copy-term-auxo-2a"
  (run 1 (t1-copy c^ var/var^-store^)
    (copy-term-auxo `((var . ()) . (var . ())) t1-copy '() '(()) c^ '() var/var^-store^))
  '((((var . (())) . (var . (()))) ;; t1-copy
     ((()))                        ;; c^
     (((var . ()) . (var . (())))) ;; var/var^-store^
     )))

(test "copy-term-auxo-2b"
  (run* (t1-copy c^ var/var^-store^)
    (copy-term-auxo `((var . ()) . (var . ())) t1-copy '() '(()) c^ '() var/var^-store^))
  '((((var . (())) . (var . (())))
     ((()))
     (((var . ()) . (var . (())))))))

(test "copy-term-auxo-3"
  (run* (t1-copy c^ var/var^-store^)
    (copy-term-auxo `(var . ()) t1-copy '() '(()) c^ '() var/var^-store^))
  '(((var . (()))
     ((()))
     (((var . ()) . (var . (())))))))

(test "copy-term-auxo-4"
  (run* (t1-copy c^ var/var^-store^)
    (copy-term-auxo `(var . ()) t1-copy '() '((())) c^ `(((var . ()) . (var . (())))) var/var^-store^))
  '(((var . (()))
     ((()))
     (((var . ()) . (var . (())))))))

(test "copy-termo-1"
  (run* (x)
    (eval-programo
     `(run* (z)
        (copy-termo '5 z))
     x))
  '((5)))

(test "copy-termo-2"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2)
          (== (cons t1 (cons t2 '())) z)
          (== (cons 3 4) t1)
          (copy-termo t1 t2)))
     x))
  '((((3 . 4)
      (3 . 4)))))

(test "copy-termo-3"
  (run* (x)
    (eval-programo
     `(run ,(peano 1) (z)
        (fresh (t1 t2 a)
          (== (cons t1 (cons t2 '())) z)
          (== (cons 'cat (cons #f (cons #t (cons 5 (cons '() '()))))) t1)
          (copy-termo t1 t2)))
     x))
  '((((cat #f #t 5 ())
      (cat #f #t 5 ())))))

(test "copy-termo-4"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a)
          (== (cons t1 (cons t2 '())) z)
          (== a t1)
          (copy-termo t1 t2)))
     x))
  '((((_. . ()) (_. . (()))))))

(test "copy-termo-5"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b)
          (== (cons t1 (cons t2 '())) z)
          (== (cons a b) t1)
          (copy-termo t1 t2)))
     x))
  '(((((_.) _. ()) ((_. (())) _. ((())))))))

(test "copy-termo-6"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a)
          (== (cons t1 (cons t2 '())) z)
          (== (cons a a) t1)
          (copy-termo t1 t2)))
     x))
  '(((((_. . ()) . (_. . ()))     ;; t1
      ((_. . (())) . (_. . (()))) ;; t2
      ))))

(test "copy-termo-7"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b)
          (== (cons t1 (cons t2 '())) z)
          (== (cons 3 (cons a (cons b (cons 4 (cons a '()))))) t1)
          (copy-termo t1 t2)))
     x))
  '((((3 (_. . ()) (_. . (())) 4 (_. . ())) ;; t1
      (3 (_. . ((()))) (_. . (((())))) 4 (_. . ((())))) ;; t2
      ))))

(test "copy-termo-8"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2)
          (== (cons t1 (cons t2 '())) z)
          (copy-termo t1 t2)))
     x))
  '((((_. . ()) ;; t1
      (_. . (())) ;; t2
      ))))

(test "copy-termo-8"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b)
          (== (cons t1 (cons t2 '())) z)          
          (== (cons 3 (cons a (cons b (cons 4 (cons a '()))))) t1)
          (== 'cat t2)
          (copy-termo t1 t2)))
     x))
  '(()))

(test "copy-termo-9"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b c)
          (== (cons t1 (cons t2 '())) z)          
          (== (cons 3 (cons a (cons b (cons 4 (cons a '()))))) t1)
          (== (cons 3 c) t2)
          (copy-termo t1 t2)))
     x))
  '((((3 (_. . ()) (_. . (())) 4 (_. . ())) ;; t1
      (3 (_. . ((()))) (_. . (((())))) 4 (_. . ((())))) ;; t2
      ))))

(test "copy-termo-10"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b c d)
          (== (cons t1 (cons t2 '())) z)          
          (== (cons 3 (cons a (cons b (cons 4 (cons a '()))))) t1)
          (== (cons 3 (cons c (cons d (cons 4 (cons c '()))))) t2)
          (copy-termo t1 t2)))
     x))
  '((((3 (_. . ()) (_. . (())) 4 (_. . ())) ;; t1
      (3 (_. . ((()))) (_. . (((())))) 4 (_. . ((())))) ;; t2
      ))))

(test "copy-termo-11"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b c)
          (== (cons t1 (cons t2 '())) z)
          (== (cons 3 (cons a (cons b (cons 4 (cons a '()))))) t1)
          (== (cons 3 (cons c (cons c (cons 4 (cons c '()))))) t2)
          (copy-termo t1 t2)))
     x))
  '((((3 (_. . ()) (_. . (())) 4 (_. . ())) ;; t1
      (3 (_. . ((()))) (_. . ((()))) 4 (_. . ((())))) ;; t2
      ))))

(test "copy-termo-12"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (t1 t2 a b c d)
          (== (cons t1 (cons t2 '())) z)
          (== (cons 3 (cons a (cons b (cons 4 (cons a '()))))) t1)
          (== (cons 3 (cons c (cons d (cons 4 (cons 'cat '()))))) t2)
          (copy-termo t1 t2)))
     x))
  '((((3 (_. . ()) (_. . (())) 4 (_. . ())) ;; t1
      (3 cat (_. . ((()))) 4 cat) ;; t2
      ))))

(test "copy-termo-synthesize-template-1"
  (time
   (run 1 (x)
     (eval-programo
      `(run* (z)
         (fresh (t1 t2 a b)
           (== (cons t1 (cons t2 '())) z)
           (== (cons 3 (cons ,x (cons b (cons 4 (cons a '()))))) t1)
           (copy-termo t1 t2)))
      '(((3 (_. . ()) (_. . (())) 4 (_. . ()))
         (3 (_. . ((()))) (_. . (((())))) 4 (_. . ((())))))))))
  '(a))


(test "copy-termo-synthesize-template-2"
  ;; 7.5 seconds on Will's 2020 MBP
  (time
   (run 2 (x)
     (eval-programo
      `(run* (z)
         (fresh (t1 t2 a)
           (== (cons t1 (cons t2 '())) z)
           (== (cons a ,x) t1)
           (copy-termo t1 t2)))
      '((((_. . ()) . (_. . ())) ((_. . (())) . (_. . (()))))))))
  '(a
    ((if _.0 a _.1) (num _.0))))

(test "copy-termo-synthesize-template-3a"
  (time
   (run* (x)
     (eval-programo
      `(run* (z)
         (fresh (t1 t2 a)
           (== (cons t1 (cons t2 '())) z)
           (== a t1)
           (== (cons 'cat (cons 'dog (cons 'cat '()))) t2)
           (copy-termo t1 t2)))
      x)))
  '((((_. . ()) (cat dog cat)))))

(test "copy-termo-synthesize-template-3b"
  (time
   (run 1 (x)
     (eval-programo
      `(run* (t2)
         (fresh (t1 a b)
           (== (cons a (cons b (cons ,x '()))) t1)
           (== (cons 'cat (cons 'dog (cons 'cat '()))) t2)           
           (copy-termo t1 t2)))
      '((cat dog cat)))))
  '(a))

(test "copy-termo-synthesize-template-3c"
  (time
   (run 1 (x)
     (eval-programo
      `(run* (t2)
         (fresh (t1 a b)
           (== (cons a (cons ,x (cons a '()))) t1)
           (== (cons 'cat (cons 'dog (cons 'cat '()))) t2)           
           (copy-termo t1 t2)))
      '((cat dog cat)))))
  '(b))

#|
;; Fans kicked on
(test "copy-termo-synthesize-template-3d"
  (time
   (run 1 (x y)
     (eval-programo
      `(run* (t2)
         (fresh (t1 a b)
           (== (cons a (cons ,x (cons ,y '()))) t1)
           (== (cons 'cat (cons 'dog (cons 'cat '()))) t2)           
           (copy-termo t1 t2)))
      '((cat dog cat)))))
  '???)
|#

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
