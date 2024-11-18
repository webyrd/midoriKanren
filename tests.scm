(load "faster-miniKanren/test-check.scm")
(load "midoriKanren.scm")

(test "copy-term-4o-1"
  (run* (x)
    (eval-programo
     `(run* (z)
        (fresh (vars-in-e vars-out-e in-e out-e a b c)
          (== (cons vars-in-e (cons in-e (cons vars-out-e (cons out-e '())))) z)
          (== (cons a (cons b '())) vars-in-e)
          (== (cons b (cons 'cat (cons c (cons a (cons b '()))))) in-e)
          ;; interface based on:
          ;; https://www.swi-prolog.org/pldoc/doc_for?object=copy_term/4          
          (copy-term-4o vars-in-e in-e vars-out-e out-e)))
     x))
  '(((((_. . ()) (_. . (()))) ;; vars-in-e
      ((_. . (())) cat (_. . ((()))) (_. . ()) (_. . (()))) ;; in-e
      ((_. . (((())))) (_. . ((((())))))) ;; vars-out-e
      ((_. . ((((()))))) cat (_. . ((()))) (_. . (((())))) (_. . ((((())))))) ;; out-e
      ))))


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

;; use copytermo for SKI reduction,
;; copying templates like `((I ,x) => ,x)
;; and then unifying the logic variable with a term.

(test "copy-I-rule-1"
  (run* (rv)
    (eval-programo
     `(run* (q)
        (fresh (rule-template rule-template-copy x)
          (== (cons rule-template (cons rule-template-copy '())) q)
          ;; rule-template = `((,x) (I ,x) => ,x)
          (== (cons (cons x '()) (cons (cons 'I (cons x '())) (cons '=> (cons x '())))) rule-template)
          (copy-termo rule-template rule-template-copy)))
     rv))
  '((((((_. . ()))   (I (_. . ()))   => (_. . ()))
      (((_. . (()))) (I (_. . (()))) => (_. . (())))))))

(test "copy-I-rule-1b"
  (run* (rv)
    (eval-programo
     `(run* (q)
        (fresh (rule-template rule-template-copy x rest)
          (== (cons rule-template (cons rule-template-copy '())) q)
          ;; rule-template = `((,x) (I ,x) => ,x)
          (== (cons (cons x '()) (cons (cons 'I (cons x '())) (cons '=> (cons x '())))) rule-template)
          (== (cons (cons 'K '()) rest) rule-template-copy)
          (copy-termo rule-template rule-template-copy)))
     rv))
  '((((((_.)) (I (_.)) => (_.))
      ((K) (I K) => K)))))

(test "copy-I-rule-1c"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x r ans)
           (== (cons rule-template (cons rule-template-copy (cons ans '()))) q)
           ;; rule-template = `((,x) (I ,x) => ,x)
           (== (cons (cons x '()) (cons (cons 'I (cons x '())) (cons '=> (cons x '())))) rule-template)
           (== (cons (cons 'K '()) (cons r (cons '=> (cons ans '())))) rule-template-copy)
           (copy-termo rule-template rule-template-copy)))
      rv)))
  '((((((_. . ())) (I (_. . ())) => (_. . ()))
      ((K) (I K) => K)
      K))))

(test "copy-S-rule-1c"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y z r ans)
           (== (cons rule-template (cons rule-template-copy (cons ans '()))) q)
           ;; rule-template = `((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
           (== (cons
                 (cons x (cons y (cons z '())))
                 (cons
                   (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                   (cons
                     '=>
                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '()))))
               rule-template)
           (== (cons
                 (cons 'I (cons 'K (cons 'K '())))
                 (cons
                   r
                   (cons
                     '=>
                     (cons ans '()))))
               rule-template-copy)
           (copy-termo rule-template rule-template-copy)
           ))
      rv)))
  '((((((_.) (_. ()) (_. (())))
       (((S (_.)) (_. ())) (_. (())))
       =>
       (((_.) (_. (()))) ((_. ()) (_. (())))))
      ((I K K)
       (((S I) K) K)
       =>
       ((I K) (K K)))
      ((I K) (K K))))))

(test "copy-S-rule-1d"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y z r ans)
           (== (cons rule-template-copy (cons ans '())) q)
           ;; rule-template = `((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
           (== (cons
                 (cons x (cons y (cons z '())))
                 (cons
                   (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                   (cons
                     '=>
                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '()))))
               rule-template)
           (== (cons
                 (cons 'I (cons 'K (cons 'K '())))
                 (cons
                   r
                   (cons
                     '=>
                     (cons ans '()))))
               rule-template-copy)
           (copy-termo rule-template rule-template-copy)
           ))
      rv)))
  '(((((I K K)
       (((S I) K) K)
       =>
       ((I K) (K K)))
      ((I K) (K K))))))

;; run 2 takes a long time or diverges
(test "copy-S-rule-1-infer-rule-1"
  (time
   (run 1 (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y z r ans)
           (== (cons rule-template-copy (cons ans '())) q)
           ;; rule-template = `((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
           (== (cons
                 (cons x (cons y (cons z '())))
                 (cons
                   (cons (cons (cons ',rv (cons x '())) (cons y '())) (cons z '()))
                   (cons
                     '=>
                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '()))))
               rule-template)
           (== (cons
                 (cons 'I (cons 'K (cons 'K '())))
                 (cons
                   r
                   (cons
                     '=>
                     (cons ans '()))))
               rule-template-copy)
           (copy-termo rule-template rule-template-copy)
           ))
      '((((I K K)
          (((S I) K) K)
          =>
          ((I K) (K K)))
         ((I K) (K K)))))))
  '(S))

(test "copy-S-rule-1-infer-rule-2a"
  (time
   (run 2 (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y z r ans)
           (== (cons rule-template-copy (cons ans '())) q)
           ;; rule-template = `((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
           (== (cons
                 (cons x (cons y (cons z '())))
                 (cons
                   (cons (cons (cons 'S (cons ,rv '())) (cons y '())) (cons z '()))
                   (cons
                     '=>
                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '()))))
               rule-template)
           (== (cons
                 (cons 'I (cons 'K (cons 'K '())))
                 (cons
                   r
                   (cons
                     '=>
                     (cons ans '()))))
               rule-template-copy)
           (copy-termo rule-template rule-template-copy)
           ))
      '((((I K K)
          (((S I) K) K)
          =>
          ((I K) (K K)))
         ((I K) (K K)))))))
  '('I x))

(test "copy-S-rule-1-infer-rule-2b"
  (time
   (run 1 (rv)
     (absento 'S rv)
     (absento 'K rv)     
     (absento 'I rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y z r ans)
           (== (cons rule-template-copy (cons ans '())) q)
           ;; rule-template = `((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
           (== (cons
                 (cons x (cons y (cons z '())))
                 (cons
                   (cons (cons (cons 'S (cons ,rv '())) (cons y '())) (cons z '()))
                   (cons
                     '=>
                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '()))))
               rule-template)
           (== (cons
                 (cons 'I (cons 'K (cons 'K '())))
                 (cons
                   r
                   (cons
                     '=>
                     (cons ans '()))))
               rule-template-copy)
           (copy-termo rule-template rule-template-copy)
           ))
      '((((I K K)
          (((S I) K) K)
          =>
          ((I K) (K K)))
         ((I K) (K K)))))))
  '(x))

(test "copy-I-rule-2"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x r ans)
           (== (cons rule-template (cons rule-template-copy (cons ans '()))) q)
           ;; rule-template = `((I ,x) => ,x)
           (== (cons (cons 'I (cons x '())) (cons '=> (cons x '()))) rule-template)
           (== (cons r (cons '=> (cons ans '()))) rule-template-copy)
           ;; rule application: (I (K S))
           (== (cons 'I (cons (cons 'K (cons 'S '())) '())) r)
           (copy-termo rule-template rule-template-copy)))
      rv)))
  '(((((I (_.)) => (_.))
      ((I (K S)) => (K S))
      (K S)))))

(test "copy-S-rule-2"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y z r ans)
           (== (cons rule-template-copy (cons ans '())) q)
           ;; rule-template = `((((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
           (== (cons
                 (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                 (cons
                   '=>
                   (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '())))
               rule-template)
           (== (cons
                 r
                 (cons
                   '=>
                   (cons ans '())))
               rule-template-copy)
           ;; rule application: (((S K) I) S)
           (== (cons (cons (cons 'S (cons 'K '())) (cons 'I '())) (cons 'S '())) r)
           (copy-termo rule-template rule-template-copy)))
      rv)))
  '(((((((S K) I) S) => ((K S) (I S))) ((K S) (I S))))))

(test "copy-K-rule-2"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (fresh (rule-template rule-template-copy x y r ans)
           (== (cons rule-template-copy (cons ans '())) q)
           ;; rule-template = `(((K ,x) ,y) => ,x)
           (== (cons
                 (cons (cons 'K (cons x '())) (cons y '()))
                 (cons '=> (cons x '())))
               rule-template)
           (== (cons
                 r
                 (cons
                   '=>
                   (cons ans '())))
               rule-template-copy)
           ;; rule application: ((K I) S)
           (== (cons (cons 'K (cons 'I '())) (cons 'S '())) r)
           (copy-termo rule-template rule-template-copy)))
      rv)))
  '((((((K I) S) => I) I))))


(test "cl-reducer-first-version-1"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (conde
                          ((== (cons 'I (cons T^ '())) T))
                          ((fresh (y)
                             (== (cons (cons 'K (cons T^ '())) (cons y '())) T)))
                          ((fresh (x y z)
                             (== (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '())) T)
                             (== (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) T^))))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(I (I S)) q)))))
      rv)))
  '(((I (I S))
     (I S)
     (I S)
     S
     S)))

(test "cl-reducer-first-version-2"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (conde
                          ((== (cons 'I (cons T^ '())) T))
                          ((fresh (y)
                             (== (cons (cons 'K (cons T^ '())) (cons y '())) T)))
                          ((fresh (x y z)
                             (== (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '())) T)
                             (== (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) T^))))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(((S K) I) K) q)))))
      rv)))
  '(((((S K) I) K) ((K K) (I K)) K ((K K) K) K)))


(test "cl-reducer-second-version-1"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (fresh (rule-template rule-template-copy x y z)
                          (== (cons T (cons '=> (cons T^ '()))) rule-template-copy)
                          (conde
                            ((== (cons
                                   (cons 'I (cons x '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons 'K (cons x '())) (cons y '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                                   (cons
                                     '=>
                                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '())))
                                 rule-template)))
                          (copy-termo rule-template rule-template-copy))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(I (I S)) q)))))
      rv)))
  '(((I (I S))
     (I S)
     (I S)
     S
     S)))

(test "cl-reducer-second-version-2"
  (time
   (run* (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (fresh (rule-template rule-template-copy x y z)
                          (== (cons T (cons '=> (cons T^ '()))) rule-template-copy)
                          (conde
                            ((== (cons
                                   (cons 'I (cons x '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons 'K (cons x '())) (cons y '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                                   (cons
                                     '=>
                                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '())))
                                 rule-template)))
                          (copy-termo rule-template rule-template-copy))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(((S K) I) K) q)))))
      rv)))
  '(((((S K) I) K) ((K K) (I K)) K ((K K) K) K)))

;; 1.7 seconds on Will's 2020 MBP
(test "cl-reducer-second-version-2-synthesize-part-of-S-rule-1"
  (time
   (run 1 (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (fresh (rule-template rule-template-copy x y z)
                          (== (cons T (cons '=> (cons T^ '()))) rule-template-copy)
                          (conde
                            ((== (cons
                                   (cons 'I (cons x '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons 'K (cons x '())) (cons y '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons (cons 'S (cons x '())) (cons ,rv '())) (cons z '()))
                                   (cons
                                     '=>
                                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '())))
                                 rule-template)))
                          (copy-termo rule-template rule-template-copy))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(((S K) I) K) q)))))
      '((((S K) I) K) ((K K) (I K)) K ((K K) K) K))))
  '(y))

;; 24 seconds on Will's 2020 MBP
(test "cl-reducer-second-version-2-synthesize-part-of-I-rule-1"
  (time
   (run 1 (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (fresh (rule-template rule-template-copy x y z)
                          (== (cons T (cons '=> (cons T^ '()))) rule-template-copy)
                          (conde
                            ((== (cons
                                   (cons 'I (cons x '()))
                                   (cons '=> (cons ,rv '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons 'K (cons x '())) (cons y '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                                   (cons
                                     '=>
                                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '())))
                                 rule-template)))
                          (copy-termo rule-template rule-template-copy))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(((S K) I) K) q)))))
      '((((S K) I) K) ((K K) (I K)) K ((K K) K) K))))
  '(x))

;; 24 seconds on Will's 2020 MBP
(test "cl-reducer-second-version-2-synthesize-part-of-K-rule-1"
  (time
   (run 1 (rv)
     (eval-programo
      `(run* (q)
         (letrec-rel ((contracto (T T^)
                        (fresh (rule-template rule-template-copy x y z)
                          (== (cons T (cons '=> (cons T^ '()))) rule-template-copy)
                          (conde
                            ((== (cons
                                   (cons 'I (cons x '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons 'K (cons ,rv '())) (cons y '()))
                                   (cons '=> (cons x '())))
                                 rule-template))
                            ((== (cons
                                   (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
                                   (cons
                                     '=>
                                     (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '())))
                                 rule-template)))
                          (copy-termo rule-template rule-template-copy))))
           (letrec-rel ((->1wo (T T^)
                          (conde
                            ((contracto T T^))
                            ((fresh (M N P)
                               (== (cons M (cons N '())) T)
                               (conde
                                 ((== (cons P (cons N '())) T^)
                                  (->1wo M P))
                                 ((== (cons M (cons P '())) T^)
                                  (->1wo N P))))))))
             (letrec-rel ((->wo (M N)
                            (conde
                              ((== M N))
                              ((fresh (P) (->1wo M P) (->wo P N))))))
               (->wo '(((S K) I) K) q)))))
      '((((S K) I) K) ((K K) (I K)) K ((K K) K) K))))
  '(x))



#|
`((,x) (I ,x) => ,x)
(list (list x) (list 'I x) '=> x)
(cons (cons x '()) (cons (cons 'I (cons x '())) (cons '=> (cons x '()))))
|#

#|
`((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
(list (list x y z) (list (list (list 'S x) y) z) '=> (list (list x z) (list y z)))
(cons
  (cons x (cons y (cons z '())))
  (cons
    (cons (cons (cons 'S (cons x '())) (cons y '())) (cons z '()))
    (cons
      '=>
      (cons (cons (cons x (cons z '())) (cons (cons y (cons z '())) '())) '()))))
|#

#|
`((,x) (I ,x) => ,x)
`((,x ,y) ((K ,x) ,y) => ,x)
`((,x ,y ,z) (((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
|#

#|
`((I ,x) => ,x)
`(((K ,x) ,y) => ,x)
`((((S ,x) ,y) ,z) => ((,x ,z) (,y ,z)))
|#

#|
Ix = x
`(I ,x) => x

Kxy = x
`((K ,x) ,y) => x

Sxyz = xz(yz)
`(((S ,x) ,y) ,z) => `((,x ,z) (,y ,z))
|#

;; TODO
;;
;; implement the equivalent of `copy_term/4` from
;;
;; https://www.swi-prolog.org/pldoc/doc_for?object=copy_term/4
;;
;; which only copies the specified variables, rather than copying all
;; free varibles in the "template".

;; TODO
;;
;; use `copytermo` for copying lambda terms
;;
;; `(lambda (,x) (lambda (,y) ,x))
;;
;; Problem is that lexical scope isn't respected with this encoding,
;; in terms of shadowing:
;;
;; `(lambda (,x) (lambda (,x) ,x))
;;
;; However, a careful encoding and use of the equivalent of
;; `copy_term/4` might be sufficient.
;;
;; Hmm---could I write an alpha renamer?
;; A gensym-er?
;; A compiler?
;;
;;
;; try using copy-termo for template for let-polymprphism for H-M type inferencer
;; -- will this allow us to have principle typing?
;;
;; actually, a type inferencer in general sounds fun and interesting, esp. for synthesis
;;
;; can we do subtyping?

;; TODO
;;
;; use `copy-termo` and templates to implement sequent calculus,
;; natural deduction, Hilbert systems, etc.
;;
;; Templates and copy-termo may help with schemas.  Also, Peano
;; axioms.  semi-unification.  anti-unification.  let-polymorphism.

;; TODO
;;
;; implement `subsumes-termo` in midoriKanren, similar to
;;
;; https://www.swi-prolog.org/pldoc/man?predicate=subsumes_term%2f2
;;
;; I think the implementation will need to be "from scratch" rather than
;; using `copy-termo` or whatever, to get the correct semantics.
;;
;; For example, would need to properly handle the equivalent of this
;; test from
;; https://www.swi-prolog.org/pldoc/man?predicate=subsumes_term%2f2
;;
;; ?- subsumes_term(f(1,B),f(A,2)).  % would unify, but Generic is not
;; "more generic" (nor is Specific "more generic")

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
