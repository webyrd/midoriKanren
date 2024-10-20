;; WEB 16 October 2024

;; Starting from `full-interp.scm`
;; (https://github.com/michaelballantyne/faster-minikanren/blob/master/full-interp.scm)
;; from `faster-miniKanren`
;; (https://github.com/michaelballantyne/faster-minikanren/), add
;; microKanren/miniKanren
;; (https://github.com/jasonhemann/microKanren).

;; The resulting interpreter is *very* slow when evaluating miniKanren
;; code (one test is ~100,000x slower, compared to running equivalent
;; code in faster-miniKanren).


;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define (peano n)
  (cond
    ((zero? n) '())
    (else (list (peano (sub1 n))))))

(defrel (evalo expr val)
  (eval-expo
   `(let ((empty-state (cons '() ;; empy-s
                             '() ;; c = 0 (Peano)
                             )))
      (let ((call/goal (lambda (g) (g empty-state))))
        (letrec ((map (lambda (p l)
                        (if (null? l)
                            '()
                            (cons (p (car l)) (map p (cdr l)))))))
          (letrec ((length (lambda (l)
                             (if (null? l)
                                 '() ;; Peano 0
                                 (cons (length (cdr l)) '())))))
            (letrec ((pull (lambda ($)
                             (if (procedure? $) (pull ($)) $))))
              (letrec ((take-all (lambda ($)
                                   (let (($ (pull $)))
                                     (if (null? $)
                                         '()
                                         (cons (car $) (take-all (cdr $))))))))
                (letrec ((take (lambda (pn $)
                                 (if (null? pn)
                                     '()
                                     (let (($ (pull $)))
                                       (if (null? $) '() (cons (car $) (take (car pn) (cdr $)))))))))
                  (letrec ((mplus (lambda ($1 $2)
                                    (if (null? $1)
                                        $2
                                        (if (procedure? $1)
                                            (lambda () (mplus $2 ($1)))
                                            (cons (car $1) (mplus (cdr $1) $2)))))))
                    (letrec ((bind (lambda ($ g)
                                     (if (null? $)
                                         '() ;; mzero
                                         (if (procedure? $)
                                             (lambda () (bind ($) g))
                                             (mplus (g (car $)) (bind (cdr $) g)))))))
                      (let ((disj (lambda (g1 g2)
                                    (lambda (s/c)
                                      (mplus (g1 s/c) (g2 s/c))))))
                        (let ((conj (lambda (g1 g2)
                                      (lambda (s/c)
                                        (bind (g1 s/c) g2)))))
                          (letrec ((assp (lambda (p l)
                                           (match l
                                             [`() #f]
                                             [`((,k . ,v) . ,rest)
                                              (if (p k)
                                                  (cons k v)
                                                  (assp p rest))]))))
                            (letrec ((walk (lambda (u s)
                                             (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
                                               (if pr (walk (cdr pr) s) u)))))
                              (letrec ((walk* (lambda (v s)
                                                (let ((v (walk v s)))
                                                  (if (var? v)
                                                      v
                                                      (if (pair? v)
                                                          (cons (walk* (car v) s)
                                                                (walk* (cdr v) s))
                                                          v))))))
                                (let ((call/fresh (lambda (f)
                                                    (lambda (s/c)
                                                      (match s/c
                                                        [`(,s . ,c)
                                                         ((f (var c)) (cons s (cons c '())))])))))
                                  (letrec ((ext-s (lambda (x v s) (cons (cons x v) s))))
                                    (letrec ((unify (lambda (u v s)
                                                      (let ((u (walk u s)))
                                                        (let ((v (walk v s)))
                                                          (if (and (var? u) (var? v) (var=? u v))
                                                              s
                                                              (if (var? u)
                                                                  (ext-s u v s)
                                                                  (if (var? v)
                                                                      (ext-s v u s)
                                                                      (if (and (pair? u) (pair? v))
                                                                          (let ((s (unify (car u) (car v) s)))
                                                                            (and s (unify (cdr u) (cdr v) s)))
                                                                          (and (equal? u v) s))))))))))
                                      (let ((== (lambda (u v)
                                                  (lambda (s/c)
                                                    (let ((s (unify u v (car s/c))))
                                                      (if s (cons (cons s (cdr s/c)) '()) '()))))))
                                        (let ((reify-name (lambda (pn)
                                                            (list '__ pn))))
                                          (letrec ((reify-s (lambda (v s)
                                                              (let ((v (walk v s)))
                                                                (if (var? v)
                                                                    (let ((n (reify-name (length s))))
                                                                      (cons (cons v n) s))
                                                                    (if (pair? v)
                                                                        (reify-s (cdr v) (reify-s (car v) s))
                                                                        s))))))
                                            (let ((reify-1st (lambda (s/c)
                                                               (let ((v (walk* (var '()) (car s/c))))
                                                                 (walk* v (reify-s v '()))))))
                                              ,expr)))))))))))))))))))))
   initial-env
   val))

(defrel (eval-expo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absent-tago val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ;; WEB TODO issues with variable capture?
    ((fresh (ge)
       (== `(Zzz ,ge) expr)
       (not-in-envo 'Zzz env)
       (eval-expo `(lambda (s/c) (lambda () (,ge s/c))) env val)))

    ;;
    ((fresh (ge)
       (== `(conj+ ,ge) expr)
       (not-in-envo 'conj+ env)
       (eval-expo `(Zzz ,ge) env val)))
    ;;
    ((fresh (ge0 ge1 ge*)
       (== `(conj+ ,ge0 ,ge1 . ,ge*) expr)
       (not-in-envo 'conj+ env)
       (eval-expo `(conj (Zzz ,ge0) (conj+ ,ge1 . ,ge*)) env val)))

    ;;
    ((fresh (ge)
       (== `(disj+ ,ge) expr)
       (not-in-envo 'disj+ env)
       (eval-expo `(Zzz ,ge) env val)))
    ;;
    ((fresh (ge0 ge1 ge*)
       (== `(disj+ ,ge0 ,ge1 . ,ge*) expr)
       (not-in-envo 'disj+ env)
       (eval-expo `(disj (Zzz ,ge0) (disj+ ,ge1 . ,ge*)) env val)))

    ;;
    ((fresh (ge ge*)
       (== `(fresh () ,ge . ,ge*) expr)
       (not-in-envo 'fresh env)
       (eval-expo `(conj+ ,ge . ,ge*) env val)))
    ;;
    ((fresh (ge ge* x0 x*)
       (== `(fresh (,x0 . ,x*) ,ge . ,ge*) expr)
       (not-in-envo 'fresh env)
       (symbolo x0)
       (eval-expo `(call/fresh
                (lambda (,x0)
                  (fresh ,x* ,ge . ,ge*)))
              env
              val)))

    ;;
    ((fresh (ge ge*)
       (== `(conde (,ge . ,ge*)) expr)
       (not-in-envo 'conde env)
       (eval-expo `(conj+ ,ge . ,ge*) env val)))
    ;;
    ((fresh (ge0 ge0* ge1 ge1* c*)
       (== `(conde (,ge0 . ,ge0*) (,ge1 . ,ge1*) . ,c*) expr)
       (not-in-envo 'conde env)
       (eval-expo `(disj (conj+ ,ge0 . ,ge0*) (conde (,ge1 . ,ge1*) . ,c*)) env val)))    

    ;;
    ((fresh (x x* ge ge*)
       (== `(run* (,x) ,ge . ,ge*) expr)
       (not-in-envo 'run* env)
       (symbolo x)
       (eval-expo `(map reify-1st (take-all (call/goal (fresh (,x) ,ge . ,ge*)))) env val)))
    ;;
    ((fresh (pne x x* ge ge*)
       (== `(run ,pne (,x) ,ge . ,ge*) expr)
       (not-in-envo 'run env)
       (symbolo x)
       (eval-expo `(map reify-1st (take ,pne (call/goal (fresh (,x) ,ge . ,ge*)))) env val)))

    ;;
    ((fresh (x ge ge*)
       (== `(project (,x) ,ge . ,ge*) expr)
       (not-in-envo 'project env)
       (symbolo x)
       (eval-expo `(lambda (s/c) (let ((,x (walk* ,x (car s/c)))) ((conj+ ,ge . ,ge*) s/c))) env val)))
    
    ((symbolo expr) (lookupo expr env val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)))
    
    ((fresh (rator x rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((,x . (val . ,a*)) . ,env^) res)
       (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
       (eval-expo body res val)
       (eval-listo rands env a*)))

    ;;
    ((fresh (x e v body)
       (== `(let ((,x ,e)) ,body) expr)
       (not-in-envo 'let env)
       (eval-expo e env v)
       (eval-expo body `((,x . (val . ,v)) . ,env) val)))            
    ;;
    
    ((fresh (rator x* rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
       (eval-listo rands env a*)
       (ext-env*o x* a* env^ res)
       (eval-expo body res val)))

    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env `(prim . ,prim-id))
       (eval-primo prim-id a* env val)
       (eval-listo rands env a*)))

    ((handle-matcho expr env val))

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (conde
         ; Variadic
         ((symbolo x))
         ; Multiple argument
         ((list-of-symbolso x)))
       (not-in-envo 'letrec env)
       (eval-expo letrec-body
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  val)))

    ((prim-expo expr env val))

    ))

(define empty-env '())

(defrel (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(defrel (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(defrel (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(defrel (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(defrel (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

#;(defrel (handle-mpluso stream1 stream2 env val)
  (conde
    ((== '() stream1) (== stream2 val))
    ((fresh (a d)
       (== `(,a . ,d) stream1)
       (conde
         ((fresh (env^)
            ;; WEB is there a nicer approach than creating a closure?
            (== `(closure (lambda () (mplus $2 ($1))) ,env^) val)
            (procedure-tago a)
            (ext-env*o '($1 $2) `(,stream1 ,stream2) env env^)))
         ((fresh (res)
            (absent-tago a)
            (== `(,a . ,res) val)
            (handle-mpluso d stream2 env res))))))))

#;(defrel (handle-bindo stream goal env val)
  (conde
    ((== '() stream) (== '() val))
    ((fresh (a d env^)
       (== `(,a . ,d) stream)
       (ext-env*o '($ g) `(,stream ,goal) env env^)
       (conde
         ((fresh ()
            ;; WEB is there a nicer approach than creating a closure?
            (== `(closure (lambda () (bind ($) g)) ,env^) val)
            (procedure-tago a)))
         ((absent-tago a)
          ;; WEB is there a nicer approach than calling `eval-expo`?
          (eval-expo `(mplus (g (car $)) (bind (cdr $) g)) env^ val)))))))

(defrel (eval-primo prim-id a* env val)
  (conde
    ;;
    [(== prim-id 'var)
     (fresh (c)
       (== `(,c) a*)
       (== `(lvar . ,c) val))]

    [(== prim-id 'var?)
     (fresh (x)
       (== `(,x) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) x)
            (conde
              ((== 'lvar a)
               (== #t val))
              ((=/= 'lvar a)
               (== #f val)))))
         ((atomo x) (== #f val))))]

    [(== prim-id 'var=?)
     (fresh (c1 c2)
       (== `((lvar . ,c1) (lvar . ,c2)) a*)
       (conde
         ((== c1 c2) (== #t val))
         ((=/= c1 c2) (== #f val))))]
    
    #;[(== prim-id 'mplus)
     (fresh ($1 $2)
       (== `(,$1 ,$2) a*)
       (handle-mpluso $1 $2 env val))]

    #;[(== prim-id 'bind)
     (fresh ($ g)
       (== `(,$ ,g) a*)
       (handle-bindo $ g env val))]
    ;;
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (not-tago val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (not-tago a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((booleano v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]
    ;;
    [(== prim-id 'procedure?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (lam-expr env^)
            (== `(closure ,lam-expr ,env^) v)
            (== #t val)))
         ((fresh (prim-id)
            (== `(prim . ,prim-id) v)
            (== #t val)))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((== '() v) (== #f val))
         ((numbero v) (== #f val))
         ((symbolo v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v) (== #f val)
            (absent-tago a)))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #t val)
            (absent-tago a)))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((== '() v) (== #f val))
         ((numbero v) (== #f val))
         ((symbolo v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (procedure-tago a)))))]))

(defrel (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(defrel (boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(defrel (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(defrel (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env v))
         ((=/= #f v)
          (eval-expo e1 env v)
          (ando `(,e2 . ,e-rest) env val)))))))

(defrel (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(defrel (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env v))
         ((== #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

(defrel (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (conde
      ((=/= #f t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val)))))

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      ;;
                      (procedure? . (val . (prim . procedure?)))
                      (pair? . (val . (prim . pair?)))                                      
                      ;;
                      (var . (val . (prim . var)))
                      (var? . (val . (prim . var?)))
                      (var=? . (val . (prim . var=?)))
                      ;(mplus . (val . (prim . mplus)))
                      ;(bind . (val . (prim . bind)))
                      ;;
                      . ,empty-env))

(defrel (procedure-tago val)
  (conde
    ((== 'closure val))
    ((== 'prim val))))

(defrel (not-tago val)
  (fresh ()
    (=/= 'closure val)
    (=/= 'prim val)))

(defrel (absent-tago val)
  (fresh ()
    (absento 'closure val)
    (absento 'prim val)))

(defrel (handle-matcho expr env val)
  (fresh (against-expr mval clause clauses)
    (== `(match ,against-expr ,clause . ,clauses) expr)
    (not-in-envo 'match env)
    (eval-expo against-expr env mval)
    (match-clauses mval `(,clause . ,clauses) env val)))

(defrel (atomo t)
  (conde
    ((== '() t))
    ((booleano t))
    ((symbolo t))
    ((numbero t))))

(defrel (not-symbolo t)
  (conde
    ((== '() t))
    ((booleano t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (not-numbero t)
  (conde
    ((== '() t))
    ((booleano t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(defrel (literalo t)
  (conde
    ((== '() t))
    ((numbero t))
    ((symbolo t) (not-tago t))
    ((booleano t))))

(defrel (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(defrel (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expo result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(defrel (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (not-tago mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(defrel (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(defrel (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(defrel (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(defrel (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(defrel (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (not-tago mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))
