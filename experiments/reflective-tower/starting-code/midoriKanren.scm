(load "../../faster-miniKanren/mk-vicare.scm")
(load "../../faster-miniKanren/mk.scm")

(define empty-s '())
(define peano-zero '())
(define init-env '())

(define (peano n)
  (if (zero? n) '() `(,(peano (- n 1)))))

(define (var?o x)
  (fresh (val)
    ;; `val` is a Peano numeral    
    (== `(var . ,val) x)))

(define (var=?o x y)  
  (fresh (val)
    ;; `val` is a Peano numeral
    (== `(var . ,val) x)
    (== `(var . ,val) y)))

(define (var=/=o x y)
  (fresh (val1 val2)
    ;; `val1` and `val2` are Peano numerals
    (== `(var . ,val1) x)
    (== `(var . ,val2) y)
    (=/= val1 val2)))

(define (booleano b)
  (conde
    [(== #t b)]
    [(== #f b)]))

(define (walko u s v)
  (conde
    [(== u v)
     (conde
       [(symbolo u) (== u v)]
       [(numbero u) (== u v)]
       [(booleano u) (== u v)]
       [(== '() u) (== u v)])]
    [(fresh (a d)
       (== `(,a . ,d) u)
       (=/= a 'var)
       (== u v))]
    [(var?o u)
     (conde
       [(== u v) (not-assp-subo u s)]
       [(fresh (pr-d)
          (assp-subo u s `(,u . ,pr-d))
          (walko pr-d s v))])]))

(define (assp-subo v s out)
  (fresh (h t h-a h-d)
    (== `(,h . ,t) s)
    (== `(,h-a . ,h-d) h)
    (var?o v)
    (var?o h-a)
    (conde
      [(== h-a v) (== h out)]
      [(=/= h-a v) (assp-subo v t out)])))

(define (not-assp-subo v s)
  (fresh ()
    (var?o v)
    (conde
      [(== '() s)]
      [(fresh (t h-a h-d)
        (== `((,h-a . ,h-d) . ,t) s)
        (var?o h-a)
        (=/= h-a v)
        (not-assp-subo v t))])))

(define (ext-so u v s s1)
  (== `((,u . ,v) . ,s) s1))

; u, v <- {logic var, number, symbol, boolean, empty list, non-empty list}
; Total 36 + 5 (types match, but terms do not) = 41 cases
(define (unifyo u-unwalked v-unwalked s s1)
  (fresh (u v)
    (walko u-unwalked s u)
    (walko v-unwalked s v)
    (conde
      [(var?o u) (var?o v) (var=?o u v) (== s s1)]
      [(var?o u) (var?o v) (var=/=o u v) (ext-so u v s s1)]
      [(var?o u) (numbero v) (ext-so u v s s1)]
      [(var?o u) (symbolo v) (ext-so u v s s1)]
      [(var?o u) (booleano v) (ext-so u v s s1)]
      [(var?o u) (== '() v) (ext-so u v s s1)]
      [(var?o u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (ext-so u v s s1)]
      [(numbero u) (var?o v) (ext-so v u s s1)]
      [(numbero u) (numbero v) (== u v) (== s s1)]
      [(numbero u) (numbero v) (=/= u v) (== #f s1)]
      [(numbero u) (symbolo v) (== #f s1)]
      [(numbero u) (booleano v) (== #f s1)]
      [(numbero u) (== '() v) (== #f s1)]
      [(numbero u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(symbolo u) (var?o v) (ext-so v u s s1)]
      [(symbolo u) (numbero v) (== #f s1)]
      [(symbolo u) (symbolo v) (== u v) (== s s1)]
      [(symbolo u) (symbolo v) (=/= u v) (== #f s1)]
      [(symbolo u) (booleano v) (== #f s1)]
      [(symbolo u) (== '() v) (== #f s1)]
      [(symbolo u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(booleano u) (var?o v) (ext-so v u s s1)]
      [(booleano u) (numbero v) (== #f s1)]
      [(booleano u) (symbolo v) (== #f s1)]
      [(booleano u) (booleano v) (== u v) (== s s1)]
      [(booleano u) (booleano v) (=/= u v) (== #f s1)]
      [(booleano u) (== '() v) (== #f s1)]
      [(booleano u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(== '() u) (var?o v) (ext-so v u s s1)]
      [(== '() u) (numbero v) (== #f s1)]
      [(== '() u) (symbolo v) (== #f s1)]
      [(== '() u) (booleano v) (== #f s1)]
      [(== '() u) (== '() v) (== s s1)]
      [(== '() u)
       (fresh (a d)
         (== `(,a . ,d) v)
         (=/= 'var a))
       (== #f s1)]
      [(var?o v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (ext-so v u s s1)]
      [(numbero v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(symbolo v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(booleano v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(== '() v)
       (fresh (a d)
         (== `(,a . ,d) u)
         (=/= 'var a))
       (== #f s1)]
      [(fresh (u-a u-d v-a v-d s-a)
         (== `(,u-a . ,u-d) u)
         (== `(,v-a . ,v-d) v)
         (=/= 'var u-a)
         (=/= 'var v-a)
         (conde
           [(== s-a #f) (== #f s1) (unifyo u-a v-a s s-a)]
           [(=/= s-a #f)
            (unifyo u-a v-a s s-a)
            (unifyo u-d v-d s-a s1)]))])))

(define mzero '())

(define (mpluso $1 $2 $)
  (conde
    [(== '() $1) (== $2 $)]
    [(fresh (d)
       (== `(delayed . ,d) $1)
       (== `(delayed mplus ,$1 ,$2) $))]
    [(fresh (a d r1)
       (== `(,a . ,d) $1)
       (=/= 'delayed a)
       (== `(,a . ,r1) $)
       (mpluso d $2 r1))]))

(define (bindo $ ge env $1)
  (conde
    [(== '() $) (== mzero $1)]
    [(fresh (d)
       (== `(delayed . ,d) $)
       (== `(delayed bind ,$ ,ge ,env) $1))]
    [(fresh ($1-a $1-d v-a v-d)
       (== `(,$1-a . ,$1-d) $)
       (=/= 'delayed $1-a)
       (eval-gexpro ge $1-a env v-a)
       (bindo $1-d ge env v-d)
       (mpluso v-a v-d $1))]))

(define (exto params args env env1)
  (conde
    [(== params '())
     (== args '())
     (== env env1)]
    [(fresh (x-a x-d v-a v-d)
       (== `(,x-a . ,x-d) params)
       (== `(,v-a . ,v-d) args)
       (symbolo x-a)
       (exto x-d v-d `((,x-a . ,v-a) . ,env) env1))]))

(define (ext-varo params c c^ env env1)
  (conde
    [(== params '())
     (== c c^)
     (== env env1)]
    [(fresh (x-a x-d)
       (== `(,x-a . ,x-d) params)
       (symbolo x-a)
       (ext-varo x-d `(,c) c^ `((,x-a . (var . ,c)) . ,env) env1))]))

(define (lookupo x env v)
  (conde
    [(fresh (y u env1)
      (== `((,y . ,u) . ,env1) env)
      (=/= y 'rec)
      (conde
        [(== x y) (== v u)]
        [(=/= x y) (lookupo x env1 v)]))]
    [(fresh (id params geb env1)
        (== `((rec (closr ,id ,params ,geb)) . ,env1) env)
        (conde
          [(== id x)
           (== `(closr ,params ,geb ,env) v)]
          [(=/= id x)
           (lookupo x env1 v)]))]
    [(fresh (id params body-expr env1)
       (== `((rec (scheme-closr ,id ,params ,body-expr)) . ,env1) env)
       (conde
         [(== id x)
          (== `(scheme-closr ,params ,body-expr ,env) v)]
         [(=/= id x)
          (lookupo x env1 v)]))]))

(define (not-in-envo x env)
  (conde
    [(== '() env)]
    [(fresh (y v env1)
       (== `((,y . ,v) . ,env1) env)
       (=/= x y)
       (not-in-envo x env1))]))

(define (eval-args args env vals)
  (conde
    [(== args '())
     (== vals '())]
    [(fresh (a d va vd)
       (== `(,a . ,d) args)
       (== `(,va . ,vd) vals)
       (eval-schemeo a env va)
       (eval-args d env vd))]))

(define (eval-conde-clauses c* s/c env $)
  (conde
    ((fresh (ge ge*)
       (== `((,ge . ,ge*)) c*)
       (eval-gexpro `(conj* ,ge . ,ge*) s/c env $)))
    ((fresh (ge ge* c2 c-rest c-$ c-rest$)
       (== `((,ge . ,ge*) ,c2 . ,c-rest) c*)
       (eval-gexpro `(conj* ,ge . ,ge*) s/c env c-$)
       (eval-conde-clauses `(,c2 . ,c-rest) s/c env c-rest$)
       (mpluso c-$ c-rest$ $)))))

(define (eval-gexpro expr s/c env $)
  (conde
    [(fresh (ge)
       (== `(delay ,ge) expr)
       (== `(delayed eval ,ge ,s/c ,env) $))]
    [(== `(conj*) expr) (== `(,s/c) $)]
    [(fresh (ge ge* ge-$)
       (== `(conj* ,ge . ,ge*) expr)
       (eval-gexpro ge s/c env ge-$)
       (bindo ge-$ `(conj* . ,ge*) env $))]
    [(fresh (x* ge ge* s c c^ env^)
       (== `(fresh ,x* ,ge . ,ge*) expr)
       (== `(,s . ,c) s/c)
       (ext-varo x* c c^ env env^)
       (eval-gexpro `(conj* ,ge . ,ge*) `(,s . ,c^) env^ $))]
    [(fresh (te1 te2 v1 v2 s c s1)
       (== `(== ,te1 ,te2) expr)
       (== `(,s . ,c) s/c)
       (eval-schemeo te1 env v1)
       (eval-schemeo te2 env v2)
       (conde
         [(== #f s1) (== '() $)]
         [(=/= #f s1) (== `((,s1 . ,c)) $)])
       (unifyo v1 v2 s s1))]
    [(fresh (c*)
       (== `(conde . ,c*) expr)
       (eval-conde-clauses c* s/c env $))]
    [(fresh (x e v ge ge* env^)
       (== `(let ((,x ,e)) ,ge . ,ge*) expr)
       (symbolo x)
       (== `((,x . ,v) . ,env) env^)
       (eval-schemeo e env v)
       (eval-gexpro `(conj* ,ge . ,ge*) s/c env^ $))]
    [(fresh (id params geb ge ge* env^)
       (== `(letrec-rel ((,id ,params ,geb)) ,ge . ,ge*) expr)
       (symbolo id)
       (== `((rec (closr ,id ,params (delay ,geb))) . ,env) env^)
       (eval-gexpro `(conj* ,ge . ,ge*) s/c env^ $))]    
    [(fresh (id params body-expr ge ge* env^)
       (== `(letrec-func ((,id ,params ,body-expr)) ,ge . ,ge*) expr)
       (symbolo id)
       (== `((rec (scheme-closr ,id ,params ,body-expr)) . ,env) env^)
       (eval-gexpro `(conj* ,ge . ,ge*) s/c env^ $))]
    [(fresh (id args params geb env1 ext-env vargs)
       ;; Call to relation
       (== `(,id . ,args) expr)
       (lookupo id env `(closr ,params ,geb ,env1))
       (eval-args args env vargs)
       (exto params vargs env1 ext-env)
       (eval-gexpro geb s/c ext-env $))]))

(define (eval-schemeo expr env val)
  (conde
    [(== expr val)
     (conde
       [(numbero expr)]
       [(booleano expr)]
       [(== '() expr)])]
    [(== `(quote ,val) expr)
     (conde
       [(numbero val)]
       [(symbolo val)]
       [(booleano val)]
       [(== '() val)]
       [(fresh (a d)
          (== `(,a . ,d) val))])
     (not-in-envo 'quote env)
     (absento 'var val)
     (absento 'closr val)]
    [(symbolo expr)
     (lookupo expr env val)]
    [(fresh (e v a d)
       (== `(null? ,e) expr)
       (not-in-envo 'null? env)
       (eval-schemeo e env v)
       (conde
         ((== #f val) (== `(,a . ,d) v))
         ((== #t val) (== '() v))))]
    [(fresh (e vd)
       (== `(car ,e) expr)
       (not-in-envo 'car env)
       (eval-schemeo e env `(,val . ,vd)))]
    [(fresh (e va)
       (== `(cdr ,e) expr)
       (not-in-envo 'cdr env)
       (eval-schemeo e env `(,va . ,val)))]
    [(fresh (e1 e2 v-e1 v-e2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v-e1 . ,v-e2) val)
       (not-in-envo 'cons env)
       (eval-schemeo e1 env v-e1)
       (eval-schemeo e2 env v-e2))]
    [(fresh (te ce ae b)
       (== `(if ,te ,ce ,ae) expr)
       (not-in-envo 'if env)
       (eval-schemeo te env b)
       (conde
         ((=/= #f b) (eval-schemeo ce env val))
         ((== #f b) (eval-schemeo ae env val))))]
    [(fresh (id args params body-expr env1 vargs ext-env)
       ;; Scheme procedure call
       (== `(,id . ,args) expr)
       (lookupo id env `(scheme-closr ,params ,body-expr ,env1))
       (eval-args args env vargs)
       (exto params vargs env1 ext-env)
       (eval-schemeo body-expr ext-env val))]))

(define (walk*o unwalked-v s u)
  (fresh (v)
    (walko unwalked-v s v)
    (conde
      [(== v u)
       (conde
         [(var?o v)]
         [(numbero v)]
         [(symbolo v)]
         [(booleano v)]
         [(== '() v)])]
      [(fresh (a d walk*-a walk*-d)
         (== `(,a . ,d) v)
         (=/= a 'var)
         (conde
           [(== '_. a)
            (== u v)]
           [(=/= '_. a)
            (== `(,walk*-a . ,walk*-d) u)
            (walk*o a s walk*-a)
            (walk*o d s walk*-d)]))])))

(define (pullo $ $1)
  (conde
    [(== '() $) (== '() $1)]
    [(fresh (a d)
       (== `(,a . ,d) $)
       (== $ $1)
       (=/= 'delayed a))]
    [(fresh (ge s/c env $2)
       (== `(delayed eval ,ge ,s/c ,env) $)
       (eval-gexpro ge s/c env $2)
       (pullo $2 $1))]
    [(fresh ($a $b $a1 $2)
       (== `(delayed mplus ,$a ,$b) $)
       (pullo $a $a1)
       (mpluso $b $a1 $2)
       (pullo $2 $1))]
    [(fresh (saved-ge saved-env saved-$ saved-$1 $2)
       (== `(delayed bind ,saved-$ ,saved-ge ,saved-env) $)
       (pullo saved-$ saved-$1)
       (bindo saved-$1 saved-ge saved-env $2)
       (pullo $2 $1))]))

(define (take-allo $ s/c*)
  (fresh ($1)
    (pullo $ $1)
    (conde
      [(== '() $1) (== '() s/c*)]
      [(fresh (a d-s/c* $d)
         (== `(,a . ,$d) $1)
         (== `(,a . ,d-s/c*) s/c*)
         (take-allo $d d-s/c*))])))

(define (take-no n $ s/c*)
  (conde
    [(== '() n) (== '() s/c*)]
    [(=/= '() n)
     (fresh ($1)
       (pullo $ $1)
       (conde
         [(== '() $1) (== '() s/c*)]
         [(fresh (n-1 d-s/c* a d)
            (== `(,a . ,d) $1)
            (== `(,n-1) n)
            (== `(,a . ,d-s/c*) s/c*)
            (take-no n-1 d d-s/c*))]))]))

(define (lengtho l len)
  (conde
    [(== '() l) (== '() len)]
    [(fresh (a d len-d)
       (== `(,a . ,d) l)
       (== `(,len-d) len)
       (lengtho d len-d))]))

(define (reify-so v-unwalked s s1)
  (fresh (v)
    (walko v-unwalked s v)
    (conde
      [(var?o v)
       (fresh (len)
         (lengtho s len)
         (== `((,v . (_. . ,len)) . ,s) s1))]
      [(== s s1)
       (conde
         [(numbero v)]
         [(symbolo v)]
         [(booleano v)]
         [(== '() v)])]
      [(fresh (a d sa)
         (=/= 'var a)
         (== `(,a . ,d) v)
         (conde
           [(== '_. a)
            (== s s1)]
           [(=/= '_. a)
            (reify-so a s sa)
            (reify-so d sa s1)]))])))

(define (reify-state/1st-varo s/c out)
  (fresh (s c v u)
    (== `(,s . ,c) s/c)
    (walk*o `(var . ()) s v)
    (reify-so v '() u)
    (walk*o v u out)))

(define (reifyo s/c* out)
    (conde
      [(== '() s/c*) (== '() out)]
      [(fresh (a d va vd)
         (== `(,a . ,d) s/c*)
         (== `(,va . ,vd) out)
         (reify-state/1st-varo a va)
         (reifyo d vd))]))

(define (eval-programo expr out)
  (conde
    [(fresh (lvar ge ge* $ s/c*)
       (symbolo lvar)
       (== `(run* (,lvar) ,ge . ,ge*) expr)
       (eval-gexpro `(fresh (,lvar) ,ge . ,ge*) `(,empty-s . ,peano-zero) init-env $)
       (take-allo $ s/c*)
       (reifyo s/c* out))]
    [(fresh (n lvar ge ge* $ s/c*)
       (symbolo lvar)
       (== `(run ,n (,lvar) ,ge . ,ge*) expr)
       (eval-gexpro `(fresh (,lvar) ,ge . ,ge*) `(,empty-s . ,peano-zero) init-env $)
       (take-no n $ s/c*)
       (reifyo s/c* out))]))
