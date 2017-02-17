;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1: Warm Up
;(a) Write a Racket function racketlist->mupllist that takes a Racket list (presumably of mupl
;    values but that will not affect your solution) and produces an analogous mupl list with the same
;    elements in the same order
(define (racketlist->mupllist rl)
  {if (null? rl)
      (aunit)
      (apair (car rl) (racketlist->mupllist (cdr rl)))})

;(b) Write a Racket function mupllist->racketlist that takes a mupl list (presumably of mupl
;    values but that will not affect your solution) and produces an analogous Racket list (of mupl
;    values) with the same elements in the same order.
(define (mupllist->racketlist ml)
  {if (aunit? ml)
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))})

;; Problem 2: Implementing the mupl Language
;  Write a mupl interpreter, i.e., a Racket function eval-exp
;  that takes a mupl expression e and either returns the mupl value that e evaluates to under the empty
;  environment or calls Racket’s error if evaluation encounters a run-time mupl type error or unbound
;  mupl variable         

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e)
         (let ([v (int-num e)])
           (if (integer? v)
               e
               (error "MUPL int applied to non-integer")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if  (> (int-num v1) (int-num v2))
                    (eval-under-env (ifgreater-e3 e) env)
                    (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater conditioned with non-number")))]
        [(fun? e)
         (let ([s1 (fun-nameopt e)]
               [s2 (fun-formal e)])
           (if s1 ; not an anonymous function?
               (if (and (string? s1) (string? s2))
                   (closure env e)
                   (error "MUPL fun applied with non-string"))
               (if (string? s2) ; an anonymous function
                   (closure env e)
                   (error "MUPL (anonymous) fun applied with non-string"))))]
        [(call? e)
         (letrec ([env-fun (eval-under-env (call-funexp e) env)])
           (if (closure? env-fun)
               (letrec ([subenv (closure-env env-fun)]
                        [func (closure-fun env-fun)]
                        [arg (eval-under-env (call-actual e) env)]
                        [argname (fun-formal func)]
                        [funcbody (fun-body func)]
                        [funcname (fun-nameopt func)])
                 (let ([subenv (cons (cons argname arg) subenv)])
                   (if funcname ; not anonymous?
                       (eval-under-env funcbody (cons (cons funcname (closure subenv func)) subenv))
                       (eval-under-env funcbody subenv))))
                 (error "MUPL call applied to non-closure")))]
        [(mlet? e)
         (let ([s (mlet-var e)]
               [v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons s v) env)))] 
        [(apair? e)
         (let([v1 (eval-under-env (apair-e1 e) env)]
              [v2 (eval-under-env (apair-e2 e) env)])
             (apair v1 v2))]
        [(fst? e)
         (let ([pr (eval-under-env (fst-e e) env)])
         (if (apair? pr)
             (apair-e1 pr)
             (error "MUPL fst applied with non-apair")))]
        [(snd? e)
         (let ([pr (eval-under-env (snd-e e) env)])
         (if (apair? pr)
             (apair-e2 pr)
             (error "MUPL snd applied with non-apair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([a (eval-under-env (isaunit-e e) env)])
           (if (aunit? a)
               (int 1)
               (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3: Expanding the Language
;  mupl is a small language, but we can write Racket functions that act like
;  mupl macros so that users of these functions feel like mupl is larger. The Racket functions produce
;  mupl expressions that could then be put inside larger mupl expressions or passed to eval-exp. Inmplementing
;  these Racket functions, do not use closure (which is used only internally in eval-exp).
;  Also do not use eval-exp (we are creating a program, not running it)
;(a) Write a Racket function ifaunit that takes three mupl expressions e1, e2, and e3. It returns a
;    mupl expression that when run evaluates e1 and if the result is mupl’s aunit then it evaluates e2
;    and that is the overall result, else it evaluates e3 and that is the overall result.
;    Sample solution: 1 line.
(define (ifaunit e1 e2 e3)
  {ifgreater (int 1) (isaunit e1) e3 e2})

;(b) Write a Racket function mlet* that takes a Racket list of Racket pairs ’((s1 . e1) . . . (si . ei)
;    . . . (sn . en)) and a final mupl expression en+1. In each pair, assume si is a Racket string and
;    ei is a mupl expression. mlet* returns a mupl expression whose value is en+1 evaluated in an
;    environment where each si is a variable bound to the result of evaluating the corresponding ei
;    for 1 ≤ i ≤ n. The bindings are done sequentially, so that each ei is evaluated in an environment
;    where s1 through si-1 have been previously bound to the values e1 through ei-1.
(define (mlet* lstlst e2)
  {if (null? lstlst)
      e2
      (let ([si (car (car lstlst))]
            [ei (cdr (car lstlst))])    
        (mlet si ei (mlet* (cdr lstlst) e2)))})

;(c) Write a Racket function ifeq that takes four mupl expressions e1, e2, e3, and e4 and returns
;    a mupl expression that acts like ifgreater except e3 is evaluated if and only if e1 and e2 are
;    equal integers. Assume none of the arguments to ifeq use the mupl variables _x or _y. Use this
;    assumption so that when an expression returned from ifeq is evaluated, e1 and e2 are evaluated
;    exactly once each.
(define (ifeq e1 e2 e3 e4)
  {mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))})
                   
;; Problem 4: Using the Language
;  We can write mupl expressions directly in Racket using the constructors for
;  the structs and (for convenience) the functions we wrote in the previous problem.
;(a) Bind to the Racket variable mupl-map a mupl function that acts like map (as we used extensively
;    in ML). Your function should be curried: it should take a mupl function and return a mupl
;    function that takes a mupl list and applies the function to every element of the list returning a
;    new mupl list. Recall a mupl list is aunit or a pair where the second component is a mupl list.
(define mupl-map
  {fun #f "f"
       (fun "mapf" "ml"
            (ifaunit (var "ml")
                     (aunit)
                     (apair (call (var "f") (fst (var "ml"))) (call (var "mapf") (snd (var "ml"))))))})
    
;(b) Bind to the Racket variable mupl-mapAddN a mupl function that takes an mupl integer i and
;    returns a mupl function that takes a mupl list of mupl integers and returns a new mupl list of
;    mupl integers that adds i to every element of the list. Use mupl-map (a use of mlet is given to
;    you to make this easier).
(define mupl-mapAddN 
  {mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x"
                                    (add (var "x") (var "i")))))})

               

             

;; Challenge Problem
;  Write a second version of eval-exp (bound to eval-exp-c) that builds closures
;  with smaller environments: When building a closure, it uses an environment that is like the current
;  environment but holds only variables that are free variables in the function part of the closure. (A free
;  variable is a variable that appears in the function without being under some shadowing binding for the
;  same variable.)

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function
;  Avoid computing a function’s free variables more than once. Do this by writing a function compute-free-vars
;  that takes an expression and returns a different expression that uses fun-challenge everywhere in
;  place of fun. The new struct fun-challenge (provided to you; do not change it) has a field freevars
;  to store exactly the set of free variables for the function. Store this set as a Racket set of Racket strings.
;  (Sets are predefined in Racket’s standard library; consult the documentation for useful functions such
;  as set, set-add, set-member?, set-remove, set-union, and any other functions you wish.)
(define (compute-free-vars e)
  (letrec ([eval-free-vars (lambda (expr) ; take a mupl expression and return a set of free vars within the expression, so as to eval fun into fun-challenge
                             (cond [(var? expr)
                                    (set (var-string expr))] ; eagerly put every parameter in the set and delete later in mlet/fun
                                   [(fun? expr)
                                    (let ([funcname (fun-nameopt expr)]
                                          [argname (fun-formal expr)]
                                          [body (fun-body expr)]) ; function's name and arg's name are not free
                                      (set-remove (set-remove (eval-free-vars body) argname) funcname))] 
                                   [(mlet? expr)
                                    (let ([letvar (mlet-var expr)] ; mlet-var name is not free
                                          [letexp (mlet-e expr)]
                                          [letbody (mlet-body expr)])
                                      (set-remove (set-union (eval-free-vars letexp) (eval-free-vars letbody)) letvar))]
                                   [(int? expr) (set)]
                                   [(add? expr)
                                    (let ([a1 (add-e1 expr)]
                                          [a2 (add-e2 expr)])
                                      (set-union (eval-free-vars a1) (eval-free-vars a2)))]
                                   [(ifgreater? expr)
                                    (let ([a1 (ifgreater-e1 expr)] [a2 (ifgreater-e2 expr)]
                                          [a3 (ifgreater-e3 expr)] [a4 (ifgreater-e4 expr)])
                                      (set-union (eval-free-vars a1) (eval-free-vars a2)
                                                 (eval-free-vars a3) (eval-free-vars a4)))]
                                   [(call? expr)
                                    (let ([callfunc (call-funexp expr)] [callact (call-actual expr)])
                                      (set-union (eval-free-vars callfunc) (eval-free-vars callact)))]
                                   [(apair? expr)
                                    (let ([a1 (apair-e1 expr)] [a2 (apair-e2 expr)])
                                      (set-union (eval-free-vars a1) (eval-free-vars a2)))]
                                   [(fst? expr)
                                    (let ([a1 (fst-e expr)])
                                      (eval-free-vars a1))]
                                   [(snd? expr)
                                    (let ([a1 (snd-e expr)])
                                      (eval-free-vars a1))]
                                   [(aunit? expr) (set)]
                                   [(isaunit? expr)
                                    (let ([a1 (isaunit-e expr)])
                                      (eval-free-vars a1))]
                                   ;[(closure? e) (map car (closure-env e))]
                                   [#t (error "MUPL eval-free-vars applied to invalid expression")]))])       
    ; things get messy if the return type of eval-free-vars is not uniform (if it returns fun-challenge at fun, and set at others)...
    ; I did not come up with an idea to get it straight at once
    ; so I split this function into two parts, one entirely for getting the free-vars, and one entirely for substituting fun with fun-challenge
    ; sorry for keep you reading, but you are halfway there :)
    (cond [(fun? e)  ; take a mupl expresion and return an expression with all fun substituted with fun-challenge
           (let ([funcname (fun-nameopt e)]
                 [argname (fun-formal e)]
                 [body (fun-body e)])
             (fun-challenge funcname argname (compute-free-vars body) (eval-free-vars e)))]
          [(mlet? e)
           (let ([letvar (mlet-var e)]
                 [letexp (mlet-e e)]
                 [letbody (mlet-body e)])
             (mlet letvar (compute-free-vars letexp) (compute-free-vars letbody)))]
          [(int? e) e]
          [(var? e) e]
          [(add? e)
           (let ([a1 (add-e1 e)] [a2 (add-e2 e)])
             (add (compute-free-vars a1) (compute-free-vars a2)))]
          [(ifgreater? e)
           (let ([a1 (ifgreater-e1 e)] [a2 (ifgreater-e2 e)]
                                          [a3 (ifgreater-e3 e)] [a4 (ifgreater-e4 e)])
             (ifgreater (compute-free-vars a1) (compute-free-vars a2)
                        (compute-free-vars a3) (compute-free-vars a4)))]
          [(call? e)
           (let ([callfunc (call-funexp e)]
                 [callact (call-actual e)])
             (call (compute-free-vars callfunc) (compute-free-vars callact)))]
          [(apair? e)
           (let ([a1 (apair-e1 e)]
                 [a2 (apair-e2 e)])
             (apair (compute-free-vars a1) (compute-free-vars a2)))]
          [(fst? e)
           (let ([a1 (fst-e e)])
             (fst (compute-free-vars a1)))]
          [(snd? e)
           (let ([a1 (snd-e e)])
             (snd (compute-free-vars a1)))]
          [(aunit? e) e]
          [(isaunit? e)
           (let ([a1 (isaunit-e e)])
               (isaunit (compute-free-vars a1)))]
          [(closure? e) (map car (closure-env e))]
          [#t (error "MUPL compute-free-vars applied to invalid expression")])))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
;  You must have a top-level function compute-free-vars that works as just described — storing the
;  free variables of each function in the freevars field — so the grader can test it directly. Then write a
;  new “main part” of the interpreter that expects the sort of mupl expression that compute-free-vars
;  returns. The case for function definitions is the interesting one.
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e)
         (let ([v (int-num e)])
           (if (integer? v)
               (int v)
               (error "MUPL int applied to non-integer")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if  (> (int-num v1) (int-num v2))
                    (eval-under-env-c (ifgreater-e3 e) env)
                    (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater conditioned with non-number")))]
        [(fun-challenge? e)
         (let* ([s1 (fun-challenge-nameopt e)]
                [s2 (fun-challenge-formal e)]
                [freeargs (fun-challenge-freevars e)]
                [bind (lambda (xs envlst) ; bind the free vars in xs to its corresponding values in envlst
                        (set-map xs (lambda (x) (assoc x envlst))))]
                [openv (bind freeargs env)]) ; the optimized environment
           (if s1 ; not an anonymous function?
               (if (and (string? s1) (string? s2))
                   (closure openv e)
                   (error "MUPL fun applied with non-string"))
               (if (string? s2) ; an anonymous function
                   (closure openv e)
                   (error "MUPL (anonymous) fun applied with non-string"))))]
        [(call? e)
         (letrec ([env-fun (eval-under-env-c (call-funexp e) env)])
           (if (closure? env-fun)
               (letrec ([subenv (closure-env env-fun)]
                        [func (closure-fun env-fun)]
                        [arg (eval-under-env-c (call-actual e) env)]
                        [argname (fun-challenge-formal func)]
                        [funcbody (fun-challenge-body func)]
                        [funcname (fun-challenge-nameopt func)])
                 (let ([subenv (cons (cons argname arg) subenv)])
                   (if funcname ; not anonymous?
                       (eval-under-env-c funcbody (cons (cons funcname (closure subenv func)) subenv))
                       (eval-under-env-c funcbody subenv))))
                 (error "MUPL call applied with non-closure")))]
        [(mlet? e)
         (let ([s (mlet-var e)]
               [v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons s v) env)))] 
        [(apair? e)
         (let([v1 (eval-under-env-c (apair-e1 e) env)]
              [v2 (eval-under-env-c (apair-e2 e) env)])
             (apair v1 v2))]
        [(fst? e)
         (let ([pr (eval-under-env-c (fst-e e) env)])
         (if (apair? pr)
             (apair-e1 pr)
             (error "MUPL fst applied with non-apair")))]
        [(snd? e)
         (let ([pr (eval-under-env-c (snd-e e) env)])
         (if (apair? pr)
             (apair-e2 pr)
             (error "MUPL snd applied with non-apair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([a (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? a)
               (int 1)
               (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))
  

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
