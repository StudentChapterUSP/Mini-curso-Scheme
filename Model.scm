(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))


(define t1 '((( hotdogs)) (and) (pickle) relish))

; (define lat?
; (define member?
; (define firsts
; (define rember (multirember? rember*?)
; (define eqlist?

; (define addtup
; (define tup+
; (define value

; (tup-op
; (rember-f












(define C
  (λ (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (+ n 2)))
         (else (C (add1 (x 3 n)))))))))












(define A
  (λ (n m)
    (cond
      ((zero ? n) (addl m))
      ((zero ? m) (A (subl n) 1))
      (else (A (subl n)
               (A n (subl m )))))))
























(define eternity
  (λ (a)
    (eternity a)))

(lambda (l)
  (cond
    ((null ? l) 0)
    (else (add1 (eternity (cdr l))))))


(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (lengtho (cdr l))))))


















( ( lambda ( length)
     (lambda ( l )
       (cond
         ( ( null? l) 0)
         (else ( add1 ( length ( cdr l)))))))
  eternity)



; Funciona?
( ( (lambda ( mk-length )
      ( mk-length mk-length))
    (lambda ( mk-length)
      (lambda (l)
        (cond
          ( ( null ? l) 0)
          (else ( add1
                  ( ( mk-length eternity)
                    ( cdr l ))))))))
  l)

; E esse?
((lambda ( mk-length)
   ( mk-length mk-length) )
 (lambda ( mk-length)
   ((lambda ( length)
      (lambda (l)
        (cond
          ( ( null ? l) 0)
          (else ( add1 ( length ( cdr l)))))))
    ( mk-length mk-length))))







; Extrair o length
((lambda ( mk-length)
   ( mk-length mk-length ) )
 (lambda ( mk-length)
   ( (lambda ( length)
       (lambda (l)
         (cond
           ( ( null? l) 0)
           (else
            ( add1 ( length ( cdr l ) ) ) ) ) ) )
     (lambda (x)
       ( ( mk-length mk-length) x ) ) ) ) )




;.
;.
;.


( (lambda ( le )
    ((lambda ( mk-length)
       ( mk-length mk-length) )
     (lambda ( mk-length)
       ( le (lambda ( x )
              ( ( mk-length mk-length) x ))))))
  (lambda ( length)
    (lambda (l)
      (cond
        ( ( null? l) 0)
        (else ( add1 ( length ( cdr l ))))))))

;.
;.
;.

; Extrai a função que parece com length
(lambda ( le)
  ((lambda ( mk-length)
     ( mk-length mk-length) )
   (lambda ( mk-length)
     ( le (lambda ( x )
            ( ( mk-length mk-length) x ))))))





(define Y
  (lambda (le)
    ((lambda (f) (f f) )
     (lambda (f)
       ( le (lambda ( x ) ( (f f ) x )))))))



























(define cookies
  (λ ()
    ( bake
      (quote (350 degrees))
      (quote ( 12 minutes))
      ( mix
        (quote (walnuts 1 cup))
        (quote (chocolate-chips 16 ounces))
        ( mix
          ( mix
            (quote (flou r 2 cups))
            (quote (oatmeal 2 cups) )
            (quote (salt .5 teaspoon) )
            (quote (ba king- powder 1 teaspoon))
            (quote (baking-soda 1 teaspoon) ) )
          ( mix
            (quote (eggs 2 large))
            (quote (vanilla 1 teaspoon))
            ( cream
              (quote (butter
                      1
                      cup))
              (quote (sugar 2 cups)) ) ))))))