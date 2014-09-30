    Title: Tutorial: Logging Source Locations in First Class Functions
    Date: 2014-09-22T09:01:26
    Tags: Racket, macros, DRAFT

Replace this with your post text. Add one or more comma-separated
Tags above. The special tag DRAFT will prevent the post from being
published.

<!-- more -->

```racket
#lang racket

(require (for-syntax syntax/parse))

(define-syntax (+/logging stx)
  (syntax-parse stx
    [(_ x y)
     #`(begin
         (displayln #,(syntax-source stx)))
         (+ x y)]))
```

```racket
#lang racket

(require syntax-quote
         (for-syntax syntax/parse))

(define (+/logging* stx x y)
  (displayln (syntax-source stx))
  (+ x y))

(define-syntax (+/logging stx)
  (syntax-parse stx
    [(_ x y)
     #`(+/logging* (quote-syntax/keep-srcloc #,stx) x y)]
    [s:id
     #`(λ (x y) (+/logging*
                 (quote-syntax/keepsrcloc #,stx) x y))]))
```

```racket
#lang racket

(require syntax-quote
         (for-syntax syntax/parse))

(define (+/logging* stx . args)
  (displayln (syntax-source stx))
  (apply + args))

(define-syntax (+/logging stx)
  (syntax-parse stx
    [(_ . args)
     #`(*/logging*
        (quote-syntax/keep-srcloc #,stx)
        . args)]
    [s:id
     #`(λ x (apply +/logging*
                   (quote-syntax/keep-srcloc #,stx)
                   x))]))
```

```racket
#lang racket

(require syntax-quote
         (for-syntax syntax/parse))

(define-syntax (define/source stx)
  (syntax-parse stx
    [(_ (f:id . args:args) body ...)
     #'(begin
         (define (f* . args) body ...)
         (define-syntax (f stx*)
           (syntax-parse stx*
             [(_ args* (... ...))
              #`(f* (quote-syntax/keep-srcloc #,stx*)
                    args* (... ...))]
             [s:id
              #`(λ args*
                  (apply f*
                         (quote-syntax/keep-srcloc #,stx*)
                         args*))])))]))
```

## Further Reading

* [Fear of macros][FearOfMacros] by [Greg Hendershott][GregHendershott] is a gentle introduction to Racket macros.

* Alternatively, the [Racket Guide][GuideMacros] contains another approach to introducing macros.

* The [Racket Documentation][SyntaxParseIntro] also contains an introduction to `syntax-parse`.

[FearOfMacros]: http://www.greghendershott.com/fear-of-macros/index.html
[GregHendershott]: http://www.greghendershott.com/
[Racket]: http://www.racket-lang.org
[SyntaxParseIntro]: http://docs.racket-lang.org/syntax/stxparse-intro.html
[GuideMacros]: http://docs.racket-lang.org/guide/macros.html
