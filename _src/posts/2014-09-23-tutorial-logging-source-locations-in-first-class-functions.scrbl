#lang scribble/manual

Title: Tutorial: Logging Source Locations in First Class Functions
Date: 2014-09-23T09:10:14
Tags: Racket, macros

Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published.

<!-- more -->

@codeblock{
#lang racket

(require (for-syntax syntax/parse))

(define-syntax (+/logging stx)
  (syntax-parse stx
    [(_ x y)
     #`(begin
         (displayln #,(syntax-source stx)))
         (+ x y)]))
}

@codeblock{
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
}

@codeblock{
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
}

@codeblock{
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
}

@section{Further Reading}

* @hyperlink[FearOfMacros]{Fear of Macros} by @hyperlink[GregHendershott]{Greg Hendershott} is a gentle introduction to Racket macros.

@;* Alternatively, the [Racket Guide][GuideMacros] contains another approach to introducing macros.

@;* The [Racket Documentation][SyntaxParseIntro] also contains an introduction to `syntax-parse`.

@(begin
   (define FearOfMacros "http://www.greghendershott.com/fear-of-macros/index.html")
   (define GregHendershott "http://www.greghendershott.com/")
   (define RacketWebsite "http://www.racket-lang.org")
   (define SyntaxParseIntro "http://docs.racket-lang.org/syntax/stxparse-intro.html")
   (define GuideMacros "http://docs.racket-lang.org/guide/macros.html"))
