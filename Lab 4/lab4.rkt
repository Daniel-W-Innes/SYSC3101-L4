#lang racket

;; SYSC 3101 A Winter 2019 Lab 4

;; Exercise 1

(define (make-upcounter counter)
  (lambda () 
    (set! counter (+ counter 1))
    counter))


;; Exercise 2 

(define (make-counter counter)
  
  (define (count-up) 
    (set! counter (+ counter 1))
    counter)
  
  (define (count-down)
    (if (> counter 0)
        (begin (set! counter (- counter 1))
               counter)
        "Counter is 0"))

  (define (dispatch cmd)
    (cond ((eq? cmd 'inc) count-up)
          ((eq? cmd 'dec) count-down)
          (else (error "Unknown command:" cmd))))
  
  dispatch)


;; Exercise 3

(define (make-counter-with-let initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (dispatch cmd)
      (cond ((eq? cmd 'inc) count-up)
            ((eq? cmd 'dec) count-down)
            (else (error "Unknown command:" cmd))))
 
    dispatch))


;; Exercise 4

(define (make-counter-ex4 initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))
    (lambda(cmd) (cond ((eq? cmd 'inc) (count-up))
            ((eq? cmd 'dec) (count-down))
            (else (error "Unknown command:" cmd))))
    ))


;; Exercise 5

(define (make-counter-ex5 initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (count-get)
      counter)

    (define (count-reset)
      (set! counter 0)
      counter)
    
    (lambda(cmd) (cond ((eq? cmd 'inc) (count-up))
            ((eq? cmd 'dec) (count-down))
            ((eq? cmd 'get) (count-get))
            ((eq? cmd 'reset) (count-reset))
            (else (error "Unknown command:" cmd))))
    ))


;; Exercise 6 - wrong

(define (make-counter-ex6-wrong initial-count step-size)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter step-size))
      counter)
 
    (define (count-down)
      (if (> counter (- step-size 1))
          (begin (set! counter (- counter step-size))
                 counter)
          "Counter is to low"))

    (define (count-get)
      counter)

    (define (count-reset)
      (set! counter 0)
      counter)
    
    (lambda(cmd) (cond ((eq? cmd 'inc) (count-up))
            ((eq? cmd 'dec) (count-down))
            ((eq? cmd 'get) (count-get))
            ((eq? cmd 'reset) (count-reset))
            (else (error "Unknown command:" cmd))))
    ))


;; Exercise 6

(define (make-counter-ex6 initial-count step-size)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter step-size))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (count-get)
      counter)

    (define (count-reset)
      (set! counter 0)
      counter)
    
    (lambda(cmd) (cond ((eq? cmd 'inc) (count-up))
            ((eq? cmd 'dec) (count-down))
            ((eq? cmd 'get) (count-get))
            ((eq? cmd 'reset) (count-reset))
            (else (error "Unknown command:" cmd))))
    ))

;; Exercise 7

(define (make-counter-ex7 initial-count step-size)

  (let* ((counter initial-count) (high-water initial-count))
 
    (define (count-up)
      (set! counter (+ counter step-size))
      (if (> counter high-water)(set! high-water counter)
    high-water)
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (count-get)
      counter)

    (define (count-reset)
      (set! counter 0)
      counter)

    (define (count-max)
      high-water)
    
    (lambda(cmd) (cond ((eq? cmd 'inc) (count-up))
            ((eq? cmd 'dec) (count-down))
            ((eq? cmd 'get) (count-get))
            ((eq? cmd 'reset) (count-reset))
            ((eq? cmd 'max) (count-max))
            (else (error "Unknown command:" cmd))))
    ))