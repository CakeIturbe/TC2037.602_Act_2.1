;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Act2.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define fahrenheit-to-celsius
  (lambda (f)
    (/ (* 5 (- f 32)) 9)))

(define sign
  (lambda (n)
    (cond
      [(= n 0) 0]
      [(> n 0) 1]
      [else -1])))

(define roots
  (lambda (a b c)
    (/(+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))

(define bmi
  (lambda (weight height)
   (bmiRange (/ weight (* height height)))))

(define bmiRange
  (lambda (bmiV)
    (cond
      [(< bmiV 20) "underweight"]
      [(and (< bmiV 25) (>= bmiV 20)) "normal"]
      [(and(< bmiV 30) (>= bmiV 25)) "obese1"]
      [(and(< bmiV 40) (>= bmiV 30)) "obese2"]
      [(>= bmiV 40) "obese3"])))

(define factorial
  (lambda (n)
    (cond
      [(= n 0) 1]
      [else (* n (factorial(- n 1)))])))

(define duplicate
  (lambda (lst)
    (cond
      [(eq? lst '()) 0]
      [else (cons (car lst)
                         (duplicate (cdr lst)))])))

(define pow
  (lambda (a b)
    (expt a b)))


(define enlist
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (cons (cons(car lst) '()) (enlist(cdr lst)))])))


(define invert-pairs
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else(cons(reverse(car lst)) (invert-pairs(cdr lst)))])))

(define binario
  (lambda (n)
    (cond
      [( = (quotient n 2) 0) '()]
      [else(reverse(cons (remainder n 2) (binario(quotient n 2))))])))
