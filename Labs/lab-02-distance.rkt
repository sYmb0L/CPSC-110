;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-distance) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; CPSC 110 Lab 2, Problem 4

;; Number Number Number Number -> Natural
;; Consumes four numbers representing two points and returns the distance between them
(check-within (distance 3 0 0 4) 5 0.001)
(check-within (distance 1 0 0 1) (sqrt 2) 0.001)

#;(define (distance x1 y1 x2 y2) 0);stub

;(define (distance x1 y1 x2 y2) ; template (provided by lab)
;  (... x1 y1 x2 y2))

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- y2 y1))
           (sqr (- x2 x1)))))