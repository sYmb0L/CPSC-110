;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-square) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; CPSC 110 Lab 2, Problem 1

(require 2htdp/image)

;; Image -> Boolean
;; Consumes an image and returns true if the image height is equal to image width
(check-expect (square? (rectangle 10 10 "solid" "blue")) true)
(check-expect (square? (rectangle 20 10 "solid" "blue")) false)
(check-expect (square? (rectangle 10 20 "solid" "blue")) false)

#;(define (square? img) true) ;stub

#;(define (square? img) ;template
  (... x))

(define (square? img)
  (= (image-height img) (image-width img)))