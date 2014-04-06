;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-boxify) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; CPSC 110 Lab 2, Problem 2

(require 2htdp/image)

;; Image -> Image
;; consumes an image and places a box underneath the image
(check-expect (boxify (circle 10 "solid" "red")) (overlay (circle 10 "solid" "red")
                                                          (rectangle 21 21 "outline" "purple")))

;(define (boxify img) (rectangle 10 10 "outline" "purple")) ;stub

#;(define (boxify img) ;template
  (... x))

(define (boxify img)
  (overlay img
           (rectangle (+ (image-width img) 1) (+ (image-height img) 1) "outline" "purple")))

(boxify (circle 100 "solid" "red"))