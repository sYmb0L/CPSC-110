;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-pluralize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; CPSC 110 Lab 2, Problem 3

;; String -> String
;; Consumes a string and appends "s" to the end, unless the string already ends in "s"
(check-expect (pluralize "foo") "foos")
(check-expect (pluralize "bars") "bars")

#;(define (pluralize str) str) ;stub

#;(define (pluralize str) ;template
  (... str))

(define (pluralize str)
  (if (string=? (substring str (- (string-length str) 1) (string-length str)) "s")
      str
      (string-append str "s")))