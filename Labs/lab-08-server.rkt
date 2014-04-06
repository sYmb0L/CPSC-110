;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-08-server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/universe)

;;
;; The server side of the TWT application.
;;
;;
;; 
;;


;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; Data definitions:

(define-struct ss (iworlds lot))
;;
;; ServerState is...

;; Message is one of:
;;
;;  (list "twt" STRING)         Client to Server   one new twt
;;
;;  (list "lot" ListOfString)   Server to Client   list of all twts
;;


;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; Functions:

;; Any -> ServerState
;; the main function for running the server
;; start the server running on the current computer, the argument is ignored
(define (server ignore)
  (universe (make-ss       empty empty) 
            (on-new        handle-new)
            (on-msg        handle-msg)
            (on-disconnect handle-disconnect)))



;; -------------------------------------------------------------------
;; ServerState IWorld -> Bundle
;; add a Client and send message(s)
(define (handle-new ss iw)
  (make-bundle (make-ss (cons iw (ss-iworlds ss)) (ss-lot ss))
               (list (make-mail iw (list "lot" (ss-lot ss))))
               empty))



;; -------------------------------------------------------------------
;; ServerState IWorld Message -> Bundle
;; add a Client and send message(s)
(define (handle-msg ss iw msg)
  (cond [(string=? (first msg) "twt")
         (local [(define iworlds (ss-iworlds ss))
                 (define old-lot (ss-lot ss))
                 (define new-lot (firstn 20 (cons (second msg)
                                                  old-lot)))]
           (make-bundle (make-ss iworlds new-lot)
                        (mail-for-all iworlds (list "lot" new-lot))
                        empty))]))

;; ListOfIWorld Message -> Mail
;; Make a Mail for each iworld to deliver msg
(define (mail-for-all iworlds msg)
  (cond [(empty? iworlds) empty]
        [else
         (cons (make-mail (first iworlds) msg)
               (mail-for-all (rest iworlds) msg))]))

(define (firstn n lot)
  (cond [(zero? n)    empty]
        [(empty? lot) empty]
        [else
         (cons (first lot)
               (firstn (sub1 n) (rest lot)))]))


;; -------------------------------------------------------------------
;; handle-disconnect: ServerState IWorld Message -> Bundle
(define (handle-disconnect ss iw)
  (make-bundle (make-ss (remove iw (ss-iworlds ss))
                        (ss-lot ss))
               empty
               (list iw)))


(server "")

