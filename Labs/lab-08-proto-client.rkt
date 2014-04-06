;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-08-proto-client) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;
;; The client side of an prototype twt application.
;;
;; In this version there is no editor, each twt is just a single key.
;;


(require 2htdp/image)
(require 2htdp/universe)

;; Constants

(define WIDTH  400)
(define HEIGHT 800)

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define TEXT-X 10)
(define TEXT-Y 10)

(define BLANK (square 0 "solid" "white"))

(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; Data Definitions

;; Message is one of:
;;
;;  (list "twt" STRING)         Client to Server   a new twt
;;
;;  (list "lot" ListOfString)   Server to Client   list of 10 recent twts
;;



(define-struct cs (uname lot))
;; ClientState is (make-cs String ListOfString)
;; interp. the username and the most recent list of twts received from the server
(define CS1 (make-cs "Luna" (list "1")))
(define CS2 (make-cs "Ginny" (list "3" "2" "1")))



;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; Functions:


;; String String -> ClientState
;; The main function for the simple counter client.
;; Starts the client, connecting it to server at server-name.
;; Start the client with something like:
;;   (client "Tom" "LOCALHOST")      if server is running on same machine
;;   (client "Tim" "dog.cat.ubc.ca") if running on another machine
;;
;;   (launch-many-worlds (client "Tom" "LOCALHOST")  to get more than one client
;;                       (client "Tim" "LOCALHOST")) running on local machine
;;
(define (client uname server-name)
  (big-bang (make-cs uname empty)
            (register   server-name)
            (to-draw    render-cs)
            (on-receive handle-msg)
            (on-key     handle-key)))



;; -------------------------------------------------------------------
;; ClientState -> Image
;; place the rendering of client-lot at TEXT-X TEXT-Y

(check-expect (render-cs (make-cs "Tim" (list "2" "1")))
              (place-image/align (above/align "left"
                                              (rectangle (- WIDTH 20)
                                                         (* 2 TEXT-SIZE)
                                                         "outline"
                                                         "black")
                                              (lot-to-image (list "2" "1")))
                                 TEXT-X TEXT-Y
                                 "left" "top"
                                 BACKGROUND))

(define (render-cs cs)
  (place-image/align (above/align "left"
                                  (rectangle (- WIDTH 20)
                                             (* 2 TEXT-SIZE)
                                             "outline"
                                             "black")
                                  (lot-to-image (cs-lot cs)))
                     TEXT-X TEXT-Y
                     "left" "top"
                     BACKGROUND))

;; ListOfString -> Image
;; render the twts one above the other
(check-expect (lot-to-image empty) BLANK)
(check-expect (lot-to-image (list "2" "1"))
              (above/align "left"
                           (txt-to-image "2")
                           (above/align "left"
                                        (txt-to-image "1")
                                        BLANK)))

(define (lot-to-image lot)
  (cond [(empty? lot) BLANK]
        [else
         (above/align "left" 
                      (txt-to-image (first lot))
                      (lot-to-image (rest lot)))]))

;; String -> Image
;; convert the string to an image of the appropriate size and color
(check-expect (txt-to-image "a") (text "a" TEXT-SIZE TEXT-COLOR))

(define (txt-to-image str)
  (text str TEXT-SIZE TEXT-COLOR))

;; -------------------------------------------------------------------

;; ClientState Message -> ClientState
;; Update client state to new list of twts
(check-expect (handle-msg (make-cs  "Tim" (list "1")) (list "lot" (list "2" "1")))
              (make-cs "Tim" (list "2" "1")))

(define (handle-msg cs msg)
  (cond [(string=? "lot" (first msg))
         (make-cs (cs-uname cs) (second msg))]))



;; -------------------------------------------------------------------

;; ClientState KeyEvent -> ClientState or Package
;; when space is pressed, send a twt of the next natural number
;; otherwise cs doesn't change
;; NOTE, we wait for message back from server to update cs-lot

(check-expect (handle-key (make-cs "Tim" (list "21")) "left")   ;nothing happens
              (make-cs "Tim" (list "21")))
(check-expect (handle-key (make-cs "Tim" (list "21")) "a")
              (make-package (make-cs "Tim" (list "21"))
                            (list "twt" "a")))
(check-expect (handle-key (make-cs "Tim" (list "21")) "5")
              (make-package (make-cs "Tim" (list "21"))
                            (list "twt" "5")))

(define (handle-key cs key)
  (cond [(= 1 (string-length key))
         (make-package cs
                       (list "twt" key))]
        [else cs]))



;; -------------------------------------------------------------------
;; -------------------------------------------------------------------

(launch-many-worlds (client "Tim" "LOCALHOST")
                    (client "Tom" "LOCALHOST"))
