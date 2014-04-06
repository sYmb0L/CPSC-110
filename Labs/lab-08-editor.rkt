;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-08-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;23456789012345678901234567890123456789012345678901234567890123456789012345678
;|<--                    78 columns wide for printing                     -->|
; ----------------------------------------------------------------------------
;;
;; A *SIMPLE* one line text editor
;;
;; The screen looks like:
;; 
;;     ab|c
;;
;;   where | is the cursor.
;;
;; Typing characters inserts them.
;; left and right arrow moves cursor
;; delete removes character before cursor
;; enter/return clears line
;;

(require 2htdp/image)
(require 2htdp/universe)

;; Constants

(define WIDTH  400)
(define HEIGHT 800)

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define CURSOR (text "|" 20 "red"))

(define TEXT-X 10)
(define TEXT-Y 10)

(define BLANK (square 0 "solid" "white"))

(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; Data Definitions

(define-struct es (txt cp))
;; EditorState (es) is (make-es String Natural)
;; interp. the current text and cursor position within that text of an editor

(define ES1 (make-es ""      0)) ; empty
(define ES2 (make-es "bbbaa" 3)) ; cursor in midst of text
(define ES3 (make-es "aa"    0)) ; cursor at beginning
(define ES4 (make-es "bb"    2)) ; cursor at end


;; -------------------------------------------------------------------
;; -------------------------------------------------------------------
;; Functions:


;; String -> WorldState
;; the main function for running an editor.
;; start with (editor "") to start editing w/ blank text
;; start with (editor "some text") to start editing w/ some text
(define (editor txt)
  (big-bang (make-es txt (string-length txt))
            (to-draw    render-es)
            (on-key     handle-key)))
 


;; -------------------------------------------------------------------
;; EditorState -> Image
;; place text with cursor at left, middle edge of MTS
(check-expect (render-es (make-es "abc" 1))
              (place-image/align (es-to-image (make-es "abc" 1)) 
                                 TEXT-X TEXT-Y 
                                 "left" "top"
                                 BACKGROUND))

(define (render-es es)
  (place-image/align (es-to-image es) 
                     TEXT-X TEXT-Y 
                     "left" "top" 
                     BACKGROUND))


;; EditorState -> Image
;; produce image of es with CURSOR at appropriate place
(check-expect (es-to-image (make-es "" 0)) CURSOR)
(check-expect (es-to-image (make-es "abc" 2))
              (beside (txt-to-image "ab")
                      CURSOR
                      (txt-to-image "c")))


(define (es-to-image es)
  (beside (text (txt-before-cp es) TEXT-SIZE TEXT-COLOR)
          CURSOR
          (text (txt-after-cp es) TEXT-SIZE TEXT-COLOR)))


;; String -> Image
;; convert a string to an image of the appropriate size and color
(check-expect (txt-to-image "a") (text "a" TEXT-SIZE TEXT-COLOR))

(define (txt-to-image str)
  (text str TEXT-SIZE TEXT-COLOR))


;; -------------------------------------------------------------------

;; EditorState KeyEvent -> EditorState
;; edit text according to keyboard command
(check-expect (handle-key (make-es "ab" 1) "x")      
              (make-es "axb" 2))

(check-expect (handle-key (make-es "ab" 1) "\b")     
              (make-es "b"   0))
(check-expect (handle-key (make-es "ab" 1) "shift")   
              (make-es "ab"  1))

(check-expect (handle-key (make-es "ab" 1) "\r")
              (make-es "" 0))


;; the design rule for key handlers is to template according to the KeyEvent argument
(define (handle-key es key)
  (cond [(key=? key "\b")          (backspace es)]
        [(key=? key "\r")          (make-es "" 0)]
        ;; TODO: right now the "left" and "right" keys do not
        ;;  do anything. But farther down in this file we have
        ;;  defined cursor-left and cursor-right functions, similar to the 
        ;;  backspace and insert functions, that provide the
        ;;  required functionality for these keys.
        ;;  Add two cases to this cond for the "left"
        ;;  and "right" keys. Also add a test for each case.      
        [(key=? key "left")        (cursor-left es)]
        [(key=? key "right")       (cursor-right es)]
        [(= (string-length key) 1) (insert es key)]
        [else es]))


;; EditorState String -> EditorState
;; Insert the 1 character string before the cursor position
(check-expect (insert  (make-es ""   0) "a") (make-es "a"   1))
(check-expect (insert  (make-es "12" 0) "a") (make-es "a12" 1))
(check-expect (insert  (make-es "12" 1) "a") (make-es "1a2" 2))
(check-expect (insert  (make-es "12" 2) "a") (make-es "12a" 3))

(define (insert es ch)
  (make-es (string-append (txt-before-cp es) ch (txt-after-cp es))
           (add1 (es-cp es))))


;; EditorState -> EditorState
;; moves the cursor left/right by 1
(check-expect (cursor-left  (make-es "ab" 0)) (make-es "ab" 0))
(check-expect (cursor-left  (make-es "ab" 1)) (make-es "ab" 0))
(check-expect (cursor-right (make-es "ab" 1)) (make-es "ab" 2))
(check-expect (cursor-right (make-es "ab" 2)) (make-es "ab" 2))

(define (cursor-left es)
  (make-es (es-txt es) 
           (max 0 (sub1 (es-cp es)))))

(define (cursor-right es)
  (make-es (es-txt es)
           (min (string-length (es-txt es)) (add1 (es-cp es)))))



;; EditorState  -> EditorState
;; Delete 1 character before the cursor position
(check-expect (backspace  (make-es "12" 1)) (make-es "2" 0))
(check-expect (backspace  (make-es "12" 2)) (make-es "1" 1))
(check-expect (backspace  (make-es "12" 0)) (make-es "12" 0))

(define (backspace es)
  (if (> (es-cp es) 0)
      (make-es (string-append (substring (es-txt es) 0 (sub1 (es-cp es)))
                              (txt-after-cp es))
               (sub1 (es-cp es)))
      es))



;; EditorState -> String
;; produce the text before the cursor position
(check-expect (txt-before-cp (make-es ""    0)) "")
(check-expect (txt-before-cp (make-es "abc" 0)) "")
(check-expect (txt-before-cp (make-es "abc" 1)) "a")
(check-expect (txt-before-cp (make-es "abc" 3)) "abc")

(define (txt-before-cp es)
  (substring (es-txt es) 0 (es-cp es)))


;; EditorState -> String
;; produce the text after the cursor position
(check-expect (txt-after-cp (make-es ""    0)) "")
(check-expect (txt-after-cp (make-es "abc" 3)) "")
(check-expect (txt-after-cp (make-es "abc" 2)) "c")
(check-expect (txt-after-cp (make-es "abc" 0)) "abc")

(define (txt-after-cp es)
  (substring (es-txt es) (es-cp es)))