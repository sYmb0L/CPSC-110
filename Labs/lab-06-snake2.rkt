;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab-06-snake2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; =================
;; =================
;; Constants:

(define BOARD-SIZE 10)   ; The board is 10 cells by 10 cells

(define CELL-PIXELS 14)                         ; cells are square
(define BOARD-WIDTH (* BOARD-SIZE CELL-PIXELS)) ;
(define BOARD-HEIGHT BOARD-WIDTH)               ;

(define GAME-SPEED (/ 1 15))  ; ticks per second

;;
;; Images for the head and body elements of the snake
;; as well as for the food.
;; (NOTE: in the first version you only use HEAD.)

(define HEAD (circle (/ CELL-PIXELS 2) "solid" "green"))
(define BODY (circle (/ CELL-PIXELS 2) "solid" "red"))
(define FOOD (circle (/ CELL-PIXELS 2) "solid" "blue"))


(define MTS (empty-scene BOARD-WIDTH BOARD-HEIGHT))


;; =================
;; =================
;; Data Definitions:


;; Direction is one of: 
;;  - "U"
;;  - "D"
;;  - "L"
;;  - "R"
;; interp. the four directions a snake could travel
;; <Examples redundant for an enumeration>
#;
(define (fn-for-dir d)
  (cond [(string=? d "U") (...)]
        [(string=? d "D") (...)]
        [(string=? d "L") (...)]
        [(string=? d "R") (...)]))


(define-struct cell (c r))   ; c and r stand for column and row
;; Cell is  (make-cell Integer[-1, BOARD-SIZE] Integer[-1, BOARD-SIZE])
;; interp. a cell position on the board from top-left corner
;;         -1 and BOARD-SIZE are on the edges of the board and indicate
;;         "going out of bounds"/game-over condition
(define C1 (make-cell -1 -1))
(define C2 (make-cell -1 BOARD-SIZE))
(define C3 (make-cell BOARD-SIZE -1))
(define C4 (make-cell BOARD-SIZE BOARD-SIZE))
(define C5 (make-cell (/ BOARD-SIZE 2) (/ BOARD-SIZE 2)))
#;
(define (fn-for-cell c)
  (... (cell-c c) (cell-c r)))


;; Body is one of:
;;  - (cons Cell empty)
;;  - (cons Cell Body)
(define B1 (cons (make-cell 1 1) empty))
(define B2 (list (make-cell 2 1)
                 (make-cell 1 1)))
(define B3 (list (make-cell 3 1)
                 (make-cell 2 1)
                 (make-cell 1 1)
                 (make-cell 1 2)))
#;
(define (fn-for-body bd)
  (... (fn-for-cell (first bd))
       (fn-for-body (rest bd))))


(define-struct snake (dir head body)) 
;; Snake is (make-snake Direction Cell Body)
;; interp. a snake with a head moving in some direction
(define S1 (make-snake "U"
                       (make-cell 1 1)
                       (cons (make-cell 1 2) empty)))
(define S2 (make-snake "L"
                       (make-cell 3 2)
                       (list (make-cell 4 2)
                             (make-cell 5 2)
                             (make-cell 5 3))))
(define S3 (make-snake "R"
                       (make-cell 2 1)
                       (list (make-cell 1 1)
                             (make-cell 0 1))))
#;
(define (fn-for-snake sn)
  (... (fn-for-dir (snake-dir sn))
       (fn-for-cell (snake-head sn))
       (fn-for-body (snake-body sn))))


(define-struct game (snake))
;; Game is (make-game Snake) ; later on we will add fields to game
;; interp. the game state with the snake
(define G1 (make-game (make-snake "D"
                                  (make-cell 0 1)
                                  (cons (make-cell 0 0) empty))))
(define G2 (make-game (make-snake "L"
                                  (make-cell 3 2)
                                  (list (make-cell 4 2)
                                        (make-cell 4 3)))))
(define G3 (make-game (make-snake "R"
                                  (make-cell 1 1)
                                  (list (make-cell 1 2)
                                        (make-cell 1 3)))))
#;
(define (fn-for-game gm)
  (... (fn-for-snake (game-snake gm))))


;; =================
;; =================
;; Functions:

;; Game -> Game
;; start the game with (main (make-game (make-snake "R" (make-cell 1 1))))
;; no tests for main

(define (main gm)
  (big-bang gm
            (on-tick move GAME-SPEED) ; Game -> Game
            (to-draw render-game)     ; Game -> Image
            (on-key handle-key)))     ; Game KeyEvent -> Game


;; Game -> Game
;; updates game board to current game state
(check-expect (move (make-game (make-snake "D" (make-cell 1 1))))
              (make-game (make-snake "D" (make-cell 1 2))))

(define (move gm)
  (make-game (move-snake (game-snake gm))))


;; Snake -> Snake
;; moves snake based on snake direction
(check-expect (move-snake (make-snake "U" (make-cell 1 1)))
              (make-snake "U" (make-cell 1 0)))
(check-expect (move-snake (make-snake "D" (make-cell 1 1)))
              (make-snake "D" (make-cell 1 2)))
(check-expect (move-snake (make-snake "L" (make-cell 1 1)))
              (make-snake "L" (make-cell 0 1)))
(check-expect (move-snake (make-snake "R" (make-cell 1 1)))
              (make-snake "R" (make-cell 2 1)))

(define (move-snake sn)
  (make-snake (snake-dir sn)
              (move-cell (snake-head sn) (snake-dir sn))))


;; Cell Direction -> Cell
;; updates cell based on given direction on board
(check-expect (move-cell (make-cell 1 1) "U")
              (make-cell 1 0))
(check-expect (move-cell (make-cell 1 0) "U")
              (make-cell 1 0))
(check-expect (move-cell (make-cell 1 1) "D")
              (make-cell 1 2))
(check-expect (move-cell (make-cell 1 (- BOARD-SIZE 1)) "D")
              (make-cell 1 (- BOARD-SIZE 1)))
(check-expect (move-cell (make-cell 1 1) "L")
              (make-cell 0 1))
(check-expect (move-cell (make-cell 0 1) "L")
              (make-cell 0 1))
(check-expect (move-cell (make-cell 1 1) "R")
              (make-cell 2 1))
(check-expect (move-cell (make-cell (- BOARD-SIZE 1) 1) "R")
              (make-cell (- BOARD-SIZE 1) 1))

(define (move-cell c d)
  (cond [(string=? d "U")
         (if (> (- (cell-r c) 1)
                -1)
             (make-cell (cell-c c)
                        (- (cell-r c) 1))
             c)]
        [(string=? d "D")
         (if (< (+ (cell-r c) 1)
                BOARD-SIZE)
             (make-cell (cell-c c)
                        (+ (cell-r c) 1))
             c)]
        [(string=? d "L")
         (if (> (- (cell-c c) 1)
                -1)
             (make-cell (- (cell-c c) 1)
                        (cell-r c))
             c)]
        [(string=? d "R")
         (if (< (+ (cell-c c) 1)
                BOARD-SIZE)
             (make-cell (+ (cell-c c) 1)
                        (cell-r c))
             c)]))


;; Game -> Image
;; renders all objects on board according to given pixels
(check-expect (render-game (make-game (make-snake "U" (make-cell 3 5))))
              (place-image HEAD
                           (* 3.5 CELL-PIXELS)
                           (* 5.5 CELL-PIXELS)
                           MTS))

(define (render-game gm)
  (render-snake (game-snake gm) MTS))


;; Snake Image -> Image
;; draws snake on img from location of head
(check-expect (render-snake (make-snake "U" (make-cell 1 2)) MTS)
              (place-image HEAD
                           (* 1.5 CELL-PIXELS)
                           (* 2.5 CELL-PIXELS)
                           MTS))

(define (render-snake sn img)
  (place-in-cell HEAD (snake-head sn) img))


;; Game KeyEvent -> Game
;; change snake direction based on key pressed
(check-expect (handle-key (make-game (make-snake "U" (make-cell 1 1))) "up")
              (make-game (make-snake "U" (make-cell 1 1))))
(check-expect (handle-key (make-game (make-snake "D" (make-cell 1 1))) "up")
              (make-game (make-snake "U" (make-cell 1 1))))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 1 1))) "down")
              (make-game (make-snake "D" (make-cell 1 1))))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 1 1))) "left")
              (make-game (make-snake "L" (make-cell 1 1))))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 1 1))) "right")
              (make-game (make-snake "R" (make-cell 1 1))))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 1 1))) "shift")
              (make-game (make-snake "U" (make-cell 1 1))))

(define (handle-key gm ke)
  (make-game (change-dir (game-snake gm) ke)))


;; Snake KeyEvent -> Snake
;; changes direction of snake head based on given direction
(check-expect (change-dir (make-snake "U" (make-cell 1 1)) "up")
              (make-snake "U" (make-cell 1 1)))
(check-expect (change-dir (make-snake "U" (make-cell 1 1)) "down")
              (make-snake "D" (make-cell 1 1)))
(check-expect (change-dir (make-snake "U" (make-cell 1 1)) "left")
              (make-snake "L" (make-cell 1 1)))
(check-expect (change-dir (make-snake "U" (make-cell 1 1)) "right")
              (make-snake "R" (make-cell 1 1)))
(check-expect (change-dir (make-snake "D" (make-cell 1 1)) "right")
              (make-snake "R" (make-cell 1 1)))

(define (change-dir sn ke)
  (make-snake (get-direction ke (snake-dir sn))
              (snake-head sn)))


;; KeyEvent Direction -> Direction
;; if KeyEvent is one of up/down/left/right, produces new direction
;; produces original direction otherwise
(check-expect (get-direction "up" "D") "U")
(check-expect (get-direction "down" "L") "D")
(check-expect (get-direction "left" "R") "L")
(check-expect (get-direction "right" "U") "R")
(check-expect (get-direction "shift" "U") "U")

(define (get-direction ke dir)
  (cond [(key=? ke "up") "U"]
        [(key=? ke "down") "D"]
        [(key=? ke "left") "L"]
        [(key=? ke "right") "R"]
        [else dir]))


;; Image Cell Image -> Image
;; place img in cell c given the board scene scn
(check-expect (place-in-cell HEAD (make-cell 5 6) MTS)
              (place-image HEAD
                           (* 5.5 CELL-PIXELS)
                           (* 6.5 CELL-PIXELS)
                           MTS))

(define (place-in-cell img c scn)
  (place-image img 
               (+ (* (cell-c c) CELL-PIXELS) (/ CELL-PIXELS 2))
               (+ (* (cell-r c) CELL-PIXELS) (/ CELL-PIXELS 2))
               scn))




