#lang racket
(require 2htdp/universe)       
(require 2htdp/image)

;; A simple pac-man game

;; Constants:
(define (not-wall? cv)
  (not (string=? "wall" cv)))

(define SCREEN-W 400)
(define SCREEN-H 380)
(define TOCK 0.2)

(define E "empty") 
(define D "dot")   
(define W "wall") 
(define C "cherry")

;; initial board
(define INIT-BOARD
  (vector (vector W W W W W W W W W W W W W)
          (vector W C D D D D D D D D D D W)
          (vector W D W D W W W W W D W D W)
          (vector W D W D W D D D W D W D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W W D W W W D W W D W)
          (vector E D D D D D E D D D D D E)
          (vector W D W W D W W W D W W D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W D W D W D W D W D W)
          (vector W D W D D D W D D D W C W)
          (vector W D W D W D D D W D W D W)
          (vector W D D C W D W D W D D D W)
          (vector W W W W W W W W W W W W W)))

(define PM-SPEED 1)
(define GT-SPEED 0.75)

(define SMALL-BOARD
  (vector (vector E E E)
          (vector E E E)))

(define CELL-SIZE 20)

(define BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref INIT-BOARD 0))))
(define BOARD-HEIGHT (* CELL-SIZE (vector-length INIT-BOARD)))

(define SMALL-BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref SMALL-BOARD 0))))
(define SMALL-BOARD-HEIGHT (* CELL-SIZE (vector-length SMALL-BOARD)))

(define SCORE-HEIGHT    30)
(define SCORE-TEXT-SIZE 20)

(define PM (overlay/align "right" "middle" 
                          (rotate -45 (right-triangle 14 14 "solid" "black")) 
                          (circle 10 "solid" "yellow")))

(define R-PM PM)
(define U-PM (rotate 90 R-PM))
(define L-PM (rotate 180 R-PM))
(define D-PM (rotate 270 R-PM))
(define C-PM (circle 10 "solid" "yellow")) ;close mouth

(define CHERRY (above (rotate -30 (rectangle 1 5 "solid" "green")) 
                      (circle 5 "solid" "red")))

(define GT (circle 10 "solid" "pink"))

(define MTC  (rectangle CELL-SIZE CELL-SIZE "solid" "black")) ; empty cell
(define DTC  (overlay (circle 3  "solid" "white") MTC))       ; dot in cell
(define WALL (rectangle CELL-SIZE CELL-SIZE "solid" "blue"))  ; wall


(define MTB 
  (empty-scene BOARD-WIDTH
               (+ BOARD-HEIGHT (* 2 SCORE-HEIGHT))
               "black"))

(define SMALL-MTB
  (empty-scene SMALL-BOARD-WIDTH
               (+ SMALL-BOARD-HEIGHT SCORE-HEIGHT)))

;; Data definitions:

;; Score is Natural
;; interp. dots eaten by pac-man since start of game

(define INIT-SCORE  0)

;; CellValue is one of:
;; - "empty"
;; - "dot"
;; - "wall"
;; - "cherry"
;; interp. the content of a board cell

;; Direction is one of:
;; - "U"
;; - "D"
;; - "L"
;; - "R"
;; interp. direction that a sprite is facing

(define-struct pos (x y))
;; Pos is (make-pos Natural Natural)
;; interp. a position on the board
(define MAP-LIST
  (local [(define i 0)
          (define j 0)
          (define lst empty)
          (define bd INIT-BOARD)]
    (begin 
      (let loopi()
        (when (< i 14)
          (begin
            (set! bd (vector-ref INIT-BOARD i))
            (let loopj()
              (when (< j 13)
                (begin
                  (if (not-wall? (vector-ref bd j))
                      (set! lst (append lst (list (make-pos j i)) ))
                      void)
                  (set! j (add1 j))
                  (loopj))))
            (set! i (add1 i))
            (set! j 0)
            (loopi))))
      lst)))

(define-struct sprite (x y dir))
;; Sprite is (make-sprite Natural Natural Direction)
;; interp. the position in Board coordinates, and the direction of a sprite
(define INIT-PM (make-sprite 6 6 "U"))

;; Board is (vectorof (vectorof CellValue))
;; interp. the game board
(define RENDER-TEST-BOARD (vector (vector W E)
                                  (vector D E)))

(define-struct ghost (x y dir))
;; Ghost is (make-ghost Natural Natural Direction)
;; interp. a ghost chasing pacman
(define INIT-GT (make-ghost 6 1 "D"))

(define-struct gs (pm board board-image score time gt))
;; GameState is (make-gs Sprite Board Image Score Natural Ghost)
;; interp. all parts of the pac-man game; pac-man, the current
;; board, the current board image, and the current score
;; time is the in seconds after the game starts
;; gt is the ghost

(define MTB-GS (make-gs INIT-PM INIT-BOARD MTB INIT-SCORE 0 INIT-GT))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Testing values:

;; Sprite:
(define R-SPRITE (make-sprite 1 1 "R"))
(define L-SPRITE (make-sprite 1 1 "L"))
(define U-SPRITE (make-sprite 1 1 "U"))
(define D-SPRITE (make-sprite 1 1 "D"))

(define R-GT (make-ghost 2 2 "R"))
;; Board:
(define EE-BOARD (vector (vector W W W W)
                         (vector W E E W)
                         (vector W W W W)))

(define ED-BOARD (vector (vector W W W W)
                         (vector W E D W)
                         (vector W W W W)))

(define DD-BOARD (vector (vector W W W W)
                         (vector W D D W)
                         (vector W W W W)))

;; GameState:
;; MTB-GS previously defined above
(define END-GS (make-gs R-SPRITE EE-BOARD SMALL-MTB 0 0 R-GT))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Functions:


;; -> GameState
;; runs the game
(define (main)
  (local [(define INIT-GS (make-gs INIT-PM
                                   INIT-BOARD
                                   (render-board INIT-BOARD)
                                   INIT-SCORE
                                   0
                                   INIT-GT))]
    (big-bang INIT-GS
              (on-tick tick TOCK)
              (to-draw render)
              (on-key key-handler)
              (stop-when game-over? last-scene))))

;; calculate final score
(define (final-score ttime tscore)
  (- tscore  ttime))

;; display last scene
(define (last-scene gs)
  (cond [(goal? (ghost->pos (gs-gt gs)) (sprite->pos (gs-pm gs)))
         (overlay/align "middle" "middle" 
                        (above (text "GAME OVER. PACMAN GOT EATEN." 20 "red")
                               (text "Thank you for playing!" 20 "white")
                               (text "Total Time:" 20 "aqua")
                               (text (number->string (ceiling (* TOCK (gs-time gs)))) 20 "aqua")
                               (text "Total Score:" 20 "yellow")
                               (text (number->string (gs-score gs)) 20 "yellow")
                               (text "Final Score:" 20 "red")
                               (text (number->string (final-score (ceiling (* TOCK (gs-time gs))) (gs-score gs))) 20 "yellow"))
                        (empty-scene SCREEN-W SCREEN-H "black"))]        
        [else (overlay/align "middle" "middle" 
                             (above (text "PACMAN EATS ALL THE DOTS." 20 "white")
                                    (text "Thank you for playing!" 20 "white")
                                    (text "Total Time:" 20 "aqua")
                                    (text (number->string (ceiling (* TOCK (gs-time gs)))) 20 "aqua")
                                    (text "Total Score:" 20 "yellow")
                                    (text (number->string (gs-score gs)) 20 "yellow")
                                    (text "Final Score:" 20 "red")
                                    (text (number->string (final-score (ceiling (* TOCK (gs-time gs))) (gs-score gs))) 20 "yellow"))
                             (empty-scene SCREEN-W SCREEN-H "black"))]))

;; Pos Pos (listof Pos) -> (listof Pos) or false
;; search for the shortest path between start and goal, false if no path exists on bd


(define (A-star start goal bd)
  (local [;; WorklistEntry is (make-wle Pos Pos Number Number Number)
          ;; interp. the worklist entry used in todo
          ;;     pos is the position , parent is the parent position, 
          ;;     f is f-score, g is g-score, h is h-score
          (define-struct wle (pos parent f g h))
          
          ;; (listof WorklistEntry) -> (listof Pos)
          ;; return the path from the worklist entries
          (define (get-path rsf)
            (if (false? rsf)
                false
                (reverse (map (λ (p) (wle-pos p)) rsf))))
          
          ;; Pos Pos -> Number
          ;; calculate the heuristic score
          (define (h-score p1 p2)
            (+ (abs (- (pos-x p1) (pos-x p2)))
               (abs (- (pos-y p1) (pos-y p2)))))
          
          ;; Initial heuristic score
          (define h0 (h-score start goal))
          
          ;; Pos (listof Pos) -> Boolean
          ;; return true if lop contains pos, false other wise
          ;; can't use the built-in function member?
          (define (pos-member? pos lop)
            (cond [(empty? lop) false]
                  [else
                   (if(goal? pos (first lop))
                      true
                      (pos-member? pos (rest lop)))]))
          
          ;; Pos (listof Pos) -> (listof Pos)
          ;; produce the neighbouring positions of p that are on bd
          (define (neighbour p bd)
            (local [(define x (pos-x p))
                    (define y (pos-y p))
                    ;; determine if n is in the board bd
                    (define (in? n) 
                      (local [(define (in-board? n bd)
                                (cond [(empty? bd) false]
                                      [else
                                       (if (goal? n (first bd))
                                           true
                                           (in-board? n (rest bd)))]))]
                        (in-board? n bd)))]
              (filter in? 
                      (list 
                       (make-pos (add1 x) y)
                       (make-pos (sub1 x) y)
                       (make-pos x (add1 y))
                       (make-pos x (sub1 y))))))
          
          (define (remove-first-n lst n)
            (cond [(or (zero? n) (empty? lst)) lst]
                  [else
                   (remove-first-n (rest lst) (sub1 n))]))
          
          
          ;; Pos Pos Number Number Number (listof WLE) (listof Pos) (listof WLE) -> (listof WLE) or false
          (define (fn-for-pos p parent f g h todo visited rsf)
            ;; can't use equal? for comparison, use goal? instead
            (if (goal? p goal)
                (cons (make-wle goal parent f g h) rsf)
                (if (pos-member? p visited)
                    (fn-for-lop todo visited rsf)                              
                    (fn-for-lop (append (map (λ (n) (local [;; hn is the heuristic score for n
                                                            (define hn (h-score n goal))
                                                            ;; g-score for n
                                                            (define gn (add1 g))
                                                            ;; f = g+ h
                                                            (define fn (+ gn hn))]
                                                      (make-wle n p fn gn hn)))
                                             (begin ;(display (neighbour p bd))
                                               (neighbour p bd)))
                                        todo) 
                                (cons p visited)
                                (if (> (length rsf) g)
                                    (cons (make-wle p parent f g h)
                                          (remove-first-n rsf (- (length rsf) g)))
                                    (cons (make-wle p parent f g h) rsf))))))
          ;; (listof WLE) (listof Pos) (listof WLE) -> false or (listof WLE)
          (define (fn-for-lop todo visited rsf)
            (cond [(empty? todo) false]
                  [else
                   (local [;; (listof WLE) -> (listof WLE)
                           ;; sort the worklist with increasing f-score, if equal then sort by h
                           (define (sort-f lst)
                             (sort 
                              (sort lst
                                    (λ (x y) (<= (wle-f x) (wle-f y))))
                              (λ (x y) (< (wle-h x) (wle-h y)))))
                           (define (sort l comp)
                             (local [(define (sort l)
                                       (cond [(empty? l) empty]
                                             [else
                                              (insert (first l)
                                                      (sort (rest l)))]))
                                     (define (insert i lst)
                                       (cond [(empty? lst) (list i)]
                                             [else
                                              (if (comp i (first lst))
                                                  (cons i lst)
                                                  (cons (first lst) (insert i (rest lst))))]))]
                               (sort l)))
                           ;; the sorted worklist
                           (define sorted-todo (sort-f todo))
                           ;; select the entry with the lowest f-score
                           (define current (first sorted-todo))]
                     (fn-for-pos (wle-pos current)
                                 (wle-parent current)
                                 (wle-f current)
                                 (wle-g current)
                                 (wle-h current)
                                 (rest sorted-todo)
                                 visited
                                 rsf))]))]
    (get-path (fn-for-pos start false h0 0 h0 empty empty empty))))

;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; on-tick handler:


;; GameState -> GameState
;; advances the game

(define (tick gs)
  (local [(define pm          (gs-pm gs))
          (define board       (gs-board gs))
          (define board-image (gs-board-image gs))
          (define score       (gs-score gs))
          (define game-time   (gs-time gs))
          (define ghost        (gs-gt gs))
          (define new-pm          (tick-pm pm board game-time))
          (define new-board       (tick-board board new-pm))
          (define new-board-image (tick-board-image board board-image new-pm))
          (define new-score       (tick-score new-pm board score))
          (define new-time        (tick-time score new-pm board game-time))
          (define new-ghost       (tick-ghost ghost new-pm new-board game-time))]
    
    (make-gs new-pm
             new-board
             new-board-image
             new-score
             new-time
             new-ghost)))

;; Natural Sprite Board Natural -> Natural
;; update the time counter
(define (tick-time score new-pm last-board t)
  (local [(define pos (board-ref last-board (sprite-x new-pm) (sprite-y new-pm)))]
    (cond [(=  score 0.0) 0.0]
          [else
           (ceiling (add1 t))])))

;; Ghost Sprite Board Time -> Ghost
;; update the position and direction of the ghost to chase pacman

(define (tick-ghost ghost pm bd t)
  (if (zero? (modulo t 6))
      ghost
      (if (integer? t)
          (if (goal? (ghost->pos ghost) (sprite->pos pm))
              ghost
              (local [(define p 
                        (local [(define search-result (A-star (ghost->pos ghost) (sprite->pos pm) MAP-LIST))]
                          (if (false? search-result)
                              (ghost->pos ghost)
                              (second search-result))))
                      (define x (pos-x p))
                      (define y (pos-y p))]
                (make-ghost x
                            y
                            (ghost-dir ghost))))
          ghost)))




;; Sprite Board Time -> Sprite
;; updates pac-man's position based on its direction

(define (tick-pm pm bd t)
  (if (and (string=? "L" (sprite-dir pm)) (= (sprite-x pm) 0))
      (make-sprite 12 (sprite-y pm) "L")
      (if (and (string=? "R" (sprite-dir pm)) (= (sprite-x pm) 12))
          (make-sprite 0 (sprite-y pm) "R")
          (make-sprite (checked-move-x (sprite-x pm) (sprite-y pm) (sprite-dir pm) bd)
                       (checked-move-y (sprite-x pm) (sprite-y pm) (sprite-dir pm) bd)
                       (sprite-dir pm)))))


;; Natural Natural Direction Board -> Natural
;; moves x in direction dir, unless it runs into a wall on bd or dir is not in the x direction
;; ASSUMPTION: assumes x, y is at least one cell away from any edge of bd

(define (checked-move-x x y dir bd)
  (cond [(string=? "L" dir) (restrict-move (sub1 x) y x (sub1 x) bd)]
        [(string=? "R" dir) (restrict-move (add1 x) y x (add1 x) bd)]
        [else x]))

;; Natural Natural Direction Board -> Natural
;; moves y in direction dir, unless it runs into a wall on bd or dir is not in the y direction
;; ASSUMPTION: assumes x, y is at least one cell away from any edge of bd


(define (checked-move-y x y dir bd)
  (cond [(string=? "U" dir) (restrict-move x (sub1 y) y (sub1 y) bd)]
        [(string=? "D" dir) (restrict-move x (add1 y) y (add1 y) bd)]
        [else y]))

;; Natural Natural Natural Natural Board -> Natural
;; produces new-coord if bd does not contain a wall at check-x, check-y; otherwise produces old-coord


(define (restrict-move check-x check-y old-coord new-coord bd)
  (if (string=? (board-ref bd check-x check-y) "wall")
      old-coord
      new-coord))

;; Board Sprite -> Board
;; if cell at pacman's position is not empty, make a new board in which it is


(define (tick-board bd pm)
  (if (string=? "empty" (board-ref bd (sprite-x pm) (sprite-y pm)))
      bd
      (new-board-w-empty-at (sprite-x pm) (sprite-y pm) bd)))

;; Number Number Board -> Board
;; produces a new board with the same cell values as bd, except it has "empty" at x, y

(define (new-board-w-empty-at x0 y0 bd)
  (map-board (lambda (x y cv) ; Natural Natural CellValue -> CellValue
               (if (and (= x x0) (= y y0))
                   "empty"
                   cv))
             bd))

;; Board Image Sprite -> Image
;; updates the board image with an empty cell at x, y if pac-man is in a cell with a dot or a cherry


(define (tick-board-image bd board-image pm)
  (local [(define x (sprite-x pm))
          (define y (sprite-y pm))]
    (if (or (string=? (board-ref bd x y) "dot")
            (string=? (board-ref bd x y) "cherry")) ;cherry is added
        (place-cell-image MTC x y board-image)
        board-image)))

;; Sprite Board Score Time -> Score
;; increases by 1 the score if pac-man is now in a cell containing a dot
;; increases by 5 the score if pac-man is now in a cell containing a cherry


(define (tick-score new-pm last-board score)
  (local [(define pos (board-ref last-board (sprite-x new-pm) (sprite-y new-pm)))]
    (cond [(string=? "dot" pos)
           (+ 5 score)]
          [(string=? "cherry" pos)
           (+ 10 score)]
          [else
           score])))

;; Sprite Sprite Board -> Sprite
;; update the position of the ghost sprite


;; Sprite -> Pos
;; take the x and y of a sprite
(define (sprite->pos sp)
  (make-pos (sprite-x sp)
            (sprite-y sp)))

;; Ghost -> Pos
;; get the position of the ghost
(define (ghost->pos gt)
  (make-pos (ghost-x gt)
            (ghost-y gt)))

;; Pos Pos -> Boolean
;; determine if two pos are same
(define (goal? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

(define (upward    p) (make-pos       (pos-x p)  (sub1 (pos-y p))))
(define (downward  p) (make-pos       (pos-x p)  (add1 (pos-y p))))
(define (leftward  p) (make-pos (sub1 (pos-x p))       (pos-y p)))
(define (rightward p) (make-pos (add1 (pos-x p))       (pos-y p)))

;; Board Pos -> (listof Pos)
(define (next-moves m p)
  (filter (λ (p1) 
            (not (solid? m p1)))
          (filter (λ (p2) 
                    (valid? m p2))
                  (list (leftward p)      ;having these four as helpers
                        (rightward p)     ;is optional, the function
                        (upward p)        ;signature would allow inlining
                        (downward p)))))
(define (valid? m p)
  (local [(define s (vector-length m))]
    (and (<= 0 (pos-x p) (add1 s))
         (<= 0 (pos-y p) (add1 s)))))
(define (solid? m p)
  (is-solid? (board-ref m (pos-x p) (pos-y p))))
(define (is-solid? v)
  (cond [(string=? v "empty") false]
        [(string=? v "wall") true]        
        [(string=? v "dot") false]
        [(string=? v "cherry") false]))

;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; on-key handler:


;; GameState KeyEvent -> GameState
;; updates pac-man's direction based on key

(define (key-handler gs key)
  (make-gs (new-dir-pm (gs-pm gs) key)
           (gs-board gs)
           (gs-board-image gs)
           (gs-score gs)
           (gs-time gs)
           #;(new-dir-ghost (gs-gt gs) key)
           (gs-gt gs)))

;; Sprite KeyEvent -> Sprite
;; produces pac-man facing in a new direction based on key


(define (new-dir-pm pm key)
  (cond [(key=? "up"    key) (make-sprite (sprite-x pm) (sprite-y pm) "U")]
        [(key=? "down"  key) (make-sprite (sprite-x pm) (sprite-y pm) "D")]      
        [(key=? "left"  key) (make-sprite (sprite-x pm) (sprite-y pm) "L")]
        [(key=? "right" key) (make-sprite (sprite-x pm) (sprite-y pm) "R")]
        [else pm]))


#;
(define (new-dir-ghost ghost key)
  (cond [(key=? "w" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "U")]
        [(key=? "s" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "D")]      
        [(key=? "a" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "L")]
        [(key=? "d" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "R")]
        [else ghost]))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; stop-when handler:


;; GameState -> Boolean
;; determines if pac-man has eaten all the dots

(define (game-over? gs)
  (or (zero? (count-dots (gs-board gs)))
      (goal? (ghost->pos (gs-gt gs)) (sprite->pos (gs-pm gs)))))


;; Board -> Natural
;; count the number of dots in the board

(define (count-dots bd)
  (foldr-board (lambda (x y cv b) ;Natural Natural CellValue Natural -> Natural
                 (if (string=? "dot" cv)
                     (add1 b)
                     b))
               0
               bd))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; to-draw handler:


;; GameState -> Image
;; draws the game


(define (render gs)
  (overlay/align "middle"
                 "center"
                 (above/align "middle"
                              (text "SIMPLE PACMAN" 20 "yellow")
                              (render-time (ceiling (* 0.25 (gs-time gs)))
                                           (render-ghost (gs-gt gs)
                                                         (render-pm (gs-pm gs)
                                                                    (render-score (gs-score gs)
                                                                                  (gs-board-image gs))
                                                                    (gs-time gs))
                                                         (gs-time gs))))
                 (empty-scene SCREEN-W SCREEN-H "black")))

;; Board -> Image
;; draws the board


(define (render-board bd)
  (foldr-board (lambda (x y cv b)
                 (place-cell-image (cell-image cv) x y b))
               MTB
               bd))

;; Sprite Image Natural -> Image
;; adds pac-man image to img (if time is odd seconds, render closed mouth pacman)


(define (render-pm pm img t)
  (local [(define PM
            (cond [(odd? t) C-PM] 
                  [(string=? "U" (sprite-dir pm)) U-PM]
                  [(string=? "D" (sprite-dir pm)) D-PM]
                  [(string=? "L" (sprite-dir pm)) L-PM]
                  [(string=? "R" (sprite-dir pm)) R-PM]))]
    (place-cell-image PM (sprite-x pm) (sprite-y pm) img)))

;; Sprite Image Natural -> Image
;; render the ghost sprite onto img
(define (render-ghost ghost img t)
  (local [(define GT-IMG
            (cond [(= 1 (modulo t 4)) (circle 10 "solid" "tomato")]
                  [(= 2 (modulo t 4)) (circle 10 "solid" "pink")]
                  [(= 3 (modulo t 4)) (circle 10 "solid" "tomato")]
                  [else
                   GT]))]
    (place-cell-image GT-IMG (ghost-x ghost) (ghost-y ghost) img)))

;; Score Image -> Image
;; adds score to img


(define (render-score score img) 
  (local [(define score-text
            (text (string-append "Score: " (number->string score)) SCORE-TEXT-SIZE "yellow"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ BOARD-HEIGHT (/ SCORE-HEIGHT 2))
                 img)))
(define (render-time score img) 
  (local [(define score-text
            (text (string-append "Time: " (number->string score)) SCORE-TEXT-SIZE "aqua"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ 5 BOARD-HEIGHT SCORE-HEIGHT)
                 img)))

;; CellValue -> Image
;; draws a board cell

(define (cell-image cv)
  (cond [(string=? cv "empty") MTC] 
        [(string=? cv "dot")   DTC]
        [(string=? cv "wall")  WALL]
        [(string=? cv "cherry") CHERRY]))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Operations on Board and other helpers:


;; Board Natural Natural -> CellValue
;; looks up the value of a Board cell


(define (board-ref bd x y)
  (vector-ref (vector-ref bd y) x))

;(board-ref EE-BOARD 1 1)
;(board-ref ED-BOARD 2 1)
;(board-ref DD-BOARD 3 1)

;; (Natural Natural CellValue -> CellValue) Board -> Board
;; the analogue of map for boards, the function is called for
;; each position in the board to produce a cell value for that
;; position in a new resulting board


(define (map-board fn bd)
  (build-vector (vector-length bd)
                (lambda (y)
                  (build-vector (vector-length (vector-ref bd y))
                                (lambda (x)
                                  (fn x y (board-ref bd x y)))))))

;; (Natural Natural CellValue X -> X) X Board -> X
;; the analogue of foldr for boards, the function is called for
;; each position in the board to produce single value


(define (foldr-board fn base bd)
  (local [(define nrows (vector-length bd))
          (define ncols (vector-length (vector-ref bd 0)))
          
          (define (rows y b)
            (cond [(= y nrows) b]
                  [else
                   (rows (add1 y)
                         (cols 0 y b))]))
          (define (cols x y b)
            (cond [(= x ncols) b]
                  [else
                   (cols (add1 x)
                         y
                         (fn x y (board-ref bd x y) b))]))]
    (rows 0 base)))

;; Image Natural Natural Image -> Image
;; adds cell-img to board-image at x, y board coordinates


(define (place-cell-image cell-img x y board-image)
  (place-image cell-img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               board-image))
(main)
