#lang racket/gui
(require racket/draw)

(define frame (new frame%
                   [label "Leaf Generator"]
                   [width 500]
                   [height 500]))

(define canvas (new canvas%
                   [parent frame]))

(define dc (send canvas get-dc))

(define stem_length (random 50 250))
(define stem_top (round (+ (+ (/ (- 400 stem_length) 2) (/ stem_length 16)) (/ stem_length 5))))
(define stem_bottom (round (+ (+ stem_length stem_top) (/ stem_length 5))))
(define stem_middle (+ (/ (- stem_bottom stem_top) 2) stem_top))

(define available_segments (vector 1 3 4 5)) 
(define segment_amount (vector-ref available_segments (random 0 4)))
(define available_angles (make-vector 6 0))

(if (= segment_amount 1)
    (for/vector ([i (length (vector->list available_angles))])
                (vector-set! available_angles i (- i 7)))
    
    (for/vector ([i (/ (length (vector->list available_angles)) 2)])
                (vector-set! available_angles i (- i 7))
                (for/vector ([i (/ (length (vector->list available_angles)) 2)])
                            (vector-set! available_angles (+ i 3) (+ i 3)))))

(define segment_angle (vector-ref available_angles (random 0 1)))
(define segment_length (round (/ stem_length segment_amount)))
(define segment_width (random 20 100))
(define segment_seperation (random -10 20))

(define segments (make-vector segment_amount 0))
(for/vector ([i segment_amount])
            (vector-set! segments i (+ stem_top (* segment_length i))))

(define leaf_r (random 60 100))
(define leaf_g (random 120 180))
(define leaf_b (random 0 10))

(define leaf_colour (make-object color% leaf_r leaf_g leaf_b))
(define leaf_pen (make-object pen% leaf_colour 2 'solid))

(define (draw_stem dc)
  (let ([stem_colour (make-object color% (- leaf_r 30) (- leaf_g 30) leaf_b)])
  (let ([stem_pen (make-object pen% stem_colour 2 'solid)])
  (send dc set-pen stem_pen)
  (send dc set-brush stem_colour 'solid)
  (for ([i (+ stem_length 25)])
       (send dc draw-rectangle 250 (+ stem_top i) 1 1)
       (send dc draw-rectangle 249 (+ stem_top i) 1 1)
       (sleep/yield 0.005)))))
                       
(define draw_segment_right (λ (start angle end)
  (send dc set-pen leaf_pen)
  (define left_fill (make-vector angle 0))
  (for ([i angle])
       (vector-set! left_fill i (+ 250 (- angle i))))
  (for ([i (reverse (vector->list left_fill))])
       (sleep/yield 0.01)
       (send dc draw-spline 250 start i (- (- (+ (/ (- end start) 2) start) (/ stem_length segment_angle)) (- i 250)) 250 (- end segment_seperation)))))

(define draw_segment_left (λ (start angle end)
  (send dc set-pen leaf_pen)
  (define right_fill (make-vector (- 0 angle) 0))
  (for ([i (- 0 angle)])
       (vector-set! right_fill i (- 250 (- (- 0 angle) i))))
  (for ([i (reverse (vector->list right_fill))])
       (sleep/yield 0.01)
       (send dc draw-spline 250 start i (+ (- (+ (/ (- end start) 2) start) (/ stem_length segment_angle)) (- i 250)) 250 (- end segment_seperation)))))        

(define (draw_leaf dc)
  (for ([i segments])
       (draw_segment_right i segment_width (+ i segment_length))
       (draw_segment_left i (- segment_width (* segment_width 2)) (+ i segment_length))))
       
(send frame show #t)
(sleep/yield 1)
(draw_leaf dc)
(draw_stem dc)
