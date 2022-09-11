#lang sicp
(#%require sicp-pict)

;;(define (flipped-pairs painter)
;;  (let ((painter2 
;;         (beside painter 
;;                 (flip-vert painter))))
;;    (below painter2 painter2)))
;
;;(define einstein4 (flipped-pairs einstein))
;
;; (paint einstein4)
;
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))
;
;(define (corner-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter 
;                                (- n 1))))
;        (let ((top-left (beside up up))
;              (bottom-right (below right 
;                                   right))
;              (corner (corner-split painter 
;                                    (- n 1))))
;          (beside (below painter top-left)
;                  (below bottom-right 
;                         corner))))))
;
;;(define (square-limit painter n)
;;  (let ((quarter (corner-split painter n)))
;;    (let ((half (beside (flip-horiz quarter) 
;;                        quarter)))
;;      (below (flip-vert half) half))))
;
;; Exercise 2.44: Define the procedure up-split used by corner-split. It is similar to right-split, except that it switches the roles of below and beside.
;;
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))
;
;(define (square-of-four tl tr bl br)
;  (lambda (painter)
;    (let ((top (beside (tl painter) 
;                       (tr painter)))
;          (bottom (beside (bl painter) 
;                          (br painter))))
;      (below bottom top))))
;
;;(define (flipped-pairs painter)
;;  (let ((combine4 
;;         (square-of-four identity 
;;                         flip-vert
;;                         identity 
;;                         flip-vert)))
;;    (combine4 painter)))
;
;;; or, more sucint:
;
;(define flipped-pairs
;  (square-of-four 
;   identity flip-vert identity flip-vert))
;
;(define (square-limit painter n)
;  (let ((combine4 
;         (square-of-four flip-horiz 
;                         identity
;                         rotate180 
;                         flip-vert)))
;    (combine4 (corner-split painter n))))
;
;; Exercise 2.45: Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating
;
;(define (split proc1 proc2)
;  (define (rec painter n)
;    (if (= n 0)
;      painter
;      (let ((smaller (rec painter 
;                                  (- n 1))))
;        (proc1 painter 
;                (proc2 smaller smaller)))))
;  (lambda (painter n) (rec painter n)))
;
;(define right-split (split beside below))
;(define up-split (split below beside))
;
;;produces procedures right-split and up-split with the same behaviors as the ones already defined.
;
;;(paint (right-split einstein 5))
;
;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect 
;      (scale-vect (xcor-vect v)
;                  (edge1-frame frame))
;      (scale-vect (ycor-vect v)
;                  (edge2-frame frame))))))
;
;;Exercise 2.46: A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:
;
;;(x1,y1)+(x2,y2) = (x1+x2,y1+y2)
;;(x1,y1)-(x2,y2) = (x1-x2,y1-y2)
;;        s*(x,y) = (sx,sy)
;
;(define (add-vect vect1 vect2)
;  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
;             (+ (ycor-vect vect1) (ycor-vect vect2))))
;
;(define (sub-vect vect1 vect2)
;  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
;             (- (ycor-vect vect1) (ycor-vect vect2))))
;
;(define (scale-vect s vect)
;  (make-vect (* s (xcor-vect vect))
;             (* s (ycor-vect vect))))
;;
;(define xcor-vect car)
;(define ycor-vect cdr)
;(define make-vect cons)
;
;;; quick test
;
;;(define vect1 (make-vect 1 2))
;;(define vect2 (make-vect 3 4))
;;
;;(display (add-vect vect1 vect2))
;;(newline)
;;(display (sub-vect vect2 vect1))
;;(newline)
;;(display (scale-vect 3 vect1))
;
;
;;Exercise 2.47: Here are two possible constructors for frames:
;
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;
;(define (make-frame-b origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;
;;For each constructor supply the appropriate selectors to produce an implementation for frames.
;
;; selectors for make-frame
;
;(define origin-frame car)
;(define edge1-frame cadr)
;(define edge2-frame caddr)
;
;; selectors for-make-frame-b
;
;(define origin-frame-b car)
;(define edge1-frame-b cadr)
;(define edge2-frame-b cddr)

;;;;;;;;


;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) 
;         (start-segment segment))
;        ((frame-coord-map frame) 
;         (end-segment segment))))
;     segment-list)))

;Exercise 2.48: A directed line segment in the plane can be represented as a pair of vectors—the vector running from the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from Exercise 2.46 to define a representation for segments with a constructor make-segment and selectors start-segment and end-segment.
;
;(define make-segment cons)
;(define start-segment car)
;(define end-segment cdr)

;Exercise 2.49: Use segments->painter to define the following primitive painters:

;The painter that draws the outline of the designated frame.
;The painter that draws an “X” by connecting opposite corners of the frame.
;The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;The wave painter.

(define outline (segments->painter (list
                                      (make-segment (make-vect 0 0) (make-vect 0 1))
                                      (make-segment (make-vect 0 1) (make-vect 1 1))
                                      (make-segment (make-vect 1 1) (make-vect 1 0))
                                      (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter (segments->painter (list
                              (make-segment (make-vect 0 0) (make-vect 1 1))
                              (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond (segments->painter (list
                                    (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                                    (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                                    (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                                    (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

(define wave (segments->painter (list
                                 ;right arm up
                                 (make-segment (make-vect 0.00 0.74) (make-vect 0.20 0.60))
                                 (make-segment (make-vect 0.20 0.60) (make-vect 0.40 0.70))
                                 (make-segment (make-vect 0.40 0.70) (make-vect 0.45 0.65))
                                 ;right arm down
                                 (make-segment (make-vect 0.00 0.65) (make-vect 0.20 0.50))
                                 (make-segment (make-vect 0.20 0.50) (make-vect 0.35 0.60))
                                 (make-segment (make-vect 0.35 0.60) (make-vect 0.40 0.55))
                                 ;head left
                                 (make-segment (make-vect 0.45 0.65) (make-vect 0.40 0.85))
                                 (make-segment (make-vect 0.40 0.85) (make-vect 0.45 1.00))
                                 ;head right
                                 (make-segment (make-vect 0.55 0.65) (make-vect 0.60 0.85))
                                 (make-segment (make-vect 0.60 0.85) (make-vect 0.55 1.00))
                                 ;left arm up
                                 (make-segment (make-vect 0.55 0.65) (make-vect 0.65 0.70))
                                 (make-segment (make-vect 0.65 0.70) (make-vect 1.00 0.40))
                                 ;left arm down
                                 (make-segment (make-vect 0.60 0.55) (make-vect 0.65 0.60))
                                 (make-segment (make-vect 0.65 0.60) (make-vect 1.00 0.30))
                                 ;lower body right
                                 (make-segment (make-vect 0.25 0.00) (make-vect 0.40 0.55))
                                 ;lower body left
                                 (make-segment (make-vect 0.75 0.00) (make-vect 0.60 0.55))
                                 ;inner thighs
                                 (make-segment (make-vect 0.35 0.00) (make-vect 0.50 0.30))
                                 (make-segment (make-vect 0.50 0.30) (make-vect 0.65 0.00)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (transform-painter 
;         painter origin corner1 corner2)
;  (lambda (frame)
;    (let ((m (frame-coord-map frame)))
;      (let ((new-origin (m origin)))
;        (painter (make-frame new-origin
;                  (sub-vect (m corner1) 
;                            new-origin)
;                  (sub-vect (m corner2)
;                            new-origin)))))))

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2
                                 
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;(define (beside painter1 painter2)
;  (let ((split-point (make-vect 0.5 0.0)))
;    (let ((paint-left  (transform-painter 
;                        painter1
;                        (make-vect 0.0 0.0)
;                        split-point
;                        (make-vect 0.0 1.0)))
;          (paint-right (transform-painter
;                        painter2
;                        split-point
;                        (make-vect 1.0 0.0)
;                        (make-vect 0.5 1.0))))
;      (lambda (frame)
;        (paint-left frame)
;        (paint-right frame)))))

;Exercise 2.50: Define the transformation flip-horiz, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


;Exercise 2.51: Define the below operation for painters. Below takes two painters as arguments. The resulting painter, given a frame, draws with the first painter in the bottom of the frame and with the second painter in the top. Define below in two different ways — first by writing a procedure that is analogous to the beside procedure given above, and again in terms of beside and suitable rotation operations (from Exercise 2.50).

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter
                       painter1
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 0.0)
                       split-point))
          (paint-top (transform-painter
                      painter2
                      split-point
                      (make-vect 1.0 0.5)
                      (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;Exercise 2.52: Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:

;Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).

(define wave-smile (segments->painter (list
                                 ;right arm up
                                 (make-segment (make-vect 0.00 0.74) (make-vect 0.20 0.60))
                                 (make-segment (make-vect 0.20 0.60) (make-vect 0.40 0.70))
                                 (make-segment (make-vect 0.40 0.70) (make-vect 0.45 0.65))
                                 ;right arm down
                                 (make-segment (make-vect 0.00 0.65) (make-vect 0.20 0.50))
                                 (make-segment (make-vect 0.20 0.50) (make-vect 0.35 0.60))
                                 (make-segment (make-vect 0.35 0.60) (make-vect 0.40 0.55))
                                 ;head left
                                 (make-segment (make-vect 0.45 0.65) (make-vect 0.40 0.85))
                                 (make-segment (make-vect 0.40 0.85) (make-vect 0.45 1.00))
                                 ;head right
                                 (make-segment (make-vect 0.55 0.65) (make-vect 0.60 0.85))
                                 (make-segment (make-vect 0.60 0.85) (make-vect 0.55 1.00))
                                 ;left arm up
                                 (make-segment (make-vect 0.55 0.65) (make-vect 0.65 0.70))
                                 (make-segment (make-vect 0.65 0.70) (make-vect 1.00 0.40))
                                 ;left arm down
                                 (make-segment (make-vect 0.60 0.55) (make-vect 0.65 0.60))
                                 (make-segment (make-vect 0.65 0.60) (make-vect 1.00 0.30))
                                 ;lower body right
                                 (make-segment (make-vect 0.25 0.00) (make-vect 0.40 0.55))
                                 ;lower body left
                                 (make-segment (make-vect 0.75 0.00) (make-vect 0.60 0.55))
                                 ;inner thighs
                                 (make-segment (make-vect 0.35 0.00) (make-vect 0.50 0.30))
                                 (make-segment (make-vect 0.50 0.30) (make-vect 0.65 0.00))
                                 ;smile
                                 (make-segment (make-vect 0.46 0.87) (make-vect 0.48 0.86))
                                 (make-segment (make-vect 0.54 0.87) (make-vect 0.52 0.86))
                                 (make-segment (make-vect 0.46 0.80) (make-vect 0.50 0.78))
                                 (make-segment (make-vect 0.50 0.78) (make-vect 0.54 0.80))
                                 )))
(paint wave-smile)

;Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(paint (corner-split wave-smile 5))

(define (corner-split-new painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right 
                         corner)))))

(paint (corner-split-new wave-smile 5))

;Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 5))

(define (square-limit-new painter n)
  (let ((quarter (corner-split-new (rotate180 painter) n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit-new einstein 5))