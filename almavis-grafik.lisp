(in-package #:almavis)

(defun stäng-knapp-tryckt (button)
  (accepting-values
   (*query-io* :own-window t) (frame-exit *application-frame*)))

;;Definierar applikationsfönstret
#|(define-application-frame almavis
  () ;Superclasses
  () ;Slots
  (:panes
   (dag :application)
   (månad :application)
   (år :application)
   (kontrollytan
    (vertically ()
                (make-pane 'push-button :label "Stäng"
                           :activate-callback #'stäng-knapp-tryckt)))
   (startup :application :background +red+)
   (layout-pane ;;Hack för att McClim inte klarar att byta layouter
    (horizontally ()
                  (1/5 kontrollytan)
                  (+fill+ startup))))
  (:layouts
   (default (horizontally ()
                  (1/5 kontrollytan)
                  (+fill+ startup)))))|#

#|(define-application-frame almavis
  () ;Superclasses
  () ;Slots
  (:panes
   (kontrollytan
    (vertically ()
                (make-pane 'push-button :label "Stäng"
                           :activate-callback #'stäng-knapp-tryckt)))
   (some-pane :application :background +gray+)) ;;År/Mån/Dag-vy här
  (:layouts
   (default
       (horizontally (:height 700 :width 1000)
                     (1/5 (outlining (:thickness 4) kontrollytan))
                     (+fill+ (spacing (:thickness 4) some-pane))))))|#

(define-application-frame almavis () () ;;går att resiza iaf
  (:panes
   (one :application
        :backgroun +blue+))
  (:layouts
   (default
    (horizontally () (+fill+ one)))))


;;För att starta almavis
(defun visa-grafiskt (årsalmanacka &optional månad dag)
  "Startar den grafiska interfacen för att visualisera almanackor"
  (clim:run-frame-top-level (clim:make-application-frame 'almavis)))


(defun gå-till-månadsvy (clim-månad)
  (format t "Har inte implementerats riktigt ännu. Månaden: ~A~%" clim-månad))

;;;;;; Allmänna funktioner för vyerna ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Bygg-tabell
;; Används för att kämpa mot Clims tabeller.
;;
;; (bygg-tabell (styrvariabel startnum stoppnum per-rad stream) kropp)
;; anropsexempel:
;; (bygg-tabell (i 1 100 10 stream) (prin1 i)) för att få rad- och cell-
;; strukturer för en 10x10 tabell med siffrorna 1-100 i, första raden med
;; siffrorna 1-10
(defmacro bygg-tabell
  ((var start stopp per-rad ström) &body body)
  (let ((i (gensym "inner-loop-var"))) 
    `(loop
     for ,i from ,start to ,stopp by ,per-rad do
     (formatting-row (,ström)
		     (loop for ,var from ,i to (+ ,i (1- ,per-rad)) do
			   (formatting-cell (,ström)
					    ,@body)))))) 

(defmacro for (var from start to end do &body body)
  "Utför body med var bundet till alla tal mellan start och end, inclusive.
   använd loop istället, det är värt att lära sig hur"
  `(let ((,var ,start))
        (dotimes (loop-index (1+ (- ,end ,start)) 'nil)
                 ,@body
                 (setq ,var (+ ,var 1)))))

(defmacro old-bygg-tabell
  ((var start stopp per-rad ström) &rest body)
  (let* ((radstopp (min stopp (+ start (1- per-rad)))))
       `(progn
         (formatting-row
          (,ström)
          (for ,var from ,start to ,radstopp do
               (formatting-cell
                (,ström)
                ,@body)))
         ,(when (< radstopp stopp)
             `(old-bygg-tabell
              (,var ,(1+ radstopp) ,stopp ,per-rad ,ström) ,@body)))))
