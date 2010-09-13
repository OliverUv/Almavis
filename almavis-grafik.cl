(in-package #:almavis)

;;; Lite f�rger f�r resten av applikationen att anv�nda sig av

; Allm�nna f�rger
(defparameter app-bg-f�rg (make-rgb-color 0.6 0.6 0.6))
(defparameter dag-bg-f�rg (make-rgb-color 1 1 1)) 
(defparameter bokad-f�rg (make-rgb-color 1 0.5 0)) 
(defparameter �verlapp-f�rg (make-rgb-color 0.7 0 0)) 

; M�nadsvyns f�rger
(defparameter dag-bg-f�rg-normal (make-rgb-color 1 1 1))
(defparameter dag-bg-f�rg-full (make-rgb-color 1 0.7 0.2))

; �rsvyns f�rger
(defparameter m�rkast-f�rgv�rde 0.2)
(defparameter ljusast-f�rgv�rde 0.85)
(defparameter m�rkare-mellanf�rg
  (+ m�rkast-f�rgv�rde
     (/ (- ljusast-f�rgv�rde
	   m�rkast-f�rgv�rde)
	3)))
(defparameter ljusare-mellanf�rg
  (* 2 m�rkare-mellanf�rg))
(defparameter tom-dag-f�rg (make-rgb-color 1 1 1))
(defparameter ingen-dag-f�rg (make-rgb-color 0 0 0))
(defparameter dagkant-f�rg (make-rgb-color 0 0 0))


(defun st�ng-knapp-tryckt (button)
  (accepting-values
   (*query-io* :own-window t) (frame-exit *application-frame*)))

;;Definierar applikationsf�nstret
#|(define-application-frame almavis
  () ;Superclasses
  () ;Slots
  (:panes
   (dag :application)
   (m�nad :application)
   (�r :application)
   (kontrollytan
    (vertically ()
                (make-pane 'push-button :label "St�ng"
                           :activate-callback #'st�ng-knapp-tryckt)))
   (startup :application :background +red+)
   (layout-pane ;;Hack f�r att McClim inte klarar att byta layouter
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
                (make-pane 'push-button :label "St�ng"
                           :activate-callback #'st�ng-knapp-tryckt)))
   (some-pane :application :background +gray+)) ;;�r/M�n/Dag-vy h�r
  (:layouts
   (default
       (horizontally (:height 700 :width 1000)
                     (1/5 (outlining (:thickness 4) kontrollytan))
                     (+fill+ (spacing (:thickness 4) some-pane))))))|#

#|(define-application-frame almavis () () ;;g�r att resiza iaf
  (:panes
   (one :application
        :backgroun +blue+))
  (:layouts
   (default
    (horizontally () (+fill+ one)))))|#


;;F�r att starta almavis
(defun visa-grafiskt (�rsalmanacka &optional m�nad dag)
  "Startar den grafiska interfacen f�r att visualisera almanackor"
  (clim:run-frame-top-level (clim:make-application-frame 'almavis)))

(defun g�-till-dagsvy (plats)
  'a)  ;TODO

(defun g�-till-m�nadsvy (clim-m�nad)
  (format t "Har inte implementerats riktigt �nnu. M�naden: ~A~%" clim-m�nad))

;;;;;; Allm�nna funktioner f�r vyerna ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Bygg-tabell
;; Anv�nds f�r att k�mpa mot Clims tabeller.
;;
;; (bygg-tabell (styrvariabel startnum stoppnum per-rad stream) kropp)
;; anropsexempel:
;; (bygg-tabell (i 1 100 10 stream) (prin1 i)) f�r att f� rad- och cell-
;; strukturer f�r en 10x10 tabell med siffrorna 1-100 i, f�rsta raden med
;; siffrorna 1-10
;; TODO: Skriver ut fler saker �n stopp s�ger att den borde
(defmacro bygg-tabell
  ((var start stopp per-rad str�m) &body body)
  (let ((i (gensym "inner-loop-var"))) 
    `(loop
     for ,i from ,start to ,stopp by ,per-rad do
     (formatting-row (,str�m)
		     (loop for ,var from ,i to (max ,stopp
						    (+ ,i (1- ,per-rad)))
			   do
			   (formatting-cell (,str�m)
					    ,@body)))))) 
