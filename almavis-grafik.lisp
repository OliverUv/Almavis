(in-package #:almavis)

;;; Lite f�rger f�r resten av applikationen att anv�nda sig av

; Allm�nna f�rger
(defparameter app-bg-f�rg (make-rgb-color 0.7 0.7 0.7))
(defparameter dag-bg-f�rg (make-rgb-color 1 1 1)) 
(defparameter bokad-f�rg (make-rgb-color 1 0.5 0)) 
(defparameter �verlapp-f�rg (make-rgb-color 0.55 0 0)) 

; M�nadsvyns f�rger
(in-package #:almavis-m�nad) 
(defparameter dag-bg-f�rg-normal (make-rgb-color 1 1 1))
(defparameter dag-bg-f�rg-full (make-rgb-color 1 0.7 0.2))

; �rsvyns f�rger -- se almavis-ar.cl f�r information om dessa
(in-package #:almavis-�r) 
(defparameter bokat-r�d 1) 
(defparameter bokat-gr�n 0) 
(defparameter bokat-bl� 0) 
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

; M�nadsvyns dimensioner
(in-package #:almavis-m�nad) 
(defparameter px-m�tesbredd 220) 
(defparameter px-m�tesh�jd 16) 
(defparameter px-m�testext-padding 2) 
(defparameter px-kant 5) 
(defparameter px-mellan-m�ten 5) 
(defparameter m�ten-per-dag 6) 
(defparameter dagar-per-rad 4) 
(defun px-dagbredd () (+ (* 2 px-kant) px-m�tesbredd)) 
(defun px-dagh�jd ()
  (+ (* 2 px-kant)
     (* m�ten-per-dag px-m�tesh�jd)
     (* (1- m�ten-per-dag) px-mellan-m�ten))) 

; �rsvyns dimensioner
(in-package #:almavis-�r) 
(defparameter m�nader-per-rad 4) ;;dvs m�nadsrutor per rad
(defparameter dagar-per-rad 8) ;;dvs per rad i m�nadsrutorna
(defparameter rader-per-m�nad (ceiling 31 dagar-per-rad))
(defparameter px-dagh�jd 25) ;;varje dagruta �r s�h�r h�g
(defparameter px-dagbredd 15) ;;varje dagruta �r s�h�r bred
(defparameter px-�verlapp-ruta-bredd 5) 
(defparameter px-�verlapp-ruta-h�jd 5) 
(defparameter px-mellan-dag 3)  ;;avst�ndet mellan dagrutor
(defparameter px-mellan-rad 5)  ;;avst�ndet mellan rader av dagrutor
(defparameter px-m�nads-padding 3) ;;m�ngd bakgrund p� varje sida om dagrutorna


; Allm�nna grafiska funktioner
(in-package #:almavis) 

(defun rita-om ()
  "Ritar om allt, typ som refresh i en browser, du vet."
  (redisplay-frame-panes *application-frame* :force-p t)) 

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
		     (loop for ,var from ,i to (min ,stopp (+ ,i (1- ,per-rad)))
			   do
			   (formatting-cell (,str�m)
					    ,@body)))))) 
