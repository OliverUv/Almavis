(in-package #:almavis)

;;; Lite färger för resten av applikationen att använda sig av

; Allmänna färger
(defparameter app-bg-färg (make-rgb-color 0.7 0.7 0.7))
(defparameter dag-bg-färg (make-rgb-color 1 1 1)) 
(defparameter bokad-färg (make-rgb-color 1 0.5 0)) 
(defparameter överlapp-färg (make-rgb-color 0.55 0 0)) 

; Månadsvyns färger
(in-package #:almavis-månad) 
(defparameter dag-bg-färg-normal (make-rgb-color 1 1 1))
(defparameter dag-bg-färg-full (make-rgb-color 1 0.7 0.2))

; Årsvyns färger -- se almavis-ar.cl för information om dessa
(in-package #:almavis-år) 
(defparameter bokat-röd 1) 
(defparameter bokat-grön 0) 
(defparameter bokat-blå 0) 
(defparameter mörkast-färgvärde 0.2)
(defparameter ljusast-färgvärde 0.85)
(defparameter mörkare-mellanfärg
  (+ mörkast-färgvärde
     (/ (- ljusast-färgvärde
	   mörkast-färgvärde)
	3)))
(defparameter ljusare-mellanfärg
  (* 2 mörkare-mellanfärg))
(defparameter tom-dag-färg (make-rgb-color 1 1 1))
(defparameter ingen-dag-färg (make-rgb-color 0 0 0))
(defparameter dagkant-färg (make-rgb-color 0 0 0))

; Månadsvyns dimensioner
(in-package #:almavis-månad) 
(defparameter px-mötesbredd 220) 
(defparameter px-möteshöjd 16) 
(defparameter px-mötestext-padding 2) 
(defparameter px-kant 5) 
(defparameter px-mellan-möten 5) 
(defparameter möten-per-dag 6) 
(defparameter dagar-per-rad 4) 
(defun px-dagbredd () (+ (* 2 px-kant) px-mötesbredd)) 
(defun px-daghöjd ()
  (+ (* 2 px-kant)
     (* möten-per-dag px-möteshöjd)
     (* (1- möten-per-dag) px-mellan-möten))) 

; Årsvyns dimensioner
(in-package #:almavis-år) 
(defparameter månader-per-rad 4) ;;dvs månadsrutor per rad
(defparameter dagar-per-rad 8) ;;dvs per rad i månadsrutorna
(defparameter rader-per-månad (ceiling 31 dagar-per-rad))
(defparameter px-daghöjd 25) ;;varje dagruta är såhär hög
(defparameter px-dagbredd 15) ;;varje dagruta är såhär bred
(defparameter px-överlapp-ruta-bredd 5) 
(defparameter px-överlapp-ruta-höjd 5) 
(defparameter px-mellan-dag 3)  ;;avståndet mellan dagrutor
(defparameter px-mellan-rad 5)  ;;avståndet mellan rader av dagrutor
(defparameter px-månads-padding 3) ;;mängd bakgrund på varje sida om dagrutorna


; Allmänna grafiska funktioner
(in-package #:almavis) 

(defun rita-om ()
  "Ritar om allt, typ som refresh i en browser, du vet."
  (redisplay-frame-panes *application-frame* :force-p t)) 

;;;; Bygg-tabell
;; Används för att kämpa mot Clims tabeller.
;;
;; (bygg-tabell (styrvariabel startnum stoppnum per-rad stream) kropp)
;; anropsexempel:
;; (bygg-tabell (i 1 100 10 stream) (prin1 i)) för att få rad- och cell-
;; strukturer för en 10x10 tabell med siffrorna 1-100 i, första raden med
;; siffrorna 1-10
;; TODO: Skriver ut fler saker än stopp säger att den borde
(defmacro bygg-tabell
  ((var start stopp per-rad ström) &body body)
  (let ((i (gensym "inner-loop-var"))) 
    `(loop
     for ,i from ,start to ,stopp by ,per-rad do
     (formatting-row (,ström)
		     (loop for ,var from ,i to (min ,stopp (+ ,i (1- ,per-rad)))
			   do
			   (formatting-cell (,ström)
					    ,@body)))))) 
