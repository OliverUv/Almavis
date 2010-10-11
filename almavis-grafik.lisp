(in-package #:almavis)

;;; Lite färger för resten av applikationen att använda sig av

; Allmänna färger
(defparameter app-bg-färg (make-rgb-color 0.7 0.7 0.7))
(defparameter dag-bg-färg (make-rgb-color 1 1 1)) 
(defparameter bokad-färg (make-rgb-color 1 0.5 0)) 
(defparameter överlapp-färg (make-rgb-color 0.55 0 0)) 

; Dagsvyns färger
(in-package #:almavis-dag) 
(defparameter ledig-färg (make-rgb-color 0.6 0.9 0.9)) 

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

; Allmänna dimensioner
(in-package #:almavis) 
(defparameter px-bokstavshöjd 15) 
(defparameter px-överlapp-ruta-bredd 5) 
(defparameter px-överlapp-ruta-höjd 5) 

; Dagsvyns dimensioner
(in-package #:almavis-dag) 
(defparameter px-dagshöjd 1800)
(defparameter px-dagsbredd 900)
(defparameter px-möteskol-bredd 220) 
(defparameter px-tidslinje-padding 10) 
(defparameter px-mellan-möteskol 5) 
(defparameter max-antal-möteskolumner 
  (floor (- px-dagsbredd (* 2 px-tidslinje-padding))
	 (+ px-möteskol-bredd px-mellan-möteskol))) 

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
(defparameter px-mellan-dag 3)  ;;avståndet mellan dagrutor
(defparameter px-mellan-rad 5)  ;;avståndet mellan rader av dagrutor
(defparameter px-månads-padding 3) ;;mängd bakgrund på varje sida om dagrutorna


; Allmänna funktioner för ritning av grafik och relaterat
(in-package #:almavis) 

(defun rita-om ()
  "Ritar om allt, typ som refresh i en browser, du vet."
  (redisplay-frame-panes *application-frame* :force-p t)) 

(defun skriv-ut-totala-möteslängder (clim-möteslista) 
  (when (alma-kan-längd-av-tp)
    (multiple-value-bind (timmar minuter)
      (truncate (räkna-ihop-möteslängder clim-möteslista) 60)
      (format t "Totalt bokat: ~d timmar, ~d minuter." timmar minuter)
      (terpri)))) 

(defun tid-till-position (tidpunkt max-position)
  "Mappar en tidpunkt till ett nummer, så att nummret är lika nära
  max-position som tiden är slutet på dagen. T.ex. 12:00 100 -> 50"
  (let ((antal-minuter
	  (multiple-value-bind (timmar minuter)
	    (truncate tidpunkt 100)
                          (+ (* 60 timmar) minuter))))
   (mappa-position 0 (* 24 60) antal-minuter 0 max-position)))

(defun mappa-position (in-min in-max invärde ut-min ut-max)
  "Mappar från en procentuell position i ett intervall till
  en motsvarande position i ett annat intervall, t.ex. så ger
  9 i intervallet 7-10 värdet 13 i intervallet 3-18, eller
  12 i intervallet 10-13. Alla intervall inklusive."
  (check-type in-min integer) 
  (check-type in-max integer) 
  (check-type invärde integer) 
  (check-type ut-min integer) 
  (check-type ut-max integer) 
  (assert (< ut-min ut-max)) 
  (assert (< in-min in-max)) 
  (assert (and (<= in-min invärde) (<= invärde in-max))) 
  (let* ((%-of-interval (/ (- invärde in-min)
			  (- in-max in-min))))
    (round
      (+ ut-min (* %-of-interval
		   (- ut-max ut-min))))))

(defun sträng-px-längd (sträng ström)
  (stream-string-width ström sträng)) 

(defun förkorta-sträng (orig-sträng px-max-längd ström)
  (loop
      for sträng = orig-sträng then (subseq sträng 0 (- (length sträng) 1))
      until (<= (sträng-px-längd sträng ström) px-max-längd)
      finally (return sträng))) 

(defun skapa-tidstext (tid)
  (check-type tid integer)
  (format nil "~A~A"
	  (cond ((< tid 10) "000") 
		((< tid 100) "00")
		((< tid 1000) "0")
		(t "")) 
	  tid))

(defun möteslängd-sträng (clim-möte)
  (if (alma-kan-längd-av-tp)
    (multiple-value-bind (timmar minuter)
      (truncate (möteslängd clim-möte) 60) 
      (format nil "[~dh ~dm]" timmar minuter)) 
    "[?h ?m]"))

(defun byt-cursor-position (ström &key x y)
  "Tar cursor-positionen från en ström (det som avgör var text skrivs ut)
  och ändrar dess x- och y-värden till det som specifieras,
  om något specifieras. Annars lämnas värdena som de är."
  (multiple-value-bind
    (original-x original-y)
    (stream-cursor-position ström)
	(setf (stream-cursor-position ström)
	      (values
		(or x original-x)
		(or y original-y))))) 

(defun skriv-rader (ström px-max-x px-max-y stränglista)
  "Skriver ut ett antal textsträngar på individuella rader.
  Ser till att inte skriva ut mer än som får plats på antalet
  pixlar som angivs av px-max-x och px-max-y."
  (multiple-value-bind 
    (original-x original-y) 
    (stream-cursor-position ström) 
    (loop
      for sträng in stränglista
      for i from 1 to (floor px-max-y px-bokstavshöjd)
      do
      (format ström "~A~%" (förkorta-sträng sträng px-max-x ström))
      (byt-cursor-position ström :x original-x))))

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
		     (loop for ,var from ,i to (min ,stopp (+ ,i (1- ,per-rad)))
			   do
			   (formatting-cell (,ström)
					    ,@body)))))) 
