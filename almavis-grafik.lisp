(in-package #:almavis)

;;; Lite f�rger f�r resten av applikationen att anv�nda sig av

; Allm�nna f�rger
(defparameter app-bg-f�rg (make-rgb-color 0.7 0.7 0.7))
(defparameter dag-bg-f�rg (make-rgb-color 1 1 1)) 
(defparameter bokad-f�rg (make-rgb-color 1 0.5 0)) 
(defparameter �verlapp-f�rg (make-rgb-color 0.55 0 0)) 

; Dagsvyns f�rger
(in-package #:almavis-dag) 

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

; Allm�nna dimensioner
(in-package #:almavis) 
(defparameter px-bokstavsh�jd 15) 
(defparameter px-�verlapp-ruta-bredd 5) 
(defparameter px-�verlapp-ruta-h�jd 5) 

; Dagsvyns dimensioner
(in-package #:almavis-dag) 
(defparameter px-dagsh�jd 1400)
(defparameter px-dagsbredd 800)
(defparameter px-m�teskol-bredd 220) 
(defparameter px-tidslinje-padding 10) 
(defparameter px-mellan-m�teskol 5) 
(defparameter max-antal-m�teskolumner 
  (floor (- px-dagsbredd (* 2 px-tidslinje-padding))
	 (+ px-m�teskol-bredd px-mellan-m�teskol))) 

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
(defparameter px-mellan-dag 3)  ;;avst�ndet mellan dagrutor
(defparameter px-mellan-rad 5)  ;;avst�ndet mellan rader av dagrutor
(defparameter px-m�nads-padding 3) ;;m�ngd bakgrund p� varje sida om dagrutorna


; Allm�nna funktioner f�r ritning av grafik och relaterat
(in-package #:almavis) 

(defun rita-om ()
  "Ritar om allt, typ som refresh i en browser, du vet."
  (redisplay-frame-panes *application-frame* :force-p t)) 

(defun skriv-ut-totala-m�tesl�ngder (clim-m�teslista) 
  (multiple-value-bind (timmar minuter)
    (truncate (r�kna-ihop-m�tesl�ngder clim-m�teslista) 60)
    (format t "Totalt bokat: ~d timmar, ~d minuter." timmar minuter)
    (terpri))) 

(defun tid-till-position (tidpunkt max-position)
  "Mappar en tidpunkt till ett nummer, s� att nummret �r lika n�ra
  max-position som tiden �r slutet p� dagen. T.ex. 12:00 100 -> 50"
  (let ((antal-minuter
	  (multiple-value-bind (timmar minuter)
	    (truncate tidpunkt 100)
                          (+ (* 60 timmar) minuter))))
   (mappa-position 0 (* 24 60) antal-minuter 0 max-position)))

(defun mappa-position (in-min in-max inv�rde ut-min ut-max)
  "Mappar fr�n en procentuell position i ett intervall till
  en motsvarande position i ett annat intervall, t.ex. s� ger
  9 i intervallet 7-10 v�rdet 13 i intervallet 3-18, eller
  12 i intervallet 10-13. Alla intervall inklusive."
  (check-type in-min integer) 
  (check-type in-max integer) 
  (check-type inv�rde integer) 
  (check-type ut-min integer) 
  (check-type ut-max integer) 
  (assert (< ut-min ut-max)) 
  (assert (< in-min in-max)) 
  (assert (and (<= in-min inv�rde) (<= inv�rde in-max))) 
  (let* ((%-of-interval (/ (- inv�rde in-min)
			  (- in-max in-min))))
    (round
      (+ ut-min (* %-of-interval
		   (- ut-max ut-min))))))

(defun str�ng-px-l�ngd (str�ng str�m)
  (stream-string-width str�m str�ng)) 

(defun f�rkorta-str�ng (orig-str�ng px-max-l�ngd str�m)
  (loop
      for str�ng = orig-str�ng then (subseq str�ng 0 (- (length str�ng) 1))
      until (<= (str�ng-px-l�ngd str�ng str�m) px-max-l�ngd)
      finally (return str�ng))) 

(defun skapa-tidstext (tid)
  (check-type tid integer)
  (format nil "~A~A"
	  (cond ((< tid 10) "000") 
		((< tid 100) "00")
		((< tid 1000) "0")
		(t "")) 
	  tid))

(defun m�tesl�ngd-str�ng (clim-m�te)
  (if (alma-kan-l�ngd-av-tp)
    (multiple-value-bind (timmar minuter)
      (truncate (m�tesl�ngd clim-m�te) 60) 
      (format nil "[~dh ~dm]" timmar minuter)) 
    "[?h ?m]"))

(defun byt-cursor-position (str�m &key x y)
  "Tar cursor-positionen fr�n en str�m (det som avg�r var text skrivs ut)
  och �ndrar dess x- och y-v�rden till det som specifieras,
  om n�got specifieras. Annars l�mnas v�rdena som de �r."
  (multiple-value-bind
    (original-x original-y)
    (stream-cursor-position str�m)
	(setf (stream-cursor-position str�m)
	      (values
		(or x original-x)
		(or y original-y))))) 

(defun skriv-rader (str�m px-max-x px-max-y str�nglista)
  "Skriver ut ett antal textstr�ngar p� individuella rader.
  Ser till att inte skriva ut mer �n som f�r plats p� antalet
  pixlar som angivs av px-max-x och px-max-y."
  (multiple-value-bind 
    (original-x original-y) 
    (stream-cursor-position str�m) 
    (loop
      for str�ng in str�nglista
      for i from 1 to (floor px-max-y px-bokstavsh�jd)
      do
      (format str�m "~A~%" (f�rkorta-str�ng str�ng px-max-x str�m))
      (byt-cursor-position str�m :x original-x))))

;;;; Bygg-tabell
;; Anv�nds f�r att k�mpa mot Clims tabeller.
;;
;; (bygg-tabell (styrvariabel startnum stoppnum per-rad stream) kropp)
;; anropsexempel:
;; (bygg-tabell (i 1 100 10 stream) (prin1 i)) f�r att f� rad- och cell-
;; strukturer f�r en 10x10 tabell med siffrorna 1-100 i, f�rsta raden med
;; siffrorna 1-10
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
