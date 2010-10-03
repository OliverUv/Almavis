(in-package #:almavis-dag)

;; Hack f�r att importera allt i almavis-paketet, eftersom det inte finns
;; n�got s�tt att importera alla symboler (�ven icke-exporterade), och inte
;; heller n�got s�tt att exportera alla symboler p�.
(do-symbols (symb :almavis) (import symb)) 

(defclass dag-view (view) ())
(defparameter +dag-vy+ (make-instance 'dag-view)) 

(defun rita-dag (frame pane)
  "Ritar ut en dag."
  (let*
    ((datah�mtare (slot-value frame 'datah�mtare))
     (plats (slot-value datah�mtare 'plats)) 
     (m�ten-att-visa (datah�mtare->clim-m�ten datah�mtare)))
    (skriv-ut-datak�llor (slot-value frame 'datah�mtare)) 
    (skriv-ut-totala-m�tesl�ngder m�ten-att-visa)
    (terpri)
    (format t "Den ~A~A ~A~%"
	    (plats-dag plats)
	    (if (> 3 (plats-dag plats)) "a" "e") 
	    (plats-m�nad plats))
    (terpri) 
    (formatting-table ;;formatting table �r fulhack h�r f�r att skriva ut
      (pane) 	      ;;allt under info-texten ovan, ist�llet f�r p� den.
      (formatting-row
	(pane)
	(formatting-cell
	  (pane)
	  (cond ((alma-kan-j�mf�r)
		 (rita-ledigheter pane (gemensam-ledighet datah�mtare)))
		((and (alma-kan-ledigt)
		      (= 1 (antal-datak�llor datah�mtare)))
		 (rita-ledigheter
		   pane
		   (ledighet datah�mtare))))
	  (let* 
	    ((px-m�teskol-x (rita-ut-tidslinjer pane))) 
	    (rita-ut-m�ten
	      px-m�teskol-x
	      (partitionera-m�ten m�ten-att-visa)
	      m�ten-att-visa
	      pane)))))))

;;TODO Kan optimeras, flera av v�rdena r�knas ut varje iteration genom
;;loopen en kan ist�llet r�knas ut bara en g�ng innan loopen k�rs.
(defun rita-ut-tidslinjer (str�m)
  "Ritar ut de horisontella linjer som indikerar tider f�r m�tena.
  Returnerar x-koordinaten f�r f�rsta m�teskolumnen"
  (loop
    for i from 0 to 2400 by 100
    for tidstext = (skapa-tidstext i)
    for px-tidstext-l�ngd = (str�ng-px-l�ngd tidstext str�m) 
    with px-text-v�nster-x = px-tidslinje-padding
    for px-linje-v�nster-x = (+ px-tidslinje-padding
			      px-text-v�nster-x
			      px-tidstext-l�ngd)
    for px-linje-h�ger-x = (+ px-linje-v�nster-x px-dagsbredd)
    for px-linje-y = (mappa-position 0 2400 i 0 px-dagsh�jd) 
    for px-tidstext-y = (- px-linje-y (/ px-bokstavsh�jd 2)) 
    do
    (draw-line* str�m
		px-linje-v�nster-x
		px-linje-y
		px-linje-h�ger-x
		px-linje-y
		:line-style (make-line-style :dashes t)) 
    (setf (stream-cursor-position str�m)
	  (values px-text-v�nster-x px-tidstext-y))
    (format str�m "~A" tidstext)
    finally (return px-linje-v�nster-x))) 

(defun partitionera-m�ten (m�ten &optional (resultat nil))
  "Delar in en lista med clim-m�ten i flera listor s� att inga m�ten
  i en underlista �verlappar varandra. Vi delar in m�tena i s� f�
  underlistor som m�jligt. Resultatet har formen: ((clim-m�te*)*)"
  (labels
    ((sortera-in-m�te
       (m�te m�teslista)
       (cond ((null m�teslista) (cons (list m�te) m�teslista))
	     ((finns-�verlapp? m�te (car m�teslista))
	      (cons (car m�teslista) (sortera-in-m�te m�te (cdr m�teslista))))
	     (t (cons (cons m�te (car m�teslista)) (cdr m�teslista))))))
    (if (null m�ten) resultat
      (partitionera-m�ten
	(cdr m�ten) 
	(sortera-in-m�te (car m�ten) resultat))))) 

(defun rita-ut-m�ten (px-kolumn-ett-x partitionerade-m�ten dagens-m�ten str�m)
  "Tar x-koordinaten f�r den f�rsta m�teskolumnen, en lista med
  partitionerade m�ten p� formen ((clim-m�te*)*) och ritar ut
  m�tena p� en str�m."
  (loop for m�tespartition in partitionerade-m�ten
	for i from 0 to (1- max-antal-m�teskolumner)
	for px-kolumn-v�nster-x = (+ px-kolumn-ett-x
				     (* i (+ px-mellan-m�teskol
					     px-m�teskol-bredd)))
	do
	(loop for m�te in m�tespartition
	      do
	      (setf (stream-cursor-position str�m)
		    (values px-kolumn-v�nster-x 0)) 
	      (with-local-coordinates
		(str�m px-kolumn-v�nster-x 0) 
		(present m�te
			 `((clim-m�te) :andra-m�ten ,(remove m�te dagens-m�ten))
			 :view +dag-vy+)))))

(define-presentation-type clim-m�te () :options ((andra-m�ten nil))) 

(define-presentation-method
  present
  (clim-m�te (type clim-m�te) stream (view dag-view) &key)
  (rita-m�te clim-m�te andra-m�ten stream))

(defun rita-m�te (clim-m�te andra-m�ten str�m)
  (with-slots
    (alma-m�te start-kl slut-kl almanacksnamn m�tesinfo) 
    clim-m�te
    (let*
      ((tidsperiod (tidsperioddel alma-m�te))
       (starttid (alma-tp-starttid tidsperiod))
       (sluttid (alma-tp-sluttid tidsperiod))
       (px-start-y (tid-till-position starttid px-dagsh�jd))
       (px-slut-y (tid-till-position sluttid px-dagsh�jd))
       (�verlappande-m�ten (�verlappande-m�ten clim-m�te andra-m�ten))) 
      (with-drawing-options ;;Rita m�tets bakgrund
	(str�m :ink bokad-f�rg)
	(draw-rectangle*
	  str�m
	  0
	  px-start-y
	  px-m�teskol-bredd	
	  px-slut-y))
      (cond ;;Rita eventuellt �verlappsgrejjer
	((null �verlappande-m�ten) nil)
	((alma-kan-�verlapp)
	 (loop
	   for m�te in �verlappande-m�ten
	   do
	   (let* ((�verlapp (m�ten-�verlapp clim-m�te m�te))
		  (starttid (alma-tp-starttid �verlapp))
		  (sluttid (alma-tp-sluttid �verlapp))
		  (px-start (tid-till-position starttid px-dagsh�jd))
		  (px-slut (tid-till-position sluttid px-dagsh�jd))) 
	     (with-drawing-options
	       (str�m :ink �verlapp-f�rg)
	       (draw-rectangle*
		 str�m
		 (- px-m�teskol-bredd px-�verlapp-ruta-bredd) 
		 px-start
		 px-m�teskol-bredd
		 px-slut)))))
	(t (with-drawing-options
	     (str�m :ink �verlapp-f�rg)
	     (draw-rectangle*
	       str�m
	       0 
	       px-start-y
	       px-�verlapp-ruta-bredd
	       (+ px-start-y px-�verlapp-ruta-h�jd))))) 
      (byt-cursor-position str�m :y px-start-y) 
      (skriv-rader str�m
		   px-m�teskol-bredd
		   (- px-slut-y px-start-y)
		   (list m�tesinfo
			 (format nil "~A - ~A  ~A"
				 (skapa-tidstext start-kl)
				 (skapa-tidstext slut-kl)
				 (m�tesl�ngd-str�ng clim-m�te))
			 (format nil "~A" almanacksnamn))))))

(defun rita-ledigheter (str�m clim-ledigheter)
  (mapc
    #'(lambda
	(clim-ledighet)
	(let*
	  ((tidsperiod (slot-value clim-ledighet 'tidsperiod))
	   (starttid (alma-tp-starttid tidsperiod))
	   (sluttid (alma-tp-sluttid tidsperiod))
	   (px-start-y (tid-till-position starttid px-dagsh�jd))
	   (px-slut-y (tid-till-position sluttid px-dagsh�jd))) 
	  (with-drawing-options
	    (str�m :ink ledig-f�rg)
	    (draw-rectangle*
	      str�m
	      0
	      px-start-y
	      (* 2 px-tidslinje-padding) 
	      px-slut-y))))
    clim-ledigheter)) 
