(in-package #:almavis-m�nad)

;; Hack f�r att importera allt i almavis-paketet, eftersom det inte finns
;; n�got s�tt att importera alla symboler (�ven icke-exporterade), och inte
;; heller n�got s�tt att exportera alla symboler p�.
(do-symbols (symb :almavis) (import symb)) 

(defclass m�nad-view (view) ())
(defparameter +m�nads-vy+ (make-instance 'm�nad-view))

(defun rita-m�nad (frame pane)
  "Ritar ut en m�nad."
  (let*
    ((datah�mtare (slot-value frame 'datah�mtare))
     (plats (slot-value datah�mtare 'plats)) 
     (m�ten-att-visa (datah�mtare->clim-m�ten datah�mtare)))
    (terpri)
    (format t "~A~%" (plats-m�nad plats))
    (skriv-ut-datak�llor (slot-value frame 'datah�mtare)) 
    (skriv-ut-totala-m�tesl�ngder m�ten-att-visa)
    (formatting-table
      (pane :x-spacing '(1 :character) :y-spacing '(1 :line))
      (bygg-tabell
	(i 1 (plats-antal-dagar plats) dagar-per-rad pane)
	(rita-m�nads-dag
	  pane
	  i
	  m�ten-att-visa
	  (specifiera-plats plats :dag i))))))

(defun rita-m�nads-dag (str�m dagnr m�nadens-m�ten plats)
  "Ritar ut en dag. Ritar en ruta per dag, som g�r att klicka f�r att komma
  till dagsvyn. I rutan ritas ett antal m�ten ut.
  Klick p� m�ten tar en till dagen d� m�tet �r planerat.
  Finns fler m�ten �n de som visas s� f�rgas bakgrunden annorlunda."
  (let* ((m�ten (remove-if #'(lambda (m�te)
				     (/= dagnr (slot-value m�te 'dag-i-m�nad)))
			   m�nadens-m�ten)))
    (cond ((endp m�ten)
	   (present plats 'plats :view +m�nads-vy+)
	   (princ " ")) 
	  ((> (length m�ten) m�ten-per-dag)
	   (present plats
		    `((plats) :f�rg ,dag-bg-f�rg-full)
		    :view +m�nads-vy+))
	  (t (present plats 'plats :view +m�nads-vy+)))
    (loop ;; <3 loop
      for i from 0 below m�ten-per-dag
      for m�te in m�ten
      with x = px-kant
      for y = (+ px-kant (* (+ px-m�tesh�jd px-mellan-m�ten) i)) 
      do
      (setf (stream-cursor-position str�m) (values (+ px-m�testext-padding x)
						   (+ px-m�testext-padding y)))
      (with-local-coordinates
	(str�m x y)
	(present m�te
		 `((clim-m�te) :andra-m�ten ,(remove m�te m�ten)) 
		 :view +m�nads-vy+)))))

(define-presentation-type plats () :options ((f�rg dag-bg-f�rg-normal))) 

(define-presentation-method
  present
  (plats (type plats) stream (view m�nad-view) &key)
  (with-drawing-options
    (stream :ink f�rg)
    (draw-rectangle* stream 0 0 (px-dagbredd) (px-dagh�jd)))) 

(define-presentation-type clim-m�te () :options ((andra-m�ten nil))) 

(define-presentation-method
  present
  (clim-m�te (type clim-m�te) stream (view m�nad-view) &key)
  (let ((�verlappande-m�ten
	  (remove-if-not #'(lambda (annat-m�te)
			     (m�ten-�verlappar annat-m�te clim-m�te))
			 andra-m�ten))) 
    (cond ((null �verlappande-m�ten) ;;Rita bara m�tesbakgrund
	   (with-drawing-options
	     (stream :ink bokad-f�rg)
	     (draw-rectangle* stream 0 0 px-m�tesbredd px-m�tesh�jd)))
	  ((alma-kan-�verlapp) ;;Rita m�tesbakgrund med specifika �verlapp
	   (with-drawing-options
	     (stream :ink bokad-f�rg)
	     (draw-rectangle* stream 0 0 px-m�tesbredd px-m�tesh�jd))
	   (loop
	     for m�te in �verlappande-m�ten
	     do
	     (let* ((�verlapp (m�ten-�verlapp clim-m�te m�te))
		    (starttid (alma-tp-starttid �verlapp))
		    (sluttid (alma-tp-sluttid �verlapp))
		    (px-start (tid-till-position starttid px-m�tesbredd))
		    (px-slut (tid-till-position sluttid px-m�tesbredd))) 
	       (with-drawing-options
		 (stream :ink �verlapp-f�rg)
		 (draw-rectangle* stream px-start 0 px-slut px-m�tesh�jd))))) 
	  (t (with-drawing-options ;;Rita helt �verlappad m�tesbakgrund
	       (stream :ink �verlapp-f�rg)
	       (draw-rectangle* stream 0 0 px-m�tesbredd px-m�tesh�jd)))))
  (princ (bygg-m�tesstr�ng stream clim-m�te px-m�tesbredd))) 

#|(defun rita-m�tes-ruta (clim-m�te str�m)
  (cond ((null clim-m�te) null)
	(()))) |#

(defun bygg-m�tesstr�ng (str�m clim-m�te max-l�ngd)
  "Returnerar en m�tesstr�ng som inte tar upp fler pixlar (p� X-axeln) �n max-l�ngd."
  (let*
    ((x-orig (stream-cursor-position str�m))
     (start (slot-value clim-m�te 'start-kl))
     (slut (slot-value clim-m�te 'slut-kl))
     (m�tesl�ngd (if (alma-kan-l�ngd-av-tp) (m�tesl�ngd-str�ng clim-m�te) ""))
     (m�tesinfo (slot-value clim-m�te 'm�tesinfo)) 
     (orig-str�ng (format nil "~A-~A~A ~A"
			  start
			  slut
			  m�tesl�ngd
			  m�tesinfo)))
    (loop
      for str�ng = orig-str�ng then (subseq str�ng 0 (- (length str�ng) 1))
      until (<= (stream-string-width str�m str�ng) max-l�ngd)
      finally (return str�ng)))) 

(defun m�tesl�ngd-str�ng (clim-m�te)
  (if (alma-kan-l�ngd-av-tp)
    (multiple-value-bind (timmar minuter)
      (truncate (m�tesl�ngd clim-m�te) 60) 
      (format nil "[~dh ~dm]" timmar minuter)) 
    ""))

(defun testa-m�nad (almanacksnamn m�nad)
  "Startar den grafiska interfacen f�r att visualisera m�nader.
   En m�nadsidentifierare �r en lista med almanacksnamnet och
   m�nadsnamnet. T.ex. (daniel februari)"
  (defvar app (clim:make-application-frame 'm�nadstest))
  (setf (slot-value app 'datah�mtare)
	(make-instance 'datah�mtare
		       :datak�llor almanacksnamn
		       :plats (make-instance 'plats :m�nad m�nad)))
  (clim:run-frame-top-level app))

(define-application-frame
  m�nadstest 
  () ;Superclasses
  ((datah�mtare :initarg :datah�mtare :accessor datah�mtare)) ;Slots
  (:panes
    (m�nad :application
	:max-height 700 :height 700
	:max-width 1000 :width 1000
	:background app-bg-f�rg
	:display-function #'rita-m�nad))
  (:layouts (default m�nad)))

(define-m�nadstest-command com-g�-till-dag
			   ((plats 'plats)) 
			   (REACTTEST))
			   ;(g�-till-dagsvy plats))

(define-presentation-to-command-translator
  g�-till-dag
  (plats com-g�-till-dag m�nadstest
	      :gesture :select
	      :documentation "G� till dagsvyn.")
  (object) (list object))
