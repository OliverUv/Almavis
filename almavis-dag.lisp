(in-package #:almavis-månad)

;; Hack för att importera allt i almavis-paketet, eftersom det inte finns
;; något sätt att importera alla symboler (även icke-exporterade), och inte
;; heller något sätt att exportera alla symboler på.
(do-symbols (symb :almavis) (import symb)) 

(defclass månad-view (view) ())
(defparameter +månads-vy+ (make-instance 'månad-view))

(defun rita-månad (frame pane)
  "Ritar ut en månad."
  (let*
    ((datahämtare (slot-value frame 'datahämtare))
     (plats (slot-value datahämtare 'plats)) 
     (möten-att-visa (datahämtare->clim-möten datahämtare)))
    (terpri)
    (format t "~A~%" (plats-månad plats))
    (skriv-ut-datakällor (slot-value frame 'datahämtare)) 
    (if (alma-kan-längd-av-tp)
      (multiple-value-bind (timmar minuter)
	(truncate (räkna-ihop-möteslängder möten-att-visa) 60)
	(format t "Totalt bokat: ~d timmar, ~d minuter." timmar minuter)
	(terpri)))
    (formatting-table
      (pane :x-spacing '(1 :character) :y-spacing '(1 :line))
      (bygg-tabell
	(i 1 (plats-antal-dagar plats) dagar-per-rad pane)
	(rita-månads-dag pane i möten-att-visa plats)))))

(defun rita-månads-dag (ström dagnr månadens-möten plats)
  "Ritar ut en dag. Ritar en ruta per dag, som går att klicka för att komma
  till dagsvyn. I rutan ritas ett antal möten ut.
  Klick på möten tar en till dagen då mötet är planerat.
  Finns fler möten än de som visas så färgas bakgrunden annorlunda."
  (let* ((möten (remove-if #'(lambda (möte)
				     (/= dagnr (slot-value möte 'dag-i-månad)))
			   månadens-möten)))
    (cond ((endp möten)
	   (present plats 'plats :view +månads-vy+)
	   (princ " ")) 
	  ((> (length möten) möten-per-dag)
	   (present plats
		    `((plats) :färg ,dag-bg-färg-full)
		    :view +månads-vy+))
	  (t (present plats 'plats :view +månads-vy+)))
    (loop ;; <3 loop
      for i from 0 below möten-per-dag
      for möte in möten
      with x = px-kant
      for y = (+ px-kant (* (+ px-möteshöjd px-mellan-möten) i)) 
      do
      (setf (stream-cursor-position ström) (values (+ px-mötestext-padding x)
						   (+ px-mötestext-padding y)))
      (with-local-coordinates
	(ström x y)
	(present möte
		 `((clim-möte) :andra-möten ,(remove möte möten)) 
		 :view +månads-vy+)))))

(define-presentation-type plats () :options ((färg dag-bg-färg-normal))) 

(define-presentation-method
  present
  (plats (type plats) stream (view månad-view) &key)
  (with-drawing-options
    (stream :ink färg)
    (draw-rectangle* stream 0 0 (px-dagbredd) (px-daghöjd)))) 

(define-presentation-type clim-möte () :options ((andra-möten nil))) 

(define-presentation-method
  present
  (clim-möte (type clim-möte) stream (view månad-view) &key)
  (let ((överlappande-möten
	  (remove-if-not #'(lambda (annat-möte)
			     (möten-överlappar annat-möte clim-möte))
			 andra-möten))) 
    (cond ((null överlappande-möten) ;;Rita bara mötesbakgrund
	   (with-drawing-options
	     (stream :ink bokad-färg)
	     (draw-rectangle* stream 0 0 px-mötesbredd px-möteshöjd)))
	  ((alma-kan-överlapp) ;;Rita mötesbakgrund med specifika överlapp
	   (with-drawing-options
	     (stream :ink bokad-färg)
	     (draw-rectangle* stream 0 0 px-mötesbredd px-möteshöjd))
	   (loop
	     for möte in överlappande-möten
	     do
	     (let* ((överlapp (möten-överlapp clim-möte möte))
		    (starttid (alma-tp-starttid överlapp))
		    (sluttid (alma-tp-sluttid överlapp))
		    (px-start (tid-till-position starttid px-mötesbredd))
		    (px-slut (tid-till-position sluttid px-mötesbredd))) 
	       (with-drawing-options
		 (stream :ink överlapp-färg)
		 (draw-rectangle* stream px-start 0 px-slut px-möteshöjd))))) 
	  (t (with-drawing-options ;;Rita helt överlappad mötesbakgrund
	       (stream :ink överlapp-färg)
	       (draw-rectangle* stream 0 0 px-mötesbredd px-möteshöjd)))))
  (princ (bygg-mötessträng stream clim-möte px-mötesbredd))) 

#|(defun rita-mötes-ruta (clim-möte ström)
  (cond ((null clim-möte) null)
	(()))) |#

(defun bygg-mötessträng (ström clim-möte max-längd)
  "Returnerar en mötessträng som inte tar upp fler pixlar (på X-axeln) än max-längd."
  (let*
    ((x-orig (stream-cursor-position ström))
     (start (slot-value clim-möte 'start-kl))
     (slut (slot-value clim-möte 'slut-kl))
     (möteslängd (if (alma-kan-längd-av-tp) (möteslängd-sträng clim-möte) ""))
     (mötesinfo (slot-value clim-möte 'mötesinfo)) 
     (orig-sträng (format nil "~A-~A~A ~A"
			  start
			  slut
			  möteslängd
			  mötesinfo)))
    (loop
      for sträng = orig-sträng then (subseq sträng 0 (- (length sträng) 1))
      until (<= (stream-string-width ström sträng) max-längd)
      finally (return sträng)))) 

(defun möteslängd-sträng (clim-möte)
  (if (alma-kan-längd-av-tp)
    (multiple-value-bind (timmar minuter)
      (truncate (möteslängd clim-möte) 60) 
      (format nil "[~dh ~dm]" timmar minuter)) 
    ""))

(defun testa-månad (almanacksnamn månad)
  "Startar den grafiska interfacen för att visualisera månader.
   En månadsidentifierare är en lista med almanacksnamnet och
   månadsnamnet. T.ex. (daniel februari)"
  (defvar app (clim:make-application-frame 'månadstest))
  (setf (slot-value app 'datahämtare)
	(make-instance 'datahämtare
		       :datakällor almanacksnamn
		       :plats (make-instance 'plats :månad månad)))
  (clim:run-frame-top-level app))

(define-application-frame
  månadstest 
  () ;Superclasses
  ((datahämtare :initarg :datahämtare :accessor datahämtare)) ;Slots
  (:panes
    (månad :application
	:max-height 700 :height 700
	:max-width 1000 :width 1000
	:background app-bg-färg
	:display-function #'rita-månad))
  (:layouts (default månad)))

(define-månadstest-command com-gå-till-dag
			   ((plats 'plats)) 
			   (REACTTEST))
			   ;(gå-till-dagsvy plats))

(define-presentation-to-command-translator
  gå-till-dag
  (plats com-gå-till-dag månadstest
	      :gesture :select
	      :documentation "Gå till dagsvyn.")
  (object) (list object))
