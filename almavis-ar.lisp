(in-package #:almavis-år)

;; Hack för att importera allt i almavis-paketet, eftersom det inte finns
;; något sätt att importera alla symboler (även icke-exporterade), och inte
;; heller något sätt att exportera alla symboler på.
(do-symbols (symb :almavis) (import symb)) 

(defclass år-view (view) ())
(defparameter +ars-vy+ (make-instance 'år-view))

;;Denna parameter bestämmer över hur många minuters uppbokning
;;vi skiljer på färgvärden. Är man uppbokad fler minuter än detta
;;värde under en dag så reflekteras det inte. Detta värde talar
;;alltså om efter hur många minuter den mörkaste färgen uppnåts.
(defparameter max-minuter 1440 "antal minuter vi skiljer på färgmässigt")

;;Dessa parametrar specifierar mellan vilka värden R,G eller B
;;får ligga i det RGB-värde som visar information om hur
;;en dag är uppbokad. Ju högre värde, desto ljusare färg.
;;Det är antaget att ljus färg representerar färre bokningar,
;;och mörk färg fler bokningar. Detta används både för att visa
;;olika mängder orangt för bokade dagar, och olika mängder blått
;;för visning av ledighet.
(defun skapa-bokad-färg (variabelt-värde) (make-rgb-color 
					      bokat-röd
					      variabelt-värde
					      bokat-blå))

(defun rita-år (frame pane)
  (let*
    ((årsalmor-att-visa
       (datahämtare->clim-data (slot-value frame 'datahämtare)))
     (årsalma (reduce #'clim-år-union årsalmor-att-visa)))
    (terpri)
    (format t "Svart: Dagen finns ej.~%Vit: Dagen obokad.~%")
    (format t "Gul-orangt: Bokat, ju mörkare färg, desto ~A.~%"
	    (if (alma-kan-längd-av-tp)
	      "längre bokad tid"
	      "fler bokade möten (upp till fyra)"))
    (format t "Rött: Överlappande mötestider.")
    (terpri)
    (terpri)
    (terpri)
    (multiple-value-bind (timmar minuter)
      (truncate (total-möteslängd årsalma) 60)
      (format t "Totalt bokat: ~d timmar, ~d minuter." timmar minuter))
    (terpri)
    (terpri)

    (formatting-table
      (pane :x-spacing '(2 :character) :y-spacing '(1 :line))
      (bygg-tabell
	(i 0 11 4 pane)
	(present (plocka-ut-månad årsalma i)
		 'clim-månad
		 :view +ars-vy+)))))

(define-presentation-method
  present
  (clim-månad (type clim-månad) stream (view år-view) &key)
  (princ ;;Skriv ut månadsnamn och bokad tid
    (format nil "~A: ~A timmar"
	    (slot-value clim-månad 'namn)
	    (truncate (total-möteslängd clim-månad) 60))) 
  (terpri) ;;nyrad efter månadsnamnet
  (with-local-coordinates
    (stream)
    (with-drawing-options ;;rita block så att hela månadsområdet kan klickas
      (stream :ink tom-dag-färg)
      (draw-rectangle* stream 0 0
		       (+ (* 2 px-månads-padding) 
			  (* (1- dagar-per-rad) px-mellan-dag) 
			  (* dagar-per-rad px-dagbredd))
		       (+ (* 2 px-månads-padding)
			  (* (1- rader-per-månad) px-mellan-rad) 
			  (* rader-per-månad px-daghöjd))))
    (with-local-coordinates
      (stream px-månads-padding px-månads-padding)
      (formatting-table ;;En tabell med dagarna i
	(stream :x-spacing `(,px-mellan-dag :pixel) :y-spacing `(,px-mellan-rad :pixel))
	(bygg-tabell (i 0 31 8 stream)
		     (rita-dagruta stream (plocka-ut-dag clim-månad i)))))))

(defun rita-dagruta (stream clim-dag)
  (cond ((null clim-dag) nil)
	(t (rita-dag stream (beräkna-dag-färg clim-dag))
	   (rita-överlapp stream clim-dag)
	   (rita-dagkant stream))))

(defun rita-icke-dag (stream)
  (rita-dag stream ingen-dag-färg))

(defun beräkna-dag-färg (clim-dag)
  (let ((möteslista (slot-value clim-dag 'möten)))
    (if (null möteslista)
      tom-dag-färg ;;färg för tomma dagar
      (bokad-färgintensitet möteslista))))

(defun bokad-färgintensitet (möteslista)
  "Returnerar det färgvärde som en bokad dag ska ha. Om almanackan kan
  räkna ut längden av tidsperioder så kan vi ge en väldigt fingradig
  färgskala, annars väljer vi ett antal steg beroende på antal möten
  istället."
  (if (alma-kan-längd-av-tp)
    (skapa-bokad-färg (mötestid-till-färgvärde
		   (räkna-ihop-möteslängder möteslista)))
    (let ((intensitet (length möteslista)))
      (cond ((< intensitet 1) (skapa-bokad-färg ljusast-färgvärde))
	    ((< intensitet 2) (skapa-bokad-färg ljusare-mellanfärg))
	    ((< intensitet 3) (skapa-bokad-färg mörkare-mellanfärg))
	    (t (skapa-bokad-färg mörkast-färgvärde))))))

(defun mötestid-till-färgvärde (mötesminuter)
  "Returnerar ett färgvärde beroende på antal mötesminuter.
  Värdet ligger mellan mörkast- och ljusast-färgvärde."
  (let ((färgstyrka (/ mötesminuter max-minuter)))
    ;;Färgstyrka ligger mellan 0 och inf
    (cond
      ((>= färgstyrka 1) mörkast-färgvärde)
      ((<= färgstyrka 0) ljusast-färgvärde)
      (t
	(+ mörkast-färgvärde ;;returnera minst det mörkaste
	   (* (- ljusast-färgvärde mörkast-färgvärde)
	      (- 1 färgstyrka))))))) ;;det ska ju bli mörkare för högre styrka

(defun rita-överlapp (stream clim-dag)
  (cond
    ((null clim-dag) nil)
    ((null (slot-value clim-dag 'möten)) nil)
    ((alma-kan-överlapp) (rita-överlappande-tider stream clim-dag))
    ((dag-har-överlapp clim-dag) (rita-dag stream överlapp-färg))))

(defun rita-överlappande-tider (stream clim-dag)
  (kvadratiskc
    #'(lambda
	(clim-möte-a clim-möte-b)
	(let ((överlapp (möten-överlapp clim-möte-a clim-möte-b)))
	  (if (null överlapp) nil
	    (rita-överlapp-tider stream
				 (alma-tp-starttid överlapp)
				 (alma-tp-sluttid överlapp)))))
    (slot-value clim-dag 'möten)))

(defun dag-har-överlapp (clim-dag)
  (labels
    ((my-or (a b) (or a b)))
    (kvadratisk
      #'möten-överlappar
      #'my-or
      '()
      #'my-or
      '()
      (slot-value clim-dag 'möten))))

(defun rita-dag (stream färg)
  (with-drawing-options
    (stream :ink färg)
    (draw-rectangle* stream 0 0 px-dagbredd px-daghöjd)))

(defun rita-dagkant (stream)
  (with-drawing-options
    (stream :ink dagkant-färg)
    (draw-rectangle* stream 0 0 px-dagbredd px-daghöjd :filled nil)))

(defun rita-överlapp-tider (stream starttid sluttid)
  (with-drawing-options
    (stream :ink överlapp-färg)
    (draw-rectangle*
      stream
      0
      (tid-till-position starttid px-daghöjd)
      px-dagbredd
      (tid-till-position sluttid px-daghöjd))))


(define-application-frame
  årtest
  () ;Superclasses
  ((datahämtare :initarg :datahämtare :accessor datahämtare)) ;Slots
  (:panes
    (år :application
	:max-height 700 :height 700
	:max-width 1000 :width 1000
	:background app-bg-färg
	:display-function #'rita-år))
  (:layouts (default år)))

(define-årtest-command com-gå-till-månad
		       ((clim-månad 'clim-månad))
		       (gå-till-månadsvy clim-månad))

(define-presentation-to-command-translator
  gå-till-månad
  (clim-månad com-gå-till-månad årtest
	      :gesture :select
	      :documentation "Gå till månadsvyn.")
  (object)
  (list object))

(defun testa-år (&rest årsalmanacksnamn)
  "Startar den grafiska interfacen för att visualisera almanackor"
  (defvar app (clim:make-application-frame 'årtest))
  (setf (slot-value app 'datahämtare)
	(make-instance 'datahämtare
		       :datakällor årsalmanacksnamn))
  (clim:run-frame-top-level app))
