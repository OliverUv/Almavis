(in-package #:almavis)

(defclass �r-view (view) ())
(defparameter +ars-vy+ (make-instance '�r-view))

;;Denna parameter best�mmer �ver hur m�nga minuters uppbokning
;;vi skiljer p� f�rgv�rden. �r man uppbokad fler minuter �n detta
;;v�rde under en dag s� reflekteras det inte. Detta v�rde talar
;;allts� om efter hur m�nga minuter den m�rkaste f�rgen uppn�ts.
(defparameter max-minuter 1440 "antal minuter vi skiljer p� f�rgm�ssigt")

;;Dessa parametrar specifierar mellan vilka v�rden R,G eller B
;;f�r ligga i det RGB-v�rde som visar information om hur
;;en dag �r uppbokad. Ju h�gre v�rde, desto ljusare f�rg.
;;Det �r antaget att ljus f�rg representerar f�rre bokningar,
;;och m�rk f�rg fler bokningar. Detta anv�nds b�de f�r att visa
;;olika m�ngder orangt f�r bokade dagar, och olika m�ngder bl�tt
;;f�r visning av ledighet.
(defun skapa-bokad-f�rg (gr�n-v�rde) (make-rgb-color 1 gr�n-v�rde 0))

(defparameter px-dagh�jd 25)
(defparameter px-dagbredd 15)

(defun rita-�r (frame pane)
  (let*
    ((�rsalmor-att-visa
       (datah�mtare->clim-data (slot-value frame 'datah�mtare)))
     (�rsalma (reduce #'clim-�r-union �rsalmor-att-visa)))
    (terpri)
    (format t "Svart: Dagen finns ej.~%Vit: Dagen obokad.~%")
    (format t "Gul-orangt: Bokat, ju m�rkare f�rg, desto ~A.~%"
	    (if (alma-kan-l�ngd-av-tp)
	      "l�ngre bokad tid"
	      "fler bokade m�ten (upp till fyra)"))
    (format t "R�tt: �verlappande m�testider.")
    (terpri)
    (terpri)
    (terpri)
    (multiple-value-bind (timmar minuter)
      (truncate (total-m�tesl�ngd �rsalma) 60)
      (format t "Totalt bokat: ~d timmar, ~d minuter." timmar minuter))
    (terpri)
    (terpri)

    (formatting-table
      (pane :x-spacing '(5 :character) :y-spacing '(3 :line))
      (bygg-tabell
	(i 0 11 4 pane)
	(present (plocka-ut-m�nad �rsalma i)
		 'clim-m�nad
		 :view +ars-vy+)))))

(define-presentation-method
  present
  (clim-m�nad (type clim-m�nad) stream (view �r-view) &key)
  (with-drawing-options ;;rita block s� att hela m�nadsomr�det kan klickas
    (stream :ink app-bg-f�rg)
    (draw-rectangle* stream 0 0
		     (+ 73 (* 8 px-dagbredd))
		     (+ 30 (* 4 px-dagh�jd))))
  (princ ;;Skriv ut m�nadsnamn och bokad tid
    (format nil "~A: ~A timmar"
	    (slot-value clim-m�nad 'namn)
	    (/ (total-m�tesl�ngd clim-m�nad) 60))) 
  (terpri) ;;nyrad efter m�nadsnamnet
  (formatting-table ;;En tabell med dagarna i
    (stream)
    (bygg-tabell (i 0 31 8 stream)
		 (rita-dagruta stream (plocka-ut-dag clim-m�nad i)))))

(defun rita-dagruta (stream clim-dag)
  (cond ((null clim-dag) nil)
	(t (rita-dag stream (ber�kna-dag-f�rg clim-dag))
	   (rita-�verlapp stream clim-dag)
	   (rita-dagkant stream))))

(defun rita-icke-dag (stream)
  (rita-dag stream ingen-dag-f�rg))

(defun ber�kna-dag-f�rg (clim-dag)
  (let ((m�teslista (slot-value clim-dag 'm�ten)))
    (if (null m�teslista)
      tom-dag-f�rg ;;f�rg f�r tomma dagar
      (bokad-f�rgintensitet m�teslista))))

(defun bokad-f�rgintensitet (m�teslista)
  "Returnerar det f�rgv�rde som en bokad dag ska ha. Om almanackan kan
  r�kna ut l�ngden av tidsperioder s� kan vi ge en v�ldigt fingradig
  f�rgskala, annars v�ljer vi ett antal steg beroende p� antal m�ten
  ist�llet."
  (if (alma-kan-l�ngd-av-tp)
    (skapa-bokad-f�rg (m�testid-till-f�rgv�rde
		   (r�kna-ihop-m�tesl�ngder m�teslista)))
    (let ((intensitet (length m�teslista)))
      (cond ((< intensitet 1) (skapa-bokad-f�rg ljusast-f�rgv�rde))
	    ((< intensitet 2) (skapa-bokad-f�rg ljusare-mellanf�rg))
	    ((< intensitet 3) (skapa-bokad-f�rg m�rkare-mellanf�rg))
	    (t (skapa-bokad-f�rg m�rkast-f�rgv�rde))))))

(defun m�testid-till-f�rgv�rde (m�tesminuter)
  "Returnerar ett f�rgv�rde beroende p� antal m�tesminuter.
  V�rdet ligger mellan m�rkast- och ljusast-f�rgv�rde."
  (let ((f�rgstyrka (/ m�tesminuter max-minuter)))
    ;;F�rgstyrka ligger mellan 0 och inf
    (cond
      ((>= f�rgstyrka 1) m�rkast-f�rgv�rde)
      ((<= f�rgstyrka 0) ljusast-f�rgv�rde)
      (t
	(+ m�rkast-f�rgv�rde ;;returnera minst det m�rkaste
	   (* (- ljusast-f�rgv�rde m�rkast-f�rgv�rde)
	      (- 1 f�rgstyrka))))))) ;;det ska ju bli m�rkare f�r h�gre styrka

(defun rita-�verlapp (stream clim-dag)
  (cond
    ((null clim-dag) nil)
    ((null (slot-value clim-dag 'm�ten)) nil)
    ((alma-kan-�verlapp) (rita-�verlappande-tider stream clim-dag))
    ((dag-har-�verlapp clim-dag) (rita-dag stream �verlapp-f�rg))))

(defun rita-�verlappande-tider (stream clim-dag)
  (kvadratiskc
    #'(lambda
	(clim-m�te-a clim-m�te-b)
	(let ((�verlapp (m�ten-�verlapp clim-m�te-a clim-m�te-b)))
	  (if (null �verlapp) nil
	    (rita-�verlapp-tider stream
				 (alma-tp-starttid �verlapp)
				 (alma-tp-sluttid �verlapp)))))
    (slot-value clim-dag 'm�ten)))

(defun dag-har-�verlapp (clim-dag)
  (labels
    ((my-or (a b) (or a b)))
    (kvadratisk
      #'m�ten-�verlappar
      #'my-or
      '()
      #'my-or
      '()
      (slot-value clim-dag 'm�ten))))

(defun rita-dag (stream f�rg)
  (with-drawing-options
    (stream :ink f�rg)
    (draw-rectangle* stream 0 0 px-dagbredd px-dagh�jd)))

(defun rita-dagkant (stream)
  (with-drawing-options
    (stream :ink dagkant-f�rg)
    (draw-rectangle* stream 0 0 px-dagbredd px-dagh�jd :filled nil)))

(defun rita-�verlapp-tider (stream starttid sluttid)
  (with-drawing-options
    (stream :ink �verlapp-f�rg)
    (draw-rectangle*
      stream
      0
      (tid-till-position starttid px-dagh�jd)
      px-dagbredd
      (tid-till-position sluttid px-dagh�jd))))


(define-application-frame
  �rtest
  () ;Superclasses
  ((datah�mtare :initarg :datah�mtare :accessor datah�mtare)) ;Slots
  (:panes
    (kontrollytan
      (vertically (:max-height +fill+ :height +fill+)
		  (make-pane 'push-button :label "St�ng"
			     :max-width +fill+ :max-height +fill+
			     :width 30 :height +fill+
			     :activate-callback #'st�ng-knapp-tryckt)))
    (�r :application
	:max-height +fill+ :height +fill+
	:background app-bg-f�rg
	:scroll-bars t
	:display-function #'rita-�r))
  (:layouts
    (default
      (horizontally (:max-height +fill+ :height +fill+
				 :max-width +fill+ :width +fill+)
		    (1/5 kontrollytan)
		    (4/5 �r)))))

(define-�rtest-command com-g�-till-m�nad
		       ((clim-m�nad 'clim-m�nad))
		       (g�-till-m�nadsvy clim-m�nad))

(define-presentation-to-command-translator
  g�-till-m�nad
  (clim-m�nad com-g�-till-m�nad �rtest
	      :gesture :select
	      :documentation "G� till m�nadsvyn.")
  (object)
  (list object))

(defun testa-�r (&rest �rsalmanacksnamn)
  "Startar den grafiska interfacen f�r att visualisera almanackor"
  (defvar app (clim:make-application-frame '�rtest))
  (setf (slot-value app 'datah�mtare)
	(make-instance 'datah�mtare
		       :datak�llor �rsalmanacksnamn))
  (clim:run-frame-top-level app))
