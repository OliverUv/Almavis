;;;;;; almavis ;;;;;;
;;; F�rfattare: Oliver Uvman
;;; Version: 0.1
;;; Senast uppdaterad: 2010-09-18
;;;
;;; Almavissystemet �r en grafisk interface till almanackan (lab 4 och 6
;;; i kursen Funktionell Programmering och Lisp p� Link�pings Universitet).
;;; I den h�r filen definierar jag �vergripande funktioner som anv�nds f�r
;;; att definiera och starta systemet. De �vriga filerna �r som f�ljer:
;;;
;;; almavis-model: Inneh�ller klassdefinitioner och de operationer som
;;; beh�vs f�r att konvertera data fr�n almanackans typsystem till CLOS.
;;; H�r finns ocks� massor av funktioner f�r att hantera den data vi har
;;; s� att det blir l�ttare att visa den grafiskt. Skulle v�l kallas f�r
;;; applikationens business logic.
;;;
;;; unit-tests: Inneh�ller funktioner f�r att definiera och k�ra automatiska
;;; tester som kontrollerar att business-logiken fungerar ungef�r som den ska,
;;; och f�r att ladda in testdata f�r att kontrollera det grafiska.
;;;
;;; almavis-grafik: Inneh�ller generella funktioner f�r att rita ut grafik.
;;; Inneh�ller ocks� konfigurationsparametrar f�r all grafisk layout och
;;; design.
;;;
;;; almavis-�r: Kod specifik f�r att visa �rsvyn.
;;; almavis-m�nad: Kod specifik f�r att visa m�nadsvyn.
;;; almavis-dag: Kod specifik f�r att visa dagsvyn.

;;;; En konvention jag anv�nder �r att skriva alma-objekt om det �r ett objekt
;;;; med almanackslabbens typsystem, och clim-objekt om det �r ett CLOS-objekt.

;;;;;; Ladda Clim ;;;;;;
(require :climxm)

(defpackage #:almavis (:use :common-lisp-user :clim-user :clim-lisp :clim))
(defpackage #:almavis-�r (:use #:almavis :common-lisp-user :clim-user :clim-lisp :clim))
(defpackage #:almavis-m�nad (:use #:almavis :common-lisp-user :clim-user :clim-lisp :clim))
(defpackage #:almavis-dag (:use #:almavis :common-lisp-user :clim-user :clim-lisp :clim))

;;Anv�nds f�r att testa om clim reagerar p� vissa saker, pga
;;att clim �r sv�rdebuggat som fasen
(defvar reactions 0) 
(defun REACTTEST () (setf reactions (1+ reactions))) 

(load "almavis-model.cl") 
(load "almavis-grafik.cl")
(load "almavis-ar.cl")
(load "almavis-manad.cl")
(load "almavis-dag.cl")
(load "unit-tests.cl")

(defun visa-grafiskt (almanacksnamn &optional m�nad dag)
  (if (almavis::alma-kan-klockslag) 
    (mp:process-run-function "almavis" #'almavis::visa-grafiskt almanacksnamn m�nad dag)
    (format t "Du m�ste implementera start-klockslag och slut-klockslag f�rst.")))

;;;; Med denna hook g�r vi s� att bokningar gjorda med boka syns p�
;;;; interfacen direkt. TODO: F� detta att fungera, just nu binds
;;;; ingenting om av n�gon anledning.
#|(let ((bokfun (fdefinition 'boka)))
  (setf (fdefinition 'boka)
	(lambda (&rest args)
	  (prog1
	    (apply bokfun args)
	    (REACTTEST) 
	    (almavis::rita-om)))))|#

;; Testfunktioner f�r snabba change/compile/run cycles
(defun tt () (funcall (find-symbol "RUN-TESTS" 'almavis)))

(defun yy ()
  (tt)
  (visa-grafiskt (list (find-symbol "OLIVER" 'common-lisp-user)) 'september 12))

(defun mm ()
  (tt)
  (funcall (find-symbol "TESTA-M�NAD" 'almavis-m�nad)
	   (list (find-symbol "LISA" 'common-lisp-user))
	   (find-symbol "SEPTEMBER" 'common-lisp-user)))

(defun mm� ()
  (tt)
  (funcall (find-symbol "TESTA-M�NAD" 'almavis-m�nad)
           (list (find-symbol "�VERLAPP-TEST-A" 'common-lisp-user)
		 (find-symbol "�VERLAPP-TEST-B" 'common-lisp-user))
	   (find-symbol "FEBRUARI" 'common-lisp-user)))

(in-package #:almavis) 

(defun st�ng-knapp-tryckt (button)
  (accepting-values
   (*query-io* :own-window t) (frame-exit *application-frame*)))

;;Definierar applikationsf�nstret
(define-application-frame
  almavis
  () ;Superclasses
  ((datah�mtare :initarg :datah�mtare :accessor datah�mtare)) ;Slots
  (:panes
    (command-menu :command-menu)
    (�r :application
	:background app-bg-f�rg
	:display-function 'almavis-�r::rita-�r)
    (m�nad :application
	   :background app-bg-f�rg
	   :display-function 'almavis-m�nad::rita-m�nad)
    (dag :application
	   :background dag-bg-f�rg
	   :display-function 'almavis-dag::rita-dag))
  (:layouts
    (�rlayout (vertically (:height 700 :width 1000)
			 (1/10 command-menu) 
			 (9/10 �r)))
    (m�nadlayout (vertically (:height 700 :width 1000)
			 (1/10 command-menu) 
			 (9/10 m�nad)))
    (daglayout (vertically (:height 700 :width 1000)
			 (1/10 command-menu) 
			 (9/10 dag)))))

(define-almavis-command (com-avsluta :menu "avsluta" :name "avsluta")
			() 
			(frame-exit *application-frame*))

(define-almavis-command
  (com-toggle-datak�lla
    :menu "almanackor"
    :name "almanackor")
  () 
  (let ((datak�lla (menu-choose (mapcar #'car *almanacka*))))
    (unless (null datak�lla)
      (ny-datah�mtare (toggle-datak�lla
			 (slot-value *application-frame* 'datah�mtare) 
			 datak�lla)))))

(define-almavis-command
  (com-g�-till-m�nad :name "g� till m�nad" :menu "g� till m�nad") ()
  (let ((m�nad (menu-choose (mapcar #'car *m�nadsdata*)))) 
    (unless (null m�nad)
      (g�-till (skapa-plats m�nad)))))

(define-almavis-command
  (com-g�-till-�r :name "g� till �r" :menu "g� till �r") ()
  (g�-till-�r))

(define-almavis-command
  (com-uppdatera :name "uppdatera" :menu "uppdatera") ()
  (rita-om))

(define-almavis-command
  (com-g�-till-specifik-dag :name "g� till specifik dag") ((plats plats))
  (g�-till plats))

(define-presentation-to-command-translator
  g�-till-specifik-dag
  (plats com-g�-till-specifik-dag almavis
	      :gesture :select
	      :documentation "G� till dagsvyn.")
  (object)
  (list object))

(define-almavis-command
  (com-g�-till-specifik-m�nad :name "g� till specifik m�nad") ((clim-m�nad clim-m�nad))
  (g�-till (m�nad-plats clim-m�nad)))

(define-presentation-to-command-translator
  g�-till-specifik-m�nad
  (clim-m�nad com-g�-till-specifik-m�nad almavis
	      :gesture :select
	      :documentation "G� till m�nadsvyn.")
  (object)
  (list object))

(define-almavis-command
  (com-visa-m�tesinfo :name "Visa m�tesinfo")
  ((clim-m�te clim-m�te))
  (with-slots
    (start-kl slut-kl almanacksnamn m�tesinfo)
    clim-m�te
    (menu-choose (list
		   (format nil "~A - ~A"
			 (skapa-tidstext start-kl)
			 (skapa-tidstext slut-kl))
		 (format nil "~A" m�tesinfo)
		 (format nil "~A" almanacksnamn))))) 

(define-presentation-to-command-translator
  visa-m�tesinfo
  (clim-m�te com-visa-m�tesinfo almavis
	     :gesture :select
	     :documentation "Visa m�tesinfo")
  (object)
  (list object)) 

;;F�r att starta almavis
(defun visa-grafiskt (almanacksnamn &optional m�nad dag)
  "Startar den grafiska interfacen f�r att visualisera almanackor"
  (let*
    ((plats (make-instance 'plats :m�nad m�nad :dag dag)) 
     (datah�mtare
       (make-instance 'datah�mtare
		      :datak�llor almanacksnamn
		      :plats plats))
     (applikation (clim:make-application-frame 'almavis))) 
    (setf (slot-value applikation 'datah�mtare) datah�mtare) 
    (cond ((and m�nad dag) 
	   (setf (frame-current-layout applikation) 'daglayout))
	  (m�nad
	     (setf (frame-current-layout applikation) 'm�nadlayout))) 
    (clim:run-frame-top-level applikation)))

(defmethod ny-datah�mtare (&optional (datah�mtare nil))
  (setf (slot-value *application-frame* 'datah�mtare)
	datah�mtare)
  (rita-om))

(defun g�-till (&optional (plats nil))
  (let ((datah�mtare (slot-value *application-frame* 'datah�mtare)))
    (setf (slot-value *application-frame* 'datah�mtare)
	  (byt-plats datah�mtare plats)))
  (setf (frame-current-layout *application-frame*) 
	(cond ((null plats) '�rlayout)
	      ((plats-dag plats) 'daglayout)
	      ((plats-m�nad plats) 'm�nadlayout)
	      (t (error "M�ste v�lja en vy.")))))

(defun g�-till-�r () (g�-till))

;D�da alla McClim-program som k�rs
;(progn (loop for port in climi::*all-ports* do (clim:destroy-port port)) (setq climi::*all-ports* nil))
