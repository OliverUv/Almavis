;;;;;; Almavis ;;;;;;
;;; Författare: Oliver Uvman
;;; Version: 0.2
;;; Senast uppdaterad: 2010-10-05
;;;
;;; Almavissystemet är en grafisk interface till almanackan (lab 4 och 6
;;; i kursen Funktionell Programmering och Lisp på Linköpings Universitet).
;;; I den här filen definierar jag övergripande funktioner som används för
;;; att definiera och starta systemet. De övriga filerna är som följer:
;;;
;;; almavis-model: Innehåller klassdefinitioner och de operationer som
;;; behövs för att konvertera data från almanackans typsystem till CLOS.
;;; Här finns också massor av funktioner för att hantera den data vi har
;;; så att det blir lättare att visa den grafiskt. Skulle väl kallas för
;;; applikationens business logic.
;;;
;;; unit-tests: Innehåller funktioner för att definiera och köra automatiska
;;; tester som kontrollerar att business-logiken fungerar ungefär som den ska,
;;; och för att ladda in testdata för att kontrollera det grafiska.
;;;
;;; almavis-grafik: Innehåller generella funktioner för att rita ut grafik.
;;; Innehåller också konfigurationsparametrar för all grafisk layout och
;;; design.
;;;
;;; almavis-år: Kod specifik för att visa årsvyn.
;;; almavis-månad: Kod specifik för att visa månadsvyn.
;;; almavis-dag: Kod specifik för att visa dagsvyn.

;;;; En konvention jag använder är att skriva alma-objekt om det är ett objekt
;;;; med almanackslabbens typsystem, och clim-objekt om det är ett CLOS-objekt.


;;;;;; Ladda Clim ;;;;;;
;; Kodraden nedan gömmer också varningar vi börjat få i ACL8.2, då de deprecate:ade
;; without-interrupts. Vi använder aldrig den funktionen själva, men det gör clim.
;; När vi uppgraderar till ACL9.0 försvinner without-interrupts - då bör det också
;; vara borta från CLIM enligt Franz.
(handler-bind ((warning #'muffle-warning)) (require :climxm))

(defpackage #:almavis (:use :common-lisp-user :clim-user :clim-lisp :clim))
(defpackage #:almavis-år (:use #:almavis :common-lisp-user :clim-user :clim-lisp :clim))
(defpackage #:almavis-månad (:use #:almavis :common-lisp-user :clim-user :clim-lisp :clim))
(defpackage #:almavis-dag (:use #:almavis :common-lisp-user :clim-user :clim-lisp :clim))

;;Används för att testa om clim reagerar på vissa saker, pga
;;att clim är svårdebuggat som fasen
(defvar reactions 0) 
(defun REACTTEST () (setf reactions (1+ reactions))) 

(load "almavis-model.cl") 
(load "almavis-grafik.cl")
(load "almavis-ar.cl")
(load "almavis-manad.cl")
(load "almavis-dag.cl")
(load "unit-tests.cl")

(defun visa-grafiskt (almanacksnamn &optional månad dag)
  (if (almavis::alma-kan-klockslag) 
    (mp:process-run-function "almavis" #'almavis::visa-grafiskt almanacksnamn månad dag)
    (format t "Du måste implementera start-klockslag och slut-klockslag först.")))

;;;; Med denna hook gör vi så att bokningar gjorda med boka syns på
;;;; interfacen direkt. TODO: Få detta att fungera, just nu binds
;;;; ingenting om av någon anledning.
#|(let ((bokfun (fdefinition 'boka)))
  (setf (fdefinition 'boka)
	(lambda (&rest args)
	  (prog1
	    (apply bokfun args)
	    (REACTTEST) 
	    (almavis::rita-om)))))|#

;; Testfunktioner för snabba change/compile/run cycles
#|(defun dd (lista)
  (almavis::alma-ta-bort-funktionalitet lista))|# 

#|(defun tt () (funcall (find-symbol "RUN-TESTS" 'almavis)))|#

#|(defun yy ()
  (tt)
  (visa-grafiskt (list (find-symbol "OLIVER" 'common-lisp-user)) 'september 12))|#

#|(defun mm ()
  (tt)
  (funcall (find-symbol "TESTA-MÅNAD" 'almavis-månad)
	   (list (find-symbol "LISA" 'common-lisp-user))
	   (find-symbol "SEPTEMBER" 'common-lisp-user)))|#

#|(defun mmö ()
  (tt)
  (funcall (find-symbol "TESTA-MÅNAD" 'almavis-månad)
           (list (find-symbol "ÖVERLAPP-TEST-A" 'common-lisp-user)
		 (find-symbol "ÖVERLAPP-TEST-B" 'common-lisp-user))
	   (find-symbol "FEBRUARI" 'common-lisp-user)))|#

(in-package #:almavis) 

;;Definierar applikationsfönstret
(define-application-frame
  almavis
  () ;Superclasses
  ((datahämtare :initarg :datahämtare :accessor datahämtare)) ;Slots
  (:panes
    (command-menu :command-menu)
    (år :application
	:background app-bg-färg
	:display-function 'almavis-år::rita-år)
    (månad :application
	   :background app-bg-färg
	   :display-function 'almavis-månad::rita-månad)
    (dag :application
	   :background dag-bg-färg
	   :display-function 'almavis-dag::rita-dag))
  (:layouts
    (årlayout (vertically (:height 700 :width 1000)
			  (1 år)))
    (månadlayout (vertically (:height 700 :width 1000)
			     (1 månad)))
    (daglayout (vertically (:height 700 :width 1000)
			   (1 dag)))))

(define-almavis-command (com-avsluta :menu "avsluta" :name "avsluta")
			() 
			(frame-exit *application-frame*))

(define-almavis-command
  (com-toggle-datakälla
    :menu "almanackor"
    :name "almanackor")
  () 
  (let ((datakälla (menu-choose (mapcar #'car *almanacka*))))
    (unless (null datakälla)
      (ny-datahämtare (toggle-datakälla
			 (slot-value *application-frame* 'datahämtare) 
			 datakälla)))))

(define-almavis-command
  (com-gå-till-år :name "årsvy" :menu "årsvy") ()
  (gå-till-år))

(define-almavis-command
  (com-gå-till-månad :name "visa månad" :menu "visa månad") ()
  (let ((månad (menu-choose (mapcar #'car *månadsdata*)))) 
    (unless (null månad)
      (gå-till (skapa-plats månad)))))

(define-almavis-command
  (com-uppdatera :name "uppdatera" :menu "uppdatera") ()
  (rita-om))

(define-almavis-command
  (com-gå-till-specifik-dag :name "gå till specifik dag") ((plats plats))
  (gå-till plats))

(define-presentation-to-command-translator
  gå-till-specifik-dag
  (plats com-gå-till-specifik-dag almavis
	      :gesture :select
	      :documentation "Gå till dagsvyn.")
  (object)
  (list object))

(define-almavis-command
  (com-gå-till-specifik-månad :name "gå till specifik månad")
  ((clim-månad clim-månad))
  (gå-till (månad-plats clim-månad)))

(define-presentation-to-command-translator
  gå-till-specifik-månad
  (clim-månad com-gå-till-specifik-månad almavis
	      :gesture :select
	      :documentation "Gå till månadsvyn.")
  (object)
  (list object))

(define-almavis-command
  (com-visa-mötesinfo :name "Visa mötesinfo")
  ((clim-möte clim-möte))
  (with-slots
    (start-kl slut-kl almanacksnamn mötesinfo)
    clim-möte
    (menu-choose (list
		   (format nil "~A - ~A"
			 (skapa-tidstext start-kl)
			 (skapa-tidstext slut-kl))
		 (format nil "~A" mötesinfo)
		 (format nil "~A" almanacksnamn)))))

(define-presentation-to-command-translator
  visa-mötesinfo
  (clim-möte com-visa-mötesinfo almavis
	     :gesture :select
	     :documentation "Visa mötesinfo")
  (object)
  (list object)) 

;;För att starta almavis
(defun visa-grafiskt (almanacksnamn &optional månad dag)
  "Startar den grafiska interfacen för att visualisera almanackor"
  (if
    (and (every #'datakälla-finns? almanacksnamn)
	 (if (null månad) t (månad-finns? månad))
	 (if (null dag) t (månad-har-dag? månad dag))) 
    (let*
      ((plats (make-instance 'plats :månad månad :dag dag)) 
       (datahämtare
	 (make-instance 'datahämtare
			:datakällor almanacksnamn
			:plats plats))
       (applikation (clim:make-application-frame 'almavis))) 
      (setf (slot-value applikation 'datahämtare) datahämtare) 
      (cond ((and månad dag) 
	     (setf (frame-current-layout applikation) 'daglayout))
	    (månad
	      (setf (frame-current-layout applikation) 'månadlayout))) 
      (clim:run-frame-top-level applikation))
    (error "Antingen så fanns inte almanackan, månaden eller dagen.")))

(defmethod ny-datahämtare (&optional (datahämtare nil))
  (setf (slot-value *application-frame* 'datahämtare)
	datahämtare)
  (rita-om))

(defun gå-till (&optional (plats nil))
  (let ((datahämtare (slot-value *application-frame* 'datahämtare)))
    (setf (slot-value *application-frame* 'datahämtare)
	  (byt-plats datahämtare plats)))
  (setf (frame-current-layout *application-frame*) 
	(cond ((null plats) 'årlayout)
	      ((plats-dag plats) 'daglayout)
	      ((plats-månad plats) 'månadlayout)
	      (t (error "Måste välja en vy.")))))

(defun gå-till-år () (gå-till))

;Döda alla McClim-program som körs
;(progn (loop for port in climi::*all-ports* do (clim:destroy-port port)) (setq climi::*all-ports* nil))
