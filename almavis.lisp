;;;;;; almavis ;;;;;;
;;; Författare: Oliver Uvman
;;; Version: 0.1
;;; Senast uppdaterad: 2010-09-18
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
(require :climxm)

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
(load "unit-tests.cl")

(defun visa-grafiskt (almanacksnamn &optional månad dag)
  (almavis::visa-grafiskt almanacksnamn månad dag))

;; Testfunktioner för snabba change/compile/run cycles
(defun tt () (funcall (find-symbol "RUN-TESTS" 'almavis)))

(defun yy ()
  (tt)
  (visa-grafiskt (list (find-symbol "OLIVER" 'common-lisp-user)
		       (find-symbol "ÖVERLAPP-TEST-A" 'common-lisp-user)
		       (find-symbol "ÖVERLAPP-TEST-B" 'common-lisp-user))))

(defun mm ()
  (tt)
  (funcall (find-symbol "TESTA-MÅNAD" 'almavis-månad)
	   (list (find-symbol "LISA" 'common-lisp-user))
	   (find-symbol "SEPTEMBER" 'common-lisp-user)))

(defun mmö ()
  (tt)
  (funcall (find-symbol "TESTA-MÅNAD" 'almavis-månad)
           (list (find-symbol "ÖVERLAPP-TEST-A" 'common-lisp-user)
		 (find-symbol "ÖVERLAPP-TEST-B" 'common-lisp-user))
	   (find-symbol "FEBRUARI" 'common-lisp-user)))

(in-package #:almavis) 

(defun stäng-knapp-tryckt (button)
  (accepting-values
   (*query-io* :own-window t) (frame-exit *application-frame*)))

;;Definierar applikationsfönstret
(define-application-frame
  almavis
  () ;Superclasses
  ((datahämtare :initarg :datahämtare :accessor datahämtare)) ;Slots
  (:pointer-documentation t) 
  (:panes
    (command-menu :command-menu)
    (interactor :interactor)
    (år :application
	:background app-bg-färg
	:display-function 'almavis-år::rita-år)
    (månad :application
	   :background app-bg-färg
	   :display-function 'almavis-månad::rita-månad))
  (:layouts
    (årlayout (vertically (:height 700 :width 900)
			 (1/10 command-menu) 
			 (1/10 interactor)
			 (8/10 år)))
    (månadlayout (vertically (:height 700 :width 900)
			 (1/10 command-menu) 
			 (1/10 interactor)
			 (8/10 månad)))))

(define-almavis-command (com-avsluta :menu "avsluta" :name "avsluta")
			() 
			(frame-exit *application-frame*))

(define-almavis-command (com-toggle-datakälla
			  :menu "toggla datakälla"
			  :name "toggla datakälla")
			() 
			(ny-datakälla (toggle-datakälla
					 (slot-value *application-frame* 'datahämtare) 
					 (menu-choose (mapcar #'car *almanacka*)))))

(define-almavis-command
  (com-gå-till-månad :name "gå till månad" :menu "gå till månad") ()
  (gå-till (skapa-plats (menu-choose (mapcar #'car *månadsdata*)))))

(define-almavis-command
  (com-gå-till-år :name "gå till år" :menu "gå till år") ()
  (gå-till-år))


(define-presentation-to-command-translator
  gå-till-månad
  (clim-månad com-gå-till-månad almavis
	      :gesture :select
	      :documentation "Gå till månadsvyn.")
  ()
  nil)

;;För att starta almavis
(defun visa-grafiskt (almanacksnamn &optional månad dag)
  "Startar den grafiska interfacen för att visualisera almanackor"
  (let*
    ((datahämtare
       (make-instance 'datahämtare
		      :datakällor almanacksnamn
		      :plats (make-instance 'plats :månad månad :dag dag)))
     (applikation (clim:make-application-frame 'almavis))) 
    (setf (slot-value applikation 'datahämtare) datahämtare) 
    (clim:run-frame-top-level applikation)))

(defmethod ny-datakälla (&optional (datahämtare nil))
  (setf (slot-value *application-frame* 'datahämtare)
	datahämtare)
  (redisplay-frame-panes *application-frame*))

(defmethod gå-till (&optional (plats nil))
  (let ((datahämtare (slot-value *application-frame* 'datahämtare)))
    (setf (slot-value *application-frame* 'datahämtare)
	  (byt-plats datahämtare plats)))
  (setf (frame-current-layout *application-frame*) 
	(cond ((null plats) 'årlayout)
	      ((plats-dag plats) 'daglayout)
	      ((plats-månad plats) 'månadlayout))))

(defun gå-till-år () (gå-till))

;Döda alla McClim-program som körs
;(progn (loop for port in climi::*all-ports* do (clim:destroy-port port)) (setq climi::*all-ports* nil))
