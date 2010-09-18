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
(load "unit-tests.cl")

(defun visa-grafiskt (�rsalmanackor &optional m�nad dag)
  (almavis::visa-grafiskt �rsalmanackor m�nad dag))

;; Testfunktioner f�r snabba change/compile/run cycles
(defun tt () (funcall (find-symbol "RUN-TESTS" 'almavis)))

(defun yy ()
  (tt)
  (funcall (find-symbol "TESTA-�R" 'almavis-�r)
           (find-symbol "OLIVER" 'common-lisp-user)
           (find-symbol "�VERLAPP-TEST-A" 'common-lisp-user)
           (find-symbol "�VERLAPP-TEST-B" 'common-lisp-user)))

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

;D�da alla McClim-program som k�rs
;(progn (loop for port in climi::*all-ports* do (clim:destroy-port port)) (setq climi::*all-ports* nil))
