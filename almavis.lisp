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

(defun visa-grafiskt (årsalmanackor &optional månad dag)
  (almavis::visa-grafiskt årsalmanackor månad dag))

;; Testfunktioner för snabba change/compile/run cycles
(defun tt () (funcall (find-symbol "RUN-TESTS" 'almavis)))

(defun yy ()
  (tt)
  (funcall (find-symbol "TESTA-ÅR" 'almavis-år)
           (find-symbol "OLIVER" 'common-lisp-user)
           (find-symbol "ÖVERLAPP-TEST-A" 'common-lisp-user)
           (find-symbol "ÖVERLAPP-TEST-B" 'common-lisp-user)))

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

;Döda alla McClim-program som körs
;(progn (loop for port in climi::*all-ports* do (clim:destroy-port port)) (setq climi::*all-ports* nil))
