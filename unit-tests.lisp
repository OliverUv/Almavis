;;;;;; Unit tests
(in-package #:almavis)

;; Vi lagrar unit-testen och deras data i dessa variabler. De b�r �ndast
;; �ndras genom anrop till nytt-unit-test och ny-test-data.
(defvar unit-tests nil)
(defvar test-data nil)

;; Efter att du definierat dina testfunktioner och din testdata kallar du p�
;; dessa funktioner.
(defun nytt-unit-test (testsymbol testfun)
  (setq unit-tests
        (cons (cons testsymbol testfun)
              unit-tests)))

(defun ny-testdata (testsymbol testdata)
  (setq test-data
        (cons (list testsymbol testdata)
              test-data)))

(defun run-tests ()
  (let
   ((lyckade 0)
    (misslyckade 0)
    (ok�rbara 0))
   (mapc
    #'(lambda
       (unit-test-data)
       (let*
        ((testnamn (car unit-test-data))
         (testdata (cdr unit-test-data))
         (testfun (cdr (assoc testnamn unit-tests)))
         (resultat nil))
        (cond ((null testfun)
               (setf ok�rbara (1+ ok�rbara))
               (format t "! ~A kunde ej k�ras, debugga unit-testet" testnamn))
              ((null (setf resultat (apply testfun testdata)))
               (setf lyckade (1+ lyckade))
               (format t "~A: ~A~%" testnamn "lyckat"))
              (t
               (setf misslyckade (1+ misslyckade))
               (format t "~A: MISSLYCKAT: ~%~A~%" testnamn resultat)))))
    test-data)
   (format t "  ----------------  ~%~%")
   (if (and (= 0 misslyckade) (= 0 ok�rbara))
       (format t "Alla tester lyckade~%~%")
       (format t "Lyckade: ~A~%Misslyckade: ~A~%Ok�rbara: ~A~%~%"
               lyckade misslyckade ok�rbara))))


;; Hj�lpfunktion f�r unit-tester. De flesta beh�ver ta en lista med
;; resultat, d�r lyckade resultat �r nil och misslyckade en textstr�ng.
;; �r allt i listan nil returneras nil, annars byggs en textstr�ng upp
;; som visar de testfall som misslyckades.
(defun samla-resultat (resultatlista)
  (let ((res (remove-if #'null resultatlista))
        (resstring ""))
       (if (null res) res
         (dolist (fel res resstring)
                 (setq
                  resstring
                  (format nil "~A~A,~%" resstring fel))))))

;; Detta unit-test kan anv�ndas om inga bieffekter eller annat
;; beh�ver testas. Registrera helt enkelt en test-data f�r testet
;; standard-test, med formen (funktion argument v�ntat-resultat),
;; s� k�rs och kontrolleras testet automatiskt.
(defun standard-test (test-data)
  (if (equal (apply (car test-data) (cadr test-data))
             (caddr test-data))
      nil
      test-data))

(nytt-unit-test
  'standard-test
  #'standard-test)

;;;;;; alma->clos
;;;; Testar om data importeras fr�n almanackans dataobjekt till CLOS-objekt
;;;; korrekt. Datan �r utformad (almanacksnamn bokningskommando*). Mycket
;;;; paket-magi som m�ste tas h�nsyn till. P� ida ska #:stdalma bytas ut mot
;;;; :common-lisp-user.

(defun alma->clos (test-data)
  (common-lisp-user::boka-alma test-data) ;;bokar alma-m�ten
  (let ((�rsalmor (h�mta-�rsalmanackor))) ;;h�mtar clos-representationer
       (samla-resultat ;;kollar vad som gick r�tt
        (mapcar
         #'(lambda ;;testar varje bokning
            (bokning)
            (if (bokning-korrekt bokning �rsalmor)
                nil       ;;�r bokningen korrekt returnerar vi nil
                bokning)) ;;annars bokningen, s� vi ser vad som gick fel
         (cdr test-data)))))

;; Kontrollerar att det finns bokningsinformation i n�gon av �rsalmanackorna
;; som �verrensst�mmer med bokningen.
(defun bokning-korrekt (bokning �rsalmor)
  (let
   ((almanamn (nth 1 bokning))
    (datum (nth 2 bokning))
    (m�nad (nth 3 bokning))
    (start-kl (klockslag-till-heltal (omvandla-klockslag (nth 4 bokning))))
    (slut-kl (klockslag-till-heltal (omvandla-klockslag (nth 5 bokning))))
    (info (nth 6 bokning)))
   (remove-if-not ;;Djup s�kning :D
    #'(lambda 
       (�rsalma)
       (clos-objekt-check
        �rsalma
        'namn almanamn
        'm�nader
        #'(lambda
           (m�nadsalma)
           (clos-objekt-check
            m�nadsalma
            'namn m�nad
            'dagar
            #'(lambda
               (dagalma)
               (clos-objekt-check
                dagalma
                'dag-i-m�nad datum
                'm�ten
                #'(lambda
                   (m�te)
                   (and (slot-har-v�rde m�te 'start-kl start-kl)
                        (slot-har-v�rde m�te 'slut-kl slut-kl)
                        (slot-har-v�rde m�te 'm�tesinfo info)
                        (slot-har-v�rde m�te 'almanacksnamn almanamn)))))))))
    �rsalmor)))

(in-package common-lisp-user)
(defun boka-alma (alma)
  (skapa (car alma))
  (dolist (m�te (cdr alma))
          (apply (car m�te) (cdr m�te))))

(almavis::nytt-unit-test
  'alma->clos
  #'almavis::alma->clos)

(almavis::ny-testdata
  'alma->clos
  '(kalle
    (boka kalle 12 september "08:00" "08.30" "Morgonm�te")
    (boka kalle 12 september "11:00" "11:30" "Styrelsem�te")
    (boka kalle 12 september "12:15" "12.45" "Lunch")
    (boka kalle 12 september "13:30" "14:15" "Rapportarbete")
    (boka kalle 12 september "15:15" "15:45" "Tr�ffa Jocke")
    (boka kalle 12 september "16:15" "16:45" "Telefonjour")
    (boka kalle 12 september "18:00" "19:00" "Handla mat")
    (boka kalle 12 september "19:30" "20:30" "Styrketr�ning")))

(almavis::ny-testdata
  'alma->clos
  '(lisa
    (boka lisa 7 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 8 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 8 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 9 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 9 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 9 september "11:15" "12:00" "Handla biljetter")
    (boka lisa 10 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 10 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 10 september "10:15" "11:00" "Handla biljetter")
    (boka lisa 11 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 11 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 11 september "10:15" "11:00" "Handla biljetter")
    (boka lisa 11 september "12:15" "12:45" "Lunch")
    (boka lisa 13 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 13 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 13 september "10:15" "11:00" "Handla biljetter")
    (boka lisa 13 september "12:15" "12:45" "Lunch")
    (boka lisa 13 september "14:15" "14:45" "M�te med designgruppen")
    (boka lisa 14 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 14 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 14 september "10:15" "11:00" "Handla biljetter")
    (boka lisa 14 september "12:15" "12:45" "Lunch")
    (boka lisa 14 september "14:15" "14:45" "M�te med designgruppen")
    (boka lisa 14 september "15:15" "15:45" "Videokonferens")
    (boka lisa 15 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 15 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 15 september "10:15" "11:00" "Handla biljetter")
    (boka lisa 15 september "12:15" "12:45" "Lunch")
    (boka lisa 15 september "14:15" "14:45" "M�te med designgruppen")
    (boka lisa 15 september "15:15" "15:45" "Videokonferens")
    (boka lisa 15 september "17:30" "18:30" "Aerobics")
    (boka lisa 12 september "07:00" "07:30" "Morgonm�te")
    (boka lisa 12 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka lisa 12 september "10:15" "11:00" "Handla biljetter")
    (boka lisa 12 september "12:15" "12:45" "Lunch")
    (boka lisa 12 september "14:15" "14:45" "M�te med designgruppen")
    (boka lisa 12 september "15:15" "15:45" "Videokonferens")
    (boka lisa 12 september "17:30" "18:30" "Aerobics")
    (boka lisa 12 september "20:00" "21:00" "Kolla p� TV")))

(almavis::ny-testdata
  'alma->clos
  '(�verlapp-test-a
    (boka �verlapp-test-a 1 mars "06:00" "15:00" "busy busy")
    (boka �verlapp-test-a 2 mars "11:00" "15:00" "busy busy")
    (boka �verlapp-test-a 1 februari "01:00" "22:30" "busy busy")
    (boka �verlapp-test-a 2 februari "02:00" "22:30" "busy busy")
    (boka �verlapp-test-a 3 februari "03:00" "22:30" "busy busy")
    (boka �verlapp-test-a 4 februari "04:00" "22:30" "busy busy")
    (boka �verlapp-test-a 5 februari "05:00" "22:30" "busy busy")
    (boka �verlapp-test-a 6 februari "06:00" "22:30" "busy busy")
    (boka �verlapp-test-a 7 februari "07:00" "22:30" "busy busy")
    (boka �verlapp-test-a 8 februari "08:00" "22:30" "busy busy")
    (boka �verlapp-test-a 9 februari "09:00" "22:30" "busy busy")
    (boka �verlapp-test-a 11 februari "07:00" "22:30" "busy busy")
    (boka �verlapp-test-a 12 februari "08:00" "22:30" "busy busy")
    (boka �verlapp-test-a 13 februari "09:00" "22:30" "busy busy")
    
    ;;Vi vill �verlappa p� alla m�jliga vis. Till h�ger visas test-b-tiden
    ;;ska resultera i 10 m�ten som visar �verlapp
    (boka �verlapp-test-a 15 februari "00:00" "00:50" "oj oj busy") ;01:00 02:00
    (boka �verlapp-test-a 15 februari "02:50" "03:00" "oj oj busy") ;03:00 04:00
    (boka �verlapp-test-a 15 februari "04:50" "05:30" "oj oj busy") ;05:00 06:00
    (boka �verlapp-test-a 16 februari "06:50" "08:00" "oj oj busy") ;07:00 08:00
    (boka �verlapp-test-a 16 februari "08:40" "10:20" "oj oj busy") ;09:00 10:00
    (boka �verlapp-test-a 16 februari "11:00" "12:20" "oj oj busy") ;11:00 12:00
    (boka �verlapp-test-a 17 februari "13:00" "14:00" "oj oj busy") ;13:00 14:00

    (boka �verlapp-test-a 18 februari "10:00" "11:00" "oj oj busy")
    (boka �verlapp-test-a 18 februari "12:00" "13:00" "oj oj busy")
    (boka �verlapp-test-a 18 februari "14:00" "15:00" "oj oj busy")
    (boka �verlapp-test-a 18 februari "18:00" "19:00" "oj oj busy")

    (boka �verlapp-test-a 10 februari "10:00" "22:30" "busy busy")))


(almavis::ny-testdata
  'alma->clos
  '(�verlapp-test-b
    (boka �verlapp-test-b 1 mars "07:00" "08:00" "B")
    (boka �verlapp-test-b 1 mars "09:00" "11:00" "B")
    (boka �verlapp-test-b 1 mars "13:00" "14:00" "B")
    (boka �verlapp-test-b 2 mars "13:00" "14:00" "B")
    (boka �verlapp-test-b 1 februari "01:00" "22:30" "B")
    (boka �verlapp-test-b 2 februari "01:00" "22:30" "B")
    (boka �verlapp-test-b 3 februari "04:00" "22:30" "B")
    (boka �verlapp-test-b 4 februari "04:00" "21:30" "B")
    (boka �verlapp-test-b 5 februari "04:00" "21:30" "B")
    (boka �verlapp-test-b 6 februari "07:00" "21:30" "B")
    (boka �verlapp-test-b 7 februari "07:00" "23:30" "B")
    (boka �verlapp-test-b 8 februari "07:00" "23:30" "B")
    (boka �verlapp-test-b 9 februari "10:00" "23:30" "B")
    (boka �verlapp-test-b 11 februari "03:00" "05:30" "B")
    ;;Testa olika �verlapp
    (boka �verlapp-test-b 15 februari "01:00" "02:00" "B")
    (boka �verlapp-test-b 15 februari "03:00" "04:00" "B")
    (boka �verlapp-test-b 15 februari "05:00" "06:00" "B")
    (boka �verlapp-test-b 16 februari "07:00" "08:00" "B")
    (boka �verlapp-test-b 16 februari "09:00" "10:00" "B")
    (boka �verlapp-test-b 16 februari "11:00" "12:00" "B")
    (boka �verlapp-test-b 17 februari "13:00" "14:00" "B")

    (boka �verlapp-test-b 18 februari "10:00" "23:00" "B")

    (boka �verlapp-test-b 10 februari "10:00" "22:30" "B")))

(almavis::ny-testdata
  'alma->clos
  '(oliver
    (boka oliver 1 januari "01:00" "22:30" "busy busy")
    (boka oliver 2 januari "02:00" "22:30" "busy busy")
    (boka oliver 3 januari "03:00" "22:30" "busy busy")
    (boka oliver 4 januari "04:00" "22:30" "busy busy")
    (boka oliver 5 januari "05:00" "22:30" "busy busy")
    (boka oliver 6 januari "06:00" "22:30" "busy busy")
    (boka oliver 7 januari "07:00" "22:30" "busy busy")
    (boka oliver 8 januari "08:00" "22:30" "busy busy")
    (boka oliver 9 januari "09:00" "22:30" "busy busy")
    (boka oliver 10 januari "10:00" "22:30" "busy busy")
    (boka oliver 11 januari "11:00" "22:30" "busy busy")
    (boka oliver 12 januari "12:00" "22:30" "busy busy")
    (boka oliver 13 januari "13:00" "22:30" "busy busy")
    (boka oliver 14 januari "14:00" "22:30" "busy busy")
    (boka oliver 15 januari "15:00" "22:30" "busy busy")
    (boka oliver 16 januari "16:00" "22:30" "busy busy")
    (boka oliver 17 januari "17:00" "22:30" "busy busy")
    (boka oliver 18 januari "18:00" "22:30" "busy busy")
    (boka oliver 19 januari "19:00" "22:30" "busy busy")
    (boka oliver 20 januari "20:00" "22:30" "busy busy")
    (boka oliver 9 september "10:00" "22:30" "busy busy")
    (boka oliver 10 september "01:00" "22:30" "Mycket grejs")
    (boka oliver 11 september "01:30" "02:00" "lite grejs")
    (boka oliver 12 september "07:00" "07:30" "Morgonm�te")
    (boka oliver 12 september "09:00" "09:30" "Tr�ffa Johanna")
    (boka oliver 12 september "10:15" "11:00" "Handla biljetter")
    (boka oliver 12 september "12:15" "12:45" "Lunch")
    (boka oliver 12 september "14:15" "14:45" "M�te med designgruppen")
    (boka oliver 12 september "15:15" "15:45" "Videokonferens")
    (boka oliver 12 september "17:30" "18:30" "Aerobics")
    (boka oliver 12 september "20:00" "21:00" "Kolla p� TV")))


;;;; Tester f�r kvadratisk i almavis.lisp
(in-package :almavis)

#|(defun kvadratisk  ;;anropas s�h�r
  (bin�r-funktion
   intern-uppsamlare
   intern-grundfall
   extern-uppsamlare
   extern-grundfall
   lista))|#

(ny-testdata
  'standard-test
  '(kvadratisk
    (max + 0 + 0 (1 2 3 4 5 6))
    70))

(ny-testdata
  'standard-test
  '(kvadratisk
    (+ cons nil append nil (1 2 3 4 5 6))
    (3 4 5 6 7 5 6 7 8 7 8 9 9 10 11)))

(ny-testdata
  'standard-test
  '(kvadratisk
    (+ cons nil cons nil (1 2 3 4))
    ((3 4 5) (5 6) (7))))
