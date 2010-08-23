(declaim (optimize debug))
;;;;;; almavis ;;;;;;
;;;; I den här filen definierar jag övergripande funktioner som behövs för
;;;; att köra almavis. Detta består av funktionen visa-grafiskt, såväl som
;;;; en massa bindningar från almanackans datatyper till CLOS-objekt och
;;;; funktioner för att göra konverteringen.

;;;; En konvention jag använder är att skriva alma-objekt om det är ett objekt
;;;; med almanackslabbens typsystem, och clim-objekt om det är ett CLOS-objekt.

;;;;;; Ladda Clim ;;;;;;
;(require :climxm)

(defun visa-grafiskt (årsalmanacka &optional månad dag)
  (almavis::visa-grafiskt årsalmanacka))

(in-package #:almavis)
;;;;;; Kommunikation med almanackan ;;;;;;
;;;; Funktioner för att testa vilken funktionalitet
;;;; studenten har implementerat hittills

(defun alma-har-funktionalitet-p (funktionalitet-str)
  (find-symbol funktionalitet-str 'stdalma))

(defun alma-kan-längd-av-tp ()
  (alma-har-funktionalitet-p "LÄNGD-AV-TIDSPERIOD"))

(defun alma-kan-tidsperioder ()
  (alma-har-funktionalitet-p "SKAPA-TIDSPERIODER"))

(defun alma-kan-skapa-tidsrymd ()
  (alma-har-funktionalitet-p "SKAPA-TIDSRYMD"))

(defun alma-kan-klockslag ()
  (and
   (alma-har-funktionalitet-p "START-KLOCKSLAG")
   (alma-har-funktionalitet-p "SLUT-KLOCKSLAG")))

(defun alma-kan-avboka-möte ()
  (alma-har-funktionalitet-p "AVBOKA-MÖTE"))

(defun alma-kan-jämför ()
  (and (alma-har-funktionalitet-p "JÄMFÖR")
       (alma-har-funktionalitet-p "GEMENSAMMA-TIDER")))

(defun alma-kan-ledigt ()
  (and
   (alma-har-funktionalitet-p "LEDIGA-TIDSPERIODER")
   (alma-har-funktionalitet-p "LEDIGT")
   (alma-har-funktionalitet-p "SAMMA-LEDIGA-PERIODER")))

(defun alma-kan-överlapp ()
  (alma-har-funktionalitet-p "ÖVERLAPP"))

;;;;;; Funktionsbindningar ;;;;;;
;;;; Vi vill använda en hel del funktioner ifrån almanackan.
;;;; Eftersom den inte är definierad i samma paket så behöver vi
;;;; för varje funktion också specifiera vilket paket funktionen
;;;; ligger i. Det blir för mycket kod och för långa kodrader om
;;;; vi ska blanda i det i vår kod, så här importerar vi den
;;;; funktionaliteten. Detta gör att vi själva inte får definiera
;;;; funktioner eller globala variabler med samma namn.

;;;; I programmeringsmiljön jag använder görs detta i almavis
;;;; paketdefinition, men den kommer troligtvis inte vara med
;;;; när vi integrerar på IDA - då behövs dessa istället.

#|((import 'common-lisp-user::dagalmanacka)
(import 'common-lisp-user::dagalmanacka?)
(import 'common-lisp-user::första-mötestid)
(import 'common-lisp-user::heltal)
(import 'common-lisp-user::minutdel)
(import 'common-lisp-user::månadsalmanacka)
(import 'common-lisp-user::månadsalmanacka?)
(import 'common-lisp-user::mötesdel)
(import 'common-lisp-user::mötestext)
(import 'common-lisp-user::resten-dagalmanacka)
(import 'common-lisp-user::skapa-dag)
(import 'common-lisp-user::skapa-månad)
(import 'common-lisp-user::slut-klockslag)
(import 'common-lisp-user::start-klockslag)
(import 'common-lisp-user::tidsperioddel)
(import 'common-lisp-user::timdel)
(import 'common-lisp-user::tom-dagalmanacka?)
(import 'common-lisp-user::typkontroll)
(import 'common-lisp-user::årsalmanacka?)


(import 'common-lisp-user::*almanacka*)
(import 'common-lisp-user::*månadsdata*))|#

;;;;;; Datatyper ;;;;;;
;;;; Här definierar vi de CLOS-objekt som vi sedan med Clim vill kunna
;;;; visualisera. Anledningen till att vi vill ha CLOS-objekten alls är
;;;; för att Clim är byggt med antagandet att man vill presentera just
;;;; CLOS-objekt. Detta gör man genom att definiera presenters. Mer av
;;;; det syns där jag implementerar almavis-vyerna.

(defclass clim-år
  ()
  ((alma-år :accessor alma-år :initarg :alma-år) ;originaldata
   (månader :accessor månader :initarg :månader) ;lista med clim-månad
   (namn :accessor namn :initarg :namn))) ;t.ex. "pelles almanacka"

(defclass clim-månad
  ()
  ((alma-månad :accessor alma-månad :initarg :alma-månad) ;originaldata
   (dagar :accessor dagar :initarg :dagar) ;lista med clim-dag, SORTERAD
   (namn :accessor namn :initarg :namn) ;t.ex. "januari"
   (almanacksnamn :accessor almanacksnamn :initarg :almanacksnamn)
   ;t.ex. "pelles almanacka"
   (antal-dagar :accessor antal-dagar :initarg :antal-dagar))) ;t.ex. 31

(defclass clim-dag 
  ()
  ((alma-dag :accessor alma-dag :initarg :alma-dag) ;originaldata
   (möten :accessor möten :initarg :möten :reader möten) ;lista med clim-möte
   (almanacksnamn :accessor almanacksnamn :initarg :almanacksnamn)
   ;t.ex. "pelles almanacka"
   (månadsnamn :accessor månadsnamn :initarg :månadsnamn)
   (dag-i-månad :accessor dag-i-månad :initarg :dag-i-månad))) ;t.ex. 1

(defclass clim-möte 
  ()
  ((alma-möte :accessor alma-möte :initarg :alma-möte) ;originaldata
   (start-kl :accessor start-kl :initarg :start-kl)
   (slut-kl :accessor slut-kl :initarg :slut-kl)
   ;integer mellan 0000 och 2359
   (almanacksnamn :accessor almanacksnamn :initarg :almanacksnamn)         
   ;t.ex. "pelles almanacka"
   (månadsnamn :accessor månadsnamn :initarg :månadsnamn)
   (dag-i-månad :accessor dag-i-månad :initarg :dag-i-månad)
   (mötesinfo :accessor mötesinfo :initarg :mötesinfo)))

(defmethod möten ((clim-år clim-år))
  (reduce
    #'append
    (mapcar
      #'möten
      (slot-value clim-år 'månader)))) 

(defmethod möten ((clim-månad clim-månad))
  (reduce
    #'append
    (mapcar
      #'möten
      (slot-value clim-månad 'dagar)))) 

(defmethod clim-år-union ((a clim-år) (b clim-år))
  "Slår ihop två clim-år till ett, resultatet får namn och alma-år
  från det första argumenet. Endast månadslistan sammansätts från
  båda åren."
  (make-instance
   'clim-år
   :alma-år (slot-value a 'alma-år)
   :månader (mapcar #'clim-månad-union
                    (slot-value a 'månader)
                    (slot-value b 'månader))
   :namn (slot-value a 'namn)))

(defmethod clim-månad-union ((a clim-månad) (b clim-månad))
  (make-instance
   'clim-månad
   :alma-månad (slot-value a 'alma-månad)
   :dagar (mapcar #'clim-dag-union
                  (slot-value a 'dagar)
                  (slot-value b 'dagar))
   :namn (slot-value a 'namn)
   :almanacksnamn (slot-value a 'almanacksnamn)
   :antal-dagar (slot-value a 'antal-dagar)))

(defmethod clim-dag-union ((a clim-dag) (b clim-dag))
  (make-instance
   'clim-dag
   :alma-dag (slot-value a 'alma-dag)
   :möten (append (slot-value a 'möten) (slot-value b 'möten))
   :almanacksnamn (slot-value a 'almanacksnamn)
   :månadsnamn (slot-value a 'månadsnamn)
   :dag-i-månad (slot-value a 'dag-i-månad)))


;;;;;; Konvertering ;;;;;;
;; Här har vi massor av kod för konvertering av almanackans datatyper
;; till CLOS-klasser. Funktionen hämta-årsalmanackor kommer att anropas av 
;; programmet, som sedan rekursivt går igenom alla årsalmanackors månads-,
;; och dags-almanackor - hämtar ut alla möten och stoppar dem i motsvarande
;; dataobjektshierarki av CLOS-objekt.

;; Notera att hämta-årsalmanackor konverterar till CLOS-objekt, medan
;; hämta-månader och hämta-dagar endast plockar ut relevanta objekt ur
;; en alma-årsalmanacka och alma-månadsalmanacka, respektive, med hjälp
;; av almanackans primitiver.

(defun hämta-årsalmanackor ()
  "Returnerar en lista med clim-år"
  (mapcar
   #'(lambda (alma-tupel)
             (konvertera-år (car alma-tupel) (cdr alma-tupel)))
             ;car är namnet på almanackan, cdr är alma-årsalmanackan
   *almanacka*))

(defun hämta-dagar (alma-månad dagar-i-månad)
  "Returnerar en lista med dagalmanackorna i en månad"
  (typkontroll alma-månad #'månadsalmanacka?)
  (let ((alma-dagalma-lista '()))
       (dotimes
        (i dagar-i-månad alma-dagalma-lista)
        (setq alma-dagalma-lista
              (cons (dagalmanacka (skapa-dag (- dagar-i-månad i)) alma-månad)
                    alma-dagalma-lista)))))

(defun hämta-möten (alma-dag)
  "Returnerar en lista med mötestiderna i en dagalmanacka"
  (typkontroll alma-dag #'dagalmanacka?)
  (if (tom-dagalmanacka? alma-dag) '()
      (cons (första-mötestid alma-dag)
            (hämta-möten (resten-dagalmanacka alma-dag)))))

(defun klockslag-till-heltal (alma-klockslag)
  (+
   (* 100 (heltal (timdel alma-klockslag)))
   (heltal (minutdel alma-klockslag))))

;;;; Funktioner för att konvertera

;;År
(defun konvertera-år (alma-namn alma-år)
  (skapa-clim-år
   alma-år
   alma-namn
   (konvertera-månader alma-namn alma-år)))

(defun skapa-clim-år (alma-år alma-namn clim-månader)
  (make-instance
   'clim-år
   :alma-år alma-år
   :månader clim-månader
   :namn alma-namn))

;;Månader - notera plural
(defun konvertera-månader (alma-namn alma-år)
  (typkontroll alma-år #'årsalmanacka?)
  (mapcar
   #'(lambda (månadstupel)
             (let* ((månadsnamn (car månadstupel))
                  (dagar-i-månad (cdr månadstupel))
                  (alma-månad
                   (månadsalmanacka (skapa-månad månadsnamn) alma-år)))
             (skapa-clim-månad
              alma-månad
              alma-namn
              månadsnamn
              dagar-i-månad
              (konvertera-dagar alma-namn
				månadsnamn
				alma-månad
				dagar-i-månad))))
   *månadsdata*))

(defun skapa-clim-månad
  (alma-månad alma-namn månadsnamn dagar-i-månad clim-dagar)
  (make-instance
   'clim-månad
   :alma-månad alma-månad
   :almanacksnamn alma-namn
   :antal-dagar dagar-i-månad
   :namn månadsnamn
   :dagar clim-dagar))

;;Dagar
(defun konvertera-dagar (alma-namn månadsnamn alma-månad dagar-i-månad)
  (typkontroll alma-månad #'månadsalmanacka?)
  (let ((dag-lista (hämta-dagar alma-månad dagar-i-månad))
        (dag-nr 1))
       (mapcar #'(lambda (alma-dag)
                         (prog1
                          (skapa-clim-dag
                           alma-dag
                           alma-namn
			   månadsnamn
                           dag-nr
                           (konvertera-möten
			     alma-namn månadsnamn dag-nr alma-dag))
                          (setq dag-nr (1+ dag-nr))))
               dag-lista)))

(defun skapa-clim-dag (alma-dag alma-namn månadsnamn dagnummer clim-möten)
  (make-instance
   'clim-dag
   :alma-dag alma-dag
   :almanacksnamn alma-namn
   :månadsnamn månadsnamn
   :dag-i-månad dagnummer
   :möten clim-möten))

;;Möten
(defun konvertera-möten (alma-namn månadsnamn dag-i-månad alma-dag)
  (let ((möten (hämta-möten alma-dag)))
       (mapcar
        #'(lambda
           (möte)
           (skapa-clim-möte
            möte
            alma-namn
	    månadsnamn
	    dag-i-månad
            (alma-tp-starttid (tidsperioddel möte))
            (alma-tp-sluttid (tidsperioddel möte))
            (mötestext (mötesdel möte))))
        möten)))

(defun skapa-clim-möte
  (alma-möte alma-namn månadsnamn dag-i-månad 
	     start-klockslag slut-klockslag mötestext)
  (make-instance
   'clim-möte
   :alma-möte alma-möte
   :almanacksnamn alma-namn
   :månadsnamn månadsnamn
   :dag-i-månad dag-i-månad
   :start-kl start-klockslag
   :slut-kl slut-klockslag
   :mötesinfo mötestext))


;;;;;; Plats och Platser  ;;;;;;;
;; Används av almavis olika vyer för att veta vad för olika objekt de
;; ska visa. Är ett sätt att specifiera år, månader och datum på och
;; har tillhörande funktioner för att hämta den data som visas.

;; Just nu implementerad i almanackans objektsystem för skojs skull.

;;; Platser :: (plats+)
;;; Plats :: (årsalmanamn [månadsnamn [dag-i-månad]])

(defconstant platser-typ 'platser)
(defun platser? (objekt) (eq platser-typ (typ objekt))) 

(defun första-platsen (platser)
  (typkontroll platser #'platser?)
  (car (packa-upp platser)))

(defun resten-platser (platser)
  (typkontroll platser #'platser?)
  (cdr (packa-upp platser))) 

(defun tom-platser (platser)
  (typkontroll platser #'platser?)
  (endp (packa-upp platser)))

(defun lägg-in-plats (plats platser)
  (typkontroll platser #'platser?) 
  (typkontroll plats #'plats?) 
  (packa-ihop platser-typ (cons plats (packa-upp platser)))) 


(defconstant plats-typ 'plats) 
(defun plats? (objekt) (eq plats-typ (typ objekt))) 

(defun ny-platser (&rest platser)
  (mapc #'(lambda (plats) (typkontroll plats #'plats?)) platser) 
  (packa-ihop platser-typ platser)) 

(defun ny-platser-från-lista (lista-med-platser)
  (mapc #'(lambda (plats) (typkontroll plats #'plats?)) lista-med-platser) 
  (packa-ihop platser-typ lista-med-platser)) 

(defun plats-lista (platser) ;;Om programmet behöver mapcara eller sånt 
  (typkontroll platser #'platser?)
  (packa-upp platser))

(defun ny-plats (alma-namn &optional månadsnamn dag-i-månad)
  (packa-ihop
    plats-typ
    (cond (dag-i-månad (list alma-namn månadsnamn dag-i-månad))
	  (månadsnamn (list alma-namn månadsnamn))
	  (alma-namn (list alma-namn))))) 

(defun plats-år (plats)
  "Returnerar platsens almanamn"
  (typkontroll plats #'plats?) 
  (nth 0 (packa-upp plats))) 

(defun plats-månad (plats)
  "Returnerar platsens månadsnamn eller null"
  (typkontroll plats #'plats?) 
  (nth 1 (packa-upp plats))) 

(defun plats-dag (plats)
  "Returnerar platsens dag-i-månad eller null"
  (typkontroll plats #'plats?) 
  (nth 2 (packa-upp plats))) 

(defun plats-är-år? (plats) 
  (typkontroll plats #'plats?) 
  (= 1 (length (packa-upp plats)))) 

(defun plats-är-månad? (plats) 
  (typkontroll plats #'plats?) 
  (= 2 (length (packa-upp plats)))) 

(defun plats-är-dag? (plats) 
  (typkontroll plats #'plats?) 
  (= 3 (length (packa-upp plats)))) 

(defun plats->clim-data (plats &optional (årsalmanackor (hämta-årsalmanackor)))
  (typkontroll plats #'plats?)
  (plocka-ut (plats-år plats)
	     :månadsnamn (plats-månad plats)
	     :dag-i-månad (plats-dag plats)
	     :årsalmor årsalmanackor)) 

(defun platser->clim-möten (platser &key (filter #'(lambda (x) nil)))
  (typkontroll platser #'platser?)
  (remove-duplicates
    (reduce #'append
      (mapcar #'möten
	      (platser->clim-data platser :filter filter))))) 

(defun platser->clim-data (platser &key (filter #'(lambda (x) nil)))
  "Tar ett platser-objekt och returnerar en lista med de clim-data
  objekt som finns utpekade av platserna."
  (typkontroll platser #'platser?)
  (remove-duplicates
    (let ((årsalma (hämta-årsalmanackor)))
      (mapcar
	#'(lambda (plats) (plats->clim-data plats årsalma))
	(remove-if-not filter (plats-lista platser)))))) 

(defun platser->clim-år (platser)
  (typkontroll platser #'platser?)
  (platser->clim-data platser :filter #'plats-är-år?)) 

(defun platser->clim-månad (platser)
  (typkontroll platser #'platser?)
  (platser->clim-data platser :filter #'plats-är-månad?)) 

(defun platser->clim-dag (platser)
  (typkontroll platser #'platser?)
  (platser->clim-data platser :filter #'plats-är-dag?)) 


;;;;;; Allmänna funktioner för traversering av objekt mm ;;;;;;;

;; Utför en funktion på varje tupel i en lista, t.ex. funktion f
;; på '(a b c) så utförs (f a b) (f a c) (f b c). Dessa samlas
;; ihop med två funktioner, en "extern" och en "intern". I exemplet
;; ovan utfördes f först på huvudet A med svansen (B C), sedan på
;; huvudet B med svansen (C). Den interna samlingsfunktionen samlar
;; ihop resultaten av varje huvuds exekvering på sin svans, medan den
;; externa uppsamlaren samlar ihop resultaten av de resultaten.
(defun kvadratisk (binär-funktion
		    intern-uppsamlare
		    intern-grundfall
		    extern-uppsamlare
		    extern-grundfall
		    lista)
  (labels
    ((svansbehandlare (binär-funktion
			intern-uppsamlare
			intern-grundfall
			huvud
			svans)
		      (if (endp svans) intern-grundfall
			(funcall intern-uppsamlare
				 (funcall binär-funktion
					  huvud
					  (car svans))
				 (svansbehandlare
				   binär-funktion
				   intern-uppsamlare
				   intern-grundfall
				   huvud
				   (cdr svans))))))
    (if (>= 1 (length lista))
      extern-grundfall
      (funcall extern-uppsamlare (svansbehandlare
				   binär-funktion
				   intern-uppsamlare
				   intern-grundfall
				   (car lista)
				   (cdr lista))
	       (kvadratisk binär-funktion
			   intern-uppsamlare
			   intern-grundfall
			   extern-uppsamlare
			   extern-grundfall
			   (cdr lista))))))

(defun kvadratiskc (binär-funktion lista)
  (labels ((do-nothing (a b) nil))
          (kvadratisk
           binär-funktion
           #'do-nothing
           nil
           #'do-nothing
           nil
           lista)))

;; Returnerar resultatet av att köra fun på ett clos-objekts slot
(defun funcall-på-slot (objekt slot fun)
  (funcall fun (slot-value objekt slot)))

(defun slot-har-värde (objekt slot value)
  (if (equalp (slot-value objekt slot) value)
      value
      nil))

;;Kollar att ett clos-objekt har en slot med ett värde, och kan även
;;göra en djupare kontroll, där den ser till att en slot har minst ett
;;värde som satisfierar en funktion. Den antar alltså att djup-slot
;;innehåller en lista av saker, och utför funktionen på varje sak i
;;listan.
;; Om något av objekten i listan satisfierar funktionen (som
;;alltså returnerar något annat än null) returneras de satisfierande
;;objekten i en lista. Detta är mest till för att rekursivt leta efter
;;clos-objekt som ligger i andra clos-objekt.
(defun clos-objekt-check
  (objekt slot värde &optional (djup-slot nil ssup) (djup-fun nil fsup))
  (let ((rätt-värde (slot-har-värde objekt slot värde)))
       (if (or (not ssup) (not fsup)) ;;om ingen djupsök
           rätt-värde ;;returnera om det är rätt värde
           (and rätt-värde ;;annars kolla först om rätt värde
                (remove-if-not djup-fun ;;returnera sen resultat av djupsök
                               (slot-value objekt djup-slot))))))

;;;Smidig funktion för att plocka ut års, månads eller dagsalmanackor i clim-format ur varandra. Dessa kan tas från en lista med clim-årsalmor, eller från de alma-almanackor som finns definierade.
(defun plocka-ut
  (alma-namn &key månadsnamn dag-i-månad (årsalmor (hämta-årsalmanackor)))
  (let*
    ((clim-år (find-if #'(lambda (årsalma)
				 (equalp alma-namn
					 (slot-value årsalma 'namn)))
		       årsalmor))
     (clim-månad
       (when (and clim-år månadsnamn)
	 (find-if #'(lambda (månad) (equalp månadsnamn
					    (slot-value månad 'namn)))
		  (slot-value clim-år 'månader))))
     (clim-dag
       (when (and clim-månad dag-i-månad)
	 (nth (1- dag-i-månad) (slot-value clim-månad 'dagar)))))
    (or clim-dag clim-månad clim-år)))


(defun plocka-ut-månad (clim-år månadsnr)
  "Plockar ut en clos-månad ur en clos-år, månadsnr från 0 till 11"
  (unless (null clim-år)
          (nth månadsnr (slot-value clim-år 'månader))))

(defun plocka-ut-dag (clim-månad dagnr)
  (unless (null clim-månad)
          (nth dagnr (slot-value clim-månad 'dagar))))

(defun hitta-årsalma (årsnamn årsalmor)
  (find-if
   #'(lambda
      (årsalma)
      (slot-har-värde årsalma 'namn årsnamn))
   årsalmor))



;;;;;; Almanacksberäkningar ;;;;;;
;;;; Funktioner för att beräkna och bolla med alma-funktioner så
;;;; att vi får ut värden vi vill använda när vi skapar grafik.

(defmethod total-möteslängd ((clim-år clim-år))
  (reduce
   #'+
   (slot-value clim-år 'månader)
   :key #'total-möteslängd))

(defmethod total-möteslängd ((clim-månad clim-månad))
  (reduce
   #'+
   (slot-value clim-månad 'dagar)
   :key #'total-möteslängd))

(defmethod total-möteslängd ((clim-dag clim-dag))
  (let ((möten (slot-value clim-dag 'möten)))
       (if (null möten)
           0
           (räkna-ihop-möteslängder möten))))
 
(defun räkna-ihop-möteslängder (clim-möteslista)
  "Ger den totala längden av mötestiderna i clim-möteslista, i minuter"
  (reduce
   #'+
   clim-möteslista
   :key
   #'(lambda
      (clim-möte)
      (let* ((alma-möte (slot-value clim-möte 'alma-möte))
             (tp (tidsperioddel alma-möte))
             (tidsrymd (funcall (alma-kan-längd-av-tp) tp))
             (timmar (heltal (timdel tidsrymd)))
             (minuter (heltal (minutdel tidsrymd))))
            (+ minuter (* 60 timmar))))))

(defmethod möten-överlappar ((a clim-möte) (b clim-möte))
  (överlappar?
   (tidsperioddel (slot-value a 'alma-möte))
   (tidsperioddel (slot-value b 'alma-möte))))

(defmethod möten-överlapp ((a clim-möte) (b clim-möte))
  (let ((tidsperiod-a
	  (tidsperioddel (slot-value a 'alma-möte)))
	(tidsperiod-b
	  (tidsperioddel (slot-value b 'alma-möte))))
    (funcall (alma-kan-överlapp) tidsperiod-a tidsperiod-b)))

(defun alma-tp-starttid (tidsperiod)
  (klockslag-till-heltal (start-klockslag tidsperiod)))

(defun alma-tp-sluttid (tidsperiod)
  (klockslag-till-heltal (slut-klockslag tidsperiod)))

(defun tid-till-position (tidpunkt max-position)
  "Mappar en tidpunkt till ett nummer, så att nummret är lika nära
  max-position som tiden är slutet på dagen. T.ex. 12:00 100 -> 50"
  (labels 
   ((antal-minuter
     (tidpunkt)
     (multiple-value-bind (timmar minuter) (truncate tidpunkt 100)
                          (+ (* 60 timmar) minuter))))
   (* max-position (/ (antal-minuter tidpunkt) (* 24 60)))))


;Döda alla McClim-program som körs
;(progn (loop for port in climi::*all-ports* do (clim:destroy-port port)) (setq climi::*all-ports* nil))
