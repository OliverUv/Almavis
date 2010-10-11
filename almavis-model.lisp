(in-package #:almavis)
;;;;;; Kommunikation med almanackan ;;;;;;
;;;; Funktioner för att testa vilken funktionalitet
;;;; studenten har implementerat hittills. Används både
;;;; för att kolla om funktionaliten finns och för att
;;;; hämta funktionsobjekten för att använda funktionaliteten.

(defun alma-har-funktionalitet-p (funktionalitet-str)
  (if
    (fboundp (find-symbol funktionalitet-str 'common-lisp-user))
    (find-symbol funktionalitet-str 'common-lisp-user)
    nil))

(defun alma-kan-längd-av-tp ()
  (alma-har-funktionalitet-p "LÄNGD-AV-TIDSPERIOD"))

(defun alma-kan-skapa-tidsperioder ()
  (alma-har-funktionalitet-p "SKAPA-TIDSPERIODER"))

(defun alma-kan-första-tidsperiod ()
  (or (alma-har-funktionalitet-p "FÖRSTA-TIDSPERIOD")
      (alma-har-funktionalitet-p "FÖRSTA-TIDSPERIODEN"))) 

(defun alma-kan-resten-tidsperioder ()
  (alma-har-funktionalitet-p "RESTEN-TIDSPERIODER"))

(defun alma-kan-tom-tidsperioder ()
  (alma-har-funktionalitet-p "TOM-TIDSPERIODER?"))

(defun alma-kan-tidsperioder ()
  (and (alma-kan-skapa-tidsperioder)
       (alma-kan-tom-tidsperioder) 
       (alma-kan-första-tidsperiod)
       (alma-kan-resten-tidsperioder)))

(defun alma-kan-skapa-tidsrymd ()
  (alma-har-funktionalitet-p "SKAPA-TIDSRYMD"))

(defun alma-kan-klockslag ()
  (and
   (alma-har-funktionalitet-p "START-KLOCKSLAG")
   (alma-har-funktionalitet-p "SLUT-KLOCKSLAG")))

(defun alma-kan-avboka-möte ()
  (alma-har-funktionalitet-p "AVBOKA-MÖTE"))

(defun alma-kan-jämför ()
  (and (alma-kan-ledigt)
       (alma-har-funktionalitet-p "SAMMA-LEDIGA-PERIODER"))) 

(defun alma-kan-ledigt ()
  (and
    (alma-kan-överlapp) 
    (alma-har-funktionalitet-p "LEDIGA-TIDSPERIODER")))

(defun alma-kan-överlapp ()
  (alma-kan-tidsperioder) 
  (alma-har-funktionalitet-p "ÖVERLAPP"))

(defun alma-ta-bort-funktionalitet (fun-lista &optional (res nil))
  (cond
    ((endp fun-lista) res)
    ((equalp (car fun-lista) 'o)
     (alma-ta-bort-funktionalitet
       (rest fun-lista)
       (cons (fmakunbound 'common-lisp-user::överlapp) res)))
    ((equalp (car fun-lista) 't)
     (alma-ta-bort-funktionalitet
       (rest fun-lista)
       (cons (fmakunbound 'common-lisp-user::längd-av-tidsperiod) res)))
    ((equalp (car fun-lista) 'l)
     (alma-ta-bort-funktionalitet
       (rest fun-lista)
       (cons (fmakunbound 'common-lisp-user::lediga-tidsperioder) res)))
    ((equalp (car fun-lista) 'j)
     (alma-ta-bort-funktionalitet
       (rest fun-lista)
       (cons (fmakunbound 'common-lisp-user::samma-lediga-perioder) res)))
    (t (format t "Kan inte ta bort ~A" (car fun-lista))
       nil)))

;;;;;; Funktionsbindningar ;;;;;;
;;;; Vi vill använda en hel del funktioner ifrån almanackan.
;;;; Eftersom den inte är definierad i samma paket så behöver vi
;;;; för varje funktion också specifiera vilket paket funktionen
;;;; ligger i. Det blir för mycket kod och för långa kodrader om
;;;; vi ska blanda i det i vår kod, så här importerar vi den
;;;; funktionaliteten. Detta gör att vi själva inte får definiera
;;;; funktioner eller globala variabler med samma namn.

(import 'common-lisp-user::dagalmanacka)
(import 'common-lisp-user::dagalmanacka?)
(import 'common-lisp-user::första-mötestid)
(import 'common-lisp-user::heltal)
(import 'common-lisp-user::minutdel)
(import 'common-lisp-user::månadsalmanacka)
(import 'common-lisp-user::månadsalmanacka?)
(import 'common-lisp-user::mötesdel)
(import 'common-lisp-user::mötestext)
(import 'common-lisp-user::omvandla-klockslag)
(import 'common-lisp-user::packa-ihop)
(import 'common-lisp-user::packa-upp)
(import 'common-lisp-user::resten-dagalmanacka)
(import 'common-lisp-user::skapa-dag)
(import 'common-lisp-user::skapa-klockslag)
(import 'common-lisp-user::skapa-minut)
(import 'common-lisp-user::skapa-månad)
(import 'common-lisp-user::skapa-timme)
(import 'common-lisp-user::slut-klockslag)
(import 'common-lisp-user::start-klockslag)
(import 'common-lisp-user::tidsperioddel)
(import 'common-lisp-user::timdel)
(import 'common-lisp-user::tom-dagalmanacka?)
(import 'common-lisp-user::typ)
(import 'common-lisp-user::typkontroll)
(import 'common-lisp-user::årsalmanacka?)
(import 'common-lisp-user::överlappar?)

(import 'common-lisp-user::*almanacka*)
(import 'common-lisp-user::*månadsdata*)

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

(defmethod möteslängd ((clim-möte clim-möte))
  "Ger ett mötes längd i minuter, kräver att alma kan tp"
  (assert (alma-kan-längd-av-tp)) 
  (let* ((alma-möte (slot-value clim-möte 'alma-möte))
             (tp (tidsperioddel alma-möte))
             (tidsrymd (funcall (alma-kan-längd-av-tp) tp))
             (timmar (heltal (timdel tidsrymd)))
             (minuter (heltal (minutdel tidsrymd))))
            (+ minuter (* 60 timmar))))

(defmethod överlappande-möten ((clim-möte clim-möte) andra-möten)
  "Returnerar en lista med de mötena i andra-möten som överlappar clim-möte."
  (remove-if-not
    #'(lambda (annat-möte)
	(möten-överlappar annat-möte clim-möte))
    andra-möten)) 
 
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

(defun hämta-årsalmanackor (&optional alma-namn-lista)
  "Returnerar en lista med clim-år"
  (let ((konverterade-år
	  (mapcar
	    #'(lambda (alma-tupel)
		(cond ((and alma-namn-lista
			    (not (member (car alma-tupel)
					 alma-namn-lista
					 :test #'equalp))) 
		       nil)
		      (t (konvertera-år (car alma-tupel) (cdr alma-tupel)))))
	    ;car är namnet på almanackan, cdr är alma-årsalmanackan
	    *almanacka*))) 
    (remove-if #'null konverterade-år)))

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


;;;;;; Datahämtare ;;;;;;;
;; Används av almavis olika vyer för att veta vad för olika objekt de
;; ska visa. En datahämtare består av en plats och flera datakällor.
;; Platsen specifierar vilken månad och eventuellt datum som ska visas.
;; Datakällorna talar om från vilka årsalmanackor vi vill visa informationen.
;; Datakällorna är en lista med almanacksnamn.

(defclass plats ()
  ((månad :initarg :månad
	  :initform (error "Plats måste ha månad")
	  :accessor plats-månad)
   (dag :initarg :dag
	:initform nil
	:accessor plats-dag))) 

(defun skapa-plats (månad &key (dag nil))
  (make-instance 'plats :månad månad :dag dag)) 

(defmethod månad-plats ((clim-månad clim-månad))
  (skapa-plats (slot-value clim-månad 'namn))) 

(defmethod dag-plats ((clim-dag clim-dag))
  (skapa-plats (slot-value clim-dag 'månadsnamn)
	       :dag (slot-value clim-dag 'dag-i-månad))) 

(defmethod möte-plats ((clim-möte clim-möte))
  (skapa-plats (slot-value clim-möte 'månadsnamn)
	       :dag (slot-value clim-möte 'dag-i-månad))) 

(defmethod specifiera-plats ((plats plats) &key månad dag)
  (skapa-plats
    (if månad månad (plats-månad plats))
    :dag (if dag dag (plats-dag plats)))) 

(defclass datahämtare ()
  ((plats ;:type plats
	  :initarg :plats
	  :initform nil
	  :accessor plats)
   (datakällor :initarg :datakällor ;;lista med almanacksnamn
	       :initform (error "Datakällor för datahämtare måste initieras.")
	       :accessor datakällor))) 

(defun datakälla-finns? (almanacksnamn)
  (member almanacksnamn *almanacka* :key #'car)) 

(defun månad-finns? (månadsnamn)
  (member månadsnamn *månadsdata* :key #'car)) 

(defun månad-har-dag? (månadsnamn dagsnummer)
  (<= dagsnummer (cdr (assoc månadsnamn *månadsdata*)))) 

(defmethod toggle-datakälla ((datahämtare datahämtare) datakälla)
  (if (null datakälla) datahämtare
    (let* ((dh datahämtare)
	   (datakällor (slot-value dh 'datakällor)))
      (setf (slot-value dh 'datakällor)
	    (if (member datakälla datakällor :test #'equalp)
	      (remove datakälla datakällor :test #'equalp)
	      (cons datakälla datakällor)))
      dh))) 

(defmethod byt-plats ((datahämtare datahämtare) plats)
  (let ((dh datahämtare))
    (setf (slot-value dh 'plats) plats)
    dh)) 

(defmethod antal-datakällor ((datahämtare datahämtare))
  (length (slot-value datahämtare 'datakällor))) 

(defmethod skriv-ut-datakällor ((datahämtare datahämtare))
  (let*
    ((almanackor-visas (slot-value datahämtare 'datakällor))
     (almanackor-ej-visas
       (remove-if
	 #'(lambda (almanacka)
	     (member almanacka almanackor-visas))
	 (mapcar #'car common-lisp-user::*almanacka*))))
    (format t "Visar almanackor: ~A~%Visar ej:         ~A~%"
	    almanackor-visas
	    almanackor-ej-visas))) 

;Hämtar ut clim-data från en datahämtare. Hämtar datan som
;specifieras av platsen i datahämtaren. Om :månad t så hämtar
;den ut månaden även om det är en dag specifierad.
(defmethod datahämtare->clim-data
  ((datahämtare datahämtare) &key (månad nil))
  (with-slots (plats datakällor) datahämtare 
    (cond
      ((null plats) (plocka-ut datakällor)) 
      ((or månad (null (plats-dag plats)))
       (plocka-ut datakällor
		  :månadsnamn (plats-månad plats)))
      (t (plocka-ut datakällor
		    :månadsnamn (plats-månad plats)
		    :dag-i-månad (plats-dag plats))))))

(defmethod datahämtare->clim-möten
  ((datahämtare datahämtare))
  (remove-duplicates
    (reduce #'append
	    (mapcar #'möten
		    (datahämtare->clim-data datahämtare))))) 

(defmethod datahämtare->clim-månad ((datahämtare datahämtare))
  "Med denna funktion kan man hämta månadsobjekt även om datahämtarens plats
  är en dag."
  (datahämtare->clim-data datahämtare :månad t)) 

(defclass clim-ledighet
  ()
  ((tidsperiod :accessor tidsperiod :initarg :tidsperiod))) 

(defun skapa-ledigheter (alma-tidsperioder)
  (assert (alma-kan-tidsperioder)) 
  (if (funcall (alma-kan-tom-tidsperioder) alma-tidsperioder)
    nil
    (cons
      (make-instance
	'clim-ledighet
	:tidsperiod (funcall (alma-kan-första-tidsperiod)
			     alma-tidsperioder))
      (skapa-ledigheter
	(funcall (alma-kan-resten-tidsperioder)
		 alma-tidsperioder))))) 

(defmethod ledighet ((datahämtare datahämtare))
  "Hämtar ut de lediga tiderna för en datakälla i en datahämtare,
  för den dag som datahämtarens plats pekar ut.
  Returnerar en lista med clim-ledigperiod objekt."
  (assert (alma-kan-ledigt)) 
  (with-slots (datakällor plats) datahämtare
    (assert (= 1 (length datakällor)))
    (assert (and (plats-månad plats) (plats-dag plats))) 
    (let* ((clim-dag (car (datahämtare->clim-data datahämtare)))
	  (alma-dag (slot-value clim-dag 'alma-dag))
	  (start-kl (skapa-klockslag (skapa-timme 0) (skapa-minut 0)))
	  (slut-kl (skapa-klockslag (skapa-timme 23) (skapa-minut 59))))
      (skapa-ledigheter (funcall (alma-kan-ledigt)
				 alma-dag start-kl slut-kl))))) 

(defmethod gemensam-ledighet ((datahämtare datahämtare))
  "Hämtar ut de gemensamma lediga tiderna för alla datakällor i
  datahämtaren, för den dagen som datahämtarens plats pekar ut.
  Returnerar en lista med clim-ledighperiod objekt."
  (assert (alma-kan-jämför)) 
  (with-slots (datakällor plats) datahämtare
    (assert (and (plats-månad plats) (plats-dag plats)))
    (let*
      ((clim-dagar (datahämtare->clim-data datahämtare))
       (start-kl (skapa-klockslag (skapa-timme 0) (skapa-minut 0)))
       (slut-kl (skapa-klockslag (skapa-timme 23) (skapa-minut 59)))
       (lediga-tidsperioder
	 (mapcar
	   #'(lambda (clim-dag)
	       (funcall (alma-kan-ledigt)
			(slot-value clim-dag 'alma-dag)
			start-kl
			slut-kl))
	   clim-dagar)))
      (skapa-ledigheter
	(reduce
	  #'(lambda (tp1 tp2)
	      (funcall (alma-kan-jämför) tp1 tp2))
	  lediga-tidsperioder))))) 

(defmethod plats-antal-dagar ((plats plats))
  (if (null (plats-månad plats)) 0 
    (cdr (assoc (plats-månad plats)
		*månadsdata*
		:test #'equalp)))) 

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
;Hämtar ut en clim-dag, en clim-månad eller ett clim-år från varje
;årsalmanacka som specifieras av datakällor.
(defun plocka-ut
  (datakällor ;;lista med almanacksnamn
    &key månadsnamn dag-i-månad
    (årsalmor (hämta-årsalmanackor datakällor)))
  (mapcar
    #'(lambda (clim-år)
	(let*
	  ((clim-månad
	     (when månadsnamn
	       (find-if #'(lambda (månad) (equalp månadsnamn
						  (slot-value månad 'namn)))
			(slot-value clim-år 'månader))))
	   (clim-dag
	     (when (and clim-månad dag-i-månad)
	       (nth (1- dag-i-månad) (slot-value clim-månad 'dagar)))))
	  (or clim-dag clim-månad clim-år)))
    årsalmor))


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
   #'möteslängd))

(defmethod möten-överlappar ((a clim-möte) (b clim-möte))
  (överlappar?
   (tidsperioddel (slot-value a 'alma-möte))
   (tidsperioddel (slot-value b 'alma-möte))))

(defmethod möten-överlapp ((a clim-möte) (b clim-möte))
  (assert (alma-kan-överlapp)) 
  (let ((tidsperiod-a
	  (tidsperioddel (slot-value a 'alma-möte)))
	(tidsperiod-b
	  (tidsperioddel (slot-value b 'alma-möte))))
    (funcall (alma-kan-överlapp) tidsperiod-a tidsperiod-b)))

(defmethod finns-överlapp? ((clim-möte clim-möte) möteslista)
  (find-if
    #'(lambda (möte) (möten-överlappar clim-möte möte))
    möteslista))

(defun alma-tp-starttid (tidsperiod)
  (klockslag-till-heltal (start-klockslag tidsperiod)))

(defun alma-tp-sluttid (tidsperiod)
  (klockslag-till-heltal (slut-klockslag tidsperiod)))
