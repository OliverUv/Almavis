(in-package #:almavis)
;;;;;; Kommunikation med almanackan ;;;;;;
;;;; Funktioner f�r att testa vilken funktionalitet
;;;; studenten har implementerat hittills

(defun alma-har-funktionalitet-p (funktionalitet-str)
  (find-symbol funktionalitet-str 'common-lisp-user))

(defun alma-kan-l�ngd-av-tp ()
  (alma-har-funktionalitet-p "L�NGD-AV-TIDSPERIOD"))

(defun alma-kan-tidsperioder ()
  (alma-har-funktionalitet-p "SKAPA-TIDSPERIODER"))

(defun alma-kan-skapa-tidsrymd ()
  (alma-har-funktionalitet-p "SKAPA-TIDSRYMD"))

(defun alma-kan-klockslag ()
  (and
   (alma-har-funktionalitet-p "START-KLOCKSLAG")
   (alma-har-funktionalitet-p "SLUT-KLOCKSLAG")))

(defun alma-kan-avboka-m�te ()
  (alma-har-funktionalitet-p "AVBOKA-M�TE"))

(defun alma-kan-j�mf�r ()
  (and (alma-har-funktionalitet-p "J�MF�R")
       (alma-har-funktionalitet-p "GEMENSAMMA-TIDER")))

(defun alma-kan-ledigt ()
  (and
   (alma-har-funktionalitet-p "LEDIGA-TIDSPERIODER")
   (alma-har-funktionalitet-p "LEDIGT")
   (alma-har-funktionalitet-p "SAMMA-LEDIGA-PERIODER")))

(defun alma-kan-�verlapp ()
  (alma-har-funktionalitet-p "�VERLAPP"))

;;;;;; Funktionsbindningar ;;;;;;
;;;; Vi vill anv�nda en hel del funktioner ifr�n almanackan.
;;;; Eftersom den inte �r definierad i samma paket s� beh�ver vi
;;;; f�r varje funktion ocks� specifiera vilket paket funktionen
;;;; ligger i. Det blir f�r mycket kod och f�r l�nga kodrader om
;;;; vi ska blanda i det i v�r kod, s� h�r importerar vi den
;;;; funktionaliteten. Detta g�r att vi sj�lva inte f�r definiera
;;;; funktioner eller globala variabler med samma namn.

;;;; I programmeringsmilj�n jag anv�nder g�rs detta i almavis
;;;; paketdefinition, men den kommer troligtvis inte vara med
;;;; n�r vi integrerar p� IDA - d� beh�vs dessa ist�llet.

(import 'common-lisp-user::dagalmanacka)
(import 'common-lisp-user::dagalmanacka?)
(import 'common-lisp-user::f�rsta-m�testid)
(import 'common-lisp-user::heltal)
(import 'common-lisp-user::minutdel)
(import 'common-lisp-user::m�nadsalmanacka)
(import 'common-lisp-user::m�nadsalmanacka?)
(import 'common-lisp-user::m�tesdel)
(import 'common-lisp-user::m�testext)
(import 'common-lisp-user::omvandla-klockslag)
(import 'common-lisp-user::packa-ihop)
(import 'common-lisp-user::packa-upp)
(import 'common-lisp-user::resten-dagalmanacka)
(import 'common-lisp-user::skapa-dag)
(import 'common-lisp-user::skapa-m�nad)
(import 'common-lisp-user::slut-klockslag)
(import 'common-lisp-user::start-klockslag)
(import 'common-lisp-user::tidsperioddel)
(import 'common-lisp-user::timdel)
(import 'common-lisp-user::tom-dagalmanacka?)
(import 'common-lisp-user::typ)
(import 'common-lisp-user::typkontroll)
(import 'common-lisp-user::�rsalmanacka?)
(import 'common-lisp-user::�verlappar?)
(import 'common-lisp-user::*almanacka*)
(import 'common-lisp-user::*m�nadsdata*)

(import 'common-lisp-user::*almanacka*)
(import 'common-lisp-user::*m�nadsdata*)

;;;;;; Datatyper ;;;;;;
;;;; H�r definierar vi de CLOS-objekt som vi sedan med Clim vill kunna
;;;; visualisera. Anledningen till att vi vill ha CLOS-objekten alls �r
;;;; f�r att Clim �r byggt med antagandet att man vill presentera just
;;;; CLOS-objekt. Detta g�r man genom att definiera presenters. Mer av
;;;; det syns d�r jag implementerar almavis-vyerna.

(defclass clim-�r
  ()
  ((alma-�r :accessor alma-�r :initarg :alma-�r) ;originaldata
   (m�nader :accessor m�nader :initarg :m�nader) ;lista med clim-m�nad
   (namn :accessor namn :initarg :namn))) ;t.ex. "pelles almanacka"

(defclass clim-m�nad
  ()
  ((alma-m�nad :accessor alma-m�nad :initarg :alma-m�nad) ;originaldata
   (dagar :accessor dagar :initarg :dagar) ;lista med clim-dag, SORTERAD
   (namn :accessor namn :initarg :namn) ;t.ex. "januari"
   (almanacksnamn :accessor almanacksnamn :initarg :almanacksnamn)
   ;t.ex. "pelles almanacka"
   (antal-dagar :accessor antal-dagar :initarg :antal-dagar))) ;t.ex. 31

(defclass clim-dag 
  ()
  ((alma-dag :accessor alma-dag :initarg :alma-dag) ;originaldata
   (m�ten :accessor m�ten :initarg :m�ten :reader m�ten) ;lista med clim-m�te
   (almanacksnamn :accessor almanacksnamn :initarg :almanacksnamn)
   ;t.ex. "pelles almanacka"
   (m�nadsnamn :accessor m�nadsnamn :initarg :m�nadsnamn)
   (dag-i-m�nad :accessor dag-i-m�nad :initarg :dag-i-m�nad))) ;t.ex. 1

(defclass clim-m�te 
  ()
  ((alma-m�te :accessor alma-m�te :initarg :alma-m�te) ;originaldata
   (start-kl :accessor start-kl :initarg :start-kl)
   (slut-kl :accessor slut-kl :initarg :slut-kl)
   ;integer mellan 0000 och 2359
   (almanacksnamn :accessor almanacksnamn :initarg :almanacksnamn)         
   ;t.ex. "pelles almanacka"
   (m�nadsnamn :accessor m�nadsnamn :initarg :m�nadsnamn)
   (dag-i-m�nad :accessor dag-i-m�nad :initarg :dag-i-m�nad)
   (m�tesinfo :accessor m�tesinfo :initarg :m�tesinfo)))

(defmethod m�ten ((clim-�r clim-�r))
  (reduce
    #'append
    (mapcar
      #'m�ten
      (slot-value clim-�r 'm�nader)))) 

(defmethod m�ten ((clim-m�nad clim-m�nad))
  (reduce
    #'append
    (mapcar
      #'m�ten
      (slot-value clim-m�nad 'dagar)))) 

(defmethod m�tesl�ngd ((clim-m�te clim-m�te))
  "Ger ett m�tes l�ngd i minuter, kr�ver att alma kan tp"
  (assert (alma-kan-l�ngd-av-tp)) 
  (let* ((alma-m�te (slot-value clim-m�te 'alma-m�te))
             (tp (tidsperioddel alma-m�te))
             (tidsrymd (funcall (alma-kan-l�ngd-av-tp) tp))
             (timmar (heltal (timdel tidsrymd)))
             (minuter (heltal (minutdel tidsrymd))))
            (+ minuter (* 60 timmar))))
 
(defmethod clim-�r-union ((a clim-�r) (b clim-�r))
  "Sl�r ihop tv� clim-�r till ett, resultatet f�r namn och alma-�r
  fr�n det f�rsta argumenet. Endast m�nadslistan sammans�tts fr�n
  b�da �ren."
  (make-instance
   'clim-�r
   :alma-�r (slot-value a 'alma-�r)
   :m�nader (mapcar #'clim-m�nad-union
                    (slot-value a 'm�nader)
                    (slot-value b 'm�nader))
   :namn (slot-value a 'namn)))

(defmethod clim-m�nad-union ((a clim-m�nad) (b clim-m�nad))
  (make-instance
   'clim-m�nad
   :alma-m�nad (slot-value a 'alma-m�nad)
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
   :m�ten (append (slot-value a 'm�ten) (slot-value b 'm�ten))
   :almanacksnamn (slot-value a 'almanacksnamn)
   :m�nadsnamn (slot-value a 'm�nadsnamn)
   :dag-i-m�nad (slot-value a 'dag-i-m�nad)))


;;;;;; Konvertering ;;;;;;
;; H�r har vi massor av kod f�r konvertering av almanackans datatyper
;; till CLOS-klasser. Funktionen h�mta-�rsalmanackor kommer att anropas av 
;; programmet, som sedan rekursivt g�r igenom alla �rsalmanackors m�nads-,
;; och dags-almanackor - h�mtar ut alla m�ten och stoppar dem i motsvarande
;; dataobjektshierarki av CLOS-objekt.

;; Notera att h�mta-�rsalmanackor konverterar till CLOS-objekt, medan
;; h�mta-m�nader och h�mta-dagar endast plockar ut relevanta objekt ur
;; en alma-�rsalmanacka och alma-m�nadsalmanacka, respektive, med hj�lp
;; av almanackans primitiver.

(defun h�mta-�rsalmanackor (&optional alma-namn-lista)
  "Returnerar en lista med clim-�r"
  (let ((konverterade-�r
	  (mapcar
	    #'(lambda (alma-tupel)
		(cond ((and alma-namn-lista
			    (not (member (car alma-tupel)
					 alma-namn-lista
					 :test #'equalp))) 
		       nil)
		      (t (konvertera-�r (car alma-tupel) (cdr alma-tupel)))))
	    ;car �r namnet p� almanackan, cdr �r alma-�rsalmanackan
	    *almanacka*))) 
    (remove-if #'null konverterade-�r)))

(defun h�mta-dagar (alma-m�nad dagar-i-m�nad)
  "Returnerar en lista med dagalmanackorna i en m�nad"
  (typkontroll alma-m�nad #'m�nadsalmanacka?)
  (let ((alma-dagalma-lista '()))
       (dotimes
        (i dagar-i-m�nad alma-dagalma-lista)
        (setq alma-dagalma-lista
              (cons (dagalmanacka (skapa-dag (- dagar-i-m�nad i)) alma-m�nad)
                    alma-dagalma-lista)))))

(defun h�mta-m�ten (alma-dag)
  "Returnerar en lista med m�testiderna i en dagalmanacka"
  (typkontroll alma-dag #'dagalmanacka?)
  (if (tom-dagalmanacka? alma-dag) '()
      (cons (f�rsta-m�testid alma-dag)
            (h�mta-m�ten (resten-dagalmanacka alma-dag)))))

(defun klockslag-till-heltal (alma-klockslag)
  (+
   (* 100 (heltal (timdel alma-klockslag)))
   (heltal (minutdel alma-klockslag))))

;;;; Funktioner f�r att konvertera

;;�r
(defun konvertera-�r (alma-namn alma-�r)
  (skapa-clim-�r
   alma-�r
   alma-namn
   (konvertera-m�nader alma-namn alma-�r)))

(defun skapa-clim-�r (alma-�r alma-namn clim-m�nader)
  (make-instance
   'clim-�r
   :alma-�r alma-�r
   :m�nader clim-m�nader
   :namn alma-namn))

;;M�nader - notera plural
(defun konvertera-m�nader (alma-namn alma-�r)
  (typkontroll alma-�r #'�rsalmanacka?)
  (mapcar
   #'(lambda (m�nadstupel)
             (let* ((m�nadsnamn (car m�nadstupel))
                  (dagar-i-m�nad (cdr m�nadstupel))
                  (alma-m�nad
                   (m�nadsalmanacka (skapa-m�nad m�nadsnamn) alma-�r)))
             (skapa-clim-m�nad
              alma-m�nad
              alma-namn
              m�nadsnamn
              dagar-i-m�nad
              (konvertera-dagar alma-namn
				m�nadsnamn
				alma-m�nad
				dagar-i-m�nad))))
   *m�nadsdata*))

(defun skapa-clim-m�nad
  (alma-m�nad alma-namn m�nadsnamn dagar-i-m�nad clim-dagar)
  (make-instance
   'clim-m�nad
   :alma-m�nad alma-m�nad
   :almanacksnamn alma-namn
   :antal-dagar dagar-i-m�nad
   :namn m�nadsnamn
   :dagar clim-dagar))

;;Dagar
(defun konvertera-dagar (alma-namn m�nadsnamn alma-m�nad dagar-i-m�nad)
  (typkontroll alma-m�nad #'m�nadsalmanacka?)
  (let ((dag-lista (h�mta-dagar alma-m�nad dagar-i-m�nad))
        (dag-nr 1))
       (mapcar #'(lambda (alma-dag)
                         (prog1
                          (skapa-clim-dag
                           alma-dag
                           alma-namn
			   m�nadsnamn
                           dag-nr
                           (konvertera-m�ten
			     alma-namn m�nadsnamn dag-nr alma-dag))
                          (setq dag-nr (1+ dag-nr))))
               dag-lista)))

(defun skapa-clim-dag (alma-dag alma-namn m�nadsnamn dagnummer clim-m�ten)
  (make-instance
   'clim-dag
   :alma-dag alma-dag
   :almanacksnamn alma-namn
   :m�nadsnamn m�nadsnamn
   :dag-i-m�nad dagnummer
   :m�ten clim-m�ten))

;;M�ten
(defun konvertera-m�ten (alma-namn m�nadsnamn dag-i-m�nad alma-dag)
  (let ((m�ten (h�mta-m�ten alma-dag)))
       (mapcar
        #'(lambda
           (m�te)
           (skapa-clim-m�te
            m�te
            alma-namn
	    m�nadsnamn
	    dag-i-m�nad
            (alma-tp-starttid (tidsperioddel m�te))
            (alma-tp-sluttid (tidsperioddel m�te))
            (m�testext (m�tesdel m�te))))
        m�ten)))

(defun skapa-clim-m�te
  (alma-m�te alma-namn m�nadsnamn dag-i-m�nad 
	     start-klockslag slut-klockslag m�testext)
  (make-instance
   'clim-m�te
   :alma-m�te alma-m�te
   :almanacksnamn alma-namn
   :m�nadsnamn m�nadsnamn
   :dag-i-m�nad dag-i-m�nad
   :start-kl start-klockslag
   :slut-kl slut-klockslag
   :m�tesinfo m�testext))


;;;;;; Datah�mtare ;;;;;;;
;; Anv�nds av almavis olika vyer f�r att veta vad f�r olika objekt de
;; ska visa. En datah�mtare best�r av en plats och flera datak�llor.
;; Platsen specifierar vilken m�nad och eventuellt datum som ska visas.
;; Datak�llorna talar om fr�n vilka �rsalmanackor vi vill visa informationen.
;; Datak�llorna �r en lista med almanacksnamn.

(defclass plats ()
  ((m�nad :initarg :m�nad
	  :initform (error "Plats m�ste ha m�nad")
	  :accessor plats-m�nad)
   (dag :initarg :dag
	:initform nil
	:accessor plats-dag))) 

(defun skapa-plats (m�nad &key (dag nil))
  (make-instance 'plats :m�nad m�nad :dag dag)) 

(defmethod m�nad-plats ((clim-m�nad clim-m�nad))
  (skapa-plats (slot-value clim-m�nad 'namn))) 

(defmethod dag-plats ((clim-dag clim-dag))
  (skapa-plats (slot-value clim-dag 'm�nadsnamn)
	       :dag (slot-value clim-dag 'dag-i-m�nad))) 

(defmethod m�te-plats ((clim-m�te clim-m�te))
  (skapa-plats (slot-value clim-m�te 'm�nadsnamn)
	       :dag (slot-value clim-m�te 'dag-i-m�nad))) 

(defclass datah�mtare ()
  ((plats ;:type plats
	  :initarg :plats
	  :initform nil
	  :accessor plats)
   (datak�llor :initarg :datak�llor ;;lista med almanacksnamn
	       :initform (error "Datak�llor f�r datah�mtare m�ste initieras.")
	       :accessor datak�llor))) 

(defmethod toggle-datak�lla ((datah�mtare datah�mtare) datak�lla)
  (if (null datak�lla) datah�mtare
    (let* ((dh datah�mtare)
	   (datak�llor (slot-value dh 'datak�llor)))
      (setf (slot-value dh 'datak�llor)
	    (if (member datak�lla datak�llor :test #'equalp)
	      (remove datak�lla datak�llor :test #'equalp)
	      (cons datak�lla datak�llor)))
      dh))) 

(defmethod byt-plats ((datah�mtare datah�mtare) plats)
  (let ((dh datah�mtare))
    (setf (slot-value dh 'plats) plats)
    dh)) 

(defmethod skriv-ut-datak�llor ((datah�mtare datah�mtare))
  (let*
    ((almanackor-visas (slot-value datah�mtare 'datak�llor))
     (almanackor-ej-visas
       (remove-if
	 #'(lambda (almanacka)
	     (member almanacka almanackor-visas))
	 (mapcar #'car common-lisp-user::*almanacka*))))
    (format t "Visar almanackor: ~A~%Visar ej:         ~A~%"
	    almanackor-visas
	    almanackor-ej-visas))) 

(defmethod datah�mtare->clim-data
  ((datah�mtare datah�mtare) &key (m�nad nil))
  (with-slots (plats datak�llor) datah�mtare 
    (cond
      ((null plats) (plocka-ut datak�llor)) 
      ((or m�nad (null (plats-dag plats)))
       (plocka-ut datak�llor
		  :m�nadsnamn (plats-m�nad plats)))
      (t (plocka-ut datak�llor
		    :m�nadsnamn (plats-m�nad plats)
		    :dag-i-m�nad (plats-dag plats))))))

(defmethod datah�mtare->clim-m�ten
  ((datah�mtare datah�mtare))
  (remove-duplicates
    (reduce #'append
	    (mapcar #'m�ten
		    (datah�mtare->clim-data datah�mtare))))) 

(defmethod datah�mtare->clim-m�nad ((datah�mtare datah�mtare))
  "Med denna funktion kan man h�mta m�nadsobjekt �ven om datah�mtarens plats
  �r en dag."
  (datah�mtare->clim-data datah�mtare :m�nad t)) 

(defmethod plats-antal-dagar ((plats plats))
  (if (null (plats-m�nad plats)) 0 
    (cdr (assoc (plats-m�nad plats)
		*m�nadsdata*
		:test #'equalp)))) 

;;;;;; Allm�nna funktioner f�r traversering av objekt mm ;;;;;;;

;; Utf�r en funktion p� varje tupel i en lista, t.ex. funktion f
;; p� '(a b c) s� utf�rs (f a b) (f a c) (f b c). Dessa samlas
;; ihop med tv� funktioner, en "extern" och en "intern". I exemplet
;; ovan utf�rdes f f�rst p� huvudet A med svansen (B C), sedan p�
;; huvudet B med svansen (C). Den interna samlingsfunktionen samlar
;; ihop resultaten av varje huvuds exekvering p� sin svans, medan den
;; externa uppsamlaren samlar ihop resultaten av de resultaten.
(defun kvadratisk (bin�r-funktion
		    intern-uppsamlare
		    intern-grundfall
		    extern-uppsamlare
		    extern-grundfall
		    lista)
  (labels
    ((svansbehandlare (bin�r-funktion
			intern-uppsamlare
			intern-grundfall
			huvud
			svans)
		      (if (endp svans) intern-grundfall
			(funcall intern-uppsamlare
				 (funcall bin�r-funktion
					  huvud
					  (car svans))
				 (svansbehandlare
				   bin�r-funktion
				   intern-uppsamlare
				   intern-grundfall
				   huvud
				   (cdr svans))))))
    (if (>= 1 (length lista))
      extern-grundfall
      (funcall extern-uppsamlare (svansbehandlare
				   bin�r-funktion
				   intern-uppsamlare
				   intern-grundfall
				   (car lista)
				   (cdr lista))
	       (kvadratisk bin�r-funktion
			   intern-uppsamlare
			   intern-grundfall
			   extern-uppsamlare
			   extern-grundfall
			   (cdr lista))))))

(defun kvadratiskc (bin�r-funktion lista)
  (labels ((do-nothing (a b) nil))
          (kvadratisk
           bin�r-funktion
           #'do-nothing
           nil
           #'do-nothing
           nil
           lista)))

;; Returnerar resultatet av att k�ra fun p� ett clos-objekts slot
(defun funcall-p�-slot (objekt slot fun)
  (funcall fun (slot-value objekt slot)))

(defun slot-har-v�rde (objekt slot value)
  (if (equalp (slot-value objekt slot) value)
      value
      nil))

;;Kollar att ett clos-objekt har en slot med ett v�rde, och kan �ven
;;g�ra en djupare kontroll, d�r den ser till att en slot har minst ett
;;v�rde som satisfierar en funktion. Den antar allts� att djup-slot
;;inneh�ller en lista av saker, och utf�r funktionen p� varje sak i
;;listan.
;; Om n�got av objekten i listan satisfierar funktionen (som
;;allts� returnerar n�got annat �n null) returneras de satisfierande
;;objekten i en lista. Detta �r mest till f�r att rekursivt leta efter
;;clos-objekt som ligger i andra clos-objekt.
(defun clos-objekt-check
  (objekt slot v�rde &optional (djup-slot nil ssup) (djup-fun nil fsup))
  (let ((r�tt-v�rde (slot-har-v�rde objekt slot v�rde)))
       (if (or (not ssup) (not fsup)) ;;om ingen djups�k
           r�tt-v�rde ;;returnera om det �r r�tt v�rde
           (and r�tt-v�rde ;;annars kolla f�rst om r�tt v�rde
                (remove-if-not djup-fun ;;returnera sen resultat av djups�k
                               (slot-value objekt djup-slot))))))

;;;Smidig funktion f�r att plocka ut �rs, m�nads eller dagsalmanackor i clim-format ur varandra. Dessa kan tas fr�n en lista med clim-�rsalmor, eller fr�n de alma-almanackor som finns definierade.
(defun plocka-ut
  (datak�llor ;;lista med almanacksnamn
    &key m�nadsnamn dag-i-m�nad
    (�rsalmor (h�mta-�rsalmanackor datak�llor)))
  (mapcar
    #'(lambda (clim-�r)
	(let*
	  ((clim-m�nad
	     (when m�nadsnamn
	       (find-if #'(lambda (m�nad) (equalp m�nadsnamn
						  (slot-value m�nad 'namn)))
			(slot-value clim-�r 'm�nader))))
	   (clim-dag
	     (when (and clim-m�nad dag-i-m�nad)
	       (nth (1- dag-i-m�nad) (slot-value clim-m�nad 'dagar)))))
	  (or clim-dag clim-m�nad clim-�r)))
    �rsalmor))


(defun plocka-ut-m�nad (clim-�r m�nadsnr)
  "Plockar ut en clos-m�nad ur en clos-�r, m�nadsnr fr�n 0 till 11"
  (unless (null clim-�r)
          (nth m�nadsnr (slot-value clim-�r 'm�nader))))

(defun plocka-ut-dag (clim-m�nad dagnr)
  (unless (null clim-m�nad)
          (nth dagnr (slot-value clim-m�nad 'dagar))))

(defun hitta-�rsalma (�rsnamn �rsalmor)
  (find-if
   #'(lambda
      (�rsalma)
      (slot-har-v�rde �rsalma 'namn �rsnamn))
   �rsalmor))



;;;;;; Almanacksber�kningar ;;;;;;
;;;; Funktioner f�r att ber�kna och bolla med alma-funktioner s�
;;;; att vi f�r ut v�rden vi vill anv�nda n�r vi skapar grafik.

(defmethod total-m�tesl�ngd ((clim-�r clim-�r))
  (reduce
   #'+
   (slot-value clim-�r 'm�nader)
   :key #'total-m�tesl�ngd))

(defmethod total-m�tesl�ngd ((clim-m�nad clim-m�nad))
  (reduce
   #'+
   (slot-value clim-m�nad 'dagar)
   :key #'total-m�tesl�ngd))

(defmethod total-m�tesl�ngd ((clim-dag clim-dag))
  (let ((m�ten (slot-value clim-dag 'm�ten)))
       (if (null m�ten)
           0
           (r�kna-ihop-m�tesl�ngder m�ten))))

(defun r�kna-ihop-m�tesl�ngder (clim-m�teslista)
  "Ger den totala l�ngden av m�testiderna i clim-m�teslista, i minuter"
  (reduce
   #'+
   clim-m�teslista
   :key
   #'m�tesl�ngd))

(defmethod m�ten-�verlappar ((a clim-m�te) (b clim-m�te))
  (�verlappar?
   (tidsperioddel (slot-value a 'alma-m�te))
   (tidsperioddel (slot-value b 'alma-m�te))))

(defmethod m�ten-�verlapp ((a clim-m�te) (b clim-m�te))
  (assert (alma-kan-�verlapp)) 
  (let ((tidsperiod-a
	  (tidsperioddel (slot-value a 'alma-m�te)))
	(tidsperiod-b
	  (tidsperioddel (slot-value b 'alma-m�te))))
    (funcall (alma-kan-�verlapp) tidsperiod-a tidsperiod-b)))

(defun alma-tp-starttid (tidsperiod)
  (klockslag-till-heltal (start-klockslag tidsperiod)))

(defun alma-tp-sluttid (tidsperiod)
  (klockslag-till-heltal (slut-klockslag tidsperiod)))

(defun tid-till-position (tidpunkt max-position)
  "Mappar en tidpunkt till ett nummer, s� att nummret �r lika n�ra
  max-position som tiden �r slutet p� dagen. T.ex. 12:00 100 -> 50"
  (labels 
   ((antal-minuter
     (tidpunkt)
     (multiple-value-bind (timmar minuter) (truncate tidpunkt 100)
                          (+ (* 60 timmar) minuter))))
   (* max-position (/ (antal-minuter tidpunkt) (* 24 60)))))
