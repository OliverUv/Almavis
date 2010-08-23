(defpackage #:almavis
  (:use #:clim-lisp #:clim)
  (:export #:visa-grafiskt #:run-tests)
  (:import-from :stdalma
                :dagalmanacka
                :dagalmanacka?
                :första-mötestid
                :heltal
                :minutdel
                :månadsalmanacka
                :månadsalmanacka?
                :mötesdel
                :mötestext
                :omvandla-klockslag
		:packa-ihop
		:packa-upp
                :resten-dagalmanacka
                :skapa-dag
                :skapa-månad
                :slut-klockslag
                :start-klockslag
                :tidsperioddel
                :timdel
                :tom-dagalmanacka?
		:typ
                :typkontroll
                :årsalmanacka?
                :överlappar?
                :*almanacka*
                :*månadsdata*))
