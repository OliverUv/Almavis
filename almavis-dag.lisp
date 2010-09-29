(in-package #:almavis-dag)

;; Hack f�r att importera allt i almavis-paketet, eftersom det inte finns
;; n�got s�tt att importera alla symboler (�ven icke-exporterade), och inte
;; heller n�got s�tt att exportera alla symboler p�.
(do-symbols (symb :almavis) (import symb)) 

(defclass dag-view (view) ())
(defparameter +dag-vy+ (make-instance 'dag-view)) 
(defun rita-dag (frame pane)
  "Ritar ut en dag."
  (let*
    ((datah�mtare (slot-value frame 'datah�mtare))
     (plats (slot-value datah�mtare 'plats)) 
     (m�ten-att-visa (datah�mtare->clim-m�ten datah�mtare)))
    (terpri)
    (format t "Den ~A~A ~A~%"
	    (plats-dag plats)
	    (if (> 3 (plats-dag plats)) "a" "e") 
	    (plats-m�nad plats))
    (skriv-ut-datak�llor (slot-value frame 'datah�mtare)) 
    (skriv-ut-totala-m�tesl�ngder m�ten-att-visa)
    ;(rita-ut-tidslinjer frame) 
    (rita-ut-m�ten (partitionera-m�ten m�ten-att-visa) frame)))

#|(defun rita-ut-tidslinjer (str�m)
  (loop
    for i from 0 below 24 do 
    with x = px-tidslinje-padding
    for y = (* i ) 
    (setf (stream-cursor-position str�m)
	  (values ))))|# 

(defun partitionera-m�ten (m�ten &optional (resultat nil))
  (labels
    ((sortera-in-m�te
       (m�te m�teslista)
       (cond ((null m�teslista) (cons (list m�te) m�teslista))
	     ((finns-�verlapp? m�te (car m�teslista))
	      (cons (car m�teslista) (sortera-in-m�te m�te (cdr m�teslista))))
	     (t (cons (cons m�te (car m�teslista)) (cdr m�teslista))))))
    (if (null m�ten) resultat
      (partitionera-m�ten
	(cdr m�ten) 
	(sortera-in-m�te (car m�ten) resultat))))) 

(defun rita-ut-m�ten (partitionerade-m�ten str�m)
  (format t "M�ten: ~A~%" partitionerade-m�ten))
