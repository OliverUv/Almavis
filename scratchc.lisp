(define-presentation-method
  present
  (clim-month (type clim-month) stream (view år-view) &key)
  (princ ;;print month name and booked time
    (format nil "~A: ~A h"
	    (slot-value clim-month 'name)
	    (if (calendar-can-calc-length)
	      (truncate (meeting-length-total clim-month) 60)
	      "?"))) 
  (terpri) ;;new row
  (with-local-coordinates
    (stream)
    (with-drawing-options ;;draw a white background block so the entire area can be clicked
      (stream :ink empty-day-colour)
      (draw-rectangle* stream 0 0
		       (+ (* 2 px-month-padding) 
			  (* (1- days-per-row) px-between-days) 
			  (* days-per-row (+ 2 day-width))) ;;plus two for the border
		       (+ (* 2 px-month-padding)
			  (* (1- rows-per-month) px-between-rows) 
			  (* rows-per-month (+ 2 px-day-height)))));;plus two for the border
    (formatting-table ;;En table with the days in it
      (stream :x-spacing `(,px-between-days :pixel) :y-spacing `(,px-between-rows :pixel))
      (build-table (i 0 31 8 stream)
		   (draw-day-rectangle stream (procure-day clim-month i))))))

;;Build-table is a macro that automatically wraps a &body in calls to formatting-row
;;and formatting-cell as appropriate. Here, this is done for numbers 0 to 31, with
;;eight cells per row.

;;The call to the presenter is itself embedded within another formatting table, which
;;is drawn by the draw-year function.

;;The function draw-day-rectangle draws a small rectangle filled with colour to represent
;;the each specific day. I does this using draw-rectangle*

(define-application-frame
  yeartest
  () ;Superclasses
  ((data-collector :initarg :data-collector :accessor data-collector)) ;Slots
  (:panes
    (year :application
	:max-height 700 :height 700
	:max-width 1000 :width 1000
	:background app-bg-colour
	:display-function #'draw-year))
  (:layouts (default year)))

(define-yeartest-command com-go-to-month
		       ((clim-month 'clim-month))
		       (REACTTEST))
;;REACTTEST increments a counter by one, this is how I check if the click goes through
;;It never does :( please help.

(define-presentation-to-command-translator
  go-to-month
  (clim-month com-go-to-month yeartest
	      :gesture :select
	      :documentation "Go to the month view.")
  (object)
  (list object)) ;;I have also tried doing (progn (REACTTEST) (list object)) here, no luck
