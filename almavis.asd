(asdf:defsystem #:almavis
  :depends-on (#:mcclim #:stdalma)
  :components ((:file "package")
               (:file "almavis"
                      :depends-on ("package"))
               (:file "almavis-grafik"
                      :depends-on ("package"
                                   "almavis"))
               (:file "almavis-ar"
                      :depends-on ("package"
                                   "almavis"
                                   "almavis-grafik"))
               (:file "almavis-manad"
                      :depends-on ("package"
                                   "almavis"
                                   "almavis-grafik"))
               (:file "unit-tests"
                      :depends-on ("package"
                                   "almavis"
                                   "almavis-grafik"
				   "almavis-manad"
                                   "almavis-ar"))))
