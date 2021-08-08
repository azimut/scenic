(uiop:define-package #:scenic
  (:use #:cepl
        #:cepl.skitter
        #:cl
        #:livesupport
        #:nineveh
        #:rtg-math
        #:vari
        #:with-setf)
  (:export #:start)
  (:import-from #:temporal-functions
                #:make-stepper
                #:seconds)
  (:import-from #:arrow-macros
                #:->)
  (:import-from #:alexandria
                #:maphash-values
                #:maphash-keys
                #:hash-table-keys
                #:positive-fixnum
                #:emptyp
                #:when-let
                #:when-let*
                #:if-let
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:serapeum
                #:op
                #:do-each
                #:vect
                #:random-in-range
                #:class-name-of))
