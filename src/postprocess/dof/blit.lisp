(in-package #:scenic)

(defmethod blit (scene (postprocess dof) (camera defered) time)
  (declare (ignore scene time))
  (with-slots (radius distance range kernel bs render-bokeh render-coc render-coc-half)
      postprocess
    (with-setf* ((depth-test-function) #'always)
      (map-g-into (fbo render-coc) #'coc-pipe bs
                  :near          (near camera)
                  :far           (far  camera)
                  :samd          (fifth (sam camera)) ;; !!!
                  :bokeh-radius   radius
                  :focus-distance distance
                  :focus-range    range)
      (map-g-into (fbo render-coc-half) #'coc-prefilter-pipe bs
                  :sam           (first (sam (prev *state*)))
                  :coc-sam       (first (sam render-coc)))
      (map-g-into (fbo render-bokeh) #'bokeh-pipe bs
                  :scene         (first (sam render-coc-half))
                  :dof-kernel     kernel
                  :bokeh-radius   radius)
      (map-g-into (fbo render-coc-half) #'bokeh-postfilter-pipe bs
                  :sam           (first (sam render-bokeh))))
    (map-g #'dof-combine-pipe bs
           :sam       (first (sam (prev *state*)))
           :coc-sam   (first (sam render-coc))
           :coc-h-sam (first (sam render-coc-half)))))
