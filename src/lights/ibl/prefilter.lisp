(in-package #:scenic)

(defclass prefilter (capture)
  ((buf       :initarg :buf)
   (roughness :initarg :roughness
              :accessor roughness))
  (:default-initargs
   :roughness 0.8
   :buf (box)
   :dim '(128 128)
   :texture-opts '(( 0 :cubes t :element-type :rgb16f :mipmap 5)
                   (:d :cubes t :element-type :depth-component24))
   :sample-opts '((:wrap           :clamp-to-edge
                   :magnify-filter :linear)
                  (:wrap           :clamp-to-edge
                   :magnify-filter :linear
                   :minify-filter  :linear-mipmap-linear)))
  (:documentation "prefilter cubemap, for specular calculation"))

(defmethod (setf roughness) :before (new-value (obj prefilter))
  (check-type new-value (float 0 1)))
(defmethod (setf roughness) :after (_ (obj prefilter))
  (setf (drawp obj) T))

(defun-g prefilter-frag ((frag-pos :vec4) &uniform (sam :sampler-cube) (roughness :float))
  (let* ((n (normalize (s~ frag-pos :xyz)))
         ;; make the simplyfying assumption that V equals R equals the normal
         (r n)
         (v r)
         (total-weight 0f0)
         (prefiltered-color (vec3 0f0)))
    (dotimes (i 1024)
      (let* ((xi (hammersley-nth-2d 1024 i))
             (h (importance-sample-ggx xi n roughness))
             (l (normalize (- (* 2 h (dot v h)) v)))
             (n-dot-l (max (dot n l) 0f0)))
        (when (> n-dot-l 0)
          (let* ((d (distribution-ggx n h roughness))
                 (n-dot-h (max (dot n h) 0))
                 (h-dot-v (max (dot h v) 0))
                 (pdf (+ .0001 (/ (* d n-dot-h)
                                  (* 4 h-dot-v))))
                 (resolution 512)
                 (sa-texel (/ (* 4 +pi+)
                              (* 6 resolution resolution)))
                 (sa-sample (/ (+ .0001 (* pdf 1024))))
                 (mip-level (if (= 0 roughness)
                                0f0
                                (* .5 (log2 (/ sa-sample sa-texel))))))
            (incf prefiltered-color
                  (* (s~ (texture-lod sam l mip-level) :xyz)
                     n-dot-l))
            (incf total-weight n-dot-l)))))
    (divf prefiltered-color (vec3 total-weight))
    (v! prefiltered-color 1)))

(defpipeline-g prefilter-pipe ()
  :vertex   (irradiance-vert g-pnt)
  :geometry (irradiance-geom)
  :fragment (prefilter-frag :vec4))

(defmethod draw ((scene scene-ibl) (camera prefilter) time)
  (log4cl:log-info "drawing prefilter")
  (with-setf* ((resolution (current-viewport)) (v! 128 128)
               (depth-test-function) #'<=
               (cull-face) :front)
    (with-slots (buf roughness fbo ubo) camera
      (map-g #'prefilter-pipe buf
             :roughness roughness
             :projections ubo
             :world (model->world camera)
             :sam (first (sam (capture scene)))))))

(defun make-prefilter (&rest args)
  (apply #'make-instance 'prefilter args))
