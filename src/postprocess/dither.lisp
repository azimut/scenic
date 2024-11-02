(in-package #:scenic)

(defclass dither (postprocess)
  ((dithers   :documentation "list of samplers of dither patterns")
   (precision :initarg       :precision
              :accessor       dither-precision
              :documentation "color depth"))
  (:default-initargs
   :precision 32f0)
  (:documentation "https://github.com/jmickle66666666/PSX-Dither-Shader"))

(defun make-dither (&rest args)
  (apply #'make-instance 'dither args))

(defmethod (setf dither-precision) :before (new-value (obj dither))
  (check-type new-value single-float))

(defmethod initialize-instance :before ((obj dither) &key precision)
  (check-type precision single-float))

(defmethod initialize-instance :after ((obj dither) &key)
  (flet ((sample-dither (path)
           (sample
            (sampler-texture
             (get-tex path t t :r8))
            :minify-filter :nearest :magnify-filter :nearest)))
    (setf (slot-value obj 'dithers)
          (list (sample-dither "static/dither/dither1.png")
                (sample-dither "static/dither/dither2.png")
                (sample-dither "static/dither/psx_dither.png")))))

(defmethod dither-next ((obj dither))
  (with-slots (dithers) obj
    (setf dithers (alexandria:rotate dithers))))

(defmethod dither-prev ((obj dither))
  (with-slots (dithers) obj
    (setf dithers (alexandria:rotate dithers -1))))

(defun-g channel-error
    ((col     :float)
     (col-min :float)
     (col-max :float))
  (let ((range  (abs (- col-min col-max)))
        (arange (abs (- col     col-min))))
    (/ arange range)))

(defun-g dithered-channel
    ((err             :float)
     (dither-block-uv :vec2)
     (dither-steps    :float)
     (dither-pattern  :sampler-2d))
  (let* ((err (/ (floor (* err dither-steps))
                 dither-steps))
         (dither-uv (v! err 0))
         (dither-uv (v! (+ (x dither-uv) (x dither-block-uv))
                        (y dither-block-uv))))
    (x (texture dither-pattern dither-uv))))

(defun-g rgb->yuv ((rgba :vec4))
  (let* ((yuva (vec4 0 0 0 0))
         (yuva (v! (+ (* (x rgba) .2126)
                      (* (y rgba) .7152)
                      (* (z rgba) .0722))
                   0 0 0))
         (yuva (v! (x yuva)
                   (/ (- (y rgba) (x yuva)) 1.8556)
                   0 0))
         (yuva (v! (x yuva)
                   (y yuva)
                   (/ (- (x rgba) (x yuva)) 1.5748)
                   0))
         (yuva (v! (x yuva)
                   (+ (s~ yuva :yz) .5)
                   (w rgba))))
    yuva))

(defun-g yuv->rgb ((yuva :vec4))
  (let ((yuva (v! (x yuva) (- (s~ yuva :yz) .5) (w yuva))))
    (v! (+ (x yuva) (* (y yuva)  0.0)      (* (z yuva)  1.5748))
        (+ (x yuva) (* (y yuva) -0.187324) (* (z yuva) -0.468124))
        (+ (x yuva) (* (y yuva)  1.8556))
        (w yuva))))

(defun-g psx-dither
    ((sam            :sampler-2d)
     (uv             :vec2)
     (precision      :float)
     (dither-texture :sampler-2d))
  (let* ((yuv  (rgb->yuv (texture sam uv)))
         ;; Clamp the YUV color to specified color depth (default: 32, 5 bits per channel, as per playstation hardware)
         (col1 (/ (floor (* yuv precision)) precision))
         (col2 (/ (ceil  (* yuv precision)) precision))
         ;; Calculate dither texture UV based on the input texture
         (dither-texture-size (* 1f0 (texture-size dither-texture 0)))
         (dither-size         (y dither-texture-size))
         (dither-steps        (/ (x dither-texture-size) dither-size))
         ;;
         (main-texture-size   (* 1f0 (texture-size sam 0)))
         ;;
         (dither-block-uv uv)
         (dither-block-uv (v! (/ (mod (x dither-block-uv)
                                      (/ dither-size (x main-texture-size)))
                                 (/ dither-size (x main-texture-size)))
                              (/ (mod (y dither-block-uv)
                                      (/ dither-size (y main-texture-size)))
                                 (/ dither-size (y main-texture-size)))))
         (dither-block-uv (v! (/ (x dither-block-uv) dither-steps)
                              (/ (y dither-block-uv)
                                 (/ dither-size (y main-texture-size)))))
         ;; Dither each channel individually
         (yuv (v! (mix (x col1) (x col2)
                       (dithered-channel (channel-error (x yuv) (x col1) (x col2))
                                         dither-block-uv
                                         dither-steps
                                         dither-texture))
                  (mix (y col1) (y col2)
                       (dithered-channel (channel-error (y yuv) (y col1) (y col2))
                                         dither-block-uv
                                         dither-steps
                                         dither-texture))
                  (mix (z col1) (z col2)
                       (dithered-channel (channel-error (z yuv) (z col1) (z col2))
                                         dither-block-uv
                                         dither-steps
                                         dither-texture))
                  (w yuv))))
    (yuv->rgb yuv)))

(defun-g dither-frag
    ((uv         :vec2)
     &uniform
     (precision  :float)
     (sam        :sampler-2d)
     (dither-sam :sampler-2d))
  (psx-dither sam uv precision dither-sam))

(defpipeline-g dither-pipe (:points)
  :fragment (dither-frag :vec2))

(defmethod blit (scene (postprocess dither) camera time)
  (declare (ignore scene camera time))
  (with-slots (bs dithers precision) postprocess
    (map-g #'dither-pipe bs
           :precision precision
           :dither-sam (first dithers)
           :sam (first (sam (prev *state*))))))
