(in-package #:scenic)

;; TODO: support 32 bit texture screenshots

(defun 16bit-texture-p (texture)
  (let ((element-type (symbol-name (texture-element-type texture))))
    (or (serapeum:string*= "16" element-type)
        (serapeum:string*= "32" element-type))))

(defun map-range (a1 a2 b1 b2 s)
  (+ b1
     (/ (* (- s a1)
	   (- b2 b1))
	(- a2 a1))))

(defun cpu-1-tone-map-reinhard (color1 exposure)
  (declare (type single-float color1 exposure))
  (let* ((col (* color1 exposure))
         (r   (/ col (+ 1f0 col))))
    (cpu-1-linear-to-srgb r)))

(defun cpu-1-linear-to-srgb (c)
  (expt c #.(/ 2.2)))

(defun cpu-1-tone-map-acesfilm (x exposure)
  (let ((x (* exposure x))
        (a 2.51)
        (b 0.03)
        (c 2.43)
        (d 0.59)
        (e 0.14))
    (cpu-1-linear-to-srgb
     (/ (* x (+ b (* a x)))
        (+ e (* x (+ d (* c x))))))))

(defun get-current-exposure ()
  (exposure (post (current-scene))))

(defun range-function-for-texture (texture)
  "returns an one(1) arg lambda that performs the value mapping"
  (let ((current-exposure (get-current-exposure)))
    (if (16bit-texture-p texture)
        (lambda (val)
          (round
           ;;(map-range 0f0 1f0 0 65535 (cpu-1-tone-map-reinhard val current-exposure))
           (map-range 0f0 1f0 0 65535 (cpu-1-tone-map-acesfilm val current-exposure))
           ))
        (lambda (val)
          (round
           (map-range   0 255 0 65535 val))))))

(defun filename-extension (filename)
  "returns the extension of filename, lowecased"
  (let ((filestring (if (pathnamep filename)
                        (file-namestring filename)
                        filename)))
    (alexandria:lastcar (cl-ppcre:split "[.]" (string-downcase filestring)))))

(defun texture-to-png (texture filename)
  "NOTE: cl-png, builds a lisp array, so we can rely on cpu side tone/color/value mapping"
  (let* ((f          (range-function-for-texture texture))
         (dimensions (dimensions texture))
         (width      (first  dimensions))
         (height     (second dimensions))
         (png-image  (png:make-image height width 3 16)))
    (with-free carray (pull1-g (texref texture))
      (loop :for h :below height
            :for hw :from (1- height) :downto 0
            :do (loop :for w :below width
                      :for color4 := (aref-c carray w hw)
                      :do (setf (aref png-image h w 0) (funcall f (x color4))
                                (aref png-image h w 1) (funcall f (y color4))
                                (aref png-image h w 2) (funcall f (z color4))))))
    (with-open-file (stream filename :element-type '(unsigned-byte 16)
                                     :direction :output
                                     :if-exists :supersede)
      (png:encode png-image stream))))

(defun filename-to-save-function (filename &key)
  "returns a 2 arg function, which takes a tex and a filename"
  (alexandria:eswitch ((filename-extension filename) :test #'string=)
    ("png" #'texture-to-png)
    ("bmp" #'dirt:save-as-image)
    ("tga" #'dirt:save-as-image)
    ("dds" #'dirt:save-as-image)))

(defun texture-to-disk (filename texture)
  (declare (type cepl:texture texture))
  (let ((f (filename-to-save-function filename)))
    (funcall f texture filename)))

(defun screen-to-disk (filename)
  "take what is already rendered on the main fbo and save it"
  (texture-to-disk filename (first (tex (current-camera)))))
