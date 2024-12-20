(in-package #:scenic)

(defvar *meshes* (make-hash-table :test #'equal))
(defvar *samplers* (make-hash-table :test #'equal)
  "GPU objects loaded")
(defvar *c-samplers* (make-hash-table :test #'equal)
  "C array objects loaded")

;;--------------------------------------------------
;; From cepl.tests

(defmacro with-free (name thing &body body)
  (let ((name (or name (gensym "thing"))))
    `(let ((,name ,thing))
       (unwind-protect (progn ,@body)
         (free ,name)))))

(defmacro with-free* (bindings &body body)
  `(let* ,bindings
     (unwind-protect (progn ,@body)
       ,@(loop :for (name) :in bindings :collect
                  `(free ,name)))))

;;------------------------------------------------------------
;; Helpers for tangent-space calculations
;; FIXME: They seem wrong on spheres, probably on everything else too?

(defstruct-g tb-data
  (tangent :vec3)
  (bitangent :vec3))

;; From play-with-verts, based on:
;; https://learnopengl.com/Advanced-Lighting/Normal-Mapping
(defun calc (verts i0 i1 i2)
  "returns a list pair of tangent and bitangent"
  (let* ((pos1 (pos (aref-c verts i0)))
         (pos2 (pos (aref-c verts i1)))
         (pos3 (pos (aref-c verts i2)))
         (uv1  (tex (aref-c verts i0)))
         (uv2  (tex (aref-c verts i1)))
         (uv3  (tex (aref-c verts i2)))
         ;;
         (edge1 (v3:- pos2 pos1))
         (edge2 (v3:- pos3 pos1))
         (delta-uv1 (v2:- uv2 uv1))
         (delta-uv2 (v2:- uv3 uv1))
         ;;
         (f (/ 1.0 (- (* (x delta-uv1) (y delta-uv2))
                      (* (x delta-uv2) (y delta-uv1)))))
         ;;
         (tangent1
           (v3:normalize
            (v! (* f (- (* (y delta-uv2) (x edge1))
                        (* (y delta-uv1) (x edge2))))
                (* f (- (* (y delta-uv2) (y edge1))
                        (* (y delta-uv1) (y edge2))))
                (* f (- (* (y delta-uv2) (z edge1))
                        (* (y delta-uv1) (z edge2)))))))
         (bitangent1
           (v3:normalize
            (v! (* f (+ (* (- (x delta-uv2)) (x edge1))
                        (*    (x delta-uv1)  (x edge2))))
                (* f (+ (* (- (x delta-uv2)) (y edge1))
                        (*    (x delta-uv1)  (y edge2))))
                (* f (+ (* (- (x delta-uv2)) (z edge1))
                        (*    (x delta-uv1)  (z edge2))))))))
    (list tangent1 bitangent1)))

(defun tbdata-from-vertex-and-indices (g-verts g-indices)
  (let* ((verts   (pull1-g g-verts))
         (indices (pull-g g-indices))
         (result  (make-gpu-array
                   nil :dimensions (first (dimensions verts))
                   :element-type 'tb-data)))
    (with-gpu-array-as-c-array (data result)
      (loop :for (i0 i1 i2)
              :on indices
                :by #'cdddr
            :do (let ((pair (calc verts i0 i1 i2)))
                  (setf (aref-c data i0) pair)
                  (setf (aref-c data i1) pair)
                  (setf (aref-c data i2) pair))))
    result))

;;------------------------------------------------------------
;; Meshes
;;
;; We cache the data based on the the arguments so we don't
;; get lots of instances in memory

(defun sphere (&optional (radius 1f0) (lines-of-latitude 30) (lines-of-longitude 30) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list radius
                   lines-of-latitude
                   lines-of-longitude
                   has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:sphere-gpu-arrays
             :radius radius
             :lines-of-latitude lines-of-latitude
             :lines-of-longitude lines-of-longitude)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun box (&optional (w 1f0) (h 1f0) (d 1f0) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list w h d has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:box-gpu-arrays :width w
                                                         :height h
                                                         :depth d)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun cylinder (&optional (radius .5f0) (height 1f0) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list radius height has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                              :height height)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun cone (&optional (radius 1f0) (height 1f0) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list radius height has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cone-gpu-arrays :radius radius
                                                          :height height)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun lattice (&optional (width 100f0) (height 100f0) (x 500) (y 500) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list :lat width height x y has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:lattice-gpu-arrays
             :width width
             :height height
             :x-segments x
             :y-segments y)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

;;----------------------------------------
;; Dirt - image loader into CEPL sampler

(defvar *system-path* nil)
(defvar *system-resolved* nil)

(defun resolve-path (file)
  ;; Cache :scenic asdf location, it takes 3sec otherwise
  (when (not *system-resolved*)
    (setf *system-resolved* T)
    (when (asdf:find-system :scenic)
      (setf *system-path* (asdf:system-source-directory :scenic))))
  ;; Try different locations
  (flet ((relative-to-project (f)
           (or (arrow-macros:some-> (uiop:getenv "APPDIR")
                 (uiop/pathname:subpathname* f)
                 (probe-file))
               (arrow-macros:some-> *system-path*
                 (uiop/pathname:subpathname* f)
                 (probe-file)))))
    (or (uiop:absolute-pathname-p file)
        (probe-file file)
        (relative-to-project file)
        (relative-to-project (merge-pathnames "static/" (serapeum:path-basename file)))
        (error "Could not resolve-path of: ~a" file))))

(defun resolve-paths (&rest args)
  (mapcar #'resolve-path args))

(defun list-tex ()
  (alexandria:maphash-keys #'print *samplers*))

(defun free-texs ()
  (maphash-values (lambda (s) (free (sampler-texture s)))
                  *samplers*)
  (clrhash *samplers*))

(defun get-tex (path &optional (force nil) (mipmap t) (image-format :rgba8))
  (log4cl:log-info "loading texture" path)
  (when force
    (let ((s (gethash path *samplers*)))
      (when s
        (free (sampler-texture s)))
      (remhash path *samplers*)))
  (or (gethash path *samplers*)
      (setf (gethash path *samplers*)
            (cepl:sample
             (dirt:load-image-to-texture
              (resolve-path path)
              image-format
              mipmap
              t)))))

;;--------------------------------------------------

(defun free-all-c-tex ()
  (maphash-values #'free *c-samplers*)
  (clrhash *c-samplers*))

(defun get-c-tex (path
                  &rest args
                  &key (force nil) (image-format :rgb)
                  &allow-other-keys)
  (when force
    (let ((s (gethash path *c-samplers*)))
      (when s
        (free s))
      (remhash path *c-samplers*)))
  (or (gethash path *c-samplers*)
      (setf (gethash path *c-samplers*)
            (apply
             #'dirt:load-image-to-c-array
             (resolve-path path)
             image-format
             args))))
