(in-package #:scenic)
;;--------------------------------------------------
;; Assimp - 3d object loader
;;
;; TODO: support more generic way to load that only returns the buffer
;; TODO: c-array for bones leaking memory on restart
;; TODO: fix the animation loop so it does it automatically...might be with a helper, is really manual now
;; TODO: scale bone transform

(defvar *default-albedo* "static/Blender-UVMap-Grid-1-K.jpg"
  "override this locally to change the albedo")

(defvar *assimp-buffers* (make-hash-table :test #'equal))

(defparameter *processing-flags*
  '(:ai-process-triangulate
    :ai-process-flip-u-vs ;; doc says for D3D but is useful for me too (?)
    ))

(defmethod initialize-instance :after ((obj assimp-thing-with-bones) &key scene)
  (with-slots (scene-offset bones-unique bones-transforms) obj
    (setf scene-offset     (serapeum:~> scene (ai:root-node) (ai:transform)))
    (setf bones-unique     (list-bones-unique scene))
    (setf bones-transforms (make-array (length bones-unique)))))

;; NOTE: same to a "g-pnt + tb-data", used for the CPU accessors
(defstruct-g assimp-mesh
  (pos       :vec3 :accessor pos)
  (normal    :vec3)
  (uv        :vec2)
  (tangent   :vec3)
  (bitangent :vec3))

;; Might be I could have used the 1 without filling the other bone slots?
(defstruct-g assimp-with-bones
  (pos           :vec3 :accessor pos)
  (normal        :vec3 :accessor norm)
  (uv            :vec2 :accessor tex)
  (tangent       :vec3 :accessor tangent)
  (bitangent     :vec3 :accessor bitangent)
  (bones-ids     :vec4 :accessor ids)
  (bones-weights :vec4 :accessor weights))

;;--------------------------------------------------
;; Pretty printers
;;--------------------------------------------------

(defmethod print-object ((obj ai::node-animation) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "~w" (ai:node-name obj))))

(defmethod print-object ((obj ai::quat-key) out)
  (print-unreadable-object (obj out :type t)
    (with-slots ((time ai::time)) obj
      (format out "~$" time))))

(defmethod print-object ((obj ai:vertex-weight) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a ~a" (ai:id obj) (ai:weight obj))))

(defmethod print-object ((obj ai:node) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (ai:name obj))))

(defmethod print-object ((obj ai:bone) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (ai:name obj))))

(defmethod print-object ((obj ai::vector-key) out)
  (print-unreadable-object (obj out :type t)
    (format out "~$" (slot-value obj 'ai::time))))

;;--------------------------------------------------
;; Loaders
;;--------------------------------------------------

(defmethod free ((actor assimp-thing-with-bones))
  (with-slots (bones) actor
    (free bones)))

(defun free-assimp-buffers ()
  (alexandria:maphash-values
   (lambda (buffer)
     (free buffer)
     (free (alexandria:lastcar (buffer-stream-gpu-arrays buffer)))
     (free (car (car (buffer-stream-gpu-arrays buffer)))))
   *assimp-buffers*)
  (clrhash *assimp-buffers*))

;;--------------------------------------------------

(defun make-gpu-index-array (faces n-vertex)
  "returns a CEPL:GPU-ARRAY, intended to be the index of a buffer-stream of mesh of
   FACES with N-VERTEX"
  (let* ((n-faces (* n-vertex 3))
         (i-array (make-gpu-array NIL :dimensions n-faces :element-type :ushort)))
    (with-gpu-array-as-c-array (c-arr i-array)
      (loop :for indices :across faces
            :for i :from 0 :by 3
            :do (setf (aref-c c-arr i)       (aref indices 0)
                      (aref-c c-arr (+ i 1)) (aref indices 1)
                      (aref-c c-arr (+ i 2)) (aref indices 2)))
      i-array)))

;; TODO: in the classimp version, it won't generate tangents if there are no uvs?
(defun make-gpu-vertex-array (vertices normals tangents bitangents &optional uvs bones)
  "returns a CEPL:GPU-ARRAY, built from the provided mesh information"
  (let* ((n-vertex  (length vertices))
         (has-bones (not (emptyp bones)))
         (has-uvs   (not (emptyp uvs)))
         (uvs       (or uvs ;; NOTE: we fill with fake uvs to respect g-pnt layout
                        (coerce (loop :repeat n-vertex
                                      :collect (v! 0 0))
                                'vector))))
    (cond (has-bones
           (s:lret ((v-arr (make-gpu-array
                            NIL
                            :dimensions n-vertex
                            :element-type 'assimp-with-bones)))
             (with-gpu-array-as-c-array (c-arr v-arr)
               (loop :for v  :across vertices
                     :for n  :across normals
                     :for ta :across tangents
                     :for bt :across bitangents
                     :for tc :across uvs
                     :for bv :across bones ;; per vertex
                     :for i  :from 0
                     :for a  := (aref-c c-arr i)
                     :do
                        (setf (assimp-mesh-uv        a) (v! (x tc) (y tc))
                              (assimp-mesh-normal    a) n
                              (assimp-mesh-tangent   a) ta
                              (assimp-mesh-bitangent a) bt)
                        (setf (pos a) v)
                        (setf (ids a)
                              (v! (serapeum:pad-end
                                   (map 'vector #'car bv) *max-bones-per-vertex* 0)))
                        (setf (weights a)
                              (v! (serapeum:pad-end
                                   (map 'vector #'cdr bv) *max-bones-per-vertex* 0)))))))
          (has-uvs
           (s:lret ((v-arr (make-gpu-array NIL :dimensions n-vertex :element-type 'assimp-mesh)))
             (with-gpu-array-as-c-array (c-arr v-arr)
               (loop :for v  :across vertices
                     :for n  :across normals
                     :for ta :across tangents
                     :for bt :across bitangents
                     :for tc :across uvs
                     :for i  :from 0
                     :for a  := (aref-c c-arr i)
                     :do (setf (pos a) v)
                         (setf (assimp-mesh-uv        a) (v! (x tc) (y tc))
                               (assimp-mesh-normal    a) n
                               (assimp-mesh-tangent   a) ta
                               (assimp-mesh-bitangent a) bt)))))
          (t
           (s:lret ((v-arr (make-gpu-array NIL :dimensions n-vertex :element-type 'g-pnt)))
             (with-gpu-array-as-c-array (c-arr v-arr)
               (loop :for v  :across vertices
                     :for n  :across normals
                     :for tc :across uvs
                     :for i  :from 0
                     :for a  := (aref-c c-arr i)
                     :do (setf (pos a) v
                               (assimp-mesh-uv     a) (v! (x tc) (y tc))
                               (assimp-mesh-normal a) n))))))))

(defun make-buffer-stream-cached (file mesh-index vertices faces normals tangents bitangents &optional uvs bones)
  "returns a CEPL:BUFFER-STREAM"
  (declare (type fixnum mesh-index))
  (let ((key (cons file mesh-index)))
    (or (gethash key *assimp-buffers*)
        (let* ((v-arr  (make-gpu-vertex-array vertices normals tangents bitangents uvs bones))
               (i-arr  (make-gpu-index-array faces (length vertices)))
               (buffer (make-buffer-stream v-arr :index-array i-arr)))
          (setf (gethash key *assimp-buffers*)
                buffer)))))

(defun get-texture-path (material key)
  "returns a string with the path to the image file for KEY in MATERIAL hash"
  (declare (type hash-table material) (type symbol key))
  (when-let* ((textures (gethash "$tex.file" material))
              (filepath (third (assoc key textures))))
    ;; Lastly try to remove windows paths
    (if (cl-ppcre:scan "\\" filepath)
        (alexandria:lastcar
         (cl-ppcre:split "\\" filepath))
        filepath)))

;;--------------------------------------------------

(defun relative-to (path to)
  (let* ((dirname (uiop:pathname-directory-pathname to))
         (basename (serapeum:path-basename path)))
    (merge-pathnames dirname basename)))

(defun assimp-save-embed-textures (scene objfile &aux (textures (ai:textures scene)))
  "saves embeded textures, in the same directory where OBJFILE is at"
  (when textures
    (loop :for texture :across textures :do
      (destructuring-bind (&key data filename &allow-other-keys) (nthcdr 1 texture)
        (let ((savepath (relative-to filename objfile)))
          (with-open-file
              (stream
               savepath
               :direction :output
               :element-type '(unsigned-byte 8)
               :if-exists nil ; FIXME: check filesize?
               :if-does-not-exist :create)
            (when stream
              (write-sequence data stream)
              (log:info (format nil "Saved embeded file: ~a" savepath)))))))))

(defun assimp-get-textures (mesh scene objfile)
  (declare (ai:mesh mesh) (ai:scene scene))
  (let* ((material-index (ai:material-index mesh))
         (material (aref (ai:materials scene) material-index))
         (albedo (let ((path (get-texture-path material :ai-texture-type-diffuse)))
                   (get-tex (if-let ((path (relative-to path objfile)))
                              path
                              *default-albedo*)
                            nil t :rgb8)))
         (normals (when-let* ((path (or (get-texture-path material :ai-texture-type-normals)
                                        (get-texture-path material :ai-texture-type-height))))
                    (get-tex (relative-to path objfile) nil t :rgb8)))
         (specular (when-let* ((path (get-texture-path material :ai-texture-type-specular)))
                     (get-tex (relative-to path objfile) nil t :r8)))
         (roughmap (when-let* ((path (get-texture-path material :ai-texture-type-shininess)))
                     (get-tex (relative-to path objfile) nil t :r8))))
    `(:albedo ,albedo
      :normals ,normals
      :roughmap ,roughmap
      :specular ,specular)))

(defgeneric assimp-mesh-to-stream (mesh scene file type))

(defmethod assimp-mesh-to-stream (mesh scene file (type (eql :untextured)))
  "only textured assimp thing"
  (declare (ai:mesh mesh)
           (ai:scene scene))
  (with-slots ((vertices ai:vertices)
               (normals  ai:normals)
               (faces    ai:faces))
      mesh
    (let ((mesh-index (position mesh (ai:meshes scene))))
      (assert (length= normals vertices))
      `(:buf
        ,(make-buffer-stream-cached
          file mesh-index vertices faces normals nil nil)))))

(defmethod assimp-mesh-to-stream (mesh scene file (type (eql :textured)))
  "only textured assimp thing"
  (declare (ai:mesh mesh) (ai:scene scene))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals)
               (texture-coords ai:texture-coords)
               (tangents       ai:tangents)
               (bitangents     ai:bitangents))
      mesh
    (let* ((mesh-index (position mesh (ai:meshes scene)))
           (uvs        (elt texture-coords 0)))
      (assert (length= bitangents tangents normals vertices uvs))
      (assimp-save-embed-textures scene file)
      (setf (ai:textures scene) #()) ;; we are done with them
      (append
       `(:buf
         ,(make-buffer-stream-cached
           file mesh-index vertices faces normals tangents bitangents uvs))
       (assimp-get-textures mesh scene file)))))

(defmethod assimp-mesh-to-stream (mesh scene file (type (eql :bones)))
  "returns an assimp actor object"
  (declare (ai:mesh mesh) (ai:scene scene))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals)
               (texture-coords ai:texture-coords)
               (tangents       ai:tangents)
               (bitangents     ai:bitangents)
               (mat-index      ai:material-index)
               (bones          ai:bones))
      mesh
    (let* ((mesh-index (position mesh (ai:meshes scene)))
           (uvs        (elt texture-coords 0))
           ;; I need context for this...hackidy hack
           (bones-per-vertex (get-bones-per-vertex scene bones (length vertices))))
      (assert (length= bitangents tangents normals vertices uvs))
      (assimp-save-embed-textures scene file)
      (setf (ai:textures scene) #()) ;; we are done with them
      (append
       `(:buf
         ,(make-buffer-stream-cached
           file mesh-index vertices faces normals tangents bitangents uvs bones-per-vertex))
       (assimp-get-textures mesh scene file)))))

(defun assimp-safe-import-into-lisp (file)
  "wrapper around ai:import-into-lisp, attempts to return a valid scene"
  (let* ((scene (or (let ((ai::*translate-verbose* t))
                      (ai:import-into-lisp file))
                    (error "cannot simple load the file")))
         (processing-flags ;; try to ensure normals
           (if (emptyp (ai:normals (aref (ai:meshes scene) 0)))
               (cons :ai-process-gen-smooth-normals *processing-flags*)
               *processing-flags*))
         (processing-flags ;; try to ensure tangents/bitangents
           (if (emptyp (ai:tangents (aref (ai:meshes scene) 0)))
               (cons :ai-process-calc-tangent-space processing-flags)
               processing-flags))
         (scene
           (ai:import-into-lisp file :processing-flags (print (remove-duplicates processing-flags))
                                ;; :properties
                                ;; '(:pp-slm-triangle-limit 25000)
                                ;;'(:pp-slm-vertex-limit 20000)
                                )))
    ;; TODO: error instead if there is an untextured among textured
    ;; Error if all texture coords are missing :(
    #+nil
    (assert (notevery #'zerop
                      (map 'vector (lambda (mesh) (length (ai:texture-coords mesh)))
                           (ai:meshes scene))))
    scene))

(defgeneric assimp-get-type (obj))
(defmethod assimp-get-type ((obj ai:scene))
  (let ((bones (list-bones obj)))
    (if (emptyp bones) :textured :bones)))
(defmethod assimp-get-type ((obj ai:mesh))
  (let ((bones (ai:bones obj))
        (n-uvs (ai:texture-coords obj)))
    (cond ((and (not (emptyp n-uvs))
                (emptyp bones)) :textured)
          ((not (emptyp bones)) :bones)
          (t                    :untextured))))

;;--------------------------------------------------

(defun remove-nil-plist (plist)
  (loop :for (p v) :on plist :by #'cddr
        :when v
          :append (list p v)))

(defun assimp-load-meshes (file)
  "returns a list of meshes, each one being a plist. Everything should
   be cached."
  (log4cl:log-info file)
  (let* ((path   (resolve-path file))
         (scene  (assimp-safe-import-into-lisp path))
         (meshes (ai:meshes scene)))
    (loop :for mesh :across meshes
          ;; NOTE: Drop meshes with not UVs, afaik they are placeholders
          ;;       and can ruin the load or rendering
          ;;:when (not (emptyp (ai:texture-coords mesh)))
          :collect
          ;; NOTE: We delay the type check because there could be meshes
          ;; with and without bones on the same scene.
          (let* ((type (assimp-get-type mesh))
                 (duration
                   (when (eq type :bones)
                     (if (not (emptyp (ai:animations scene)))
                         (coerce
                          (ai:duration
                           (aref (ai:animations scene) 0)) ;; hardcoded animation 0
                          'single-float)
                         0f0))))
            (destructuring-bind (&key buf albedo normals specular roughmap)
                (assimp-mesh-to-stream mesh scene path type)
              (remove-nil-plist
               (list
                ;; TODO: concat name of all bones
                :name (or (and (string/= "" (ai:name mesh))
                               (ai:name mesh))
                          (and (plusp (length (ai:bones mesh)))
                               (ai:name (aref (ai:bones mesh) 0)))
                          "")
                :type type
                :scene scene
                :buf buf
                :albedo albedo
                :roughmap roughmap
                :normals normals
                :specular specular
                :duration duration
                :bones (when (eq type :bones)
                         (make-c-array ;; TODO: leaking
                          (coerce
                           ;; NOTE: init using the first transform in the animation, for those that only have 1
                           ;; frame of "animation"
                           (get-bones-transforms scene duration);; FIXME: i know it exists, but not the best
                           'list)
                          :element-type :mat4)))))))))

(defun assimp-load-mesh (file)
  "returns a single buffer"
  (serapeum:~> (resolve-path file)
               (assimp-load-meshes)
               (first)
               (getf :buf)))
