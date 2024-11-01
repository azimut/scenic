(in-package #:scenic)

(defmethod blit (scene (postprocess bloom) camera time)
  "OUTPUT: at fbos[0]"
  (with-accessors ((fbos     bloom-fbo-fbos)
                   (samplers bloom-fbo-samplers)
                   (widths   bloom-fbo-widths)
                   (heights  bloom-fbo-heights))
      (fbos postprocess)
    (with-setf* ((depth-test-function) #'always ; different from :around
                 (clear-color) (v! 0 0 0 1))
      ;;
      ;; Extract Brightest Parts
      ;;
      (with-fbo-bound ((aref fbos 0))
        (clear-fbo (aref fbos 0))
        (map-g #'bloom-threshold-pipe (bs postprocess)
               :intensity (intensity-gamma postprocess)
               :threshold (threshold postprocess)
               :soft (threshold-soft postprocess)
               :sam (first (sam (prev *state*)))))
      ;;
      ;; Downsample ⬇️
      ;;
      (loop :for src :from 0 :to 3 :do
        (with-fbo-bound ((aref fbos (1+ src)))
          (clear-fbo (aref fbos (1+ src)))
          (map-g #'bloom-blur-pipe (bs postprocess)
                 :delta 1f0
                 :sam (aref samplers src)
                 :x   (aref widths   src)
                 :y   (aref heights  src))))
      ;;
      ;; Upsample ⬆️
      ;;
      (loop :for dst :from 3 :downto 0 :do
        (with-blending (blending postprocess)
          (with-fbo-bound ((aref fbos dst))
            (clear-fbo (aref fbos dst))
            (map-g #'bloom-blur-pipe (bs postprocess)
                   :sam (aref samplers (+ 1 dst))
                   :x   (aref widths   (+ 1 dst))
                   :y   (aref heights  (+ 1 dst))
                   :delta 0.5))))
      ;;
      ;; Render (captured fbo in :AFTER)
      ;;
      (map-g #'add-textures-pipe (bs postprocess)
             :sam1 (aref samplers 0)
             :sam2 (first (sam (prev *state*)))
             :x    (aref widths 0)
             :y    (aref heights 0))))
  ;;#+nil
  (draw-tex-tr
   (aref (bloom-fbo-samplers (fbos postprocess)) 0)))
