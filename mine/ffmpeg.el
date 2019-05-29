;; -*- lexical-binding: t -*-

(require 'elisp-lib)

;;
;; add code to use brew or something else to instal ffmpeg
;;

(defun ffmpeg-binary-path ()
  (which "ffmpeg"))

(defun ffmpeg-is-installed ()
  (not (not (ffmpeg-binary-path))))

(assert (ffmpeg-is-installed) nil "Please install ffmpeg before using this module.
   Try brew or another package manager.")

;;

;;  mkdir frames
;;  ffmpeg -i input -vf scale=320:-1:flags=lanczos,fps=10 frames/ffout%03d.png

;; Then use convert (or gm convert if you prefer GraphicsMagick) to make your animated GIF:

;;  convert -loop 0 frames/ffout*.png output.gif

;; $ ffmpeg -ss 61.0 -t 2.5 -i StickAround.mp4 -filter_complex "[0:v] fps=12,scale=w=480:h=-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=new=1" StickAroundPerFrame.gif
(cl-defun ffmpeg-to-gif (input-file &key (scale-width 480) start minutes length)
  (assert (file-exists-p input-file))

  (let ((output-file (concat (file-name-sans-extension input-file) ".gif"))
        (start (+ start (or (* 60 minutes) 0))))
    (do-cmd
     (list "ffmpeg"
           "-ss" (format "%s" start)
           "-t"  (format "%s" length)
           "-i" input-file
           "-filter_complex" (format "[0:v] fps=12,scale=w=%d:h=-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=new=1"
                                     scale-width)
           output-file)
     :throw t)
    output-file))

;; ffprobe

;; On the second sylable every time we sing a relation, Laura now goes down a note.
;; she doesn't hold it steady.
;;
(cl-defun ffmpeg-to-mp3 (input-file)
  (do-cmd
   ;;(list "ffmpeg" "-i" input-file "-vn" "-acodec" "copy" )
   ;; ffmpeg -i sample.avi -q:a 0 -map a sample.mp3
   ))
;;
;; -filters
;; -codecs
;;



;;
;; stuff to support:
;;
;; chop parts of a video
;; turn parts of a video into a gif.
;; convert video from one format to another
;; get the audio from a video file.
;;
;; might want a way to pass around standard options
;;  - like an options alist or something.
;;

(defun ffmpeg-slice ())

(provide 'ffmpeg)
