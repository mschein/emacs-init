;; -*- lexical-binding: t -*-

;;
;; Explore more complex filters:
;;
;; See: https://ffmpeg.org/ffmpeg-filters.html
;; and https://github.com/kkroening/ffmpeg-python
;;  for a python example.
;;
;; a tutorial: http://dranger.com/ffmpeg/tutorial01.html
;;
;;
;;

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

(cl-defun ffmpeg-slice-clip (input-file &key (minutes 0) seconds length overwrite-output)
  (assert (file-exists-p input-file) "Input file does not exist.")
  (assert length)

  (let ((output-file (format "%s-%s.%s"
                             (file-name-sans-extension input-file)
                             "clip"
                             (file-name-extension input-file)))
        (seconds (+ seconds (or (* 60 minutes) 0))))
    (when (and overwrite-output
               (file-exists-p output-file))
      (delete-file output-file t))

    (do-cmd-async
     (list "ffmpeg"
           "-ss" (format "%s" seconds)
           "-t"  (format "%s" length)
           "-i" input-file
           output-file)
     :callback-fn (lambda (&rest foo)
                    (message "Finished splitting %s" input-file))
     :throw t)
    output-file))

;;

;;  mkdir frames
;;  ffmpeg -i input -vf scale=320:-1:flags=lanczos,fps=10 frames/ffout%03d.png

;; Then use convert (or gm convert if you prefer GraphicsMagick) to make your animated GIF:

;;  convert -loop 0 frames/ffout*.png output.gif

;; $ ffmpeg -ss 61.0 -t 2.5 -i StickAround.mp4 -filter_complex "[0:v] fps=12,scale=w=480:h=-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=new=1" StickAroundPerFrame.gif
(cl-defun ffmpeg-to-gif (input-file &key (scale-width 680) (minutes 0) seconds length overwrite-output)
  (assert (file-exists-p input-file) "Input file does not exist.")
  (assert length)

  (let ((output-file (concat (file-name-sans-extension input-file) ".gif"))
        (seconds (+ seconds (or (* 60 minutes) 0))))
    (when (and overwrite-output
               (file-exists-p output-file))
      (delete-file output-file t))

    (do-cmd-async
     (list "ffmpeg"
           "-ss" (format "%s" seconds)
           "-t"  (format "%s" length)
           "-i" input-file
           "-filter_complex" (format "[0:v] fps=12,scale=w=%d:h=-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=new=1"
                                     scale-width)
           output-file)
     :callback-fn (lambda (&rest foo)
                    (message "Finished processing %s to gif" input-file))
     :throw t)
    output-file))

;; ffprobe
(cl-defun ffmpeg-to-mp3 (input-file &optional output-file)
  (let ((output-file (or output-file
                         (concat (file-name-base input-file) ".mp3"))))
    (do-cmd-async
     (list "ffmpeg" "-i" input-file "-q:a" "0" "-map" "a" output-file)
     :throw t
     :callback-fn (lambda (&rest foo) (message "Finished processing %s -> %s" input-file output-file))
     ))
;;         "-i" input-file "-vn" "-acodec" "copy" )
   ;; ffmpeg -i sample.avi -q:a 0 -map a sample.mp3
   )

(cl-defun ffmpeg-to-audio (input-file &optional output-file)
  (let ((output-file (or output-file
                         (concat (file-name-base input-file) ".aac"))))
    (do-cmd (list "ffmpeg" "-i" input-file "-vn" "-acodec" "copy" output-file)
            )))

(cl-defun ffmpeg-to-audio-dir (dir &key match)
  ;; I know this is lame, but it's just a place holder.
  (pushd dir
    (message "Start processing %s" dir)
    (cl-loop for file in (list-directory-entries dir :match match)
             do (progn
                  (message "Process file %s" file)
                  (ffmpeg-to-audio file)))))


;;
;; Audio to single image movie.
;;
;; ffmpeg -f image2 -loop 1 -i picture.png -i music.mp3 -c:v libx264 -tune stillimage -c:a copy -shortest movie.mp4
;;

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

;;
;; mkv -> mp4
;;
;;  ffmpeg -i input.mkv -codec copy output.mp4
;;

(cl-defun ffmpeg-slice (input-file &key (minutes 0) seconds length (overwrite-output t))
  "Get a slice from a video."
  (let ((output-file (concat (file-name-sans-extension input-file) "-out." (file-name-extension input-file)))
        (seconds (+ seconds (or (* 60 minutes) 0))))
    (when (and overwrite-output
               (file-exists-p output-file))
      (delete-file output-file t))

    (do-cmd-async
     (list "ffmpeg"
           "-ss" (format "%s" seconds)
           "-t"  (format "%s" length)
           "-i" input-file
           "-c" "copy"
           output-file)
     :callback-fn (lambda (&rest foo)
                    (message "Finished processing %s" input-file))
     :throw t)
    output-file))

(cl-defun ffmpeg-remove-segment (path &key start end)
  )

(defun ffmpeg-get-movie-metadata (path)
  (run-to-json "ffprobe" "-v" "quiet" "-print_format" "json" "-show_format" path))

(defun ffmpeg-movie-length (path)
  (string-to-number (assoc1 '(format duration) (ffmpeg-get-movie-metadata path))))

(cl-defun ffmpeg-async-file-modifier (path ffmpeg-args &key cb-fn (replace t))
  (let* ((path (if (file-name-absolute-p path)
                   path
                 (path-join default-directory path)))
         (output-file (concat (file-name-sans-extension path) "-out." (file-name-extension path))))
    (do-cmd-async (concatenate 'list
                               (list "ffmpeg"
                                     "-i" path)
                               ffmpeg-args
                               (list output-file))
                  :throw t
                  :callbackfn (lambda (resp)
                                (when replace
                                  (osx-move-to-trash path)
                                  (rename-file output-file path))
                                (message "Finished processing file %s" path)
                                (when cb-fn
                                  (funcall cb-fn path))))))

;;
;; Make a general "async and update/replace file" function.
;;
(cl-defun ffmpeg-reduce-size (path &key (switch-codec t) (replace t) cb-fn)
  ;;
  ;; keep this simple and hard coded for now.
  ;;
  ;; ffmpeg -i input.mp4 -vcodec libx265 -crf 28 -b 800k output.mp4
  ;;
  ;; also:
  ;;
  ;; lower the bit rate.
  ;; ffmpeg -i input.mp4 -b 800k output.mp4
  ;;
  (ffmpeg-async-file-modifier path
                              `("-crf" "28"
                                "-b" "850k"
                                ,@(when switch-codec
                                    (list "-vcodec" "libx264")))
                              :replace replace
                              :cb-fn cb-fn))

(cl-defun ffmpeg-conv-video-file-type (file-to-convert &key (codec "aac") (replace t) cb-fn)
  (ffmpeg-async-file-modifier file-to-convert
                              (list "-vcodec" "libx264"
                                    "-acodec" codec)
                              :replace replace
                              :callback-fn cb-fn))

;;
;; extract metadata
;; ffmpeg -i <some-movie>.mkv -f ffmetadata metadata
;;


;; Convert VOB to mp4
;; ffmpeg -i weighing.VOB -vcodec libx264 -acodec aac weighing.mp4

(cl-defun ffmpeg-dir-to-mp4 (path &key (extension-pattern "\.mkv$"))
  (do-list-async (list-directory-entries path :full t :match extension-pattern)
                 :fn (lambda (entry call-next-fn)
                       (ffmpeg-video-to-mp4 entry :cb-fn call-next-fn))))

(provide 'ffmpeg)
