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
;; A guide: https://img.ly/blog/ultimate-guide-to-ffmpeg/
;;

(require 'elisp-lib)

;;
;; add code to use brew or something else to install ffmpeg
;;

(defun ffmpeg-binary-path ()
  (which "ffmpeg"))

(defun ffmpeg-is-installed ()
  (not (not (ffmpeg-binary-path))))

(assert (ffmpeg-is-installed) nil "Please install ffmpeg before using this module.
   Try brew or another package manager.")

(defun ffmpeg--clip-time-to-seconds (time)
  (cl-loop for num in (reverse (mapcar #'string-to-number
                                       (split-string time ":")))
           for operand = 1 then (* operand 60)
           sum (* num operand)))

(cl-defun ffmpeg-slice-clip (input-file &key (minutes 0) (seconds 0) length end-time overwrite)
  (assert (file-exists-p input-file) "Input file does not exist.")
  (assert (or length end-time))

  (let* ((output-file (format "%s-%s.%s"
                              (file-name-sans-extension input-file)
                              "clip"
                              (file-name-extension input-file)))
         (seconds (+ seconds (* 60 minutes)))
         (length (or length (- (ffmpeg--clip-time-to-seconds end-time) seconds))))
    (when (and overwrite
               (file-exists-p output-file))
      (osx-move-to-trash output-file))

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

(cl-defun ffmpeg-join-clips (clips output-file &key overwrite)
  (let ((clips (mapcar #'expand-file-name clips)))
    (assert (every #'file-exists-p clips))

    (when (file-exists-p output-file)
      (if overwrite
          (osx-move-to-trash output-file)
        (error "File %s exists" output-file)))

    (let ((video-file (expand-file-name "~/videos.txt")))
      (barf (string-join (mapcar (| format "file '%s'" %) clips) "\n")
            video-file)

      (do-cmd-async
       (list "ffmpeg"
             "-f" "concat"
             "-safe" "0"
             "-i" video-file
             "-c" "copy"
             output-file)
       :throw t
       :callback-fn (lambda (&rest foo)
                      (when (file-exists-p video-file)
                        (delete-file video-file))
                      (message "Finished concat into %s" output-file))))))


;;

;;  mkdir frames
;;  ffmpeg -i input -vf scale=320:-1:flags=lanczos,fps=10 frames/ffout%03d.png

;; Then use convert (or gm convert if you prefer GraphicsMagick) to make your animated GIF:

;;  convert -loop 0 frames/ffout*.png output.gif

;; $ ffmpeg -ss 61.0 -t 2.5 -i StickAround.mp4 -filter_complex "[0:v] fps=12,scale=w=480:h=-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=new=1" StickAroundPerFrame.gif
(cl-defun ffmpeg-to-gif (input-file &key (scale-width 480) (minutes 0) seconds length start-time end-time (overwrite t))
  (assert (file-exists-p input-file) "Input file does not exist.")
  (assert (or length end-time))

  (let* ((output-file (concat (file-name-sans-extension input-file) ".gif"))
         (seconds (if start-time
                      (ffmpeg--clip-time-to-seconds start-time)
                      (+ seconds (or (* 60 minutes) 0))))
         (length (or length (- (ffmpeg--clip-time-to-seconds end-time) seconds))))
    (when (and overwrite
               (file-exists-p output-file))
      (osx-move-to-trash output-file))

    (do-cmd-async
     `("ffmpeg"
       ,@(when (< 0 seconds)
           (list "-ss" (format "%s" seconds)))
       ,@(when (< 0 length)
           (list "-t"  (format "%s" length)))
       "-i" ,input-file
       "-filter_complex" ,(format "[0:v] fps=12,scale=w=%d:h=-1,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=new=1"
                                     scale-width)
       ,output-file)
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
  (let ((audio-suffix ".aac"))
    (when output-file
      (assert (string-ends-with audio-suffix output-file)))
    (let ((output-file (or output-file
                           (concat (file-name-base input-file) audio-suffix))))
      (do-cmd (list "ffmpeg" "-i" input-file "-vn" "-acodec" "copy" output-file)))))

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

;; Add an optional end time instead of length
(cl-defun ffmpeg-slice (input-file &key (minutes 0) seconds length overwrite-output)
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

;; combine this stuff
(defun ffmpeg-get-video-metadata (path)
  (assoc1 'streams (run-to-json "ffprobe" "-show_streams" "-print_format" "json" path)))

(defun ffmpeg-get-movie-dimensions (path)
  (elt (assoc1 'streams (run-to-json "ffprobe" "-v" "error"
                                    "-select_streams" "v"
                                    "-show_entries"
                                    "stream=width,height,bit_rate,avg_frame_rate,duration"
                                    "-of" "json=compact=1"
                                    path))
       0))

(defun ffmpeg-movie-length (path)
  (string-to-number (assoc1 '(format duration) (ffmpeg-get-movie-metadata path))))

(cl-defun ffmpeg-async-file-modifier (path ffmpeg-args &key output-file cb-fn (replace t))
  (let* ((path (if (file-name-absolute-p path)
                   path
                 (path-join default-directory path)))
         (output-file (concat (file-name-sans-extension path) "-out." (file-name-extension path))))
    (when (file-exists-p output-file)
      (osx-move-to-trash output-file))
    (do-cmd-async (concatenate 'list
                               (list "ffmpeg"
                                     "-i" path)
                               ffmpeg-args
                               (list output-file))
                  :throw t
                  :callback-fn (lambda (resp)
                                 ;; Is the replacement file smaller
                                 (when replace
                                   (osx-move-to-trash path)
                                   (rename-file output-file path))
                                 (message "Finished processing file %s" path)
                                 (when cb-fn
                                   (funcall cb-fn path))))))

(defun ffmpeg-find-lower-scale-options (width height)
  (destructuring-bind (bigger smaller) (if (> width height)
                                           (list width height)
                                         (list height width))
      (cl-loop for bn from (1- bigger) above 1
               ;; cl-round returns a list with the integer and
               ;; the remainder.
               for sn-list = (cl-round (/ (* (float bn) smaller) bigger))
               ;; Make our rounding behavior match ffmpeg.
               for sn = (if (eql (second sn-list) .5)
                            (1+ (cl-first sn-list))
                          (cl-first sn-list))
               when (and (= (mod bn 2) 0)
                         (= (mod sn 2) 0))
                  collect (list bn sn))))

;;
;; Make a general "async and update/replace file" function.
;;
;; This worked pretty well too.
;; ffmpeg -i movie scale=1208:-1 -preset slow output.mp4
;;
(cl-defun ffmpeg-reduce-size (path &key frame-rate
                                        switch-codec
                                        preset-encoding-speed  ; how quickly it gets encoded...
                                        bit-rate
                                        (replace t)
                                        (scale-width -1)
                                        crf-quality    ; lower is better quality 0 - 51
                                        cb-fn)


  (when preset-encoding-speed
    (cl-assert (member preset-encoding-speed '(ultrafast
                                               superfast
                                               veryfast
                                               faster
                                               fast
                                               medium ; Default if unspecified
                                               slow
                                               slower
                                               veryslow))))
  (when switch-codec
    (cl-assert (member switch-codec '("libx265" "libx264"))))

  (ffmpeg-async-file-modifier path
                              `(
                                ,@(when preset-encoding-speed
                                    (list "-preset" (symbol-name preset-encoding-speed)))
                                ,@(when crf-quality
                                    (list "-crf" (number-to-string crf-quality)))
                                ,@(when frame-rate
                                    (list "-r" (number-to-string frame-rate)))
                                ,@(when bit-rate
                                    (list "-b:v" (number-to-string bit-rate)))
                                ,@(when (and scale-width (< 0 scale-width))
                                    (list "-vf" (format "scale=%d:-1,setsar=1:1" scale-width)))
                                ,@(when switch-codec
                                    (list "-vcodec" switch-codec)))
                              :replace replace
                              :cb-fn cb-fn))

(cl-defun ffmpeg-reduce-size-gif (path &key (replace t))
  (ffmpeg-reduce-size path :replace replace :scale-width 480))

(cl-defun ffmpeg-reduce-size-list (list &rest ffmpeg-args)
  (do-list-async list
                 :fn (lambda (entry call-next-fn)
                       (apply #'ffmpeg-reduce-size entry :cb-fn call-next-fn
                              ffmpeg-args))))

(defun ffmpeg-frame-rate-to-num (frame-rate-str)
  (destructuring-bind (l r)
      (mapcar #'string-to-number (split-string frame-rate-str "/"))
    (if (<= r 0)
        100000000
      (/ l r))))

(cl-defun ffmpeg-find-closest-width (desired-width current-width height)
  (dolist (candidate (cons (list current-width height)
                           (ffmpeg-find-lower-scale-options current-width height)))
    (destructuring-bind (w h) candidate
      (if (<= w desired-width)
          (cl-return-from ffmpeg-find-closest-width w)))))

(cl-defun ffmpeg--check-limit (limit value cmp-fn)
  (if limit
      (funcall cmp-fn limit value)
    value))


(cl-defun ffmpeg--calc-bit-rate (movie-bit-rate &key min-bit-rate max-bit-rate bit-rate-reduction)
  (when movie-bit-rate
    (let ((movie-bit-rate (if (integerp movie-bit-rate)
                              movie-bit-rate
                            (string-to-number movie-bit-rate))))
      ;; Do we need to do anything?
      (if (and (or min-bit-rate max-bit-rate bit-rate-reduction)
               (or (not min-bit-rate) (>= movie-bit-rate min-bit-rate)))
          (let* ((bit-rate (ffmpeg--check-limit bit-rate-reduction movie-bit-rate
                                                (fn (limit value)
                                                  (round (* limit value)))))
                 (bit-rate (ffmpeg--check-limit max-bit-rate
                                                bit-rate
                                                #'min))
                 (bit-rate (ffmpeg--check-limit min-bit-rate bit-rate
                                                #'max)))
            bit-rate)
        movie-bit-rate))))
;;
;; I may want to have a function that a data size limit
;;
;; https://trac.ffmpeg.org/wiki/Encode/H.265
;;
;; h265 is great but processing is super slow.
;;  - 23 min for 1GB -> 220mb
;;
;; (cl-defun ffmpeg-reduce-size-list-perc (list &key percent scale-width max-size-byte))


(cl-defun ffmpeg-reduce-size-list-auto (list &key max-bit-rate min-bit-rate max-width max-frame-rate switch-codec crf-quality (preset-encoding-speed 'medium) bit-rate-reduction (replace t))
  (do-list-async list
                 :fn (lambda (entry call-next-fn)
                       ;; Calc the best values for new video parameters
                       (let* ((video-metadata (ffmpeg-get-movie-dimensions entry))
                              (movie-bit-rate (assoc-get 'bit_rate video-metadata))
                              (bit-rate (ffmpeg--calc-bit-rate movie-bit-rate
                                                               :bit-rate-reduction bit-rate-reduction
                                                               :min-bit-rate min-bit-rate
                                                               :max-bit-rate max-bit-rate))
                              (movie-frame-rate (ffmpeg-frame-rate-to-num (assoc1 'avg_frame_rate video-metadata)))
                              (frame-rate (when (< max-frame-rate movie-frame-rate)
                                            max-frame-rate))

                              (movie-width (assoc1 'width video-metadata))
                              (scale-width (when (< max-width movie-width)
                                             (ffmpeg-find-closest-width max-width
                                                                        movie-width
                                                                        (assoc1 'height video-metadata)))))

                         (message "Reduce(%s) -> (width %s to %s) (bit-rate %s to %s) (frame-rate %s to %s)"
                                  entry movie-width scale-width movie-bit-rate bit-rate movie-frame-rate frame-rate)

                         (ffmpeg-reduce-size entry
                                             :cb-fn call-next-fn
                                             :switch-codec switch-codec
                                             :scale-width scale-width
                                             :crf-quality crf-quality
                                             :bit-rate bit-rate
                                             :preset-encoding-speed preset-encoding-speed
                                             :frame-rate frame-rate
                                             :replace replace)))))

(cl-defun ffmpeg-conv-video-to-mp4 (file-to-convert &key cb-fn)
  (ffmpeg-async-file-modifier file-to-convert
                              '("-vcodec" "libx264" "-acodec" "aac")
                              :output-file (concat (file-name-sans-extension file-to-convert)
                                                   ".mp4")
                              :replace nil
                              :cb-fn cb-fn))

;;
;; extract metadata
;; ffmpeg -i <some-movie>.mkv -f ffmetadata metadata
;;


;; Convert VOB to mp4
;; ffmpeg -i weighing.VOB -vcodec libx264 -acodec aac weighing.mp4

(cl-defun ffmpeg-dir-to-mp4 (path &key (extension-pattern "\.mkv$"))
  (do-list-async (list-directory-entries path :full t :match extension-pattern)
                 :fn (lambda (entry call-next-fn)
                       (ffmpeg-conv-video-to-mp4 entry :cb-fn call-next-fn))))

(provide 'ffmpeg)
