;;; emms-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emms-cache-toggle emms-cache-disable emms-cache-enable)
;;;;;;  "emms-cache" "emms-cache.el" (19690 5063))
;;; Generated autoloads from emms-cache.el

(autoload 'emms-cache-enable "emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-disable "emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-toggle "emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-lyrics-toggle emms-lyrics-disable emms-lyrics-enable)
;;;;;;  "emms-lyrics" "emms-lyrics.el" (19690 5063))
;;; Generated autoloads from emms-lyrics.el

(autoload 'emms-lyrics-enable "emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-disable "emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-toggle "emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-mode-line-toggle emms-mode-line-disable emms-mode-line-enable)
;;;;;;  "emms-mode-line" "emms-mode-line.el" (19690 5063))
;;; Generated autoloads from emms-mode-line.el

(autoload 'emms-mode-line-enable "emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-disable "emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-toggle "emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-player-mpd-show emms-player-mpd-connect)
;;;;;;  "emms-player-mpd" "emms-player-mpd.el" (19690 5063))
;;; Generated autoloads from emms-player-mpd.el

(autoload 'emms-player-mpd-connect "emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload 'emms-player-mpd-show "emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

;;;***

;;;### (autoloads (emms-playing-time-disable-display emms-playing-time-enable-display)
;;;;;;  "emms-playing-time" "emms-playing-time.el" (19690 5063))
;;; Generated autoloads from emms-playing-time.el

(autoload 'emms-playing-time-enable-display "emms-playing-time" "\
Display playing time on mode line.

\(fn)" t nil)

(autoload 'emms-playing-time-disable-display "emms-playing-time" "\
Remove playing time from mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-playlist-mode) "emms-playlist-mode" "emms-playlist-mode.el"
;;;;;;  (19690 5063))
;;; Generated autoloads from emms-playlist-mode.el

(autoload 'emms-playlist-mode "emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-score-toggle emms-score-disable emms-score-enable)
;;;;;;  "emms-score" "emms-score.el" (19690 5063))
;;; Generated autoloads from emms-score.el

(autoload 'emms-score-enable "emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload 'emms-score-disable "emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload 'emms-score-toggle "emms-score" "\
Toggle emms-score.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-default-players emms-devel emms-all emms-standard
;;;;;;  emms-minimalistic) "emms-setup" "emms-setup.el" (19690 5063))
;;; Generated autoloads from emms-setup.el

(autoload 'emms-minimalistic "emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload 'emms-standard "emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode, reading information from tagged
audio files, and a metadata cache.

\(fn)" nil nil)

(autoload 'emms-all "emms-setup" "\
An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload 'emms-devel "emms-setup" "\
An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge.

\(fn)" nil nil)

(autoload 'emms-default-players "emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emms-locate emms-source-file-regex emms-source-file-directory-tree)
;;;;;;  "emms-source-file" "emms-source-file.el" (19690 5063))
;;; Generated autoloads from emms-source-file.el
 (autoload 'emms-play-file "emms-source-file" nil t)
 (autoload 'emms-add-file "emms-source-file" nil t)
 (autoload 'emms-play-directory "emms-source-file" nil t)
 (autoload 'emms-add-directory "emms-source-file" nil t)
 (autoload 'emms-play-directory-tree "emms-source-file" nil t)
 (autoload 'emms-add-directory-tree "emms-source-file" nil t)
 (autoload 'emms-play-find "emms-source-file" nil t)
 (autoload 'emms-add-find "emms-source-file" nil t)
 (autoload 'emms-play-dired "emms-source-file" nil t)
 (autoload 'emms-add-dired "emms-source-file" nil t)

(autoload 'emms-source-file-directory-tree "emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload 'emms-source-file-regex "emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload 'emms-locate "emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
 (autoload 'emms-play-url "emms-source-file" nil t)
 (autoload 'emms-add-url "emms-source-file" nil t)
 (autoload 'emms-play-streamlist "emms-source-file" nil t)
 (autoload 'emms-add-streamlist "emms-source-file" nil t)
 (autoload 'emms-play-lastfm "emms-lastfm" nil t)
 (autoload 'emms-add-lastfm "emms-lastfm" nil t)

;;;***

;;;### (autoloads nil "emms-source-playlist" "emms-source-playlist.el"
;;;;;;  (19690 5063))
;;; Generated autoloads from emms-source-playlist.el
 (autoload 'emms-play-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

;;;***

;;;### (autoloads (emms-streams) "emms-streams" "emms-streams.el"
;;;;;;  (19690 5063))
;;; Generated autoloads from emms-streams.el

(autoload 'emms-streams "emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-tag-editor-compile-pattern emms-tag-editor-guess-tag-filename)
;;;;;;  "emms-tag-editor" "emms-tag-editor.el" (19690 5063))
;;; Generated autoloads from emms-tag-editor.el

(autoload 'emms-tag-editor-guess-tag-filename "emms-tag-editor" "\
A pattern is a string like \"%a-%t-%y\" which stand for
the file name is constructed by artist, title, year with seperator '-'.
see `emms-tag-editor-compile-pattern' for detail about pattern syntax.
Available tags are list in `emms-tag-editor-tags'.

if with prefix argument, the information will extract from full
name, otherwise just match in file name.

An example to guess tag from file name, which the file directory is
the aritist and file name is the title. It can be done like:
C-u M-x emms-tag-editor-guess-tag-filename RET
%{a:[^/]+}/%{t:[^/]+}.mp3 RET

\(fn PATTERN FULLNAME)" t nil)

(autoload 'emms-tag-editor-compile-pattern "emms-tag-editor" "\
A pattern to regexp convertor. \"%a-%{b:[a-z]+}\" will compile to
\"\\([^-]+\\)-\\([a-z]+\\)\".

\(fn PATTERN)" nil nil)

;;;***

;;;### (autoloads (emms-volume-mode-minus emms-volume-mode-plus emms-volume-lower
;;;;;;  emms-volume-raise) "emms-volume" "emms-volume.el" (19690
;;;;;;  5063))
;;; Generated autoloads from emms-volume.el

(autoload 'emms-volume-raise "emms-volume" "\
Raise the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-lower "emms-volume" "\
Lower the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-mode-plus "emms-volume" "\
Raise volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(autoload 'emms-volume-mode-minus "emms-volume" "\
Lower volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

;;;***

;;;### (autoloads (emms-volume-amixer-change) "emms-volume-amixer"
;;;;;;  "emms-volume-amixer.el" (19690 5063))
;;; Generated autoloads from emms-volume-amixer.el

(autoload 'emms-volume-amixer-change "emms-volume-amixer" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

;;;***

;;;### (autoloads (oggc-show-header) "ogg-comment" "ogg-comment.el"
;;;;;;  (19690 5063))
;;; Generated autoloads from ogg-comment.el

(autoload 'oggc-show-header "ogg-comment" "\
Show a pretty printed representation of the Ogg Comments in FILE.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (tq-create) "tq" "tq.el" (19690 5063))
;;; Generated autoloads from tq.el

(autoload 'tq-create "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

\(fn PROCESS)" nil nil)

;;;***

;;;### (autoloads nil nil ("emms-bookmarks.el" "emms-browser.el"
;;;;;;  "emms-compat.el" "emms-history.el" "emms-i18n.el" "emms-info-libtag.el"
;;;;;;  "emms-info-mp3info.el" "emms-info-ogg.el" "emms-info-ogginfo.el"
;;;;;;  "emms-info.el" "emms-last-played.el" "emms-lastfm.el" "emms-mark.el"
;;;;;;  "emms-metaplaylist-mode.el" "emms-mode-line-icon.el" "emms-pkg.el"
;;;;;;  "emms-player-mpg321-remote.el" "emms-player-mplayer.el" "emms-player-simple.el"
;;;;;;  "emms-player-xine.el" "emms-playlist-sort.el" "emms-stream-info.el"
;;;;;;  "emms-url.el" "emms.el" "jack.el" "later-do.el") (19690 5063
;;;;;;  181084))

;;;***

(provide 'emms-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emms-autoloads.el ends here
