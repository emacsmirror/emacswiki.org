;;; .emacs-emms.el -- Ma config emms avec alsaplayer

;; Code:

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emms/")
(add-to-list 'load-path "~/elisp/emms/")
(setq emms-score-file "~/.emacs.d/score")
(setq emms-stream-bookmarks-file "~/.emacs.d/streams")
(setq emms-history-file "~/.emacs.d/emms-history")
(setq emms-cache-file "~/.emacs.d/emms-cache")

(require 'emms-setup)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-streams)
(require 'emms-info)
(emms-devel)
(setq emms-player-list (list 'emms-player-alsaplayer
			     'emms-player-mplayer
			     'emms-player-ogg123))

(define-emms-simple-player mplayer '(file url)
  "mms://"
  "mplayer" "-nocache") 

(define-emms-simple-player alsaplayer '(file url playlist streamlist) (regexp-opt
'(".ogg" ".mp3" ".wav" ".flac" ".pls" ".m3u" "http://"))
"alsaplayer" "--quiet" "--nosave")

;;format par defaut playlists
(setq emms-source-playlist-default-format "pls")

;;dossier musique par defaut
(setq emms-source-file-default-directory "~/music")

;;pour avoir des images dans le browser
;;(image par defaut si il n'y a pas 
;;d'image nommée cover_small.jpg dans le dossier de l'album)
(setq emms-browser-default-covers
      (list "/home/thierry/covers/cover_small.jpg" nil nil))

;; show all files (no streamlists, etc)
(emms-browser-make-filter
 "all-files" (emms-browser-filter-only-type 'file))

;;only stream-playlists
(emms-browser-make-filter
 "sky-radios" (emms-browser-filter-only-dir "~/playlists/"))

;;config de lastfm
(setq emms-lastfm-username "xxxxx"
      emms-lastfm-password "xxxxx")
(emms-lastfm-enable)

;;affichage d'infos dans la modeline
(require 'emms-mode-line)
(emms-mode-line 1)
(require 'emms-playing-time)
(emms-playing-time 1)

;;keys bindings pour emms
(global-set-key (kbd "<f6> r") 'emms-streams)
(global-set-key (kbd "<f6> +") 'emms-volume-raise)
(global-set-key (kbd "<f6> -") 'emms-volume-lower)
(global-set-key (kbd "<f6> b") 'emms-smart-browse)
(global-set-key (kbd "<f6> s") 'emms-stop)
(global-set-key (kbd "<f6> c") 'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> l a") 'emms-lastfm-radio-artist-fan)
(global-set-key (kbd "<f6> l g") 'emms-lastfm-radio-global-tag)
(global-set-key (kbd "<f6> l b") 'emms-lastfm-radio-ban)
(global-set-key (kbd "<f6> l l") 'emms-lastfm-radio-love)

;;les radios bookmarquées
(setq emms-stream-bookmarks-file "~/.emacs.d/emms-bookmarks")
(setq emms-stream-default-action "play")

;;; .emacs-emms.el ends here
