;;;.emacs-emms.el -- My conf file for emms with mpd

;       $Id: .emacs-emms.el,v 1.2 2008/01/19 22:04:30 thierry Exp $     


;; Code:

;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/emms/")
(add-to-list 'load-path "~/elisp/emms/")
;;(setq emms-score-file "~/.emacs.d/emms/scores")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")
(setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-cache-file "~/.emacs.d/emms/emms-cache")
;;(setq emms-lastfm-submission-verbose-p t)
(require 'emms-setup)
(emms-devel)
(emms-default-players)

;; enable emms scoring
(setq emms-score-enabled-p t)

;; Start browser with album
(setq emms-browser-default-browse-type 'info-album)

;;les radios bookmarquées
(setq emms-stream-default-action "play")

;;j'essaie ici de definir les urls de radios mms:// dans les regex par default pour mplayer
;; c'est codé en dur dans emms-mplayer.el mais sans ce code emacs lance ssh via tramp!! 
(define-emms-simple-player mplayer '(file url)
  "mms://"
  "mplayer" "-nocache") 

;;pour avoir des images dans le browser
;;(image par defaut si il n'y a pas 
;;d'image nommée cover_small.jpg dans le dossier de l'album)
(setq emms-browser-default-covers
      (list "/home/thierry/mpd/covers/cover_small.jpg" nil nil))

;;pas de speed quand je scroll mes albums
(setq scroll-up-aggressively 0.0)
(setq scroll-down-aggressively 0.0)

;; show all files (no streamlists, etc)
(emms-browser-make-filter
 "all-files" (emms-browser-filter-only-type 'file))

;;show only tracks in one folder
(emms-browser-make-filter
 "Sky_radios" (emms-browser-filter-only-dir "~/mpd/playlists"))

;;affichage d'infos dans la modeline
(require 'emms-mode-line)
(emms-mode-line 1)
(require 'emms-playing-time)
(emms-playing-time 1)

(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-streams)
(require 'emms-info)

;;config pour mpd
(require 'emms-player-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6601")
(setq emms-player-mpd-music-directory "~/mpd/music")
(add-to-list 'emms-info-functions 'emms-info-mpd)

;;j'utilise PCM avec amix pour mon volume
(setq emms-volume-amixer-control "PCM")

;;stop mpd
(defun tv-stop-mpd ()
  (interactive)
  (if emms-player-playing-p
      (emms-stop)
    (shell-command "mpc stop")))

;;quelques bindings
(global-set-key (kbd "<f6> r") 'emms-streams)
(global-set-key (kbd "<f6> +") 'emms-volume-raise)
(global-set-key (kbd "<f6> -") 'emms-volume-lower)
(global-set-key (kbd "<f6> b") 'emms-smart-browse)
(global-set-key (kbd "<f6> t") 'emms-player-mpd-show)
(global-set-key (kbd "<f6> s") 'tv-stop-mpd)
(global-set-key (kbd "<f6> c") 'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> l a") 'emms-lastfm-radio-artist-fan)
(global-set-key (kbd "<f6> l g") 'emms-lastfm-radio-global-tag)
(global-set-key (kbd "<f6> l l") 'emms-lastfm-radio-love)
(global-set-key (kbd "<f6> l b") 'emms-lastfm-radio-ban)
(global-set-key (kbd "<f6> l s") 'emms-lastfm-radio-skip)
(global-set-key (kbd "<f6> p") 'emms-player-mpd-pause)
(global-set-key (kbd "<f6> >") 'emms-player-mpd-next)
(global-set-key (kbd "<f6> <") 'emms-player-mpd-previous)

;;config de lastfm
(setq emms-lastfm-username "XXXXXXX"
      emms-lastfm-password "XXXXXXX")
(emms-lastfm-enable)

;; Add music to mpd
(defun tv-update-mpd ()
  (interactive)
  (let ((mpd-dir "~/mpd/music"))
    (and (emms-player-mpd-update-directory mpd-dir)
         (emms-cache-set-from-mpd-all)
         (emms-cache-set-from-mpd-directory mpd-dir)
         (emms-player-mpd-update-all))))

;;; .emacs-emms.el ends here
