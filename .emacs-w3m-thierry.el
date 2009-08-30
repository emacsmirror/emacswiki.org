;;; .emacs-config-w3m.el -- config w3m for thierry's laptop


;;; Code:

;; emacs-w3m is in ~/elisp
(setq w3m-icon-directory "~/elisp/emacs-w3m/icons")
(require 'w3m-load)
(require 'mime-w3m)

;; download with wget
(require 'w3m-wget)

;; env settings for w3m

(setq w3m-coding-system 'utf-8
      w3m-language 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;;w3m default browser
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; Local meteo: use my w3m-meteo.el
(require 'w3m-meteo)
(define-key w3m-mode-map (kbd "ma") 'w3m-meteo-bookmark-add)
(define-key w3m-mode-map (kbd "mk") 'w3m-meteo-bookmark-delete)

;;(global-set-key "\C-xp" 'browse-url-at-point)
(global-set-key (kbd "<f7> h") 'w3m) 
(global-set-key (kbd "<f7> w") 'w3m-frweather) 
(global-set-key (kbd "<f7> t") 'w3m-dtree) 
(global-set-key (kbd "<f7> j") 'webjump) 
(global-set-key (kbd "<f7> z") 'w3m-namazu)

;;w3m-namazu
(setq w3m-namazu-default-index "~/namazu-index")

;;w3m homepage
(setq w3m-home-page "http://www.google.fr")

;;webjump sites
(setq webjump-sites
      '(("pythonlib" .  "http://docs.python.org/lib/genindex.html")
        ("pythondoc" . "http://docs.python.org/index.html")
        ("gentooforumfr" .  "http://www.gentoo.fr/forum/index.php")
        ("gentoowikifr" . "http://fr.gentoo-wiki.com/Accueil")
        ("gentoosearch" . "http://cosearch.googlepages.com/GentooSearchFrancais.html")
        ("delicious" . "http://del.icio.us/thiedlecques")
        ("gentooman" . "http://www.gentoo.org/doc/fr/handbook/handbook-amd64.xml?full=1#book_part1")
        ("emacsmanfr" . "http://www.linux-france.org/article/appli/emacs/manuel/html/index.html")
        ("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/SiteMap")
        ("wikipedia" . "http://fr.wikipedia.org/wiki/Accueil")
        ("planner" . "http://wjsullivan.net/static/doc/planner/html_node/index.html")
        ("elispcode" . "http://www.emacswiki.org/cgi-bin/wiki/Cat%c3%a9gorieCode")
        ("elisp-reference-manual" . "http://www.gnu.org/software/emacs/elisp/html_node/index.html")
        ("developpez-faq" . "http://python.developpez.com/faq/")
        ))

;;recherche directe dans google

(defun search-word (word)
  (interactive "sGoogleSearch: ")
  (browse-url (concat "http://www.google.com/search?hl=fr&ie=ISO-8859-1&q=" word)))
(global-set-key (kbd "<f7> s g") 'search-word)

;; search in delicious
(defun tv-search-delicious (word)
  (interactive "sDeliciousSearch: ")
  (let ((type (if current-prefix-arg
                  "delicious"
                "user")))
    (browse-url (concat "http://del.icio.us/search/?fr=del_icio_us&p=" word "&type=" type))))

(global-set-key (kbd "<f7> s d") 'tv-search-delicious)

;; search in gentoo-portage
(defun tv-search-gentoo (word)
  (interactive "sPortageSearch: ")
  (browse-url (concat "http://gentoo-portage.com/Search?search=" word)))
(global-set-key (kbd "<f7> s p") 'tv-search-gentoo)

;; search gmane
(defun tv-search-gmane (query &optional group author)
  (interactive (list
                (read-from-minibuffer "Query: ")
                (completing-read "Group: "
                                 '("gmane.emacs.gnus.general"
                                   "gmane.emacs.gnus.cvs"
                                   "gmane.emacs.gnus.user"
                                   "gmane.emacs.help"
                                   "gmane.lisp.clisp.devel"
                                   "gmane.lisp.clisp.general"
                                   "gmane.lisp.emacs-cl"
                                   "gmane.linux.gentoo.devel"
                                   "gmane.linux.gentoo.user"
                                   "gmane.linux.gentoo.cvs"
                                   "gmane.emacs.planner.general"
                                   "gmane.emacs.muse.general"
                                   "gmane.emacs.dvc.devel")
                                 nil nil nil 'minibuffer-history)
                (read-from-minibuffer "Author: ")))
  (w3m-browse-url (concat "http://search.gmane.org/?query="
                          query
                          "&author="
                          author
                          "&group="
                          group
                          "&sort=relevance&DEFAULTOP=and&TOPDOC=80&xP=Zemac&xFILTERS=A"
                          author
                          "---A")))

(global-set-key (kbd "<f7> s u") 'tv-search-gmane)

;;enable cookies in w3m
(setq w3m-use-cookies t)
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)

;;bookmark in delicious
(defun /thierry-delicious-url ()
  "Post either the url under point or the url of the current w3m page to delicious."
  (interactive)
  (let ((w3m-async-exec nil))
    (if (thing-at-point-url-at-point)
        (unless (eq (current-buffer) (w3m-alive-p)) ;bookmark url at point if we are not in w3m
          (w3m-goto-url (thing-at-point-url-at-point))))
    (w3m-goto-url
     (concat "http://del.icio.us/my_nickname?"
             "url="    (w3m-url-encode-string w3m-current-url)
             "&title=" (w3m-url-encode-string w3m-current-title)))))
(define-key w3m-mode-map "\C-x\C-a" '/thierry-delicious-url) ;except offline url at point

;;fonction search dans emacswiki

(define-key w3m-mode-map "\C-c\C-c" 
  '(lambda ()
     (interactive)
     (if (member 'w3m-href-anchor (text-properties-at (point)))
         (w3m-view-this-url)
       (w3m-submit-form))))

;;w3m antenna
(autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites." t)

;;netscape vs firefox
(setq browse-url-netscape-program '"firefox")

;;; .emacs-config-w3m.el ends here
