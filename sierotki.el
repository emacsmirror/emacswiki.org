;;; sierotki.el --- Introduce tildes after single-letter words
;;
;; Copyright (C) 1999-2006  Michał Jankowski, Jakub Narębski
;; 
;; Authors:    Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;             Michał Jankowski <michalj@fuw.edu.pl>
;;             Jakub Narębski <jnareb@gmail.com>
;; Maintainer: Jakub Narębski <jnareb@gmail.com>
;; Created:    3 Nov 1999
;;
;; Last-Updated: Tue Jun 23 17:56:54 2009 (7200 CEST)
;;           By: Jakub Narebski
;;     Update #: 111
;;
;; Version:     2.8.3
;; Keywords:    TeX, wp, convenience
;; URL:         http://www.emacswiki.org/emacs/sierotki.el
;; EmacsWiki:   NonbreakableSpace
;;
;; Compatibility: Emacs21, XEmacs21


;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://www.fsf.org/copyleft/gpl.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:

;;; Installation:

;; To use this package, put the following line in your .emacs:
;;
;;    (require 'sierotki)
;;
;; If you do not want to load this package before it is necessary, you
;; can make use of the `autoload' feature, e.g. adding to your .emacs
;; the following lines
;;
;;    (autoload 'tex-magic-space-mode "sierotki"
;;              "TeX Magic Space minor mode" t)
;;    (define-key mode-specific-map " " 'tex-magic-space-mode)
;;
;; Then after turning on `tex-magic-space-mode' via `C-c SPC'
;; the whole package will be loaded.  Attention: using autoload means
;; that this mode _can't_ be turned on automatically in LaTeX modes.
;;
;; If you want to have the TeX Magic Space mode turned on in known
;; TeX modes put the following line in your .emacs after (require 'sierotki)
;;
;;    (turn-on-tex-magic-space-in-tex-modes)
;;
;; If you want filling (`fill-paragraph' and `auto-fill-mode') to not break
;; line after single letter words, put one of the following lines in your
;; .emacs after (require 'sierotki)
;;
;;    (setq fill-nobreak-predicate 'fill-single-letter-word-nobreak-p)
;; or
;;    (setq fill-nobreak-predicate 'fill-tex-magic-space-nobreak-p)


;;; Installation[pl]:

;; Aby użyć tego pakietu, umieść następującą linijkę w swoim pliku .emacs
;;
;;    (require 'sierotki)
;;
;; Jeśli nie chcesz go ładować zanim nie będzie potrzebny, możesz użyć
;; ładowania na żądanie, np. dodając do .emacs następujące linie
;;
;;    (autoload 'tex-magic-space-mode "sierotki"
;;              "TeX Magic Space minor mode" t)
;;    (define-key mode-specific-map " " 'tex-magic-space-mode)
;;
;; Wówczas po wciśnięciu `C-c SPC' zostanie włączony TeX Magic Space mode
;; i zostanie załadowana reszta funkcji.  Uwaga: przy używaniu
;; automatycznego ładowania ten tryb _nie może_ być automatycznie włączany
;; w trybach LaTeX-owych.
;;
;; Jeśli chcesz by TeX Magic Space mode był automatycznie włączany
;; w znanych trybach TeX-owych dodaj następującą linijkę do swojego pliku
;; .emacs po (require 'sierotki)
;;
;;    (turn-on-tex-magic-space-in-tex-modes)
;;
;; Jeśli chcesz by Emacs łamiąc wiersze (`fill-paragraph' i
;; `auto-fill-mode') nie łamał linii za jednoliterowymi wyrazami
;; ('sierotkami'), dodaj jedną z poniższych linii do swojego .emacs 
;; za (require 'sierotki) 
;;
;;    (setq fill-nobreak-predicate 'fill-single-letter-word-nobreak-p)
;; lub
;;    (setq fill-nobreak-predicate 'fill-tex-magic-space-nobreak-p)



;;; Description:

;; The purpose of this package is to connect some defined words (by default
;; one letter Polish prepositions) with the following words by tilde, which
;; is the non-breakable space in TeX.  This is needed to avoid one letter
;; prepositions at line endings in TeX documents, which is required by
;; the Polish and Czech typography/typesetting rules.
;;
;; This program serves two purposes.  First of them is to check the text
;; and suggest adding missing tildes in some places.  This function is
;; implemented in `tex-hard-spaces' via `query-replace-regexp'.  It is
;; provided for convenience only to have both functionalities in the
;; same module.  More elaborated implementation can be found in the
;; `tildify' package which is part of GNU Emacs (ATTENTION: default
;; variable settings in the tildify package are suited for Czech
;; language, those here are for Polish).
;;
;; The second purpose is the automatic, in-the-fly insertion of tildes
;; after one letter prepositions during writing.  It is implemented
;; via the `tex-magic-space' command which is a kind of electric space
;; and should be bound to SPC to work.  To activate this functionality
;; you have to turn on `tex-magic-space-mode'.  After loading this
;; package this command is bound to the `C-c SPC'.  The minor mode TeX
;; Magic Space can be also turned on from the modeline minor mode
;; menu.  This mode is denoted by " ~" in the modeline.  The ":Chk"
;; after " ~" in the modeline shows that test are enabled.  You can
;; enable tests using `tex-magic-space-toggle-checking' command, bound to the
;; `C-c C-SPC'.
;;
;; For the time being the tests in `tex-magic-space-tests' are in early beta
;; phase; if you want to insert ` ' where `tex-magic-space-mode' inserts
;; `~', use `C-q SPC' to enter single space, or turn off the TeX Magic Space
;; mode fro editing the fragment of document where nonbreakable spaces are
;; not needed.
;;
;; The TeX Magic Space mode can be automatically turned on in the TeX modes
;; by adding the equivalent of `turn-on-tex-magic-space-mode' to the
;; hooks defined in the variable `tex-magic-space-mode-hooks-list' using
;; the command `turn-on-tex-magic-space-in-tex-modes'.
;;
;; NEW: There are also defined two fill predicates,
;; `fill-single-letter-word-nobreak-p' and `fill-tex-magic-space-nobreak-p',
;; which after set as value of `fill-nobreak-predicate' variable makes
;; filling (`M-q' aka `fill-paragraph' and `auto-fill-mode') to not break
;; line after single letter words.  The latter predicate uses the same test
;; as TeX Magic Space mode.  Not shown in modeline.

;; See also: http://www.emacswiki.org/cgi-bin/wiki/NonbreakableSpace
;; Documentation and comments: Jakub Narębski.


;;; Description[pl]:

;; Ten pakiet służy do dowiązywania zdefiniowanych wyrazów (domyślnie
;; jednoliterowych spójników) do następujących po nich słów za pomocą znaku
;; `~' (tyldy), niełamliwej spacji TeX-owej.  Służy to temu, aby w
;; dokumentach TeX-owych uniknąć jednoliterowych spójników na końcach linii,
;; co jest wymagane przez polskie (i czeskie) reguły typograficzne.
;;
;; Pakiet ten dostarcza dwu funkcjonalności.  Pierwszą z nich jest
;; sprawdzenie (istniejącego) tekstu i zasugerowanie dodania brakujących
;; tyld.  Jest ona implementowana przez komendę `tex-hard-spaces', za pomocą
;; `query-replace-regexp'.  Tę samą (a nawet rozszerzoną) funkcjonalność
;; znaleźć można w pakiecie `tildify' (UWAGA: domyślne ustawienia w tym
;; pakiecie są dostosowane do języka czeskiego).
;;
;; Drugą z funkcjonalności jest automatyczne wpisywanie tyld po
;; jednoliterowych spójnikach podczas pisania tekstu (w locie).  Jest ona
;; implementowana przez komendę `tex-magic-space', którą należy podpiąć do
;; spacji.  Do aktywowania tej funkcjonalności należy włączyć
;; `tex-magic-space-mode'.  Po załadowaniu tego pakietu polecenie to jest
;; przypisane do `C-c SPC'.  Tryb (minor mode) TeX Magic Space można
;; aktualnie włączyć także z modeline minor mode menu; jest on oznaczany za
;; pomocą " ~".  Dodatkowe oznaczenie ":Chk" po " ~" informuje, że
;; porady/testy są aktywne.  Testy można włączyć za pomocą polecenia
;; `tex-magic-space-toggle-checking' przypisanego do `C-c C-SPC'.
;;
;; Na razie sprawdzanie czy należy wstawiać niełamliwe spacje po
;; jednoliterowych spójnikach jest w wersji wstępnej; jeśli chcesz wstawić 
;; ` ' tam gdzie mode wstawia `~' użyj `C-q SPC' lub wyłącz tryb na czas
;; edycji fragmentu gdzie niełamliwe spacje nie są pożądane. 
;;
;; Funkcjonalność ta może być automatycznie włączana w trybach TeX-owych za
;; pomocą dodania odpowiednika `turn-on-tex-magic-space-mode' do odpowiednich
;; haczyków (zdefiniowanych w zmiennej `tex-magic-space-mode-hooks-list') za
;; pomocą polecenia (funkcji) `turn-on-tex-magic-space-in-tex-modes'.
;;
;; NOWE: Zostały także zdefiniowane dwie funkcje
;; `fill-single-letter-word-nobreak-p' i `fill-tex-magic-space-nobreak-p',
;; które wstawione jako wartość zmiennej `fill-nobreak-predicate' powodują,
;; że Emacs łamiąc linie (`M-q' czyli `fill-paragraph', oraz
;; `auto-fill-mode') nie zostawia samotnych jednoliterowych wyrazów na końcu
;; wiersza (sierotek).  Druga z funkcji używa tego samego testu co  TeX
;; Magic Space mode.  Nie pokazywane automatycznie w modeline.

;; Zobacz także: http://www.emacswiki.org/cgi-bin/wiki/NonbreakableSpace
;; Dokumentacja i komentarze: Jakub Narębski.



;;; TODO:
;; * More and better tests checking if use nonbreakable space, testing
;;   e.g. if we are in comment, table, verbatimlike environment.
;; * Guessing if the TeX Magic Space mode should be turned on based on the
;;   contents of the (La)TeX file header (preamble): either separate command
;;   'guess-*' or appropriate style files for AUCTeX: LaTeX packages polski
;;   and babel.
;; * Extend `tex-magic-space' to add tildes also after e.g. 'tys.'
;; * Add `tex-magic-space-checking-why' (a la `texmathp-why'), which would
;;   tell which tests caused magic space to be inactive. 
;; * Make the abbreviations, expansion of which ends in single-letter word,
;;   to have `~' (tilde) instead of ` ' (space) after expanded abbrev. 
;; * Bring back History: section?


;;; TODO[pl]:
;; * Wiecej i lepsze testy sprawdzające czy używać niełamliwej spacji, 
;;   np. w komentarzach, tabelach, otoczeniach typu verbatim.
;; * Zgadywanie czy należy włączyć TeX Magic Space mode na podstawie
;;   nagłówka pliku (La)TeX-owego: albo osobne polecenie 'guess-*',
;;   albo odpowiednie pliki stylu dla AUCTeX-a: pakiety polski i babel.
;; * Rozszerzyć `tex-magic-space' by dowiązywało także np. `tys.'.
;; * Dodać `tex-magic-space-checking-why' (a la `texmathp-why'), które
;;   będzie podawało dlaczego magiczna spacja jest nieaktywna.
;; * Sprawić by skróty których rozwinięcie kończy się jednoliterowym
;;   spójnikiem miały wstawianą `~' zamiast ` ' po rozwinięciu.
;; * Przywrócić sekcję Historia[pl]:?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:

;; Version 1.2 (RCS revision 1.2):
;; * Added `tex-toggle-magic-space'.
;; Version 1.3 (RCS revision 1.4):
;; * Regexps in variables and not hardcoded.
;; Version 2.0 (RCS revision 1.6):
;; * New implementation of `tex-magic-space'.
;; Version 2.3 (RCS revision 1.12):
;; * TeX Magic Space minor mode (bound to `C-c SPC')
;; Version 2.4 (RCS revision 1.17):
;; * Added checking if the `tex-magic-space' should be active or not
;;   (e.g. it should be inactive in math mode detected using `texmathp').
;;   It was implemented using advices.
;; Version 2.5 (RCS revision 1.26):
;; * Removed `tex-toggle-magic-space'; use `tex-magic-space-mode' instead.
;; Version 2.6 (RCS revision 1.31):
;; * Checking if `tex-magic-space' should be active was changed from
;;   the around advice(s) to the conditional in main function.
;; Version 2.7 (RCS revision 1.43, update #61):
;; * Mode maked customizable.
;; * Prefer `tildify-buffer' to own version of `tex-hard-spaces'.
;; * Prefer `define-minor-mode' to `add-minor-mode' in GNU Emacs.
;; Version 2.8 (RCS revision 1.46, update #103):
;; * The main test put into `tex-magic-space-p'.
;; * Added `fill-single-letter-word-nobreak-p' and
;;   `fill-tex-magic-space-nobreak-p' to use as value of
;;   `fill-nobreak-predicate' - TeX Magic Space equivalent for plain text.


;;; Change Log[pl]:

;; Wersja 1.2 (RCS revision 1.2):
;; * Dodano `tex-toggle-magic-space'.
;; Wersja 1.3 (RCS revision 1.4):
;; * Wyrażenia regularne w zmiennych, a nie zapisane wewnątrz funkcji.
;; Wersja 2.0 (RCS revision 1.6):
;; * Nowa implementacja `tex-magic-space'.
;; Wersja 2.3 (RCS revision 1.12):
;; * Pojawił się TeX Magic Space minor mode (przypisany do `C-c SPC').
;; Wersja 2.4 (RCS revision 1.17):
;; * Dodane porady i polecenie do ich włączana (przypisane do `C-c @'), aby
;;   `tex-magic-space' pozostawała nieaktywna tam gdzie nie trzeba (np.
;;   w trybie matematycznym wykrywanym za pomocą `texmathp').
;; Wersja 2.5 (RCS revision 1.26):
;; * Usunięcie `tex-toggle-magic-space'; użyj `tex-magic-space-mode'.
;; Wersja 2.6 (RCS revision 1.31):
;; * Sprawdzania czy `tex-magic-space' powinno być nieaktywne zostało
;;   przepisane za pomocą instrukcji warunkowej w głównej funkcji zamiast
;;   używania do tego porad (advice).
;; Wersja 2.7 (RCS revision 1.43, update #61):
;; * Parametry mode (trybu) ustalalne za pomocą `customize'.
;; * Preferuj `tildify-buffer' zamiast własnej wersji `tex-hard-spaces'.
;; * Preferuj `define-minor-mode' zamiast `add-minor-mode' w GNU Emacs.
;; Wersja 2.8 (RCS revision 1.46, update #103):
;; * Test na sierotki wydzielony do `tex-magic-space-p'.
;; * Dodano funkcje `fill-single-letter-word-nobreak-p' 
;;   i `fill-tex-magic-space-nobreak-p' do użycia jako wartość zmiennej 
;;   `fill-nobreak-predicate' - równoważnik TeX Magic Space dla tekstu.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defgroup sierotki nil
  "Support for non-breakable spaces after short (single-letter) words."
  :tag "Tex Magic Space"
  :group 'tex)

(require 'cl) 			; to use `some' in `tex-magic-space'

;;;; ======================================================================
;;;; Add non-breakable spaces in existing document, interactively.
;;;; Usuwanie sierotek w istniejącym dokumencie, interaktywne.

;;; Hard spaces by Ryszard Kubiak <rysiek@ipipan.gda.pl>
;;; Modified by Jakub Narębski <jnareb@fuw.edu.pl>

;; Zastępuje znaki odstępu przez znaki tyldy `~', czyli TeX-ową niełamliwą
;; spację, po jednoliterowych [polskich] spójnikach w całym buforze.
;; Poniższa zmienna definiuje wyrażenie regularne używane w `tex-hard-spaces'
(if (fboundp 'tildify-buffer)
    ;; use better tildify.el solution
    ;; użyj rozwiązania z tildify.el
    (progn
      (message "tex-hard-spaces aliased to tildify-buffer")
      (defalias 'tex-hard-spaces 'tildify-buffer))

  (defcustom tex-hard-spaces-regexp "\\<\\([aeiouwzAEIOUWZ]\\)\\s +"
    "*Regular expression which detects single [aeiouwz] for `tex-hard-spaces'.
The part of regular expression which matches string to be saved
should be in parentheses, so the replace part \\\\1~ will work.

Used as first argument to `query-replace-regexp'."
    :type '(regexp)
    :group 'sierotki)
  
  ;; Zwykłe `query-replace-regexp', czyli C-M-% dla odpowiedniego
  ;; wyrażenia regularnego, zapisanego w `tex-hard-spaces-regexp'
  (defun tex-hard-spaces ()
    "Replace whitespace characters after single-letter word with `~'.
Replaces whitespace characters following single-letter conjunctions by `~',
the TeX non-breakable space in whole buffer, interactively.
Uses `tex-hard-spaces-regexp' for single-letter conjunctions detection.

It can be used to bind single-letter conjunction to the word following it in
the existing text, using `~' (the TeX non-breakable space), so there are no
single-letter conjunctions at the end of the line (known as 'orphans').

For on-the-fly 'tildification' turn on TeX Magic Space minor mode using
command \\[tex-magic-space-mode].

It is implemented using `query-replace-regexp'."
    (interactive)
    (query-replace-regexp tex-hard-spaces-regexp
			  "\\1~"))
  ) ;; end (if (fboundp 'tildify-buffer) ...)


;;;; ======================================================================
;;;; On-the-fly inserting of non-breakable spaces.
;;;; Zapobieganie powstawaniu sierotek 'w locie'

;;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
;;; Modified by Jakub Narębski <jnareb@fuw.edu.pl>

;;; ----------------------------------------------------------------------
;;; Tests for `tex-magic-space'
;;; Testy dla `tex-magic-space'

;; Didn't work in XEmacs
;; Nie działał w XEmacs
(defun texinverbp ()
  "Determine if point is inside LaTeX \\verb command.
Returns nil or the pair (POINT-VERB-BEG . POINT-VERB-END) of positions where
\\verb argument begins and ends or the position POINT-VERB-BEG where \\verb
command argument begins if \\verb is unfinished (has no closing delimiter).

This command uses the fact that the argument to \\verb cannot contain end of
line characters.  Does not work with nested \\verbs."
  (interactive)
  (let ((point (point))
	beg end	delim)
  (save-excursion
    (and (setq beg (and (re-search-backward "\\\\verb\\*?\\([^a-zA-Z*\\n]\\)"
					   (point-at-bol) t)
		       (match-end 0)))
	 (setq delim (regexp-quote (match-string 1)))
	 (goto-char beg)
	 ;;(or (insert "!") t)
	 (setq end (and (skip-chars-forward (concat "^" delim)
					    (point-at-eol))
			(point)))
	 (or (eolp)
	     (looking-at (concat "[" delim "]")))
	 ;;(or (insert "!") t)
	 (cond ((>= point end) nil)
	       ((eolp) beg)
	       (t (cons beg end)))))))


;;; ......................................................................
;;; Turning on tests for tex-magic-space
;;; Aktywacja sprawdzania/testów dla tex-magic-space i podobne
(defcustom tex-magic-space-do-checking nil
  "*Non-nil if `tex-magic-space' should check `tex-magic-space-tests'.
Set it to non-nil value if you want TeX Magic Space mode to check
if it should add extra checking in addition to testing if we are
after single-letter word (as defined by `tex-magic-space-regexp').
For example such tests (as defined in `tex-magic-space-tests') could
check if we are in restricted horizontal mode, like math mode.

Set also by `tex-magic-space-toggle-checking'"
    :type '(boolean)
    :group 'sierotki)

(defcustom tex-magic-space-tests
  (list
   (unless (and (boundp 'running-xemacs) running-xemacs) 'texinverbp)
   (if (or (featurep 'tex-site) (fboundp 'texmathp)) 'texmathp))
  "List of test functions for `tex-magic-space'.

List of functions which are invoked, in order, to determine whether
`tex-magic-space' could insert a ~ (i.e., a tex non-breakable
space).  The tilde can be inserted only when every function returns
a nil value.  The tests are run only when `tex-magic-space-do-checking'
has non-nil value"
  :type '(repeat function)
  :group 'sierotki)


;;; ----------------------------------------------------------------------
;;; On-the-fly tildes insertion
;;; Wstawianie tyld w locie

;; UWAGA: [czasami] polskie literki są traktowane jako koniec słowa dla 8bit
;;        tzn. przy użyciu `standard-display-european' do ich wprowadzania.
;;        Będę próbować znaleźć dokładne warunki wystąpienia błędu.
(defcustom tex-magic-space-regexp "\\<[aeiouwzAEIOUWZ]\\'"
  "*Regular expression which detects single [aeiouwz] for `tex-magic-space'.
`tex-magic-space' inserts `~' if this expression matches two characters before
point, otherwise it inserts the key it is bound to (\\[tex-magic-space]),
usually SPC.

This regular expression should end with [aeiouwzAEIOUWZ]\\\\' to match possible
single letter conjunction against the letter directly before the point.  The
part before [aeiouwzAEIOUWZ] should match word beginning/boundary.

ATTENTION: sometimes in unibyte mode the non US-ASCII letters are considered
word boundary, even when they are word constituents."
  :type '(regexp)
  :group 'sierotki)

;; !!! Expand docstring !!!
;; !!! Docstring do poprawienia !!!
(defun tex-magic-space-p ()
  "Returns true if there should be inserted nonbreakable space."
  (string-match tex-magic-space-regexp 
	   (buffer-substring (max (point-min) (- (point) 2)) (point))))

(defun tex-magic-space (&optional prefix)
  "Magic-space - insert non-breakable space after a single-letter word.
Interactively, PREFIX is the prefix arg (default 1).
Uses `tex-magic-space-regexp' for single-letter words detection.

Works well with auto filling unless `~' is in the table `auto-fill-chars',
in which case `~' is inserted but might be followed by line break.
Works with abbrev expansion with the following exceptions:
 - doesn't do abbrev expansion if abbrev is single letter word
   and `~' is word constituent (according to current syntax table)
 - abbrevs ending with single-letter word will have `~' instead of space
   after the expansion
 - abbrevs with expansion ending with single-letter word won't have
   the SPC following single-letter word substituted with `~'

Should not be used directly.

To use it turn on TeX Magic Space minor mode using command
`tex-magic-space-mode' (\\[tex-magic-space-mode]).

See also: `tex-hard-spaces'"
  (interactive "p")
  (unless (and tex-magic-space-do-checking 
	       (some (lambda (f) (and (functionp f) (funcall f))) 
		     tex-magic-space-tests))
    (when (tex-magic-space-p) 
      (setq last-command-char ?~))) 
  (self-insert-command (or prefix 1)))

(defun debug-tex-magic-space (&optional prefix)
  "Version of `tex-magic-space' which doesn't do any testing."
  (interactive "p")
  (let ((tex-magic-space-do-checking nil))
    (tex-magic-space prefix)))

;;; ----------------------------------------------------------------------
;;; The TeX Magic Space mode equivalent for filling (word wrap)
;;; Równoważnik TeX Magic Space mode dla automatycznego zawijania linii

;;; see: http://www.emacswiki.org/cgi-bin/wiki/FillParagraph
;; It is simplified `fill-french-nobreak-p' from textmodes/fill.el.
;; The function `fill-french-nobreak-p' first appeared in textmodex/fill.el
;; rev. 1.132, and the single-letter detection code first appeared in
;; rev. 1.132, correct in 1.181.  Not present in GNU Emacs 21.3
(defun fill-single-letter-word-nobreak-p ()
  "Don't break a line after single letter word.
This is used in `fill-nobreak-predicate' to prevent breaking lines just
after a single letter word."
  (save-excursion
    (skip-chars-backward " \t")
    (unless (bolp)
      (backward-char 1)
      ;; Don't cut right after a single-letter word.
      (and (memq (preceding-char) '(?\t ?\ ))
	   (eq (char-syntax (following-char)) ?w)))))

(defun fill-tex-magic-space-nobreak-p ()
  "Don't break a line after where `tex-magic-space' would insert `~'.

Don't break a line after place where function `tex-magic-space' (or, to be
more exact `tex-magic-space-p' test) would insert non-breakable space,
i.e. tilde ('~').
 
This is used in `fill-nobreak-predicate'."
  (save-excursion
    (skip-chars-backward " \t")
    (unless (bolp)
      (backward-char 1)
      (tex-magic-space-p))))

;; `fill-region-as-paragraph' used by `fill-paragraph', and
;; `do-auto-fill' used by `auto-fill-mode' uses this predicate.
;(setq fill-nobreak-predicate 'fill-single-letter-word-nobreak-p)

;;; ----------------------------------------------------------------------
;;; The TeX Magic Space mode definition and initialization
;;; Definicja trybu TeX Magic Space i jego inicjalizacja

;;; this can be done via defcustom, a la:
;;     (defcustom show-paren-mode nil
;;       "Toggle Show Paren mode..."
;;       :set (lambda (symbol value)
;;              (show-paren-mode (or value 0)))
;;       :initialize 'custom-initialize-default
;;       :type 'boolean
;;       :group 'paren-showing
;;       :require 'paren)

(defvar tex-magic-space-mode nil
  "*Determines if TeX Magic Space mode is active.
You can set it directly or use the command `tex-magic-space-mode'.")
(make-variable-buffer-local 'tex-magic-space-mode)

;; Internal tex-magic-space-mode +/- checking designation string for mode line.
(defvar tex-magic-space-mode-string " ~")
(make-variable-buffer-local 'tex-magic-space-mode-string)

(defvar tex-magic-space-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'tex-magic-space)
    (if (and (boundp 'running-xemacs) running-xemacs)
	(define-key map [(control c) (control space)] 'tex-magic-space-toggle-checking)
      (define-key map [?\C-c ?\C- ] 'tex-magic-space-toggle-checking))
    map)
  "Keymap for TeX Magic Space mode.")


;;;###autoload
(defun turn-on-tex-magic-space-mode ()
  "Turn on TeX Magic Space mode.
Does not force the modeline update."
  (setq tex-magic-space-mode t))

;;;###autoload
(defun tex-magic-space-mode (&optional arg)
  "Toggle TeX Magic Space mode.
With ARG, turn TeX Magic Space mode on if and only if ARG is positive.
In TeX Magic Space mode typing a space inserts tilde, the TeX non-breakable
space, after single-letter prepositions described by `tex-magic-space-regexp'
if we are not in one of situations described by `tex-magic-space-tests'.
The testing can be toggled using `\\[tex-magic-space-toggle-checking]' which
runs `tex-magic-space-toggle-checking'.
 
\\<tex-magic-space-mode-map>"
  (interactive "P")
  (setq tex-magic-space-mode
	(if (null arg) (not tex-magic-space-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defun tex-magic-space-toggle-checking (&optional arg)
  "Toggle whether `tex-magic-space' check `tex-magic-space-tests'.
With prefix argument ARG, activate checking if ARG is positive,
otherwise deactivate it.

Sets `tex-magic-space-do-checking'."
  (interactive "P")
  (setq tex-magic-space-do-checking
	(if (null arg) (not tex-magic-space-do-checking)
	  (> (prefix-numeric-value arg) 0)))
  (if tex-magic-space-do-checking
      (setq tex-magic-space-mode-string " ~:Chk")
    (setq tex-magic-space-mode-string " ~"))
  (if tex-magic-space-mode
      (force-mode-line-update)
    (message "Checking tests for tex-magic-space %sctivated."
	     (if tex-magic-space-do-checking "a" "dea"))))


(defun use-add-minor-mode ()
  "Use `add-minor-mode' to create minor mode `tex-magic-space-mode'.
In XEmacs `add-minor-mode' is compiled Lisp function, in GNU Emacs 
it is an XEmacs-compatibility functio in `subr'."
  (message "Define tex-magic-space-mode using add-minor-mode") ; DEBUG!
  (put 'tex-magic-space-mode :included '(memq major-mode '(latex-mode
							   tex-mode)))
  (put 'tex-magic-space-mode :menu-tag "TeX Magic Space")
  (add-minor-mode 'tex-magic-space-mode
		  tex-magic-space-mode-string
		  tex-magic-space-mode-map))

(defun use-define-minor-mode ()
  "Use `define-minor-mode' to create minor mode `tex-magic-space-mode'.
The `define-minor-mode' function is defined in `easy-mmode'."
  (message "Define tex-magic-space-mode using define-minor-mode") ; DEBUG!
  (define-minor-mode tex-magic-space-mode
    "Toggle TeX Magic Space mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

In TeX Magic Space mode typing a space inserts tilde, the TeX non-breakable
space, after single-letter prepositions described by `tex-magic-space-regexp'
if we are not in one of situations described by `tex-magic-space-tests'.
The testing can be toggled using `\\[tex-magic-space-toggle-checking]' which
runs `tex-magic-space-toggle-checking'.
 
\\<tex-magic-space-mode-map>"
    (memq major-mode '(latex-mode tex-mode)) ; INIT-VALUE
    tex-magic-space-mode-string
    tex-magic-space-mode-map
    ;; BODY
    (define-key mode-line-mode-menu [tex-magic-space-mode]
      `(menu-item ,(purecopy "TeX Magic Space") tex-magic-space-mode
		  :visible (memq ,major-mode (latex-mode tex-mode))
		  :button (:toggle . tex-magic-space-mode)))
    :group 'sierotki))
    
(defun use-own-code-to-define-mode ()
  "Define minor mode `tex-magic-space-mode' \"by hand\"."
  (message "Define tex-magic-space-mode using own code") ; DEBUG!
  (unless (assq 'tex-magic-space-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tex-magic-space-mode (" ~" (tex-magic-space-do-checking ":Chk")))
		;; (propertize " ~"
		;;	       'local-map mode-line-minor-mode-keymap
		;;	       'help-echo "mouse-3: minor mode menu")
		minor-mode-alist)))
  (unless (assq 'tex-magic-space-mode-map minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'tex-magic-space-mode tex-magic-space-mode-map)
		minor-mode-map-alist)))
  (define-key mode-line-mode-menu [tex-magic-space-mode]
    `(menu-item ,(purecopy "TeX Magic Space") tex-magic-space-mode
		:visible (memq ,major-mode (latex-mode tex-mode))
		:button (:toggle . tex-magic-space-mode))))


;; Define minor mode `tex-magic-space-mode'
;; Zdefiniuj `tex-magic-space-mode' 
(if (and (boundp 'running-xemacs) running-xemacs)
    (cond 				; XEmacs
     ((fboundp 'add-minor-mode) (use-add-minor-mode))
     ((fboundp 'define-minor-mode) (use-define-minor-mode))
     (t	(use-own-code-to-define-mode)))
  ;;
  (cond 				; GNU Emacs
   ((fboundp 'define-minor-mode) (use-define-minor-mode))
   ((fboundp 'add-minor-mode) (use-add-minor-mode))
   (t (use-own-code-to-define-mode))))


;;;; ======================================================================
;;; Initialization by Jakub Narębski <jnareb@fuw.edu.pl>
;;; and Adam Przepiórkowski <adamp_at@at_ipipan.waw.pl>
;;; Inicjalizacja dla zapobiegania powstawaniu sierotek 'w locie'

;; Set globally `C-c SPC' to `tex-magic-space-mode'
;; Przypisz globalnie `tex-magic-space-mode' do `C-c SPC'
(define-key mode-specific-map " " 'tex-magic-space-mode)

;; Turn on TeX Magic Space mode for known (La)TeX modes
;; Włącz TeX Magic Space mode dla znanych trybów (La)TeX-owych
(defmacro tex-magic-space-mode-add-to-hook (hook)
  "Add `turn-on-tex-magic-space-mode' to HOOK."
  `(add-hook ,hook 'turn-on-tex-magic-space-mode))

(defmacro tex-magic-space-mode-initialize (hooks)
  "Add `turn-on-tex-magic-space-mode' to each of HOOKS."
  `(dolist (hook ,hooks)
     (tex-magic-space-mode-add-to-hook hook)))

(defvar tex-magic-space-mode-hooks-list
  '(TeX-mode-hook LaTeX-mode-hook 	; for AUCTeX
    tex-mode-hook			; for tex-mode
    reftex-mode-hook			; for RefTeX minor mode
    bibtex-mode-hook)			; for BibTeX
  "List of hooks to which add turning on TeX Magic Space minor mode.")

(defun turn-on-tex-magic-space-in-tex-modes ()
  "Turn on TeX Magic Space mode automatically in TeX modes.
Adds `turn-on-tex-magic-space-mode' to the hooks listed in the
variable `tex-magic-space-mode-hooks-list'."
  (tex-magic-space-mode-initialize tex-magic-space-mode-hooks-list))


;;;; ======================================================================
;;;; Announce
;;;; Zakończenie

(provide 'sierotki)

;; Local variables:
;; coding: iso-latin-2
;; End:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sierotki.el ends here
