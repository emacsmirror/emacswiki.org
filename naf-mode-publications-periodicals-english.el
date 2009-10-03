;;; this is naf-mode-publications-periodicals-english.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-publications-periodicals-english
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS: 
;;; `naf-mode-publications-periodicals-english-one-word', 
;;; `naf-mode-publications-periodicals-english'
;;;
;;; VARIABLES:
;;; `*naf-publications-periodicals-english*'
;;; `*naf-publications-periodicals-english-one-word*'
;;; `*naf-mode-publications-periodicals-english-xrefs*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED: 
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/naf-mode-publications-periodicals-english.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T20:18:42-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-09T16:59:15-04:00Z}#{09327} - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; ==============================
;;; Copyright (C) 2009 MON KEY
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T17:25:09-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-mode-publications-periodicals-english-xrefs*
  '(*naf-publications-periodicals-english*
    *naf-publications-periodicals-english-one-word*
    *naf-publications-periodicals-french*
    *naf-naf-publications-periodicals-intnl*
    *naf-mode-publications-periodicals-english-xrefs*
    mon-help-naf-mode-faces)
  "*List of symbol names of variables which xref each other in the
`naf-mode-publications-periodicals-english' package.
See FILE: \"./naf-mode-publications-periodicals-english.el\"."))
;;
;;;test-me; *naf-mode-publications-periodicals-english-xrefs*
;;
;;;(progn (makunbound '*naf-mode-publications-periodicals-english-xrefs*)
;;;       (unintern '*naf-mode-publications-periodicals-english-xrefs*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T17:28:34-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-publications-periodicals-english*
  '("Advertising Arts"
    "Amazing Stories"
    "American Legion Monthly"
    "American Legion Weekly"
    "American Magazine"
    "American Printer"
    "Apparel Arts"
    "Argosy Weekly"
    "Arts & Architecture"
    "Arts & Decoration"
    "Arts and Decoration"
    "Asia Magazine"
    "Ballou's Pictorial"
    "Ballyhoo"
    "Black Cat"
    "Blue Book"
    "Boys' Life"
    "Burr McIntosh Monthly"
    "Cartoons Magazine"
    "Chicago Tribune"
    "Child Life"
    "Child Life: The Children's Own Magazine"
    "College Humor"
    "Collier's Weekly"
    "Collier's"
    "Colliers"
    "Country Gentleman"
    "Country Life"
    "Every Week"
    "Everybody's Magazine"
    "Excella Fashion Quarterly"
    "Fashion Age"
    "Fashions of the hour"
    "Field & Stream"
    "Field and Stream"
    "Film Fun"
    "Flair Magazine"
    "Fleuron"
    "Fortune Magazine"
    "Frank Leslie's"
    "Gentle Reader"
    "Good Housekeeping"
    "Gourmet Magazine"
    "Graphis"
    "Hampton's Magazine"
    "Harper's Bazaar"
    "Harper's Bazar"
    "Harpers Magazine"
    "Harper's Magazine"
    "Harper's Monthly"
    "Harper's Weekly"
    "Harper's Young People"
    "Harper's"
    "Harpers Bazaar"
    "Harpers Magazine"
    "Harpers Weekly"
    "Hearst's International"
    "Home & Field"
    "Home and Field"
    "House & Garden"
    "House and Garden"
    "House Beautiful"
    "House and Garden"
    "Household Magazine"
    "Illustrated London News"
    "Inland Printer"
    "International Studio"
    "International Textiles"
    "Journal of Typographic Research"
    "Ladies Home Journal"
    "Ladies' Home Journal"
    "Leslie's Illustrated Weekly"
    "Leslie's Weekly"
    "Leslie's"
    "Liberty Magazine"
    "Life Magazine"
    "Literary Digest"
    "McCall's"
    "McClure's"
    "Metropolitan Magazine"
    "Modern Priscilla"
    "Modes & manners"
    "Motion Picture Magazine"
    "Munsey's"
    "National Geographic"
    "New Masses"
    "New York Daily News Syndicate"
    "New York Herald Tribune"
    "New York Times Magazine"
    "New York Times"
    "New York Tribune Magazine"
    "New Yorker"
    "Newsweek"
    "Opus International"
    "Outdoor Life"
    "Outing Magazine"
    "Paris Vivant"
    "Penrose Annual"
    "Penrose's Pictorial Annual"
    "People's Favorite"
    "People's Popular Monthly"
    "Peoples Home Journal"
    "Peoples' Popular Monthly"
    "Photoplay"
    "Physical Culture"
    "Pictorial Review"
    "Picture Post"
    "Popular Magazine"
    "Popular Science"
    "Printer's Ink"
    "Punch Almanac"
    "Radio Digest"
    "Ramparts Magazine"
    "Ramparts"
    "Redbook"
    "Saturday Evening Post"
    "School Arts Magazine"
    "Scribner's"
    "Scribners"
    "Shadowland"
    "Smart Set"
    "Saint Nicholas"
    "St\. Nicholas"
    "Style Arts"
    "Success Magazine"
    "Successful Farming"
    "Sunset Magazine"
    "The Century"
    "The Dance"
    "The Farmer's Wife"
    "The Herald"
    "The New Yorker"
    "The Post"
    "The Shriner"
    "The Sketch"
    "The Strand"
    "The Studio"
    "The Times"
    "Theatre Magazine"
    "Time Magazine"
    "Town & Country"
    "Town and Country"
    "Travel Magazine"
    "Vanity Fair"
    "Vogue Magazine"
    "Vogue magazine"
    "Vogue"
    "Weird Tales"
    "Western Story"
    "Windsor Magazine"
    "Woman's Day"
    "Woman's World"
    "Women's Home Companion"
    "Workers Monthly"
    "Yellow Book"
    "Youth's Companion")
  "*Keyword list of English periodical names for `naf-mode' font-locking."))
;;
(eval-and-compile
(defconst naf-mode-publications-periodicals-english
  (concat  (regexp-opt *naf-publications-periodicals-english* 'paren) "\\_>" )))
;;
(eval-and-compile
  (mon-help-swap-var-doc-const-val
      *naf-publications-periodicals-english* naf-mode-publications-periodicals-english
      *naf-mode-publications-periodicals-english-xrefs* naf-mode-publication-periodical-fface))
;;
;;(progn (makunbound '*naf-publications-periodicals-english*) 
;;       (unintern '*naf-publications-periodicals-english*)
;;       (makunbound 'naf-mode-publications-periodicals-english)
;;       (unintern 'naf-mode-publications-periodicals-english))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T17:30:29-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-publications-periodicals-english-one-word*
  '("Beau"
    "Argosy"
    "Judge"
    "Life"
    "Liberty"
    "Cosmopolitan"
    "Esquire"
    "Delineator"
    "Evergreen"
    "Everybody's"
    "Fortune"
    "Flair"
    "Gourmet"
    "Zoom"
    "Puck"
    "Punch"
    "Metropolitan")
  "*Keyword list of English periodical names \(one-word only\)
for `naf-mode' font-locking."))
;;
(eval-and-compile
(defconst naf-mode-publications-periodicals-english-one-word
  (regexp-opt *naf-publications-periodicals-english-one-word* 'paren)))
;;
(eval-and-compile
  (mon-help-swap-var-doc-const-val
      *naf-publications-periodicals-english-one-word* naf-mode-publications-periodicals-english-one-word
      *naf-mode-publications-periodicals-english-xrefs* naf-mode-publication-periodical-fface))
;;
;;(progn (makunbound '*naf-publications-periodicals-english-one-word*) 
;;;      (unintern '*naf-publications-periodicals-english-one-word*)
;;       (makunbound 'naf-mode-publications-periodicals-english-one-word) 
;;       (unintern 'naf-mode-publications-periodicals-english-one-word))

;;; ==============================
(provide 'naf-mode-publications-periodicals-english)
;;; ==============================

;;; ================================================================
;;; naf-mode-publications-periodicals-english.el ends here
;;; EOF
