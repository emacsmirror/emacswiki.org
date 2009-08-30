;;; amazon.el -- Emacs client for Amazon E-Commerce Service (i.e. use Emacs to search books on Amazon.com)

;;; Version: 0.1
;;; Time-stamp: <2007-04-24 09:37:21 hhalvors>
;;; Author: hhalvors at princeton dot edu

;;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEPENDENCIES: 
;;;
;;; 1. A recent version of url.el -- viz., the version in Emacs 22 CVS
;;; -- and you must not load an older version of url.el (e.g. as
;;; included in the now-defunct w3).  We use the function
;;; "url-retrieve" with three arguments, not two (as in previous
;;; versions of url.el).
;;;
;;; 2. eieio.el -- objects in Emacs lisp
;;; http://cedet.sourceforge.net/eieio.shtml  In Gentoo: "emerge app-emacs/cedet"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOCUMENTATION
;;;
;;; To install, put 'amazon.el' your load path.  Then insert the
;;; following into your .emacs file:
;;;
;;;    (defconst amazon-subscription-id "XYZ")
;;;    (require 'amazon) 
;;;    
;;; replacing "XYZ" with your Amazon web service Subscription ID.  You
;;; can subscribe (for free) to amazon web services at aws.amazon.com.


;;; USAGE: M-x amazon 
;;; =================
;;; you will then be asked to enter a search string.  The input format
;;; is as:
;;;
;;;       :title critique of pure reason :author kant
;;;
;;; The valid search fields are ":title", ":author", ":isbn" and
;;; ":keywords".  You can also abbreviate any search field with its
;;; first letter, such as ":a" for ":author".  For example, the
;;; following is a valid search:
;;; 
;;;       :t critique of pure reason :a kant
;;; 
;;; If you enter more than one word into a field, you do *not* need to
;;; wrap the words in quotes.  We assume that any words not prefixed
;;; by ":" are part of your input to that field.
;;;
;;; If the search succeeds, you will be directed to a buffer called
;;; "*amazon search results*".  For what to do next, skip below to
;;; "Search Results Buffer".


;;; BEHIND THE SCENES
;;; =================
;;; In the background, everything is based on the eieio object system.
;;; (If you are familiar with Common Lisp, eieio is to Emacs Lisp as
;;; CLOS is to Common Lisp.
;;;
;;; When you issue a search request, you are building a 'CartCreate'
;;; object, which is then used to construct a REST url to post to the
;;; Amazon Ecommerce server.  Amazon returns results in xml format.
;;; We then use 'xml.el' to parse these results, and to build a
;;; 'CartCreateResponse' object out of the parsed results.  To a first
;;; approximation, a CartCreateResponse object is simply a list of
;;; 'Item' objects; and an 'Item' object has all the slots you might
;;; expect to describe a book that is for sale -- e.g. Author, Title,
;;; ListPrice.
;;;
;;; Since humans cannot directly access eieio objects, we print these
;;; objects (i.e. the Items) in human readable format.  Furthremore,
;;; so that we can manipulate the behind the scenes objects, we add a
;;; text property 'noumenon' to the printed representation of the
;;; object.  The value of 'noumenon' for a string is simply the Item
;;; of which the string is a representation.
;;;
;;; The Item object itself contains much more information than is
;;; represented in the textual output -- e.g. the Item contains a list
;;; of "Offers" from third party sellers, as well as Editorial Reviews
;;; and Customer Reviews.  We define some commands that provide easy
;;; access to these slots of the Item: 'amazon-offers' and 'amazon
;;; reviews'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORGANIZATION OF BUFFERS AND MOVEMENT BETWEEN BUFFERS
;;;
;;; amazon.el has the following buffer types:
;;; 
;;;  1. Item Search Response : The visual representation of an
;;;     ItemSearchResponse object
;;;  
;;;  2. Offers : The visual representation of the offers slot of an Item object
;;;     
;;;     Note that as an Item object is displayed in an
;;;     ItemSearchResponse buffer, an Offers buffer will usually have
;;;     an ItemSearchResponse buffer as a 'parent.'
;;; 
;;;  3. Reviews : The visual representation of the union of the
;;;     'editorialreviews' and 'customerreviews' slots of an Item
;;;     object
;;;
;;;     Since Item objects are displayed in Item Search Response
;;;     buffers, a Reviews buffer will typically be a 'child' of an
;;;     Item Search Response buffer.

;;; Search Results Buffer:
;;; ====================== 
;;; This buffer contains the textual representation of items, separated by the string
;;; 'amazon-item-view-separator'.  This buffer is in amazon-mode,
;;; which has the following salient keybindings:

;;;  "a"       - add this item to the local cart 
;;;  "C-c C-o" - show "offers" for this item
;;;  "w"       - visit the Amazon URL for this item in your favorite web browser
;;;  "i"       - lookup this ASIN/ISBN (only works when point is on an ASIN or ISBN, listed under 'Alternate Versions')
;;;  "C-c C-t" - show contents of the local cart
;;;  "n"       - jump to the next item
;;;  "p"       - jump to the previous item
;;;  "r"       - show reviews for this item


;;; Offers Buffer:
;;; ==============
;;; ... uses the same keymap as Search Results Buffer. 


;;; Local Cart Buffer:
;;; ==================
;;; The contents of the local cart are displayed in a buffer in
;;; amazon-cart-mode.  This mode has the following salient
;;; keybindings:

;;; "C-c C-c" - submit the local cart to Amazon
;;; "C-c C-r" - remove item under point from the local cart
;;; "C-c C-d" - remove item under point from the local cart


;;; Buying Books:
;;; =============
;;; Amazon E-Commerce Service does not allow direct purchases via REST
;;; URL requests.  As a result, purchases must be completed by
;;; visiting an Amazon URL and logging in to one's Amazon account.
;;; When one submits one's local cart, Amazon returns a
;;; "CartCreateResponse" object, which includes a "Purchase URL".  If
;;; one then visits this Purchase URL, then one finds the cart waiting
;;; to be purchased.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TO DO LIST: 

;; 1. Deal with case of no results for ItemSearch and ItemLookup

;; 2. Implement reading reviews of third party Sellers

;; 3. Check if request is Valid, signal error if it is not

;; 4. Can one add items to one's Wish List? (need to set Amazon
;; identity cookie?)

;; 5. The generic function 'amazon-send' currently only returns the
;; first ten offers for any given Item; we want to be able to loop
;; through to get all offers

;; 6. The *amazon-items* buffer modeline should give total number of
;; offers and (?) search terms

;; 7. Make a 'search summary' page whose lines contain previous
;; searches if we click on line, then we go to the results page for
;; that search

;; 8. Perhaps integrate 'search summary' page with 'cart(s)' by the
;; way: completely trivial to implement multiple carts

;; 9. Implement actions on the remote cart: Add item, Remove item,
;; etc.. (Maybe this is superfluous for Emacs, since we can just
;; modify the local cart and then resubmit the request to AWS.)

;; 10. Export book records to BibTeX

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CODE

(eval-when-compile (require 'cl))
(require 'xml)
(require 'url)
(require 'mm-url)
(require 'eieio) 

(defcustom amazon-item-view-separator 2
  "How many blank lines to put between displayed items."
  :group 'amazon)

(defcustom amazon-associates-id ""
  "Amazon Web Services associate id"
  :group 'amazon)

(defcustom amazon-subscription-id  ""
  "Amazon Web Services subscription id"
  :group 'amazon)

(defvar amazon-mode-map nil
   "Keymap for amazon mode.")

(if amazon-mode-map
    ()
  (setq amazon-mode-map (make-keymap))
  (suppress-keymap amazon-mode-map)
  (define-key amazon-mode-map "w" 'amazon-browse-url)
  (define-key amazon-mode-map "\C-c\C-w" 'amazon-browse-url)
  (define-key amazon-mode-map "\C-c\C-o" 'amazon-offers)
  (define-key amazon-mode-map "\C-c\C-a" 'amazon-add-to-cart)
  (define-key amazon-mode-map "r" 'amazon-show-reviews)
  (define-key amazon-mode-map "\C-c\C-t" 'amazon-cart-show)
  (define-key amazon-mode-map "q" 'bury-buffer)
  (define-key amazon-mode-map "n" 'amazon-next-item)
  (define-key amazon-mode-map "p" 'amazon-previous-item)
  (define-key amazon-mode-map "i" 'amazon-lookup-isbn)    ;; when point is at an ISBN, lookup the book
  (define-key amazon-mode-map "l" 'amazon-lookup-isbn)    ;; when point is at an ISBN, lookup the book
  (define-key amazon-mode-map "\r" 'amazon-lookup-isbn)     
  ; TO DO: define a command that will copy the record at point
)

(defvar amazon-offers-mode-map nil
   "Keymap for amazon offers mode.")

(if amazon-offers-mode-map
    ()
  (setq amazon-offers-mode-map (make-keymap))
  (suppress-keymap amazon-offers-mode-map)
  (define-key amazon-offers-mode-map "\C-c\C-a" 'amazon-add-to-cart)
  (define-key amazon-offers-mode-map "\C-c\C-t" 'amazon-cart-show)
  (define-key amazon-offers-mode-map "\C-c\C-k" 'amazon-buffer-up)
  (define-key amazon-offers-mode-map "q" 'amazon-buffer-up)
  (define-key amazon-offers-mode-map "n" 'amazon-next-item)
  (define-key amazon-offers-mode-map "p" 'amazon-previous-item)
)

(defvar amazon-reviews-mode-map nil
   "Keymap for amazon reviews mode.")

(if amazon-reviews-mode-map
    ()
  (setq amazon-reviews-mode-map (make-keymap))
  (suppress-keymap amazon-reviews-mode-map)
  (define-key amazon-reviews-mode-map "\C-c\C-t" 'amazon-cart-show)
  (define-key amazon-reviews-mode-map "\C-c\C-k" 'amazon-buffer-up)
  (define-key amazon-reviews-mode-map "q" 'amazon-buffer-up)
  (define-key amazon-reviews-mode-map "n" 'amazon-next-item)
  (define-key amazon-reviews-mode-map "p" 'amazon-previous-item)
)

(defvar amazon-buffer-hash (make-hash-table)
  "A hash table to keep track of a buffer's 'origin'.")

;; the following function is ill-named, because it is not just 'up a
;; buffer', but also changes the window structure.  Oh well ...

(defun amazon-buffer-up ()
  (interactive)
  (let* ((origin (current-buffer))
	 (ursprung (gethash origin amazon-buffer-hash)))
    (delete-windows-on origin)
    (kill-buffer origin)
    (when ursprung (pop-to-buffer ursprung))))
      
(defun amazon-mode ()
  (setq major-mode 'amazon-mode)
  (setq mode-name "Amazon mode")
  (use-local-map amazon-mode-map)
  (setq buffer-read-only t))

(defun amazon-offers-mode ()
  (setq major-mode 'amazon-offers-mode)
  (setq mode-name "Amazon offers mode")
  (use-local-map amazon-offers-mode-map)
  (setq buffer-read-only t))

(defun amazon-reviews-mode ()
  (setq major-mode 'amazon-review-mode)
  (setq mode-name "Amazon reviews mode")
  (use-local-map amazon-reviews-mode-map)
  (setq buffer-read-only t))

(defvar amazon-cart-mode-map nil
  "Keymap for amazon cart buffer")

(if amazon-cart-mode-map
    ()
  (setq amazon-cart-mode-map (make-keymap))
  (suppress-keymap amazon-cart-mode-map)
  (define-key amazon-cart-mode-map "\C-c\C-d" 'amazon-cart-remove)
  (define-key amazon-cart-mode-map "\C-c\C-r" 'amazon-cart-remove)
  (define-key amazon-cart-mode-map "\C-c\C-t" 'amazon-cart-show) ; for now, this will do for 'refresh'
  (define-key amazon-cart-mode-map "\C-c\C-c" 'amazon-cart-local-submit)
)

(defun amazon-cart-mode ()
  (setq major-mode 'amazon-cart-mode)
  (setq mode-name "Amazon cart mode")
  (use-local-map amazon-cart-mode-map)
  (setq buffer-read-only t))

(defvar amazon-reviews-map nil 
  "Keymap for amazon reviews buffer")

(defun amazon-buffer-goto-parent ()
  (interactive)
  (let ((parent (gethash (current-buffer) amazon-buffer-hash)))
    (when parent
      (progn
	(kill-buffer (current-buffer))
	(pop-to-buffer parent)))))

(defun amazon-repeat-char (char number)
  (let ((out ""))
    (dotimes (i number out)
      (setq out (concat (char-to-string char) out)))))

(defvar amazon-item-break (amazon-repeat-char ?\n (+ 1 amazon-item-view-separator)))

(if amazon-reviews-map
    ()
  (setq amazon-reviews-map (make-keymap))
  (suppress-keymap amazon-reviews-map)
  (define-key amazon-reviews-map "q" 'delete-window)
  (define-key amazon-reviews-map "a" 'amazon-add-to-cart))

(defcustom amazon-search-limit 10
  "Maximum number of pages of results to return."
  :type 'number
  :group 'amazon)

(defvar amzn-base-url "http://xml.amazon.com/onca/xml")
(defvar *amazon-base-url* "http://xml.amazon.com/onca/xml")
(defvar *amazon-items* nil)
(defvar *amazon-offers* nil)
(defvar amzn-oos-offers nil)
(defvar *amazon-lisp-xml* nil)
(defvar *amazon-local-cart-contents* nil) ;  current contents of local cart; should be emptied when remote cart is updated
(defvar *amazon-purchase-url* nil)
(defvar *amazon-cart* nil)

(defgroup amazon nil "" :group 'tools)

(defvar amazon-response-group "Large,OffersFull"
"see http://docs.amazonwebservices.com/AWSEcommerceService/2005-03-23/ApiReference/ResponseGroupsArticle.html for a description of Response Group options."
)

(defvar amazon-output '()
  "amazon-output is an association list of page numbers and lists of results.")
(defvar amazon-total-pages 1)

(defun with-color (string color)
  (propertize string 'face `(:foreground ,color)))

;;  ... to display slot names for objects in the Item class.

(defcustom amazon-item-display-alist 
  `((author . ,(with-color "Author:" "cyan"))
    ; FIX ME: creator not always Editor
    (Creator . ,(with-color "Editor:" "cyan"))
    (title . ,(with-color "Title:" "cyan"))
    (Publisher . ,(with-color "Publisher:" "cyan"))
    (PublicationDate . ,(with-color "Date:" "cyan"))
    (edition . ,(with-color "Edition:" "cyan"))
    (binding . ,(with-color "Binding:" "cyan"))
    (ListPrice . ,(with-color "List Price:" "cyan"))
    (price-lowest-new . ,(with-color "Lowest New Price:" "cyan"))
    (price-lowest-used . ,(with-color "Lowest Used Price:" "cyan"))
    (AlternateVersions . ,(with-color "Alternate Versions:" "cyan")))
  "How to pretty print the slot-names for objects of the Item class."
  :group 'amazon)

(defcustom amazon-offer-display-alist
  `((Condition . ,(with-color "Condition:" "cyan"))
    (SubCondition . ,(with-color "SubCondition:" "cyan"))
    (ConditionNote . ,(with-color "Condition Note:" "cyan"))
    (formatted-price . ,(with-color "Price:" "cyan"))
    (Seller . ,(with-color "Seller:" "cyan"))
    (Availability . ,(with-color "Availability:" "cyan")))
  "How to pretty print the slot-names for objects of the Offer class."
  :group 'amazon)

(defcustom amazon-cartitem-display-alist
  `((Title . "Title:")
    (Price . "Price:"))
  "How to pretty print the slot-names for objects of the CartItem class"
  :group 'amazon)

(defcustom amazon-offer-verbose-display-alist
  (append `((author . ,(with-color "Author:" "cyan"))
	    (title . ,(with-color "Title:" "cyan"))
	    (binding . ,(with-color "Binding:" "cyan")))
	  amazon-offer-display-alist)
  "How to pretty print the slot-names for objects of the Offer class."
  :group 'amazon)

(defvar amazon-cartitem-slot-name-length
  (apply #'max (mapcar (lambda (x) (length (cdr x))) amazon-cartitem-display-alist)))

(defvar amazon-item-slot-name-length
  (apply #'max (mapcar (lambda (x) (length (cdr x))) amazon-item-display-alist)))

(defvar amazon-offer-slot-name-length
  (apply #'max (mapcar (lambda (x) (length (cdr x))) amazon-offer-display-alist)))

;; FIX ME: the following is no longer used??

(defvar *amazon-item-ids* (make-hash-table :test #'equal :size 300)
  "A hash table of the form: ItemId => Item, where Item is the corresponding eieio object.")

;;  for US locale, Amazon's
;;; merchant id is: "ATVPDKIKX0DER"

(defcustom amazon-merchant-id "ATVPDKIKX0DER"
  "Amazon's merchant id.  Default is Amazon.com -- US locale."
  :group 'amazon)

(defcustom amazon-offer-from-amazon 
  (lambda (x) (with-color x "orange"))
  "This function should map a string to a string.  It takes the
string that denotes an Offer from Amazon itself (not a third
party seller), and it modifies that string so as to indicate that
its special status.  The default is to color the string orange.
But you can do something like concatenate with \"Offer from
Amazon:\\n\".  We're all about customizability here."
  :group 'amazon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  EIEIO Classes

;; the universe of Amazon Web Service objects
(defclass aws () () ) 

;; The class of Request objects - something the client creates and
;; sends to Amazon The main categories (subclasses) of Requests are:
;; ItemLookup, ItemSearch, CartCreate, CartAdd, CartRemove

(defclass Request (aws) 
  ((operation :initarg :request)))

;; FIX ME: max # {response groups} = 3 ??

(defclass ItemSearch (Request) 
  ((operation :initvalue "ItemSearch" :allocation :class)    ; constant for all members of this class 
   (keywords :initarg :keywords :accessor keywords)
   (search-index :initvalue "Books" :allocation :class)
   ; (response-group :initform "Small,ItemIds,ItemAttributes,OfferFull,AlternateVersions" :accessor response-group) 
   (response-group :initform "Large,AlternateVersions,OfferFull" :accessor response-group) 
   (author :initarg :author :accessor author)
   (title :initarg :title :accessor title)
   (results-page :initform "1" :accessor results-page :initarg :results-page)
   (ecs-version :initform "2007-02-22" :accessor get-ecs-version :initarg :ecs-version) ; the version of ECS 4.0 WSDL
   (merchantid :initform "All" :accessor merchantid :initarg :merchantid)
   (condition :initform "All" :accessor get-condition :initarg :condition)))

(defclass ItemLookup (Request)
  ((operation :initform "ItemLookup" :allocation :class)
   (response-group :initform "Large,AlternateVersions" :accessor response-group) 
   (isbn :accessor get-isbn :initarg :isbn)
   (condition :initform "All" :accessor get-condition :initarg :condition)
   (merchantid :initform "All" :accessor merchantid :initarg :merchantid)
   (results-page :initform "1" :accessor results-page :initarg :results-page)
   (ecs-version :initform "2007-02-22" :accessor get-ecs-version :initarg :ecs-version) ; the version of ECS 4.0 WSDL
   (merchantid :initform "All" :accessor merchantid :initarg :merchantid)
   (condition :initform "All" :accessor get-condition :initarg :condition)))

(defclass CartCreate (Request)
  ((Items :initarg :Items :accessor get-Items)
   (Offers :initarg :Offers :accessor get-Offers)))  ;; CartCreate holds a list of Items and Offers

(defclass CartAdd (Request)
  ((asin-list :initarg :asin)
   (itemid-list :initarg :itemid)
   (hmac :initarg :hmac)))

(defclass CartRemove (Request)
  ((cartitemid :initarg :cartitemid)  ;; hmm, is this  right?
   (hmac :initarg :hmac)))

;;; Response class = object sent from AWS to user; to be populated from
;;; the xml that AWS returns

(defclass Response (aws) ())

(defclass ItemSearchResponse (Response) 
  ((OperationRequest :accessor get-OperationRequest :initarg :OperationRequest)
   (items :accessor items :initarg :items)))

(defclass OperationRequest (aws)
  ((Arguments :accessor get-Arguments :initarg :Arguments)))

(defun amazon-get-operationrequest (lxml)
  (OperationRequest ""
		    :Arguments (amazon-xpath "OperationRequest/Arguments/Argument" (car lxml))))

(defun amazon-request-argument (list attribute)
  (let ((tail (member-if #'(lambda (x) (equal (xml-get-attribute x 'Name) attribute)) list)))
    (if tail (xml-get-attribute (car tail) 'Value))))

(defmethod amazon-show-search-args ((x ItemSearchResponse))
  (let* ((op-req (get-OperationRequest x))
	 (arguments (get-Arguments op-req))
	 (author (amazon-request-argument arguments "Author"))
	 (title (amazon-request-argument arguments "Title"))
	 (keywords (amazon-request-argument arguments "Keywords"))
	 (out ""))
    (when author (setq out (concat ":A " author out)))
    (when title (setq out (concat ":T " title out)))
    (when keywords (setq out (concat ":K " keywords out)))
    out))

(defclass ItemLookupResponse (Response) 
  ((items :accessor items :initarg :items :initform '())))

;;; Amazon returns a page of xml; we are goind to transform this xml
;;; into a Response object that contains the data we need
;;; xml -> lisp-xml (i.e. sexps) -> Response

(defclass CartCreateResponse (Response) 
  ((CartId :accessor get-cartid :initarg :cartid)
   (HMAC :accessor get-hmac :initarg :hmac)
   (PurchaseURL :accessor get-purchaseurl :initarg :purchaseurl)
   (Subtotal :accessor get-subtotal :initarg :subtotal)
   (CartItems :accessor get-cartitems :initarg :cartitems)))

(defclass CartAddResponse (Response) ())

(defclass CartRemoveResponse (Response) ())

(defclass Cart (CartCreateResponse CartAddResponse CartRemoveResponse)
  ((cartid :initarg :cartid :accessor cartid)
   (hmac :initarg :hmac :accessor hmac)
   (urlencodedhmac :initarg :urlencodedhmac :accessor urlencodedhmac)
   (purchaseurl :initarg :purchaseurl :accessor purchaseurl)
   (subtotal :initarg :subtotal :accessor subtotal)
   (cartitems :initarg :cartitems :accessor cartitems)  ; HMM, is CartItems properly thought of as a subclass, 
                                                        ; or as a list that contains objects that are members of a subclass?
                                                        ; I'm going to treat CartItems as a slot that holds a list of CartItem objects
							
   ))

;; contrast the above with *amazon-cart-local*.  The Cart class
;; consists of responses from Amazon. So, it is *not* a "cart" in the
;; sense that it is something you take to the checkout -- it is the
;; response you get from taking your items to the checkout

(defclass Item (aws) 
  ((asin :initarg :asin :accessor get-asin :initform "")
   (title :initarg :title :accessor get-title :initform "")
   (author :initarg :author :accessor get-author :initform "")
   (url :initarg :url :accessor get-url :initform "")
   (binding :initarg :binding :accessor get-binding :initform "")
   (edition :initarg :edition :accessor get-edition :initform "")
   (ListPrice :initarg :ListPrice :accessor get-ListPrice)    ; this should be a Price object
   (price-lowest-new :initarg :new-price :accessor get-new-price :initform "")
   (price-lowest-used :initarg :used-price :accessor get-used-price :initform "")
   (offers :initarg :offers :accessor get-Offers :initform '())   ; a list of offers
   (editorialreviews :initarg :editorial-reviews :accessor get-editorial-reviews :initform "No editorial reviews for this item.")
   (customerreviews :initarg :customer-reviews :accessor get-customer-reviews :initform "No customer reviews for this item.")
   (AlternateVersions :initarg :AlternateVersions :accessor get-AlternateVersions) ; list of alternate versions
   (Publisher :initarg :Publisher :accessor get-Publisher)
   (PublicationDate :initarg :PublicationDate :accessor get-PublicationDate)
  ; Creator is a special case, such as <Creator role="Editor">
   (Creator :initarg :Creator :accessor get-Creator)
  ))

; AlternateVersions/AlternateVersion/ASIN
; AlternateVersions/AlternateVersion/Binding
; AlternateVersions/AlternateVersion/Title

(defclass AlternateVersion (aws)
  ((ASIN :initarg :ASIN :accessor get-ASIN)
   (Binding :initarg :Binding :accessor get-Binding)
   (Title :initarg :Title :accessor get-Title)))

(defclass Merchant (aws)
  ((MerchantId :initarg :MerchantId :accessor get-MerchantId)
   (Name :initarg :Name :accessor get-Name)
   (GlancePage :initarg :GlancePage :accessor get-GlancePage)))

(defclass Seller (aws)
  ((SellerId :initarg :SellerId :accessor get-SellerId)
   (SellerName :initarg :SellerName :accessor get-SellerName)
   (Nickname :initarg :Nickname :accessor get-Nickname)
   (GlancePage :initarg :GlancePage :accessor get-GlancePage)
   (About :initarg :About :accessor get-About)
   (MoreAbout :initarg :MoreAbout :accessor get-MoreAbout)
   (Location :initarg :Location :accessor get-Location)
   (AverageFeedbackRating :initarg :AverageFeedbackRating :accessor get-AverageFeedbackRating)
   (TotalFeedback :initarg :TotalFeedback :accessor get-TotalFeedback)))

(defclass Offer (aws)
  ((Condition :initarg :Condition :accessor get-Condition)
   (SubCondition :initarg :SubCondition :accessor get-SubCondition)
   (ConditionNote :initarg :ConditionNote :accessor get-ConditionNote)
   (listingid :initarg :listingid :initform "" :accessor get-listingid)
   (formatted-price :initarg :formatted-price :initform "" :accessor get-formatted-price)
   (Merchant :initarg :Merchant :accessor get-Merchant)
   (Seller :initarg :Seller :accessor get-Seller)
   (price :initarg :price :initform "" :accessor get-price)
   ; we add an ASIN slot so that we can reconstruct an Item from an Offer
   ; the ASIN -> Item correspondence is stored in the *amazon-item-ids* hash table
   (asin :initarg :asin :initform "" :accessor get-asin)
   (Availability :initarg :Availability :accessor get-Availability)))

(defmethod amazon-merchant-is-amazon ((x Offer))
  (string= (get-MerchantId (get-Merchant x)) amazon-merchant-id))

;  Offer/OfferListing/OfferListingId
;  Offer/OfferAttributes/Condition
;  Offer/OfferAttributes/SubCondition
;  Offer/OfferAttributes/ConditionNote
;  Offer/OfferListing/Price/FormattedPrice
;  Offer/OfferListing/Price/Amount
;  Offer/OfferListing/Availability ***
;  Offer/Seller/AverageFeedbackRating
;  Offer/Seller/TotalFeedback 

;; TO DO: for Offer, fetch and show shipping time!

(defun amazon-get-merchant (lxml)
  (Merchant ""
	    :MerchantId (amazon-node-text (amazon-xpath "MerchantId" lxml))
	    :Name (amazon-node-text (amazon-xpath "Name" lxml))
	    :GlancePage (amazon-node-text (amazon-xpath "GlancePage" lxml))))

(defun amazon-get-seller (lxml)
  (Seller ""
	  :SellerId (amazon-node-text (amazon-xpath "SellerId" lxml))
	  :SellerName (amazon-node-text (amazon-xpath "SellerName" lxml))
	  :Nickname (amazon-node-text (amazon-xpath "Nickname" lxml))
	  :GlancePage (amazon-node-text (amazon-xpath "GlancePage" lxml))
	  :About (amazon-node-text (amazon-xpath "About" lxml))
	  :MoreAbout (amazon-node-text (amazon-xpath "MoreAbout" lxml))
	  :Location (amazon-node-text (amazon-xpath "Location" lxml))
	  :AverageFeedbackRating (amazon-node-text (amazon-xpath "AverageFeedbackRating" lxml))
	  :TotalFeedback (amazon-node-text (amazon-xpath "TotalFeedback" lxml))))

;; amzn-get-offer :: lisp-xml -> Offer
(defun amzn-get-offer (lisp-xml &optional asin) 
  (Offer ""
   :listingid (amazon-node-text (amazon-xpath "OfferListing/OfferListingId" lisp-xml))
   :Availability (amazon-node-text (amazon-xpath "OfferListing/Availability" lisp-xml))
   :Condition (amazon-node-text (amazon-xpath "OfferAttributes/Condition" lisp-xml))
   :SubCondition (amazon-node-text (amazon-xpath "OfferAttributes/SubCondition" lisp-xml))
   :ConditionNote (amazon-node-text (amazon-xpath "OfferAttributes/ConditionNote" lisp-xml))
   :formatted-price (amazon-node-text (amazon-xpath "OfferListing/Price/FormattedPrice" lisp-xml))
   :price (amazon-node-text (amazon-xpath "OfferListing/Price/Amount" lisp-xml))
   :Seller (amazon-get-seller (car (amazon-xpath "Seller" lisp-xml)))
   :Merchant (amazon-get-merchant (car (amazon-xpath "Merchant" lisp-xml)))
;   :seller-feedback-avg  (amazon-node-text (amazon-xpath "Seller/AverageFeedbackRating" lisp-xml))
;   :seller-feedback-total (amazon-node-text (amazon-xpath "Seller/TotalFeedback" lisp-xml))
   :asin asin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARROWS / ACTIONS

;;; Arrows from Client to Amazon

;; return an appropriate REST URL for a Request 

(defgeneric amazon-url ((x Request)) )

(defvar amzn-request-base-url
  (let* ((pairs `(("SubscriptionId" . ,amazon-subscription-id)
		  ("type" . "heavy")
		  ("f" . "xml")
		  ("mode" . "books")   ; does this apply even in CartCreate requests?
		  ("Service" . "AWSECommerceService"))))
  (concat amzn-base-url "?" (mm-url-encode-www-form-urlencoded pairs))))

(defmethod amazon-url ((x ItemLookup))
  (let* ((pairs `(("Operation" . "ItemLookup")
		  ("ItemId" . ,(get-isbn x))
		  ("ResponseGroup" . ,(response-group x))
		  ("MerchantId" . ,(merchantid x))
		  ("Condition" . ,(get-condition x)))))
    (concat amzn-request-base-url "&" (mm-url-encode-www-form-urlencoded pairs))))

(defmethod amazon-url ((x ItemSearch))
  (let* ((pairs `(("Operation" . "ItemSearch")
		  ("SearchIndex" . "Books")
		  ("MerchantId" . ,(merchantid x))
		  ("Version" . ,(get-ecs-version x))
		  ("Condition" . ,(get-condition x))
		  ("ResponseGroup" . ,(response-group x))
		  ("ItemPage" . ,(results-page x)))))
    (when (and (slot-boundp x 'author) (author x)) (push `("Author" . ,(author x)) pairs))
    (when (and (slot-boundp x 'title) (title x)) (push `("Title" . ,(title x)) pairs))
    (when (and (slot-boundp x 'keywords) (keywords x)) (push `("Keywords" . ,(keywords x)) pairs))
    (concat amzn-request-base-url "&" (mm-url-encode-www-form-urlencoded pairs))))

(defgeneric amazon-send ()
  "Send some object -- viz. a request -- to the AWS server.
  Specialize on the various types of requests.")

;; 'amazon-send' returns an object of class ItemSearchResponse or ItemLookupResponse.
;; Most typically, we will want to invoke the 'amazon-display' generic function on the result
;; in order to display the results of the search

;; amazon-send :: ItemLookup -> ItemLookupResponse
(defmethod amazon-send ((x ItemLookup))
  (setq amazon-output '())
  (url-retrieve (amazon-url x)
		(lambda (y)
		  (setf amazon-output (xml-parse-region (point-min) (point-max)))
		  (message "Finished getting results"))
		nil)
  (while (not amazon-output)
    (progn (message "Getting results")
	   (sleep-for 0.1)))
  (ItemLookupResponse "" :items (mapcar #'amzn-get-item (amzn-xml-items (car amazon-output)))))

(defmethod show ((x ItemLookupResponse))
  (pop-to-buffer (generate-new-buffer "*amazon-item-lookup-results*"))
  (insert (mapconcat #'show (items x) amazon-item-break))
  (goto-char (point-min))
  (amazon-mode)
  (setq buffer-read-only t)
  (delete-other-windows))

(defun amzn-xml-total-pages (lxml)
  (string-to-number
   (amazon-node-text (amazon-xpath "Items/TotalPages" (car lxml)))))

(defgeneric amazon-search-recursive (x) )

(defvar amazon-operation-request nil)

;; amazon-search-recursive :: ItemSearch -> ItemSearchResponse
(defmethod amazon-search-recursive ((obj ItemSearch))
  (setq amazon-output '())
  (setq amazon-total-pages nil)
  (url-retrieve (amazon-url obj)
		(lambda (y) 
		  (let ((lxml (xml-parse-region (point-min) (point-max))))
		    (setf amazon-total-pages (amzn-xml-total-pages lxml))
		    (setf amazon-operation-request (amazon-get-operationrequest lxml))
		    (setf amazon-output (list (cons 1 (amzn-xml-items (car lxml)))))
		    (message "Finished processing the first page")))
		nil)
  (message "Processing the first page of results") 
  (while (not amazon-total-pages)
    (progn (message "Please wait ... getting information about how many pages to fetch")
	   (sleep-for 0.1)))
  (let ((tots (min amazon-search-limit amazon-total-pages)))
    (message "There are %s pages of results, of which I will retrieve %s" 
	   amazon-total-pages tots)
    (loop for x from 2 to (min amazon-total-pages amazon-search-limit)
	  do (lexical-let ((x x))
	       (let ((new-obj (clone obj :results-page (number-to-string x)))) 
		 (url-retrieve (amazon-url new-obj)
			       (lambda (y) 
				 (let ((lxml (xml-parse-region (point-min) (point-max))))
				   (push (cons x (amzn-xml-items (car lxml))) amazon-output)
				   (message "Finished processing page %s" x)))
			       nil))))
    ; don't return a value until we have fully populated 'amazon-output'
    (let ((mout '()))
      (loop for x from 1 to tots
	    do (while (not (assoc x amazon-output)) (sleep-for 0.2))
	    do (setq mout (append (cdr (assoc x amazon-output)) mout)))
      (ItemSearchResponse "" :OperationRequest amazon-operation-request :items (mapcar #'amzn-get-item mout)))))



(defun amzn-xml-attrib (attrib item)
  (caddar (xml-get-children (car (xml-get-children item 'ItemAttributes)) attrib)))

;  Item/ItemAttributes/Author
;  Item/ItemAttributes/Creator
;  Item/ItemAttributes/Title 
;  Item/OfferSummary/LowestNewPrice/FormattedPrice
;  Item/OfferSummary/LowestUsedPrice/FormattedPrice
;  ItemAttributes/Binding
;  ItemAttributes/Edition

(defclass EditorialReview (aws)
  ((Source :initarg :Source :accessor get-Source)
   (Content :initarg :Content :accessor get-Content)))

(defmethod show ((x EditorialReview))
  (insert (concat 
	   (with-color (format "< Review | %s >" (get-Source x)) "cyan")
	   " "
	   (get-Content x))))

(defun amazon-show-reviews (point)
  (interactive "d")
  (let* ((item (reconstruct-object point))
	 (edrev (get-editorial-reviews item))
	 (curev (get-customer-reviews item))
	 (faux (get-title item))
	 (parent-buffer (current-buffer))
	 (child-buffer (pop-to-buffer (generate-new-buffer (format "*amazon reviews for %s*" faux)))))
    (puthash child-buffer parent-buffer amazon-buffer-hash)
    (loop for x in edrev
	  do (show x)
	  do (insert "\n\n"))
    (loop for x in curev
	  do (show x)
	  do (insert "\n\n"))
    (text-mode)
    (fill-region (point-min) (point-max))
    (goto-char (point-min))
    (amazon-reviews-mode)))

(defclass CustomerReview (Item)
  ((AverageRating :initarg :AverageRating :accessor get-AverageRating)
   (Reviews :initarg :Reviews :accessor get-Reviews)  ; a list of Review objects
   ))

; Item/CustomerReview/AverageRating
; Item/CustomerReview/Review/Content
; Item/CustomerReview/Review/ASIN
; Item/CustomerReview/Review/Date
; Item/CustomerReview/Review/Rating
; Item/CustomerReview/Review/HelpfulVotes
; Item/CustomerReview/Review/Summary
; Item/CustomerReview/Review/TotalVotes
; Item/CustomerReview/Review/Reviewer/{Name,CustomerId,Nickname,Location}

(defclass Review (CustomerReview)
  ((ASIN :initarg :ASIN :accessor get-ASIN)
   (Content :initarg :Content :accessor get-Content)
   (Date :initarg :Date :accessor get-Date)
   (Rating :initarg :Rating :accessor get-Rating)
   (HelpfulVotes :initarg :HelpfulVotes :accessor get-HelpfulVotes)
   (Summary :initarg :Summary :accessor get-Summary)
   (TotalVotes :initarg :TotalVotes :accessor get-TotalVotes)
   (Reviewer :initarg :Reviewer :accessor get-Reviewer))
  )

(defclass Price ()
  ((Amount :initarg :Amount :accessor get-Amount)
   (CurrencyCode :initarg :CurrencyCode :accessor get-CurrencyCode)
   (FormattedPrice :initarg :FormattedPrice :accessor get-FormattedPrice)))

(defclass CartItem (aws)
  ((CartItemId :initarg :CartItemId :accessor get-CartItemId)
   (ASIN :initarg :ASIN :accessor get-ASIN)
   (Price :initarg :Price :accessor get-Price)
   (ProductGroup :initarg :ProductGroup :accessor get-ProductGroup)
   (MerchantId :initarg :MerchantId :accessor get-MerchantId)
   (SellerId :initarg :SellerId :accessor get-SellerId)
   (ExchangeId :initarg :ExchangeId :accessor get-ExchangeId)
   (Quantity :initarg :Quantity :accessor get-Quantity)
   (Title :initarg :Title :accessor get-Title)))

(defvar amazon-html-replace 
  '(("<p>" . "\n\n")
    ("<P>" . "\n\n")
    ("</ BR>" . "\n\n")
    ("<br>" . "\n\n"))
  "Regular expresssion replacements for converting html fragments
  to text.")

(defun amazon-clean (html)
  (loop for x in amazon-html-replace
	do (setq html (replace-regexp-in-string (car x) (cdr x) html))
	finally return html))

(defmethod show ((x Review))
  (insert 
   (concat (with-color (format "< Review by %s | Rating %s | %s >" 
			       (get-Name (get-Reviewer x))
			       (get-Rating x)
			       (get-Date x)) "cyan")
	   (format " %s" 
		   (amazon-clean (get-Content x))))))

;; get-item :: lisp-xml -> Item
(defun amzn-get-item (item)
  (lexical-let* ((asin (amazon-node-text (amazon-xpath "ASIN" item)))
		 (foo (Item ""
			    :author (amazon-node-text (amazon-xpath "ItemAttributes/Author" item))
			    ; FIX ME: Creator not always Editor
			    :Creator (amazon-node-text (amazon-xpath "ItemAttributes/Creator" item))
			    :title (amazon-node-text (amazon-xpath "ItemAttributes/Title" item))
			    :ListPrice (amazon-get-price (car (amazon-xpath "ItemAttributes/ListPrice" item)))
			    :new-price (amazon-node-text (amazon-xpath "OfferSummary/LowestNewPrice/FormattedPrice" item))
			    :used-price (amazon-node-text (amazon-xpath "OfferSummary/LowestUsedPrice/FormattedPrice" item))
			    :url (concat "http://www.amazon.com/dp/" asin)
			    :binding (amazon-node-text (amazon-xpath "ItemAttributes/Binding" item))
			    :edition (amazon-node-text (amazon-xpath "ItemAttributes/Edition" item))
			    :editorial-reviews (mapcar #'amazon-get-editorialreview 
						       (amazon-xpath "EditorialReviews/EditorialReview" item))
			    :customer-reviews (mapcar #'amazon-get-review (amazon-xpath "CustomerReviews/Review" item)) 
			    :asin asin
			    :offers (mapcar (lambda (x) (amzn-get-offer x asin)) (amzn-xml-offers item))
			    :Publisher (amazon-node-text (amazon-xpath "ItemAttributes/Publisher" item))
			    :PublicationDate (amazon-node-text (amazon-xpath "ItemAttributes/PublicationDate" item))
			    :AlternateVersions (mapcar #'amzn-get-alt-version 
						       (amazon-xpath "AlternateVersions/AlternateVersion" item)))))
    ; put a unique label for the item in the *amazon-item-ids* hash table
    ; FIX ME: this is now superfluous
    (puthash asin foo *amazon-item-ids*)))

(defun amzn-get-alt-version (alt)
  (AlternateVersion ""
		    :ASIN (amazon-node-text (amazon-xpath "ASIN" alt))
		    :Binding (amazon-node-text (amazon-xpath "Binding" alt))
		    :Title (amazon-node-text (amazon-xpath "Title" alt))))

(defun amazon-get-editorialreview (editorialreview)
  (EditorialReview ""
		   :Source (amazon-node-text (amazon-xpath "Source" editorialreview))
		   :Content (amazon-node-text (amazon-xpath "Content" editorialreview))))

(defclass Reviewer (aws)
  ((Name :initarg :Name :accessor get-Name)
   (CustomerId :initarg :CustomerId :accessor get-CustomerId)
   (Nickname :initarg :Nickname :accessor get-Nickname)
   (Location :initarg :Location :accessor get-Location)))

(defun amazon-get-review (review)  ; here review is supposed to be lisp-xml
  (Review ""
	  :ASIN (amazon-node-text (amazon-xpath "ASIN" review))
	  :Content (amazon-node-text (amazon-xpath "Content" review))
	  :Date (amazon-node-text (amazon-xpath "Date" review))
	  :Rating (amazon-node-text (amazon-xpath "Rating" review))
	  :HelpfulVotes (amazon-node-text (amazon-xpath "HelpfulVotes" review))
	  :Summary (amazon-node-text (amazon-xpath "Summary" review))
	  :TotalVotes (amazon-node-text (amazon-xpath "TotalVotes" review))
	  :Reviewer (amazon-get-reviewer (car (amazon-xpath "Reviewer" review)))))

(defun amazon-get-reviewer (lxml)
   (Reviewer ""
	     :Name (amazon-node-text (amazon-xpath "Name" lxml))
	     :CustomerId (amazon-node-text (amazon-xpath "CustomerId" lxml))
	     :Nickname (amazon-node-text (amazon-xpath "Nickname" lxml))
	     :Location (amazon-node-text (amazon-xpath "Location" lxml))))

(defvar amazon-key-regexp ":\\w+\\( \\)*")

;; amzn-helper :: string -> plist

;; Example:
;; (amzn-helper ":title farewell to arms")
;;    => (:title "farewell to arms")

(defun amzn-helper (string)
  "Transforms a string of keywords and content into a plist."
  (cond
   ((zerop (length string)) '())
   ((and (string-match amazon-key-regexp string) (= (match-beginning 0) 0))
    (let* ((keyword (intern (substring string 0 (- (match-end 0) 1))))
	   (begin-content (match-end 0))
	   (end-content (or (string-match amazon-key-regexp string 1) (length string)))
	   (content (substring string begin-content end-content)))
      (append (list keyword content) (amzn-helper (substring string end-content)))))
   (t (error "Mangled input"))))

;; amzn-expand-searchwords :: [keyword] -> [keyword]
(defun amzn-expand-searchwords (list)
  (mapcar
   (lambda (x)
     (cond ((equal x :t) :title)
	   ((equal x :k) :keywords)
	   ((equal x :a) :author)
	   ((equal x :i) :isbn)
	   (t x)))
   list))

(defun amazon (&rest rest)
  (interactive "sAmazon search terms: ")
  (let* ((string (car rest))
	 (reduced-string (replace-regexp-in-string " [ ]+" " " string))
	 (plist (amzn-expand-searchwords (amzn-helper reduced-string)))
	 (obj (apply ItemSearch (cons "" plist))))
    (show (amazon-search-recursive obj))))

(defun amzn-assoc (key list)
  (cdr (assoc key list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESSING and DISPLAYING results

;;; PROCESSING the returned xml

(defun amazon-split (string)
  (let ((split (position ?\/ string)))
    (cond
     ((not split) (list string))
     (t (cons (substring string 0 split) (amazon-split (substring string (+ 1 split))))))))

(defun amazon-descend (list lxml)
  (cond
   ((= (length list) 1) (xml-get-children lxml (intern (car list))))
   (t (xml-get-children (car (amazon-descend (cdr list) lxml)) (intern (car list))))))

(defun amazon-xpath (string lxml)
  (let ((list (reverse (amazon-split string))))
    (amazon-descend list lxml)))

(defun amazon-node-text (lxml)
  (car (reverse (car lxml))))

(defun amzn-xml-items (lisp-xml)
  "starting within lisp-xml, returns a *list* of the Item nodes.
Each element of the list is itself of lisp-xml form, and so we
can get its children, etc.."
  (amazon-xpath "Items/Item" lisp-xml))

(defun amzn-xml-list-price (item)
  (amazon-node-text (amazon-xpath "ItemAttributes/ListPrice/FormattedPrice" item)))

(defun amzn-xml-asin (item)
  (amazon-node-text (amazon-xpath "ASIN" item)))

(defun amzn-xml-url (item)
  ;; rather than use the Amazon-supplied URL, use http://www.amazon.com/dp/ASIN
  (concat "http://www.amazon.com/dp/" (amzn-xml-asin item)))

(defun amzn-xml-new-price (item)
  (amazon-node-text (amazon-xpath "OfferSummary/LowestNewPrice/FormattedPrice" item)))

(defun amzn-xml-used-price (item)
    (amazon-node-text (amazon-xpath "OfferSummary/LowestUsedPrice/FormattedPrice" item)))

(defun amzn-xml-offers (item)
  "returns a list of OFFERS (lisp xml format) for ITEM (list xml format)."
  (amazon-xpath "Offers/Offer" item) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAYING results in a human-readable fashion

(defvar *amazon-buf-hash* (make-hash-table)
  "Associates points in a buffer with objects.")

(make-variable-buffer-local '*amazon-buf-hash*)

(defgeneric show () "Display the object in human-readable format.")

(defgeneric show ((x Response)) "Display the response from amazon.")

(defmethod show ((x ItemSearchResponse))
  (pop-to-buffer (generate-new-buffer (concat "*amazon search results for " (amazon-show-search-args x) "*")))
  (insert (mapconcat #'show (items x) "\n\n\n"))
  (goto-char (point-min))
  (amazon-mode)
  (setq buffer-read-only t)
  (delete-other-windows))

(defun amazon-spaces (number)
  (cond
   ((zerop number) "")
   (t (concat " " (amazon-spaces (- number 1))))))

(defgeneric amazon-print-slot (obj) )

(defun amazon-wrap-string (string text-width indent)
  (let ((list (amzn-break-string string text-width)))
    (mapconcat (lambda (x) x) list (concat "\n" (amazon-spaces indent)))))

(defcustom amazon-field-display-width 80
  "How wide do you want the text of displayed fields, such as
  book titles?  This variable controls text wrapping in results
  buffers."
  :group 'amazon)

(defmethod show ((x AlternateVersion))
  (concat (slot-value x 'Binding) " " (slot-value x 'ASIN)))

;; careful: slot value of 'AlternateVersions will be a list, each
;; element of which is an AlternateVersion object

;; pretty printing slot-name / slot-value pairs
;; amazon-print-slot :: Item -> slot -> string
(defmethod amazon-print-slot ((obj Item) slot)  ; SLOT should be a symbol
  (let* ((slot-name (cdr (assoc slot amazon-item-display-alist)))
	 (stretch (- (+ 3 amazon-item-slot-name-length) (length slot-name))) ; 
	 (value (show-generic (slot-value obj slot))))
    (cond
     ((not (slot-boundp obj slot)) "")
     ((not value) "")
     ((equal slot 'AlternateVersions)
      (concat slot-name
	      (amazon-spaces stretch)
	      (mapconcat #'show (slot-value obj 'AlternateVersions) " / ")))
     ((and (stringp value) (zerop (length value))) "")
     ((zerop (length value)) "")
     (t (concat slot-name
		(amazon-spaces stretch)
		(amazon-wrap-string value amazon-field-display-width (+ 3 amazon-item-slot-name-length)))))))

(defmethod show ((x Price))
  (get-FormattedPrice x))

(defmethod amazon-print-slot ((obj CartItem) slot)
  (let* ((slot-name (cdr (assoc slot amazon-cartitem-display-alist)))
	 (stretch (- (+ 2 amazon-cartitem-slot-name-length) (length slot-name)))
	 (value (show-generic (slot-value obj slot))))
    (concat slot-name (amazon-spaces stretch)
	    (amazon-wrap-string value
				amazon-field-display-width (+ 3 amazon-cartitem-slot-name-length)))))


(defmethod amazon-print-slot ((obj Offer) slot)
  (let* ((asin (get-asin obj))
	 (item (gethash asin *amazon-item-ids*)))
    (cond
     ; special treatment of 'author' and 'title' and 'binding' since they are not slots of Offer
     ((member slot '(author title binding))
      (amazon-print-slot item slot))
     (t 
      (let* ((slot-name (cdr (assoc slot amazon-offer-display-alist)))
	     (stretch (- (+ 3 amazon-offer-slot-name-length) (length slot-name)))
	     (value (show-generic (slot-value obj slot))))
	(cond
	 ((not (slot-boundp obj slot)) "")
	 ((zerop (length value)) "")
     	 (t (concat slot-name
		    (amazon-spaces stretch)
		    (amazon-wrap-string value amazon-field-display-width (+ 3 amazon-offer-slot-name-length))))))))))

;; The following 'show' function puts a 'text property' at each
;; character in the string, namely a property named 'item' with value
;; the object (of class 'Item') being displayed.
;; we will use this text property in the function 'reconstruct-object'

;; show :: Item -> string
(defmethod show ((x Item))
  (propertize
   (mapconcat (lambda (w) w) 
	     (remove-if (lambda (z) (zerop (length z))) 
			(mapcar (lambda (y) (amazon-print-slot x y)) 
				(mapcar #'car amazon-item-display-alist))) "\n")
   'noumenon x))


(defmethod show ((x Offer))
  (let ((output (propertize
		 (mapconcat (lambda (w) w)
			    (remove-if (lambda (z) (zerop (length z)))
				       (mapcar (lambda (y) (amazon-print-slot x y))
					       (mapcar #'car amazon-offer-display-alist))) "\n")
		 'noumenon x)))
    (if (amazon-merchant-is-amazon x)
	(funcall amazon-offer-from-amazon output)
      output)))

(defun amazon-show (obj slot-list)
  (propertize
   (mapconcat (lambda (w) w)
	      (remove-if (lambda (z) (zerop (length z)))
			 (mapcar (lambda (slot) (amazon-print-slot obj slot)) slot-list)) "\n")
   'noumenon obj))

(defgeneric show-verbose (x) )

(defmethod show-verbose ((x Item))
  (show x))

(defmethod show-verbose ((x Offer))
    (propertize
     (mapconcat (lambda (w) w)
		(remove-if (lambda (z) (zerop (length z)))
			   (mapcar (lambda (y) (amazon-print-slot x y))
				   (mapcar #'car amazon-offer-verbose-display-alist))) "\n")
   'noumenon x))

(defmethod show ((x Seller))
  (concat (get-Nickname x) (format " -- average feedback %s (from %s total)" (get-AverageFeedbackRating x) (get-TotalFeedback x))))

;; reconstruct-object :: point -> Item
(defun reconstruct-object (point)
  (interactive "d")
  (get-text-property point 'noumenon))

;; is therer some way that one buffer can be related to another as a 'parent'?
;; compare with Wanderlust's handling of message buffers

(defun amazon-offers (point)
  (interactive "d")
  ;; remember whence you came
  (let* ((parent-buffer (current-buffer))
	 (item (reconstruct-object point))
	 (offers (get-Offers item))
	 (child-buffer (pop-to-buffer (generate-new-buffer (format "*amazon offers for %s*" (get-title item))))))
    (puthash child-buffer parent-buffer amazon-buffer-hash)
    (insert (mapconcat #'show offers "\n\n\n"))
    (goto-char (point-min))
    (amazon-offers-mode)))

(defun amazon-browse-url (point)
  ;; N.B.: this should only be used in 'items' buffer
  ;; ... could enforce this by checking current major mode??
  (interactive "d")
  (browse-url-firefox (get-url (reconstruct-object point))))

;; the following should work in both *items* buffer, and in *offers*
;; buffer.  In the former case, we are adding the item offered by Amazon


;; amazon-cart-add :: IO(point) -> local-cart(list)
(defun amazon-cart-add (point)
  "Add the offer at point to the local cart."
)

;; amzn-break-string :: string -> [string]
(defun amzn-break-string (string number)
"Break string into strings of length less than number; only break on spaces."
; recursive: go to NUMBER, go back until you hit a space, split, repeat
;
; TO FIX: currently doesn't know what to do with words that are longer than NUMBER
(cond ((zerop (length string)) '())
      ((< (length string) number) (list string))
      (t 
	(let* ((first-substring (subseq string 0 number))
	       (break-point (position ?\s first-substring :from-end t))
	       (head (subseq string 0 break-point))
	       (tail (subseq string (+ 1 break-point))))
	  (append (list head) (amzn-break-string tail number))))))

(defun amazon-string-wrap (string number)
  "Map string to a new string that is appropriately wrapped")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL PURCHASE LIST

(defvar *amazon-cart-local* (CartCreate "" :Items '() :Offers '())
  "*amazon-cart-local* is a CartCreate object whose slots are initially empty.")

(defun amazon-cart-clear ()
  (interactive)
  (when (yes-or-no-p "Do you really want to clear your local Amazon purchase list?")
  (oset *amazon-cart-local* :Items '())
  (oset *amazon-cart-local* :Offers '())
  (message "Your purchase list is now empty.")))

(defun amazon-cart-empty ()
  (interactive)
  (amazon-cart-clear))

(defgeneric amazon-push-cart () 
  "Push an ITEM or an OFFER into *amazon-cart-local*.")

(defmethod amazon-push-cart ((x Item))
  (oset *amazon-cart-local* :Items (cons x (get-Items *amazon-cart-local*))))

(defmethod amazon-push-cart ((x Offer)) 
  (oset *amazon-cart-local* :Offers (cons x (get-Offers *amazon-cart-local*))))

(defun amazon-add-to-cart (point)
  (interactive "d")
  (amazon-push-cart (reconstruct-object point))
  (message "item added to local cart"))


;; FIX ME: cart view should have its own keymap
;;; e.g. "s" to submit the thing to Amazon

;; FIX ME: need to add buffer hash so we can remove items and stuff like that
  
(defmethod show ((x CartCreate))
  (let* ((stuff (append (get-Items x) (get-Offers x)))
	(num (length stuff)))
    (pop-to-buffer (format "*amazon local cart; %s items*" num))
    (setq buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (insert (mapconcat #'amazon-show-as-cart-element stuff "\n\n"))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (amazon-cart-mode)))
	       
(defgeneric amazon-show-as-cart-element (obj) )

(defmethod amazon-show-as-cart-element ((x Item))
  (amazon-show x '(author title price-lowest-new)))

(defmethod amazon-offer-item ((x Offer))
  (gethash (get-asin x) *amazon-item-ids*))

(defmethod amazon-show-as-cart-element ((x Offer))
  (let ((item (amazon-offer-item x)))
    (propertize (concat 
     (amazon-show item '(author title)) "\n"
     (amazon-show x '(formatted-price))) 'noumenon x)))
     
(defun amazon-cart-show ()
  (interactive)
  (if (and (not (get-Items *amazon-cart-local*))
	   (not (get-Offers *amazon-cart-local*)))
      (message "No cart contents to show")
    (show *amazon-cart-local*)
    (use-local-map amazon-cart-mode-map)))

(defgeneric amazon-cart-local-remove (x) )

(defun amazon-cart-remove (point)
  (interactive "d")
  (amazon-cart-local-remove (reconstruct-object point))
  (kill-buffer (current-buffer))
  (amazon-cart-show))

(defmethod amazon-cart-local-remove ((x Item))
  (interactive "d")
  (oset *amazon-cart-local* :Items (remove x (get-Items *amazon-cart-local*))))

(defmethod amazon-cart-local-remove ((x Offer))
  (interactive "d")
  (oset *amazon-cart-local* :Offers (remove x (get-Offers *amazon-cart-local*))))
  
(defun amazon-cart-local-subtotal ()
  (apply #'+ (mapcar (lambda (x) (string-to-number x))
		     (mapcar (lambda (x) (substring x 1))))))

;; FIX ME: I don't think we use the following any more
;; the current upload function does not verify number of cart items

(defun amazon-cart-upload ()
  (interactive)
  ; verify contents of local cart - brief display
  (when (yes-or-no-p (format "Upload Amazon purchase list with %s items? " (length *amazon-cart-local*)))
  ; make a CartCreate object
    (let ((x (CartCreate "" :itemid (mapcar #'get-listingid (remove-if-not #'Offer-p *amazon-cart-local*))
			    :asin (mapcar #'asin (remove-if-not #'Item-p *amazon-cart-local*)))))
      (print x))))

(defgeneric amazon-url-helper (x) )

(defmethod amazon-url-helper ((x Item))
  (concat "ASIN=" (get-asin x)))

(defmethod amazon-url-helper ((x Offer))
  (concat "OfferListingId=" (get-listingid x)))

;; to do: need to add AWS keyid, etc..

(defmethod amazon-url ((x CartCreate))
  (let ((stuff (append (get-Items x) (get-Offers x))))
    (concat amzn-request-base-url "&Operation=CartCreate"
	    (labels ((foo (object-list counter) 
			  (cond 
			   ((not object-list) (values "" 0))
			   (t (progn
				(incf counter)
				(values (concat (format "&Item.%s." counter)
						(amazon-url-helper (car object-list))
						(format "&Item.%s.Quantity=1" counter)
						(car (foo (cdr object-list) counter)))
					counter))))))
	    (car (foo stuff 0))))))

(defun amazon-get-price (lxml)
  (Price ""
   :Amount (amazon-node-text (amazon-xpath "Amount" lxml))
   :CurrencyCode (amazon-node-text (amazon-xpath "CurrencyCode" lxml))
   :FormattedPrice (amazon-node-text (amazon-xpath "FormattedPrice" lxml))))

;; how do we define a method specialized to elisp types?  e.g. I want
;; to define a show method for the type 'string' -- it would just
;; return the string

;; ahh -- apparently this is not yet implemented in eieio; methods can
;; only be specialized on eieio classes

;; so we hack it ourselves:

(defun show-generic (x)
  (cond
   ((stringp x) x)
   ((listp x) x)
   (t (show x))))

(defun amazon-get-cartitem (lxml)
  (CartItem ""
   :CartItemId (amazon-node-text (amazon-xpath "CartItemId" lxml))
   :ASIN (amazon-node-text (amazon-xpath "ASIN" lxml))
   :Price (car (mapcar #'amazon-get-price (amazon-xpath "Price" lxml)))
   :Title (amazon-node-text (amazon-xpath "Title" lxml))))

(defmethod show ((x CartItem))
  (propertize
   (mapconcat (lambda (w) w)
	      (remove-if (lambda (z) (zerop (length z)))
			 (mapcar (lambda (y) (amazon-print-slot x y))
				 (mapcar #'car amazon-cartitem-display-alist))) "\n")))

;; CartCreateResponse/Cart/CartId
;; CartCreateResponse/Cart/HMAC
;; CartCreateResponse/Cart/URLEncodedHMAC
;; CartCreateResponse/Cart/PurhcaseURL
;; CartCreateResponse/Cart/CartItems/SubTotal/FormattedPrice
;; CartCreateResponse/Cart/CartItems/CartItem/CartItemId
;; CartCreateResponse/Cart/CartItems/CartItem/

;; extract-cart-response :: lisp-xml -> CartCreateResponse
(defun amazon-get-cartresponse  (lisp-xml)
  (CartCreateResponse ""
   :cartid (amazon-node-text (amazon-xpath "Cart/CartId" lisp-xml))
   :purchaseurl (amazon-node-text (amazon-xpath "Cart/PurchaseURL" lisp-xml))
   :hmac (amazon-node-text (amazon-xpath "Cart/HMAC" lisp-xml))
   :subtotal (amazon-node-text (amazon-xpath "Cart/CartItems/SubTotal/FormattedPrice" lisp-xml))
   :cartitems (mapcar #'amazon-get-cartitem (amazon-xpath "Cart/CartItems/CartItem" lisp-xml))))

(defmethod amazon-send ((x CartCreate))
  (if *amazon-cart* (setq *amazon-cart* nil))
  (url-retrieve (amazon-url x)
		(lambda (y) 
		  (setq *amazon-cart* (amazon-get-cartresponse (car (xml-parse-region (point-min) (point-max))))))
		nil)
  (while (not *amazon-cart*)
    (progn (message "Waiting for response ... ")
	   (sleep-for 0.1)))
  *amazon-cart*)

(defun amazon-cart-local-submit ()
  (interactive)
  (show (amazon-send *amazon-cart-local*)))

(defun amazon-cart-submit ()
  (interactive)
  (show (amazon-send *amazon-cart-local*)))

(defmethod show ((x CartCreateResponse)) 
  (pop-to-buffer (generate-new-buffer "*CartCreate response from amazon*"))
  (insert (concat "Subtotal: " (get-subtotal x) "\n"
		  "Purchase URL: "))
  ;; we copy the purchase URL to the kill ring so we can paste it into our browser, or into an email message
  (let ((beg (point)))
    (insert (get-purchaseurl x))
    (copy-region-as-kill beg (point)))
  (insert "\n\nCart Contents:") 
  (loop for cartitem in (get-cartitems x)
	for i from 1
	do (insert (format "\n\nItem #%s\n%s" i (show cartitem))))
  (setq buffer-read-only t)
  (goto-char (point-min))
  (amazon-cart-mode))
	 
(defun amzn-xml-offer-print (offer)
  (car (xml-get-children (car (xml-get-children offer 'OfferListing)) 'OfferListingId)))

;; remove those offers that don't meet my criteria: 
;; (1) price is most X% of Amazon price (TO DO) 
;; (2) feedback score is at least 4.7 out of 5

(defgeneric amzn-satisfactory (x) "Is this satisfactory?")

;; FIX ME -- uh, I don't think we use the following ...

;; ;; amzn-satisfactory :: oos:offer -> Bool
;; (defmethod amzn-satisfactory ((x Offer))
;;   (and 
;;    (> (string-to-number (get-price x)) 0)  ; FIX ME how to get Amazon base price info?
;;    (and (> (length (get-seller-feedback-avg x)) 0) (> (string-to-number (get-seller-feedback-avg x)) 4.6))))

;; amzn-remove-offers :: [oos:offer] -> [oos:offer]
(defun amzn-remove-offers (list)
  "Remove those offers that do not meet my criteria, as set in 'amzn-satisfactory'"
  (remove-if-not 'amzn-satisfactory list))
 
;; amzn-get-slot :: oos:offer -> String -> String
(defmethod amzn-get-slot ((x Offer))
  (let* ((string (completing-read "Show object slot: " (mapcar #'symbol-name (object-slots x))))
	 (foo (intern (concat "get-" string))))
    (print (funcall foo x))))

(defmethod amzn-get-slot ((x Item))
  (let* ((string (completing-read "Show object slot: " (mapcar #'symbol-name (object-slots x))))
	 (foo (intern (concat "get-" string))))
    (print (funcall foo x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor Movement in results buffers

; FIX ME: this function is probably poorly implemented; maybe it
; should use the correspondence between items and buffer regions?  

;; use amazon-item-view-separator to figure out how far to go down

(defun amazon-next-item ()
  (interactive)
  (forward-paragraph)
  (forward-line (- amazon-item-view-separator 1))
  (next-line))

; FIX ME: make sure entire paragraph is visible when we jump to it

(defun amazon-previous-item ()
  (interactive)
  (forward-paragraph (- 0 amazon-item-view-separator))
  (next-line))

;; ughh: the following function is not so good, because there is no
;; sort of prohibition on calling the function on an arbitrary word.
;; so sloppy users will try to lookup-isbn while on words.
;; perhaps we can add an ASIN text property??

(defun amazon-lookup-isbn ()
  (interactive)
  (let ((isbn (current-word)))
    (if isbn (show (amazon-send (ItemLookup "" :isbn isbn)))
      (message "No isbn at point."))))

(provide 'amazon)

;;; end of file amazon.el


