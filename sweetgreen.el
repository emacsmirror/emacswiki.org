DeletedPage
;;; sweetgreen.el --- Order Salads from sweetgreen.com -*- lexical-binding: t -*-

;; Change Log:
;; 16-Nov-2015    Diego Berrocal
;;    Add Documentation on functions

;; Change Log:
;; 16-Nov-2015    Diego Berrocal
;;    Add Org-Readme

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Homepage: https://www.github.com/CestDiego/sweetgreen.el
;; Created: Tue Nov  3 22:33:41 2012 (-0500)
;; Version: 0.4
;; URL: https://github.com/cestdiego/sweetgreen.el
;; Package-Requires: ((dash "2.12.1") (helm "1.5.6") (request "0.2.0") (cl-lib "0.5"))
;; Keywords: salad, food, sweetgreen, request

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:
(require 'request)
(require 'dash)
(require 'helm)
(require 'cl-lib)
(require 'json)

(defgroup sweetgreen nil
  "Order a variety of products from Sweetgreen without leaving your editor.")

(defcustom sweetgreen--username nil
  "Sweetgreen Accounr Username"
  :type 'string
  :group 'sweetgreen)

(defcustom sweetgreen--password nil
  "Sweetgreen Account Password"
  :type 'string
  :group 'sweetgreen)

(defvar sweetgreen--csrf-token-regexp "<meta content=\"\\([^\"]+\\).*?csrf-token.*?>"
  "Regular Expression used to grab the CSRF Token from the index page.")

(defvar sweetgreen--cookie-regexp "_session_id=\\([^;]+\\)"
  "Regular expression to get the Session ID from the response's headers")

(defvar sweetgreen--csrf-token ""
  "CSRF Token for http://orders.sweetgreen.com")

(defvar sweetgreen--cookie-string ""
  "Cookies for http://orders.sweetgreen.com")

(defvar sweetgreen--restaurants-alist ()
  "Nearby Restaurants alist")

(defvar sweetgreen--menu-alist ()
  "Menu for Current restaurant")
(defvar sweetgreen--products-alist ()
  "Menu for Current restaurant")
(defvar sweetgreen--curr-restaurant nil
  "Current Restaurant")

(defvar sweetgreen--available-times nil
  "Lis of times for current order")

(defvar sweetgreen--items-alist ()
  "Items available in the menu for the current RESTAURANT")
(defvar sweetgreen--curr-basket ()
  "Current Basket or Shopping Cart")
(defvar sweetgreen--curr-user      nil
  "Current logged in USER")
(defvar sweetgreen--curr-order-id  nil
  "Last order (product added to cart) we've done")
(defvar sweetgreen--curr-basket-id nil
  "Current Basket or Shopping Cart ID")

(defun => (alist &rest keys)
  "Accessor that makes it easy to traverse nested alists"
  (-reduce-from (lambda (acc item) (assoc-default item acc)) alist keys))

(defun sweetgreen//auth (&optional username password)
  "Authenticate USERNAME with PASSWORD to sweetgreen and get all cookies"
  (interactive)
  (unless sweetgreen--username
    (setq sweetgreen--username (read-from-minibuffer "Username: ")))
  (unless sweetgreen--password
    (setq sweetgreen--password (read-passwd "Super Secret Password: ")))
  (sweetgreen//fetch-csrf-token)
  (sweetgreen//fetch-auth-cookie username password))

(defun sweetgreen//fetch-csrf-token ()
  "Parse CSRF-Token out of Sweetgreen's Homepage"
  (let* ((response (request
                   "https://order.sweetgreen.com"
                   :type "GET"
                   :sync t
                   :parser 'buffer-string
                   :error
                   (cl-function (lambda (&key data error-thrown &allow-other-keys&rest _)
                                  (error "Got error: %S" error-thrown)))
                   ))
        (data  (request-response-data response))
        (csrf-token (progn
                      (string-match sweetgreen--csrf-token-regexp data)
                      (match-string 1 data))))
    (setq sweetgreen--csrf-token csrf-token)))

(defun sweetgreen//fetch-auth-cookie (username password)
  "Login to get a session cookie"
  (let* ((response (request
                    "https://order.sweetgreen.com/api/customers/login"
                    :type "POST"
                    :sync t
                    :data `(("customer[email]" . ,username)
                            ("customer[password]" . ,password))
                    :headers '(("Accept"       . "application/json")
                               ("Content-Type" . "application/x-www-form-urlencoded"))
                    :parser 'json-read
                    :error
                    (cl-function (lambda (&key data error-thrown &allow-other-keys&rest _)
                                   (error "Got error: %S" error-thrown)))
                    ))
         (header (request-response-header response "set-cookie"))
         (data (request-response-data response))
         (cookie-string (progn
                          (string-match sweetgreen--cookie-regexp header)
                          (concat "_session_id=" (match-string 1 header)))))
    (setq sweetgreen--curr-user     (=> data 'customer) )
    (setq sweetgreen--cookie-string cookie-string)))

(defun sweetgreen//logout (curr-user)
  "Logout CURR-USER and reset Session Cookie to `nil'. "
  (unless curr-user
    (error "You try to log out but you are not logged in m8"))
  (let* ((response (request
                    (format "https://order.sweetgreen.com/api/customers/%.0f"
                            (=> sweetgreen--curr-user 'id))
                    :type "DELETE"
                    :sync t
                    :headers '(("Accept"       . "application/json")
                               ("Content-Type" . "application/x-www-form-urlencoded")
                               ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                    :parser 'buffer-string))
         (header (request-response-header response "set-cookie"))
         (cookie-string (progn
                          (string-match sweetgreen--cookie-regexp header)
                          (concat "_session_id=" (match-string 1 header)))))
    (setq sweetgreen--curr-user nil)
    (setq sweetgreen--cookie-string cookie-string)))

(defun sweetgreen/helm-restaurants (zip_code)
  "Helm Interface to select Sweetgreen Restaurants"
  (interactive "sZip Code: ")
  (let* ((restaurant-alist (sweetgreen//get-restaurants zip_code)))
    (setq sweetgreen--restaurants-alist restaurant-alist)
    (helm :sources
          (helm-build-sync-source "Sweetgreen Restaurants"
            :candidates restaurant-alist
            :candidate-transformer
            (lambda (candidates)
              (--map
               `(,(format
                   "%+25s     ---->     %s miles away"
                   (propertize (=> (cdr it) 'name)
                    'face 'font-lock-warning-face)
                   (propertize (format "%.2f" (=> (cdr it) 'distance))
                    'face 'font-lock-function-name-face))
                 . ,it)
               candidates)
              )
            :persistent-action
            (lambda (selected_restaurant)
              (browse-url
               (concat "https://order.sweetgreen.com/"
                       (=> selected_restaurant 'restaurant_slug))))
            :action
            (lambda (candidate)
              (setq sweetgreen--curr-restaurant candidate)))
          :buffer "*Sweetgreen ❤ Restaurants*")))

(defun sweetgreen/helm-menu (restaurant_id)
  "Helm Interface to select Items from the Menu of a Sweetgreen Restaurant"
  (unless restaurant_id
    (error "No Restaurant ID specified"))
  (setq sweetgreen--menu-alist (sweetgreen//get-menu restaurant_id))
  (helm
   :sources (sweetgreen//make-helm-menu-sources restaurant_id)
   :buffer "*Sweetgreen ❤ Menu List*"))

(defun sweetgreen//make-helm-menu-sources (restaurant_id)
  "Sources for SWEETGREEN/HELM-MENU"
  (-map (lambda (menu)
          (let* ((name (upcase-initials (car menu)))
                 (menu-list (cdr menu))
                 (menu-alist (--map `(,(format "%+35s     ---->       %s"
                                               (propertize
                                                (upcase-initials (=> it 'name))
                                                'face
                                                'font-lock-warning-face)
                                               (propertize
                                                (format "%.2f" (/ (=> it 'cost) 100))
                                                'face
                                                'font-lock-function-name-face))
                                      . ,it)
                                    menu-list)))
            (helm-build-sync-source name
              :candidates menu-alist
              :persistent-action (lambda (candidate)
                                   (browse-url
                                    (concat "https://order.sweetgreen.com/nolita/"
                                            (=> candidate 'product_slug))))
              :action (lambda (candidate)
                        candidate))))
        sweetgreen--menu-alist))


(defun sweetgreen//get-restaurants (zip_code)
  "Get Restaurants alist out of your zip code"
  (when (and sweetgreen--csrf-token
             sweetgreen--cookie-string)
    (let* ((response    (request
                         "https://order.sweetgreen.com/api/restaurants"
                         :type "GET"
                         :sync t
                         :params `(("zip_code" . ,zip_code))
                         :headers `(("Cookie" . ,sweetgreen--cookie-string)
                                    ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                         :parser 'json-read))
           (data        (request-response-data response))
           (restaurants (--map `(,(=> it 'id) . ,it)
                               (=> data 'restaurants))))
      restaurants)))

(defun sweetgreen//get-menu (restaurant_id)
  "Get the MENU available at RESTAURANT_id"
  (when (and sweetgreen--csrf-token
             sweetgreen--cookie-string)
    (let* ((menu-response (request
                           (concat "https://order.sweetgreen.com/api/menus/"
                                   restaurant_id)
                           :type "GET"
                           :sync t
                           :headers `(("Cookie" . ,sweetgreen--cookie-string)
                                      ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                           :parser 'json-read))
           (menu-data     (request-response-data menu-response))
           (products      (append (=> menu-data 'products) nil))
           (menu (--group-by (=> it 'category_name) products)))
      (setq sweetgreen--products-alist (--map `(,(=> it 'id) . ,it) products))
      menu)))

(defun sweetgreen//add-to-cart (product)
  "Add PRODUCT to CURR-BASKET and to the online Shopping Cart"
  (let* ((response      (request
                         "https://order.sweetgreen.com/api/line_items"
                         :type "POST"
                         :sync t
                         :data (json-encode
                                `(("line_item" . (("quantity" . 1)
                                                  ("product_id" . ,(=> product 'id))
                                                  ("restaurant_id" . ,(=> product 'restaurant_id))
                                                  ("calories" . ,(=> product 'calories))))))
                         :headers `(("Content-Type" . "application/json")
                                    ("Cookie" . ,sweetgreen--cookie-string)
                                    ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                         :parser 'json-read))
         (data          (request-response-data response))
         (item          (=> data 'line_item))
         (item_id       (=> item 'id))
         (order_id      (=> item 'ignored_order_id))
         (curr_basket (sweetgreen//fetch-basket (number-to-string order_id))))
    (push `(,item_id . ,item) sweetgreen--items-alist)
    (setq sweetgreen--curr-order-id (number-to-string order_id))
    curr_basket))

(defun sweetgreen//fetch-basket (order-id)
  "Get CURR-BASKET out of ORDER-ID"
  (let* ((response  (request
                     "https://order.sweetgreen.com/api/orders"
                     :type    "GET"
                     :sync    t
                     :params  `(("id" . ,order-id))
                     :headers `(("Content-Type" . "application/json")
                                ("Cookie" . ,sweetgreen--cookie-string)
                                ("X-CSRF-Token" . ,sweetgreen--csrf-token))

                     :parser 'json-read
                     :error
                     (cl-function (lambda (&key data error-thrown &allow-other-keys&rest _)
                                    (error "Got error: %S" error-thrown)))))
         (data       (request-response-data response))
         (order      (aref (=> data 'orders) 0)))
    (setq sweetgreen--curr-basket-id (=> order 'basket_id))
    (setq sweetgreen--available-times (=> order 'available_wanted_times_tuples))
    (setq sweetgreen--curr-basket order)))

(defun sweetgreen/confirm-product (product)
  "Build prompt with random pun and interactively confirm order"
  (let* ((name     (upcase-initials (=> product 'name)))
         (restaurant (=> sweetgreen--restaurants-alist (=> product 'restaurant_id)))
         (location (=> restaurant 'name))
         (address (concat (=> restaurant 'address) ", " (=> restaurant 'state)))
         (instructions (=> restaurant 'pickup_instructions))
         (random-pun (nth (random 4) '("Orange you glad you use Emacs?"
                                       "Do you like to party?? Lettuce turnip the beet!"
                                       "Don't forget to lettuce know if you came from RC"
                                       "Romaine calm! You haven't order your salad yet")) )
         (cost     (/ (=> product 'cost) 100))
         (calories (=> product 'calories)))
    (y-or-n-p
     (format
      "%s
You are buying the %s
At the %s location @ %s
%s
Price before Taxes is $%.2f
It contains %.0f calories
Confirm your order? "
      random-pun
      name
      location
      address
      instructions
      cost
      calories))))

(defun sweetgreen//helm-select-time (order)
  "Select time to pickup order"
  (unless order
    (error "You have given no order to select time"))
  (let ((available-times (--map `(,(=> it 'formatted) . ,(=> it 'original))
                                (=> order 'available_wanted_times_tuples))))
    (helm
     :sources (helm-build-sync-source "Available pickup times"
                :candidates available-times
                :action 'identity)
     :buffer "*Sweetgreen ❤ Available Pickup Times  *")))

(defun sweetgreen//order-product (product)
  "Contact Sweetgreen server to order product"
  (let* ((basket (sweetgreen//add-to-cart product))
         (wanted_time (sweetgreen//helm-select-time basket)))
    (if (y-or-n-p "really continue?")
        (sweetgreen//checkout basket wanted_time)
      (sweetgreen//cancel-orders basket))))

(defun sweetgreen//cancel-item (id)
  "Cancel Item with ID"
  (let ((request-url (concat "https://order.sweetgreen.com/api/line_items/"
                             (number-to-string id))))
    (request
     request-url
     :type "DELETE"
     :headers `(("Cookie" . ,(concat "_session_id=" sweetgreen--cookie-string))
                ("X-CSRF-Token" . ,sweetgreen--csrf-token))

     :status-code '((204 . (lambda (&rest _) (message "Deleted item successfully")))
                    (500 . (lambda (&rest _) (message "Item doesn't seem to exist")))))))

(defun sweetgreen//cancel-orders (order)
  "Cancel all items from ORDER"
  (let ((item_ids (=> order 'line_item_ids)))
    (--map (sweetgreen//cancel-item it) item_ids)
    (setq sweetgreen--curr-basket nil)
    (setq sweetgreen--curr-basket-id nil)))

(defun sweetgreen//checkout (basket wanted_time)
  "Checkout BASKET to be picked up at WANTED_TIME"
  (let ((data `(("order" .
                 (
                  ("available_wanted_times_tuples" . ,(=> basket 'available_wanted_times_tuples))
                  ("basket_id"                     . ,(=> basket 'basket_id))
                  ("created_at"                    . ,(=> basket 'created_at))
                  ("coupon_code"                   . ,(=> basket 'coupon_code))
                  ("coupon_discount"               . ,(=> basket 'coupon_discount))
                  ("placed_time"                   . ,(=> basket 'placed_time))
                  ("formatted_wanted_time"         . ,(=> basket 'formatted_wanted_time))
                  ("restaurant_id"                 . ,(=> basket 'restaurant_id))
                  ("sales_tax"                     . ,(=> basket 'sales_tax))
                  ("subtotal"                      . ,(=> basket 'subtotal))
                  ("total"                         . ,(=> basket 'total))
                  ("shows_feedback_form"           . ,(=> basket 'shows_feedback_form))
                  ("wanted_time"                   . ,wanted_time)
                  ("uploaded_at")
                  ("contact_number" . "XXXXXXX")
                  ("state" . "complete")
                  ("billing_account" .
                   (
                    ("card_type" . "cash")
                    ("card_number")
                    ("zip")
                    ("last_four")
                    ("cvv")
                    ("expiry_month")
                    ("expiry_year")
                    ("description" . "sweetgreen Rewards (Pay with App)")
                    ("save_on_file" . :json-false))))))))
    (request
     (concat "https://order.sweetgreen.com/api/orders/" (number-to-string (=> basket 'id)))
     :type "PUT"
     :headers `(("Cookie" . ,(concat "_session_id=" sweetgreen--cookie-string))
                ("Content-Type" . "application/json")
                ("X-CSRF-Token" . ,sweetgreen--csrf-token))
     :data (json-encode data)
     :parser 'json-read
     :complete (cl-function
                (lambda (&key data response &allow-other-keys)
                  (let* ((basket     (aref (=> data 'orders) 0))
                         (basket_id (=> basket 'basket_id)))
                    (print data)
                    (message "Yeah salad is ordered")))))))

;;;###autoload
(defun sweetgreen (args)
  "Order salad from http://sweetgreen.com"
  (interactive "P")
  (when args
    (setq sweetgreen--curr-restaurant nil))
  (when sweetgreen--curr-user
    (sweetgreen//logout sweetgreen--curr-user))
  ;; Get CSRF Token and Cookie Headers
  (call-interactively 'sweetgreen//auth)
  ;; Get Current Restaurant and Item to Buy
  (let* ((curr-restaurant (or sweetgreen--curr-restaurant
                              (call-interactively 'sweetgreen/helm-restaurants)))
         (curr-restaurant-id (number-to-string (=> curr-restaurant 'id)))

         (curr-product         (sweetgreen/helm-menu curr-restaurant-id))
         (confirmed-product    (sweetgreen/confirm-product curr-product)))
    (when confirmed-product
      (sweetgreen//order-product curr-product))))

(provide 'sweetgreen)

;; Local Variables:
;; coding: utf-8
;; End:

;;; sweetgreen.el ends here
