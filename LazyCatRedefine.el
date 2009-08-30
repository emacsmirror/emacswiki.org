;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 重定义函数 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ### `mouse-avoidance-banish-destination' ###
;;; --- 鼠标的自动跳转函数, 改变跳转的位置到FRAME的右下角
(defun mouse-avoidance-banish-destination ()
  "The position to which Mouse-Avoidance mode `banish' moves the mouse.
You can redefine this if you want the mouse banished to a different corner."
  (let* ((pos (window-edges)))
    (cons (- (nth 1 pos) 50)                   ;make mouse hide in left of frame
          (+ (nth 1 pos) (frame-height) 50)))) ;make mouse hide in bottom of frame

;;; ### `w3m-bookmark-kill-entry' ###
;;; --- W3m删除书签, 刷新后保持当前位置不懂
(defun w3m-bookmark-kill-entry (num)
  "Kill the bookmark entry of the current line.
With prefix argument, kill that many entries from point."
  (interactive "p")
  (let ((entries (w3m-bookmark-current-number))
        (current-point (point))         ;I add, make rememeber current postion
        )
    (when entries
      (setq entries (list entries))
      (while (> (decf num) 0)
        (push (1+ (car entries)) entries))
      (condition-case nil
          (w3m-bookmark-kill-entries entries)
        (file-supersession nil))
      (w3m-bookmark-view t))
    (goto-char current-point)           ;I add, jump origianl position
    ))

;;; ### `emms-stream-play' ###
;;; --- EMMS播放流媒体, 用mplayer在后台播放, EMMS自带的函数播放不出来
(defun emms-stream-play ()
  "Play stream"
  (interactive)
  (let* ((line (or (get-text-property (point) 'emms-stream)
                   (progn
                     (goto-char (or (previous-single-property-change
                                     (point) 'emms-stream)
                                    (point-min)))
                     (goto-char (or (previous-single-property-change
                                     (point) 'emms-stream)
                                    (point-min)))
                     (get-text-property (point) 'emms-stream))
                   (error "No stream found at point")))
         (url (emms-stream-url line)))
    (when url                                        ;如果连接存在
      (emms-stop)                                    ;停止EMMS
      (shell-command-unique "killall mplayer")       ;杀掉MPLAYER进程
      (sit-for 1)                                    ;等待1秒
      (shell-command-unique "mplayer -playlist" url) ;播放列表
      (message "Loading stream from %s ..." url)     ;显示信息
      )))

;;; ### `emms-browser-view-in-dired' ###
;;; --- 跳转到具体的文件
(defun emms-browser-view-in-dired (&optional bdata)
  "View the current file in dired."
  (interactive)
  (if bdata
      (if (eq (emms-browser-bdata-type bdata) 'info-title)
          (let* ((track (car (emms-browser-bdata-data bdata)))
                 (path (emms-track-get track 'name))
                 (dir (file-name-directory path)))
            (find-file dir)
            (dired-goto-file path))     ;add this, jump to sepcial file
        (emms-browser-view-in-dired (car (emms-browser-bdata-data bdata))))
    (emms-browser-view-in-dired (emms-browser-bdata-at-point))))

;;; ### `company-expand-common ###
;;; --- 修改行为时连续两次用这个命令时不显示提示
(defun company-expand-common ()
  (interactive)
  (unless company-mode (error "company-mode not enabled"))
  (unless company-completions
    (company-begin))
  (if company-completions
      (if (eq last-command 'company-expand-common)
          ;; hit twice, show message
          (message "No COMMON part, please continue type or select TOP!")
        (let ((common (company-chop company-common)))
          ;; this calls after-change hook!
          (company-insert-completion common)
          (unless (cdr company-completions)
            (company-done))
          common))
    (when (called-interactively-p)
      (error "No completions found"))))

(provide 'LazyCatRedefine)
