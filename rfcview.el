;;; rfcview.el --- RFC viewer for Emacs

;; Copyright (C) 2016 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Homepage: http://github.com/zeph1e/rfcview.el
;; Created: 5 Oct 2016
;; Version: 0.2
;; Keywords: docs
;; License: WTFPL (http://sam.zoy.org/wtfpl)
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

;;; Change Log:
;;
;;  0.2 - 12 Oct 2016 - family link & filters
;;
;;   - add links of family RFC, which succeed or precede the RFC,
;;     into the bottom of RFC entries in index.
;;   - supports filters: recent, favorite, keywords search,
;;   - misc. fixes & refactorings on listing & line-wrapping.
;;
;;; Code:

(require 'rfcview-index)
(require 'rfcview-reader)

;;;###autoload
(defun rfcview ()
  "Shows RFC index"
  (interactive)
  (let ((buffer (get-buffer "*RFC INDEX*")))
    (unless buffer
      (setq buffer (set-buffer (get-buffer-create "*RFC INDEX*")))
      (buffer-disable-undo)
      (rfcview:initialize)
      (rfcview:index-mode))
    (select-window (display-buffer buffer))
    (rfcview:refresh-index)))

(provide 'rfcview)
;;; rfcview.el ends here
