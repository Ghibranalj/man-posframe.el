;;; man-posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C)
;; Author: Ghibranalj
;; Maintainer: Ghibranalj
;; URL: https://github.com/Ghibranalj/man-posframe.el
;; Version: 0.5.0
;; Keywords: convenience, tooltip
;; Package-Requires: ((emacs "27") (posframe "1.3.3"))

;;; Commentary:
;; * man-posframe README
;; :README:

;; man-posframe. View man pages on a popup
;; ;; More info please see: readme.md

;;; Code:
;; * man-posframe code                         :CODE:

(require 'man)
(require 'posframe)

(defgroup man-posframe nil
  "Man posframe"
  :prefix "man-posframe"
  :group 'man)

(defvar man-posframe--buffer nil)
(defvar man-posframe--frame nil)

(defcustom man-posframe-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'man-posframe-close)
    (define-key map (kbd "j") 'man-posframe-scroll-down)
    (define-key map (kbd "k") 'man-posframe-scroll-up)
    (define-key map (kbd "<escape>") 'man-posframe-close)
    map)
  "Keymap for controlling posframes."
  :type 'keymap
  :group 'man-posframe)

(defcustom man-posframe-parameters nil
  "frame parameters used by man-posframe"
  :type 'string
  :group 'man-posframe)

(defcustom man-posframe-poshandler 'posframe-poshandler-frame-center
  "posframe used by man-posframe"
  :type 'symbol
  :group 'man-posframe)

(defcustom man-posframe-width nil
  "man-posframe width"
  :type 'number
  :group 'man-posframe)

(defcustom man-posframe-height nil
  "man-posframe height"
  :type 'number
  :group 'man-posframe)

(defcustom man-posframe-border-width 2
  "man-posframe border width"
  :type 'number
  :group 'man-posframe)


(defface man-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the man-posframe"
  :group 'man-posframe)

(defun man-posframe-say-hello ()
  (interactive)
  (message "hello"))

(defun man-posframe-show (topic)
  (interactive "sTopic: ")

  (if man-posframe--buffer
      (posframe-hide man-posframe--buffer)f)

  (let ((prefnotify Man-notify-method)
        (ignored (setq Man-notify-method 'quiet))
        (buffer (Man-getpage-in-background topic))
        (frame nil))

    (setq frame
          (posframe-show
           buffer
           :poshandler man-posframe-poshandler
           :height man-posframe-height :min-height man-posframe-height
           :width man-posframe-height :min-width man-posframe-width
           :parameters man-posframe-parameters
           :border-color (face-attribute 'man-posframe-border :background)
           :border-width man-posframe-border-width
           ))
    ;; set keymap until frame is closed
    (set-transient-map man-posframe-keymap
                       (lambda () man-posframe--frame)
                       'man-posframe-close)

    (setq man-posframe--buffer buffer)
    (setq man-posframe--frame frame)
    (setq Man-notify-method prefnotify)))

(defun man-posframe-close ()
  (interactive)
  (posframe-hide man-posframe--buffer)
  ;; NOTE if you override this function
  ;; you need to set this variable to nil
  (setq man-posframe--frame nil))


(defun man-posframe-scroll-down ()
  (interactive)
  (with-selected-frame man-posframe--frame
    (scroll-up 1)))

(defun man-posframe-scroll-up ()
  (interactive)
  (with-selected-frame man-posframe--frame
    (scroll-down 1)))

(provide 'man-posframe)
