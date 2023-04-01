;;; dimmer-extra.el --- Configure dimmer -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/dimmer-extra
;; Version: 0.1.0
;; Keywords: faces, editing
;; Package-Requires: ((emacs "28.1") (dimmer "0.4.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure dimmer

;;; Code:

(require 'dimmer)

(defvar dimmer-extra-fraction-num 0.20)

;;;###autoload
(defun dimmer-extra-set ()
  "Set `dimmer-fraction' and `dimmer-adjustment-mode' with `custom-set-variables'."
  (interactive)
  (custom-set-variables (list 'dimmer-fraction
                              dimmer-extra-fraction-num)
                        (list 'dimmer-adjustment-mode
                              dimmer-adjustment-mode)
                        (list 'dimmer-use-colorspace
                              dimmer-use-colorspace))
  (dimmer-restore-all)
  (dimmer-process-all t))

;;;###autoload
(defun dimmer-extra-save ()
  "Save values for `dimmer-fraction' and `dimmer-adjustment-mode'."
  (interactive)
  (customize-save-variable 'dimmer-fraction
                           dimmer-extra-fraction-num)
  (customize-save-variable 'dimmer-adjustment-mode
                           dimmer-adjustment-mode)
  (customize-save-variable 'dimmer-use-colorspace
                           dimmer-use-colorspace)
  (message
   "Dimmer extra: saved fraction %s, adjustment %s and colorspace %s"
   dimmer-extra-fraction-num
   dimmer-adjustment-mode
   dimmer-use-colorspace)
  (dimmer-restore-all)
  (dimmer-process-all t))

(defun dimmer-extra-set-fraction (sym val)
  "Set VAL to SYM if it is a valid fraction."
  (when (and (numberp val)
             (>= val 0)
             (<= val 1))
    (set sym val)
    (dimmer-restore-all)
    (let* ((selected (current-buffer))
           (ignore   (cl-some (lambda (f)
                                (and (fboundp f)
                                     (funcall f)))
                              dimmer-prevent-dimming-predicates))
           (visbufs  (dimmer-visible-buffer-list))
           (filtbufs (dimmer-filtered-buffer-list visbufs)))
      (dimmer--dbg 1 "dimmer-process-all: force %s" t)
      (setq dimmer-last-buffer selected)
      (when (not ignore)
        (dolist (buf visbufs)
          (dimmer--dbg 2 "dimmer-process-all: buf %s" buf)
          (if (or (eq buf selected)
                  (not (memq buf filtbufs)))
              (dimmer-restore-buffer buf)
            (dimmer-dim-buffer buf val)))))))

(defun dimmer-extra-change-value (val)
  "Increment and save value of `dimmer-extra-fraction-num' to VAL."
  (dimmer-restore-all)
  (let ((next (string-to-number (format "%.02f"
                                        (+ val dimmer-extra-fraction-num)))))
    (dimmer-extra-set-fraction 'dimmer-extra-fraction-num next)))

;;;###autoload
(defun dimmer-extra-set-other-fraction ()
  "Set fraction to `dimmer-extra-fraction-num'."
  (interactive)
  (let ((num (read-number "Value: " dimmer-extra-fraction-num)))
    (dimmer-extra-set-fraction 'dimmer-extra-fraction-num num)))

;;;###autoload
(defun dimmer-extra-up ()
  "Increase value of dimmer fraction."
  (interactive)
  (dimmer-extra-change-value 0.01))

;;;###autoload
(defun dimmer-extra-down ()
  "Decrease value of dimmer fraction."
  (interactive)
  (dimmer-extra-change-value -0.01))

(defun dimmer-extra-format-value (value)
  "Format VALUE with face."
  (propertize (format "(%s)" value) 'face 'transient-value))

;;;###autoload
(defun dimmer-extra-set-foreground (&rest _)
  "Update value of `dimmer-adjustment-mode' to foreground."
  (interactive)
  (setf dimmer-adjustment-mode :foreground)
  (dimmer-restore-all)
  (dimmer-process-all t))

;;;###autoload
(defun dimmer-extra-set-background (&rest _)
  "Update value of `dimmer-adjustment-mode' to background."
  (interactive)
  (setf dimmer-adjustment-mode :background)
  (dimmer-restore-all)
  (dimmer-process-all t))

;;;###autoload
(defun dimmer-extra-set-both (&rest _)
  "Update value of `dimmer-adjustment-mode' to both."
  (interactive)
  (setf dimmer-adjustment-mode :both)
  (dimmer-restore-all)
  (dimmer-process-all t))

(defun dimmer-extra-get-options (var)
  "Extract options from custom variable VAR.
Return ALIST of descriptions and values."
  (mapcar (lambda (it)
            (cons (seq-find
                   #'stringp it)
                  (car
                   (last it))))
          (cdr
           (get
            var
            'custom-type))))

;;;###autoload
(defun dimmer-extra-set-ciealab-colorspace (&rest _)
  "Update value of `dimmer-adjustment-mode' to CIELAB."
  (interactive)
  (setf dimmer-use-colorspace :cielab)
  (dimmer-restore-all)
  (dimmer-process-all t))

;;;###autoload
(defun dimmer-extra-set-hsl-colorspace (&rest _)
  "Update value of `dimmer-use-colorspace' to HSL."
  (interactive)
  (setf dimmer-use-colorspace :hsl)
  (dimmer-restore-all)
  (dimmer-process-all t))

;;;###autoload
(defun dimmer-extra-set-rgb-colorspace (&rest _)
  "Update value of `dimmer-adjustment-mode' to RGB."
  (interactive)
  (setf dimmer-use-colorspace :rgb)
  (dimmer-restore-all)
  (dimmer-process-all t))

(defun dimmer-extra-make-toggle-description (description value &optional
                                                         on-label off-label)
  "Enhance DESCRIPTION for VALUE with ON-LABEL or OFF-LABEL."
  (concat  (or description "")
           (propertize " "
                       'display
                       '(space :align-to
                               40))
           (if value
               (propertize  (or on-label "(+)") 'face
                            'transient-argument)
             (propertize  (or off-label "(-)") 'face
                          'transient-inactive-value))) )

;;;###autoload (autoload 'dimmer-extra-transient "km-dimmer.el" nil t)
(transient-define-prefix dimmer-extra-transient ()
  "Configure dimmer variables on the fly."
  :transient-non-suffix 'transient--do-stay
  ["Dimmer mode"
   ("t" dimmer-mode
    :description (lambda ()
                   (dimmer-extra-make-toggle-description
                    "Dimmer mode"
                    (symbol-value 'dimmer-mode)
                    "[on]"
                    "[off]"))
    :transient t)]
  [:description
   (lambda ()
     (concat (propertize "Dim mode" 'face `(:inherit transient-heading)
                         'display '((height 1.1)))
             " "
             (dimmer-extra-format-value
              (format "%s"
                      (car
                       (rassoc dimmer-adjustment-mode
                               (dimmer-extra-get-options
                                'dimmer-adjustment-mode)))))))
   ("f" dimmer-extra-set-foreground
    :description
    (lambda ()
      (dimmer-extra-make-toggle-description
       "Foreground"
       (eq dimmer-adjustment-mode :foreground)))
    :transient t)
   ("b" dimmer-extra-set-background
    :description (lambda ()
                   (dimmer-extra-make-toggle-description
                    "Background"
                    (eq dimmer-adjustment-mode :background)))
    :transient t)
   ("a" dimmer-extra-set-both
    :description (lambda ()
                   (dimmer-extra-make-toggle-description
                    "Both"
                    (eq dimmer-adjustment-mode :both)))
    :transient t)]
  [:description
   (lambda ()
     (concat (propertize "Colorspace" 'face
                         `(:inherit transient-heading)
                         'display '((height 1.1)))
             " "
             (dimmer-extra-format-value
              (format "%s"
                      (car
                       (rassoc dimmer-use-colorspace
                               (dimmer-extra-get-options
                                'dimmer-use-colorspace)))))))
   ("c" dimmer-extra-set-ciealab-colorspace
    :description
    (lambda ()
      (dimmer-extra-make-toggle-description
       "CIELAB"
       (eq dimmer-use-colorspace :cielab)))
    :transient t)
   ("h" dimmer-extra-set-hsl-colorspace
    :description (lambda ()
                   (dimmer-extra-make-toggle-description
                    "HSL"
                    (eq dimmer-use-colorspace :hsl)))
    :transient t)
   ("r" dimmer-extra-set-rgb-colorspace
    :description (lambda ()
                   (dimmer-extra-make-toggle-description
                    "RGB"
                    (eq dimmer-use-colorspace :rgb)))
    :transient t)]
  [:description
   (lambda ()
     (concat (propertize "Dimmer fraction " 'face `(:inherit transient-heading)
                         'display '((height 1.1)))
             (dimmer-extra-format-value dimmer-fraction)
             "\n"
             (propertize "New fraction " 'face `(:inherit transient-heading)
                         'display '((height 1.1)))
             (dimmer-extra-format-value dimmer-extra-fraction-num)))
   ("<up>" "Increase value of dimmer fraction." dimmer-extra-up
    :transient t)
   ("<right>" "Increase value of dimmer fraction." dimmer-extra-up
    :transient t)
   ("<left>" "Decrease value of dimmer fraction." dimmer-extra-down
    :transient t)
   ("<down>" "Decrease value of dimmer fraction." dimmer-extra-down
    :transient t)
   ("o" "Other" dimmer-extra-set-other-fraction)]
  [("s" "Set" dimmer-extra-set)
   ("RET" "Save" dimmer-extra-save)]
  (interactive)
  (setq dimmer-extra-fraction-num dimmer-fraction)
  (transient-setup 'dimmer-extra-transient))

(defun dimmer-extra-mode-off ()
  "Turn off dimmer mode."
  (dimmer-mode -1)
  (add-hook 'minibuffer-exit-hook #'dimmer-mode))

(defun dimmer-extra-inibit-dimmer-in-minibuffer ()
  "Disable dimmer mode in minibuffer."
  (if (bound-and-true-p dimmer-mode)
      (add-hook 'minibuffer-setup-hook #'dimmer-extra-mode-off)
    (remove-hook 'minibuffer-setup-hook #'dimmer-extra-mode-off)
    (remove-hook 'minibuffer-exit-hook #'dimmer-mode)))


(provide 'dimmer-extra)
;;; dimmer-extra.el ends here










