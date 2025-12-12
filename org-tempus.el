;;; org-tempus.el --- Org functions to track work hours  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Raul Benencia

;; Author: Raul Benencia <id@rbenencia.name>
;; Maintainer: Raul Benencia <id@rbenencia.name>
;; URL: https://github.com/rul/org-tempus
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, time, clock

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; TODO

;;; Code:
(require 'org-clock)

(defgroup org-tempus ()
  "Display mode line indicator with information about clocked time."
  :group 'org)

(defcustom org-tempus-add-to-global-mode-string t
  "When non-nil, append the Org Tempus construct to the mode line."
  :type 'boolean
  :package-version '(org-tempus . "0.0.1")
  :group 'org-tempus)

(defcustom org-tempus-update-interval 60
  "Seconds between automatic mode line refreshes."
  :type 'integer
  :group 'org-tempus)

(defvar org-tempus-mode-line-string ""
  "Org Tempus mode line indicator.")

(defvar org-tempus--timer nil
  "Timer used to refresh the Org Tempus mode line.")

(defun org-tempus--current-task-name ()
  "Return unpropertized name of current task."
  (substring-no-properties org-clock-current-task))

(defun org-tempus--sum-today ()
  "Return total time clocked today across agenda files as a duration string."
  (let* ((range (org-clock-special-range 'today nil t))
         (tstart (nth 0 range))
         (tend (nth 1 range))
         (total 0))
    (dolist (file (org-agenda-files t) total)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (setq total (+ total (org-clock-sum tstart tend))))))
    (org-duration-from-minutes total)))

(defun org-tempus--current-task-time ()
  "Return clocked time for current task as a duration string."
  (org-duration-from-minutes (org-clock-get-clocked-time)))

(defun org-tempus--update-mode-line ()
  "Update the Org Tempus mode line indicator."
  (setq org-tempus-mode-line-string
        (propertize
         (if (org-clock-is-active)
             (concat "🧉 " (org-tempus--current-task-name) " " (org-tempus--current-task-time) " " (org-tempus--sum-today))
           (concat "☠️ " " "))
         'mouse-face 'mode-line-highlight))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode org-tempus-mode
  "Minor mode to enhance time tracking in ‘org-mode’."
  :lighter " Tempus fugit"
  :global t
  (if org-tempus-mode
      (progn
        (when (timerp org-tempus--timer)
          (cancel-timer org-tempus--timer))
        (setq org-tempus--timer
              (run-at-time org-tempus-update-interval
                           org-tempus-update-interval
                           #'org-tempus--update-mode-line))
        (add-hook 'org-clock-in-hook #'org-tempus--update-mode-line)
        (add-hook 'org-clock-out-hook #'org-tempus--update-mode-line)
        (when org-tempus-add-to-global-mode-string
          (or global-mode-string (setq global-mode-string '("")))
          (or (memq 'org-tempus-mode-line-string global-mode-string)
              (setq global-mode-string
                    (append global-mode-string '(org-tempus-mode-line-string)))))
        (org-tempus--update-mode-line))

    (when org-tempus-add-to-global-mode-string
      (or global-mode-string (setq global-mode-string '("")))
      (setq global-mode-string
            (remove 'org-tempus-mode-line-string global-mode-string))
      (force-mode-line-update))
    (when (timerp org-tempus--timer)
      (cancel-timer org-tempus--timer))
    (setq org-tempus--timer nil)
    (remove-hook 'org-clock-in-hook #'org-tempus--update-mode-line)
    (remove-hook 'org-clock-out-hook #'org-tempus--update-mode-line)))

(provide 'org-tempus)
;;; org-tempus.el ends here
