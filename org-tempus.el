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

(defface org-tempus-mode-line-face
  '((t :inherit mode-line))
  "Face used for the Org Tempus mode line string."
  :group 'org-tempus)

(defface org-tempus-mode-line-hover-face
  '((t :inherit mode-line-highlight))
  "Face used when hovering over the Org Tempus mode line string."
  :group 'org-tempus)

(defcustom org-tempus-add-to-global-mode-string t
  "When non-nil, append the Org Tempus construct to the mode line."
  :type 'boolean
  :package-version '(org-tempus . "0.0.1")
  :group 'org-tempus)

(defcustom org-tempus-update-interval 60
  "Seconds between automatic mode line refreshes."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-session-gap-seconds 60
  "Maximum gap in seconds to consider consecutive clocks the same session."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-session-threshold-seconds 1800
  "Seconds of continuous session after which a notification is sent."
  :type 'integer
  :group 'org-tempus)

(defface org-tempus-session-face
  '((t :inherit (error mode-line) :weight bold))
  "Face used for the session duration in the mode line after threshold."
  :group 'org-tempus)

(defvar org-tempus-mode-line-string ""
  "Org Tempus mode line indicator.")

(defvar org-tempus--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ignore)
    map)
  "Keymap used for the Org Tempus mode line.")

(defconst org-tempus--mode-line-format '(:eval org-tempus-mode-line-string)
  "Mode line construct used by Org Tempus.")

(defvar org-tempus--timer nil
  "Timer used to refresh the Org Tempus mode line.")

(defvar org-tempus--session-start-time nil
  "Internal session start time as a value returned by `current-time'.")

(defvar org-tempus--session-threshold-notified nil
  "Internal flag indicating the session threshold notification was sent.")

(defun org-tempus--notify (msg)
  "Notify user with MSG using desktop notifications when available."
  (let ((sent nil))
    (when (or (fboundp 'notifications-notify)
              (require 'notifications nil t))
      (setq sent
            (condition-case err
                (progn
                  (notifications-notify :title "Org Tempus" :body msg)
                  t)
              (error
               (message "Org Tempus notification error: %s" err)
               nil))))
    (unless sent
      (message "%s" msg))))

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

(defun org-tempus--update-session-start ()
  "Update session start time, keeping short switches in the same session."
  (when org-clock-start-time
    (let* ((last-out org-clock-out-time)
           (gap (and last-out
                     (float-time (time-subtract org-clock-start-time last-out)))))
      (setq org-tempus--session-start-time
            (if (and gap
                     (>= gap 0)
                     (<= gap org-tempus-session-gap-seconds))
                (or org-tempus--session-start-time org-clock-start-time)
              (progn
                (setq org-tempus--session-threshold-notified nil)
                org-clock-start-time))))))

(defun org-tempus--current-session-duration ()
  "Return current session duration in seconds.
A session does not reset when switching tasks within
`org-tempus-session-gap-seconds'."
  (if (not (org-clock-is-active))
      0
    (unless org-tempus--session-start-time
      (setq org-tempus--session-start-time org-clock-start-time))
    (floor (float-time (time-subtract (current-time)
                                      org-tempus--session-start-time)))))

(defun org-tempus--maybe-notify-session-threshold (session-seconds)
  "Send a one-time notification when SESSION-SECONDS crosses threshold."
  (when (and (>= session-seconds org-tempus-session-threshold-seconds)
             (not org-tempus--session-threshold-notified))
    (setq org-tempus--session-threshold-notified t)
    (let ((msg (format "Org Tempus session reached %s"
                       (org-duration-from-minutes
                        (/ session-seconds 60.0)))))
      (org-tempus--notify msg))))

(defun org-tempus--update-mode-line ()
  "Update the Org Tempus mode line indicator."
  (let* ((raw (if (org-clock-is-active)
                  (let* ((session-seconds (org-tempus--current-session-duration))
                         (session (org-duration-from-minutes
                                   (/ session-seconds 60.0)))
                         (session-str (if (>= session-seconds
                                              org-tempus-session-threshold-seconds)
                                          (propertize session 'face 'org-tempus-session-face)
                                        session)))
                    (org-tempus--maybe-notify-session-threshold session-seconds)
                    (concat "⏳ [S " session-str
                            " | T " (org-tempus--sum-today) "] ("
                            (org-tempus--current-task-name)
                            " <" (org-tempus--current-task-time)
                            ">)"))
                (concat "⌛️ [T " (org-tempus--sum-today)"]")))
         (str (propertize raw
                          'mouse-face 'org-tempus-mode-line-hover-face
                          'local-map org-tempus--mode-line-map
                          'keymap org-tempus--mode-line-map
                          'help-echo "Org Tempus"
                          'pointer 'hand)))
    (add-face-text-property 0 (length str) 'org-tempus-mode-line-face 'append str)
    (setq org-tempus-mode-line-string str))
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
      (add-hook 'org-clock-in-hook #'org-tempus--update-session-start)
      (add-hook 'org-clock-in-hook #'org-tempus--update-mode-line t)
      (add-hook 'org-clock-out-hook #'org-tempus--update-mode-line)
      (when org-tempus-add-to-global-mode-string
        (or global-mode-string (setq global-mode-string '("")))
        (or (memq org-tempus--mode-line-format global-mode-string)
              (setq global-mode-string
                    (append global-mode-string (list org-tempus--mode-line-format)))))
        (setq org-clock-clocked-in-display nil)
        (when (org-clock-is-active)
          (org-tempus--update-session-start))
        (org-tempus--update-mode-line))

    (when org-tempus-add-to-global-mode-string
      (or global-mode-string (setq global-mode-string '("")))
      (setq global-mode-string
            (remove org-tempus--mode-line-format global-mode-string))
      (force-mode-line-update))
    (when (timerp org-tempus--timer)
      (cancel-timer org-tempus--timer))
    (setq org-tempus--timer nil)
    (remove-hook 'org-clock-in-hook #'org-tempus--update-session-start)
    (remove-hook 'org-clock-in-hook #'org-tempus--update-mode-line)
    (remove-hook 'org-clock-out-hook #'org-tempus--update-mode-line)))

(provide 'org-tempus)
;;; org-tempus.el ends here
