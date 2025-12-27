;;; org-tempus.el --- Tracks work sessions and breaks at a glance  -*- lexical-binding: t; -*-

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

(defcustom org-tempus-hide-org-mode-line-string t
  "When non-nil, hide the stock Org mode line indicator."
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

(defcustom org-tempus-break-threshold-seconds 10800
  "Maximum break seconds to display when no task is clocked in."
  :type 'integer
  :group 'org-tempus)

(defvar org-tempus--idle-timer nil
  "Timer used to check session idle activity.")

(defcustom org-tempus-idle-check-interval 10
  "Seconds between idle checks for out-of-clock activity."
  :type 'integer
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p org-tempus-mode)
           (when (timerp org-tempus--idle-timer)
             (cancel-timer org-tempus--idle-timer))
           (setq org-tempus--idle-timer nil)
           (when (> value 0)
             (setq org-tempus--idle-timer
                   (run-at-time value value #'org-tempus--maybe-notify-idle)))))
  :group 'org-tempus)

(defcustom org-tempus-idle-active-threshold-seconds 30
  "Maximum idle seconds to consider the user active."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-idle-active-streak-seconds 120
  "Seconds of continuous activity before notifying."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-idle-notify-cooldown-seconds 600
  "Minimum seconds between idle notifications."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-idle-provider 'emacs
  "Provider used to query idle time.
Known providers are `emacs' (activity inside Emacs),
`mutter' (GNOME Mutter IdleMonitor), and
`freedesktop-screensaver' (org.freedesktop.ScreenSaver)."
  :type '(choice (const emacs)
                 (const mutter)
                 (const freedesktop-screensaver))
  :group 'org-tempus)

(defcustom org-tempus-show-legend t
  "When non-nil, show legend labels (S, T, B) in the mode line."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p org-tempus-mode)
           (org-tempus--update-mode-line)))
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

(defvar org-tempus--saved-org-mode-line-string nil
  "Saved value of `org-mode-line-string' when `org-tempus-mode' is enabled.")

(defvar org-tempus--saved-org-mode-line-string-present nil
  "Non-nil when `org-mode-line-string' was in `global-mode-string'.")

(defvar org-tempus--saved-org-clock-clocked-in-display nil
  "Saved value of `org-clock-clocked-in-display' while Org Tempus is enabled.")

(defvar org-tempus--idle-active-streak 0
  "Accumulated seconds of continuous active user presence.")

(defvar org-tempus--last-idle-notify-time nil
  "Last time an idle notification was sent.")

(defvar org-tempus--session-start-time nil
  "Internal session start time as a value returned by `current-time'.")

(defvar org-tempus--session-threshold-notified nil
  "Internal flag indicating the session threshold notification was sent.")

(defun org-tempus--hide-org-mode-line ()
  "Hide the stock Org mode line indicator while Org Tempus is active."
  (when org-tempus-hide-org-mode-line-string
    (setq org-mode-line-string nil)
    (when (memq 'org-mode-line-string global-mode-string)
      (setq org-tempus--saved-org-mode-line-string-present t))
    (setq global-mode-string
          (delq 'org-mode-line-string global-mode-string))
    (force-mode-line-update)))

(defun org-tempus--maybe-hide-org-mode-line (&rest _)
  "Hide Org's mode line string when Org Tempus is active."
  (when (and org-tempus-mode org-tempus-hide-org-mode-line-string)
    (org-tempus--hide-org-mode-line)))

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
  "Update session start time.  Keep a short task change within the same session."
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

(defun org-tempus--current-break-duration ()
  "Return break duration in seconds, or nil when not applicable."
  (when (and (not (org-clock-is-active))
             org-clock-out-time)
    (let ((gap (float-time (time-subtract (current-time) org-clock-out-time))))
      (when (and (>= gap 0)
                 (<= gap org-tempus-break-threshold-seconds))
        (floor gap)))))

(defun org-tempus--format-mode-line-item (label value)
  "Format a mode line VALUE with optional LABEL."
  (if org-tempus-show-legend
      (concat label " " value)
    value))

(defun org-tempus--mode-line-separator ()
  "Return the separator used between mode line items."
  (if org-tempus-show-legend
      " | "
    "|"))

(defun org-tempus--idle-seconds-from-emacs ()
  "Return idle seconds based on Emacs input activity."
  (let ((idle (current-idle-time)))
    (if idle
        (float-time idle)
      0)))

(defun org-tempus--idle-seconds-from-mutter ()
  "Return idle seconds from GNOME Mutter's IdleMonitor."
  (when (require 'dbus nil t)
    (condition-case _err
        (let ((idle-ms (dbus-call-method
                        :session
                        "org.gnome.Mutter.IdleMonitor"
                        "/org/gnome/Mutter/IdleMonitor/Core"
                        "org.gnome.Mutter.IdleMonitor"
                        "GetIdletime")))
          (when (numberp idle-ms)
            (/ idle-ms 1000.0)))
      (error nil))))

(defun org-tempus--idle-seconds-from-freedesktop-screensaver ()
  "Return idle seconds from org.freedesktop.ScreenSaver."
  (when (require 'dbus nil t)
    (condition-case _err
        (let ((idle-ms (dbus-call-method
                        :session
                        "org.freedesktop.ScreenSaver"
                        "/org/freedesktop/ScreenSaver"
                        "org.freedesktop.ScreenSaver"
                        "GetSessionIdleTime")))
          (when (numberp idle-ms)
            (/ idle-ms 1000.0)))
      (error nil))))

(defun org-tempus--session-idle-seconds ()
  "Return session idle time in seconds or nil when unavailable."
  (pcase org-tempus-idle-provider
    ('emacs (org-tempus--idle-seconds-from-emacs))
    ('mutter (org-tempus--idle-seconds-from-mutter))
    ('freedesktop-screensaver
     (org-tempus--idle-seconds-from-freedesktop-screensaver))
    (_ nil)))

(defun org-tempus--maybe-notify-idle ()
  "Notify when user is active but no clock is running."
  (let ((idle-seconds (org-tempus--session-idle-seconds)))
    (when idle-seconds
      (if (< idle-seconds org-tempus-idle-active-threshold-seconds)
          (setq org-tempus--idle-active-streak
                (+ org-tempus--idle-active-streak org-tempus-idle-check-interval))
        (setq org-tempus--idle-active-streak 0))
      (when (and (>= org-tempus--idle-active-streak
                     org-tempus-idle-active-streak-seconds)
                 (not (org-clock-is-active)))
        (let* ((now (current-time))
               (since (and org-tempus--last-idle-notify-time
                           (float-time
                            (time-subtract now org-tempus--last-idle-notify-time)))))
          (when (or (not since)
                    (>= since org-tempus-idle-notify-cooldown-seconds))
            (setq org-tempus--last-idle-notify-time now)
            (org-tempus--notify
             "You seem active but no task is clocked in.")))))))

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
                    (concat "⏳["
                            (org-tempus--format-mode-line-item "S" session-str)
                            (org-tempus--mode-line-separator)
                            (org-tempus--format-mode-line-item
                             "T" (org-tempus--sum-today))
                            "] ("
                            (org-tempus--current-task-name)
                            " <" (org-tempus--current-task-time)
                            ">)"))
                (let* ((break-seconds (org-tempus--current-break-duration))
                       (break-str (when break-seconds
                                    (org-duration-from-minutes
                                     (/ break-seconds 60.0)))))
                  (concat "⌛️["
                          (org-tempus--format-mode-line-item
                           "T" (org-tempus--sum-today))
                          (if break-str
                              (concat (org-tempus--mode-line-separator)
                                      (org-tempus--format-mode-line-item
                                       "B" break-str))
                            "")
                          "]"))))
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
      (add-hook 'org-clock-in-hook #'org-tempus--hide-org-mode-line t)
      (add-hook 'org-clock-out-hook #'org-tempus--update-mode-line)
      (add-hook 'org-clock-out-hook #'org-tempus--hide-org-mode-line)
      (advice-add 'org-clock-update-mode-line :after #'org-tempus--maybe-hide-org-mode-line)
      (when (timerp org-tempus--idle-timer)
        (cancel-timer org-tempus--idle-timer))
      (setq org-tempus--idle-timer nil)
      (when (> org-tempus-idle-check-interval 0)
        (setq org-tempus--idle-timer
              (run-at-time org-tempus-idle-check-interval
                           org-tempus-idle-check-interval
                           #'org-tempus--maybe-notify-idle)))
      (when org-tempus-add-to-global-mode-string
        (or global-mode-string (setq global-mode-string '("")))
        (or (memq org-tempus--mode-line-format global-mode-string)
              (setq global-mode-string
                    (append global-mode-string (list org-tempus--mode-line-format)))))
        (setq org-tempus--saved-org-clock-clocked-in-display org-clock-clocked-in-display)
        (setq org-clock-clocked-in-display nil)
        (when org-tempus-hide-org-mode-line-string
          (setq org-tempus--saved-org-mode-line-string org-mode-line-string)
          (setq org-tempus--saved-org-mode-line-string-present nil)
          (org-tempus--hide-org-mode-line))
        (when (org-clock-is-active)
          (org-tempus--update-session-start))
        (org-tempus--update-mode-line))

    (when org-tempus-add-to-global-mode-string
      (or global-mode-string (setq global-mode-string '("")))
      (setq global-mode-string
            (remove org-tempus--mode-line-format global-mode-string))
      (force-mode-line-update))
    (setq org-clock-clocked-in-display org-tempus--saved-org-clock-clocked-in-display)
    (when org-tempus-hide-org-mode-line-string
      (setq org-mode-line-string org-tempus--saved-org-mode-line-string)
      (if (org-clock-is-active)
          (progn
            (when (or org-tempus--saved-org-mode-line-string-present
                      (memq org-clock-clocked-in-display '(mode-line both)))
              (or global-mode-string (setq global-mode-string '("")))
              (unless (memq 'org-mode-line-string global-mode-string)
                (setq global-mode-string
                      (append global-mode-string (list 'org-mode-line-string)))))
            (org-clock-update-mode-line))
        (setq global-mode-string
              (delq 'org-mode-line-string global-mode-string))
        (setq org-mode-line-string "")
        (force-mode-line-update)))
    (when (timerp org-tempus--timer)
      (cancel-timer org-tempus--timer))
    (setq org-tempus--timer nil)
    (when (timerp org-tempus--idle-timer)
      (cancel-timer org-tempus--idle-timer))
    (setq org-tempus--idle-timer nil)
    (remove-hook 'org-clock-in-hook #'org-tempus--update-session-start)
    (remove-hook 'org-clock-in-hook #'org-tempus--update-mode-line)
    (remove-hook 'org-clock-in-hook #'org-tempus--hide-org-mode-line)
    (remove-hook 'org-clock-out-hook #'org-tempus--hide-org-mode-line)
    (remove-hook 'org-clock-out-hook #'org-tempus--update-mode-line)
    (advice-remove 'org-clock-update-mode-line #'org-tempus--maybe-hide-org-mode-line)))

(provide 'org-tempus)
;;; org-tempus.el ends here
