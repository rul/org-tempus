;;; org-tempus.el --- Enhance Org time tracking  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Raul Benencia
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Raul Benencia <id@rbenencia.name>
;; Maintainer: Raul Benencia <id@rbenencia.name>
;; URL: https://github.com/rul/org-tempus
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: calendar

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
;; Org Tempus augments Org's native time tracking capabilities by
;; setting customizable thresholds for a session, and computing the
;; total clocked time in a day.  The package will optionally send
;; notifications when thresholds are reached.  It can also detect
;; activity and auto clock in to a default task, or auto clock out
;; when idle.  The idle detection mechanism supports multiple sources,
;; such as Emacs itself, Mutter (for GNOME), and freedesktop.org
;; ScreenSaver.  Additionally, all the logged time and thresholds are
;; displayed in the mode line.  This information can optionally be
;; sent somewhere else (such as dconf, to integrate with a GNOME
;; desktop environment, for example).

;;; Code:
(require 'org-clock)
(require 'org-id)

(defgroup org-tempus ()
  "Display mode line indicator with information about clocked time."
  :group 'org)

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

(defcustom org-tempus-debug nil
  "When non-nil, log Org Tempus decisions and events.
Debug logs are appended to the *Org-Tempus-Debug* buffer."
  :type 'boolean
  :group 'org-tempus)

(defvar org-tempus-mode nil
  "Non-nil when Org Tempus mode is enabled.")

(defvar org-tempus--timer nil
  "Timer used to refresh the Org Tempus mode line.")

(defvar org-tempus--idle-timer nil
  "Timer used to check session idle activity.")

(defvar org-tempus--last-idle-check-time nil
  "Time of the last awake baseline used to detect a suspend gap.")

(defvar org-tempus--notification-reset-timer nil
  "Timer used to reset notification streaks.")

(defcustom org-tempus-idle-check-interval 60
  "Seconds between idle checks for out-of-clock activity."
  :type 'integer
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p org-tempus-mode)
           (org-tempus--start-timers)))
  :group 'org-tempus)

(defcustom org-tempus-notification-reset-seconds 3600
  "Seconds after which notification streaks reset."
  :type 'integer
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p org-tempus-mode)
           (org-tempus--start-timers)))
  :group 'org-tempus)

(defvar org-tempus--last-dconf-value nil
  "Last string posted to dconf, to avoid redundant updates.")

(defvar org-tempus--notification-count 0
  "Notifications sent in the current streak.")

(defvar org-tempus--notification-last-time nil
  "Time of the last notification sent, or nil.")

(declare-function notifications-notify "notifications" (&rest args))

(defun org-tempus--debug (format-string &rest args)
  "Log a debug message when `org-tempus-debug' is non-nil.
FORMAT-STRING and ARGS follow `format'."
  (when org-tempus-debug
    (with-current-buffer (get-buffer-create "*Org-Tempus-Debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%F %T] "))
      (insert (apply #'format format-string args))
      (insert "\n"))))

(defcustom org-tempus-dconf-path nil
  "When non-nil, post the Org Tempus mode line string to this dconf path.
The value is a string like:
\"/org/gnome/shell/extensions/simple-message/message\"."
  :type '(choice (const :tag "Disabled" nil) string)
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq org-tempus--last-dconf-value nil)
         (when (bound-and-true-p org-tempus-mode)
           (org-tempus--update-mode-line)))
  :group 'org-tempus)

(defcustom org-tempus-session-gap-seconds 60
  "Maximum gap in seconds to consider consecutive clocks the same session."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-session-starts-on-activity t
  "When non-nil, start session tracking as soon as activity is detected.
When nil, start session tracking on clock-in."
  :type 'boolean
  :group 'org-tempus)

(defcustom org-tempus-session-threshold-minutes 30
  "Minutes of continuous session after which a notification is sent."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-total-threshold-minutes 300
  "Minutes clocked today after which a notification is sent."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-notification-cooldown-seconds 600
  "Minimum seconds between break and idle notifications."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-notifications-enabled t
  "When non-nil, send notifications."
  :type 'boolean
  :group 'org-tempus)

(defcustom org-tempus-notification-max-count 3
  "Maximum number of break or idle notifications."
  :type 'integer
  :group 'org-tempus)

(defun org-tempus--set-idle-check-baseline (&optional time)
  "Record an awake suspend-detection baseline at TIME.
Clear the baseline when idle polling is disabled, since without it
nothing refreshes the baseline and every gap looks like a suspend."
  (setq org-tempus--last-idle-check-time
        (and (> org-tempus-idle-check-interval 0)
             (or time (current-time)))))

(defun org-tempus--stop-timers ()
  "Stop Org Tempus timers."
  (when (timerp org-tempus--timer)
    (cancel-timer org-tempus--timer))
  (setq org-tempus--timer nil)
  (when (timerp org-tempus--idle-timer)
    (cancel-timer org-tempus--idle-timer))
  (setq org-tempus--idle-timer nil)
  (when (timerp org-tempus--notification-reset-timer)
    (cancel-timer org-tempus--notification-reset-timer))
  (setq org-tempus--notification-reset-timer nil)
  (setq org-tempus--last-idle-check-time nil))

(defun org-tempus--start-timers ()
  "Start Org Tempus timers."
  (org-tempus--stop-timers)
  (setq org-tempus--timer
        (run-at-time org-tempus-update-interval
                     org-tempus-update-interval
                     #'org-tempus--update-mode-line))
  (when (> org-tempus-idle-check-interval 0)
    (org-tempus--set-idle-check-baseline)
    (setq org-tempus--idle-timer
          (run-at-time org-tempus-idle-check-interval
                       org-tempus-idle-check-interval
                       #'org-tempus--handle-idle)))
  (when (> org-tempus-notification-reset-seconds 0)
    (setq org-tempus--notification-reset-timer
          (run-at-time org-tempus-notification-reset-seconds
                       org-tempus-notification-reset-seconds
                       #'org-tempus--reset-notification-state))))

(defcustom org-tempus-break-threshold-seconds 10800
  "Maximum break seconds to display when no task is clocked in."
  :type 'integer
  :group 'org-tempus)

(defvar org-tempus--auto-clock-out-time nil
  "Time when Org Tempus last auto clocked out.")

(defcustom org-tempus-idle-active-streak-seconds 120
  "Seconds of continuous activity before notifying to clock-in."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-auto-clock-enabled nil
  "When non-nil, allow Org Tempus auto clock in/out."
  :type 'boolean
  :group 'org-tempus)

(defcustom org-tempus-auto-clock-out-seconds 240
  "Idle seconds after which to auto clock out.
Set to 0 to disable auto clock-out."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-auto-clock-out-backdate t
  "Whether to back-date auto clock-out by the idle duration."
  :type 'boolean
  :group 'org-tempus)

(defcustom org-tempus-auto-clock-in-last t
  "When non-nil, auto clock in to the last task after activity."
  :type 'boolean
  :group 'org-tempus)

(defcustom org-tempus-auto-clock-in-window-minutes 120
  "Minutes after auto clock-out during which auto clock-in to old task is allowed."
  :type 'integer
  :group 'org-tempus)

(defcustom org-tempus-auto-clock-default-task-id nil
  "Org ID of the default task used for auto clock-in.
You can set this by running `org-id-get-create' on a heading and
assigning the resulting ID to this variable."
  :type '(choice (const :tag "None" nil) string)
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

(defcustom org-tempus-task-name-max-length 20
  "Maximum width of the task name in the mode line.
Longer names are truncated with an ellipsis.  Set to nil to never
truncate."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'org-tempus)

(defcustom org-tempus-show-legend t
  "When non-nil, show legend labels (S, T, B) in the mode line."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p org-tempus-mode)
           (org-tempus--update-mode-line)))
  :group 'org-tempus)

(defvar org-tempus-mode-line-string ""
  "Org Tempus mode line indicator.")

(defconst org-tempus--mode-line-format '(:eval org-tempus-mode-line-string)
  "Mode line construct used by Org Tempus.")

(defvar org-tempus--saved-org-mode-line-string-present nil
  "Non-nil when `org-mode-line-string' was in `global-mode-string'.")

(defvar org-tempus--saved-org-clock-clocked-in-display nil
  "Saved value of `org-clock-clocked-in-display' while Org Tempus is enabled.")

(defvar org-tempus--idle-active-streak 0
  "Accumulated seconds of continuous active user presence.")

(defvar org-tempus--session-start-time nil
  "Internal session start time as a value returned by `current-time'.")

(defvar org-tempus--total-threshold-notified-date nil
  "Date string of last total threshold notification.")

(defvar org-tempus--notification-id nil
  "Notification id used to replace existing notifications.")

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
  "Hide Org's mode line string when Org Tempus is active.
The `org-tempus-mode' guard matters: `org-tempus--disable' calls
`org-clock-update-mode-line' while this advice is still installed."
  (when org-tempus-mode
    (org-tempus--hide-org-mode-line)))

(defun org-tempus--notify (msg)
  "Notify user with MSG using desktop notifications when available."
  (when org-tempus-notifications-enabled
    (org-tempus--debug "Notify: %s (count %s/%s)"
                       msg
                       org-tempus--notification-count
                       org-tempus-notification-max-count)
    (let ((sent nil))
      (when (or (fboundp 'notifications-notify)
                (require 'notifications nil t))
        (setq sent
              (condition-case err
                  (progn
                    (setq org-tempus--notification-id
                          (notifications-notify
                           :title "Org Tempus"
                           :body msg
                           :timeout 5000
                           :replaces-id org-tempus--notification-id))
                    t)
                (error
                 (message "Org Tempus notification error: %s" err)
                 nil))))
      (unless sent
        (message "%s" msg)))))

(defun org-tempus--current-task-name ()
  "Return unpropertized name of current task, trimmed when too long.
See `org-tempus-task-name-max-length'."
  (let ((name (substring-no-properties org-clock-current-task)))
    (if org-tempus-task-name-max-length
        (truncate-string-to-width name org-tempus-task-name-max-length
                                  nil nil t)
      name)))

(defun org-tempus--sum-today-minutes ()
  "Return total time clocked today across agenda files in minutes."
  (let* ((range (org-clock-special-range 'today nil t))
         (tstart (nth 0 range))
         (tend (nth 1 range))
         (total 0))
    (dolist (file (org-agenda-files t) total)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (setq total (+ total (org-clock-sum tstart tend))))))
    total))

(defun org-tempus--current-task-effort-minutes ()
  "Return effort for current task in minutes, or nil."
  (let ((marker (and (org-clock-is-active) org-clock-marker)))
    (when (and marker (marker-buffer marker))
      (org-with-point-at marker
        (let ((effort (org-entry-get nil "EFFORT")))
          (when effort
            (org-duration-to-minutes effort)))))))

(defun org-tempus-reset-session ()
  "Reset the current session timer."
  (interactive)
  (org-tempus--reset-notification-state)
  (if (org-clock-is-active)
      (setq org-tempus--session-start-time (current-time))
    (setq org-tempus--session-start-time nil))
  (org-tempus--update-mode-line)
  (message "Org Tempus session reset."))

(defun org-tempus-toggle-legend ()
  "Toggle legend labels in the mode line."
  (interactive)
  (setq org-tempus-show-legend (not org-tempus-show-legend))
  (org-tempus--update-mode-line)
  (message "Org Tempus legend %s."
           (if org-tempus-show-legend "enabled" "disabled")))

(defun org-tempus-toggle-notifications ()
  "Toggle Org Tempus notifications."
  (interactive)
  (setq org-tempus-notifications-enabled (not org-tempus-notifications-enabled))
  (message "Org Tempus notifications %s."
           (if org-tempus-notifications-enabled "enabled" "disabled")))

(defun org-tempus-toggle-auto-clock ()
  "Toggle Org Tempus auto clock in/out."
  (interactive)
  (setq org-tempus-auto-clock-enabled (not org-tempus-auto-clock-enabled))
  (unless org-tempus-auto-clock-enabled
    (org-tempus--reset-auto-clock-state))
  (message "Org Tempus auto clock %s."
           (if org-tempus-auto-clock-enabled "enabled" "disabled")))

(defun org-tempus--reset-notification-state ()
  "Reset notification state."
  (setq org-tempus--notification-count 0)
  (setq org-tempus--notification-last-time nil)
  (setq org-tempus--idle-active-streak 0))

(defun org-tempus--reset-auto-clock-state ()
  "Reset auto clock-in/out related state."
  (setq org-tempus--auto-clock-out-time nil)
  (org-tempus--reset-notification-state))


(defun org-tempus--notification-allowed-p ()
  "Return non-nil when a notification can be sent."
  (let ((last org-tempus--notification-last-time))
    (and (> org-tempus-notification-max-count 0)
         (< org-tempus--notification-count org-tempus-notification-max-count)
         (or (not last)
             (>= (float-time (time-subtract (current-time) last))
                 org-tempus-notification-cooldown-seconds)))))

(defun org-tempus--record-notification ()
  "Record a notification."
  (setq org-tempus--notification-count (1+ org-tempus--notification-count))
  (setq org-tempus--notification-last-time (current-time)))

(defun org-tempus--update-session-start ()
  "Update session start time.  Keep a short task change within the same session."
  (when org-clock-start-time
    (org-tempus--set-idle-check-baseline)
    (org-tempus--reset-auto-clock-state)
    (let* ((last-out org-clock-out-time)
           (gap (and last-out
                     (float-time (time-subtract org-clock-start-time last-out)))))
      (setq org-tempus--session-start-time
            (cond
             ((and org-tempus-session-starts-on-activity
                   org-tempus--session-start-time
                   (not last-out))
              org-tempus--session-start-time)
             ((and gap
                   (>= gap 0)
                   (<= gap org-tempus-session-gap-seconds))
              (or org-tempus--session-start-time org-clock-start-time))
             (t
              (org-tempus--reset-notification-state)
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
  (when (and (>= session-seconds (* 60 org-tempus-session-threshold-minutes))
             (org-tempus--notification-allowed-p))
    (org-tempus--record-notification)
    (let ((msg (format "Org Tempus session reached %s"
                       (org-duration-from-minutes
                        (/ session-seconds 60.0)))))
      (org-tempus--debug "Notify session threshold: %s" msg)
      (org-tempus--notify msg))))

(defun org-tempus--maybe-notify-total-threshold (total-seconds)
  "Send a one-time notification when TOTAL-SECONDS crosses threshold."
  (let ((today (format-time-string "%F")))
    (when (and (> org-tempus-total-threshold-minutes 0)
               (>= total-seconds (* 60 org-tempus-total-threshold-minutes))
               (not (equal org-tempus--total-threshold-notified-date today))
               (org-tempus--notification-allowed-p))
      (setq org-tempus--total-threshold-notified-date today)
      (org-tempus--record-notification)
      (let ((msg (format "Org Tempus total reached %s today"
                         (org-duration-from-minutes
                          (/ total-seconds 60.0)))))
        (org-tempus--debug "Notify total threshold: %s" msg)
        (org-tempus--notify msg)))))

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

(defun org-tempus--idle-seconds-from-dbus (service path interface method)
  "Return idle seconds from METHOD on SERVICE, PATH, and INTERFACE.
METHOD is expected to answer in milliseconds.  Return nil when the
session bus, the service, or the method is unavailable."
  (when (require 'dbus nil t)
    (condition-case _err
        (let ((idle-ms (dbus-call-method
                        :session service path interface method)))
          (when (numberp idle-ms)
            (/ idle-ms 1000.0)))
      (error nil))))

(defun org-tempus--session-idle-seconds ()
  "Return session idle time in seconds or nil when unavailable."
  (pcase org-tempus-idle-provider
    ('emacs (float-time (or (current-idle-time) 0)))
    ('mutter (org-tempus--idle-seconds-from-dbus
              "org.gnome.Mutter.IdleMonitor"
              "/org/gnome/Mutter/IdleMonitor/Core"
              "org.gnome.Mutter.IdleMonitor"
              "GetIdletime"))
    ('freedesktop-screensaver
     (org-tempus--idle-seconds-from-dbus
      "org.freedesktop.ScreenSaver"
      "/org/freedesktop/ScreenSaver"
      "org.freedesktop.ScreenSaver"
      "GetSessionIdleTime"))
    (_ nil)))

(defun org-tempus--suspend-gap-seconds (&optional now)
  "Return a pending suspected suspend gap in seconds, or nil.
NOW defaults to `current-time'.  A normal scheduled idle poll is not
enough evidence of suspension; at least one expected poll must have
been missed."
  (let* ((now (or now (current-time)))
         (last-check org-tempus--last-idle-check-time)
         (interval org-tempus-idle-check-interval)
         (since-last
          (and last-check
               (float-time (time-subtract now last-check)))))
    (when (and since-last
               (> interval 0)
               (> since-last (* 2 interval))
               org-tempus-auto-clock-enabled
               (org-clock-is-active)
               (> org-tempus-auto-clock-out-seconds 0)
               (>= since-last org-tempus-auto-clock-out-seconds))
      since-last)))

(defun org-tempus--maybe-auto-clock-out-after-suspend (&optional now)
  "Auto clock out a running clock when NOW follows a suspend gap.
Return non-nil when a clock is stopped."
  (let* ((now (or now (current-time)))
         (last-check org-tempus--last-idle-check-time)
         (since-last (org-tempus--suspend-gap-seconds now)))
    (when since-last
      (org-tempus--debug "Auto clock-out after suspend gap: %.1fs" since-last)
      ;; Update this before `org-clock-out' runs hooks that refresh the mode line.
      (setq org-tempus--last-idle-check-time now)
      (setq org-tempus--auto-clock-out-time now)
      (if org-tempus-auto-clock-out-backdate
          (org-clock-out nil t last-check)
        (org-clock-out nil t))
      (setq org-tempus--session-start-time nil)
      (org-tempus--reset-notification-state)
      (org-tempus--notify
       (format "Auto clocked out after %s away."
               (org-duration-from-minutes
                (/ since-last 60.0))))
      t)))

(defun org-tempus--clock-out-advice (oldfun &optional switch-to-state fail-quietly at-time)
  "Repair a pending suspend gap before OLDFUN writes a clock-out entry.
SWITCH-TO-STATE, FAIL-QUIETLY, and AT-TIME are the arguments of
`org-clock-out'.  An explicit AT-TIME is always preserved."
  (let* ((now (current-time))
         (since-last (and (not at-time)
                          (org-tempus--suspend-gap-seconds now)))
         (corrected-at-time
          (if (and since-last org-tempus-auto-clock-out-backdate)
              org-tempus--last-idle-check-time
            at-time)))
    (when since-last
      (org-tempus--debug "Repair clock-out after suspend gap: %.1fs" since-last)
      (setq org-tempus--last-idle-check-time now)
      (setq org-tempus--session-start-time nil))
    (prog1
        (funcall oldfun switch-to-state fail-quietly corrected-at-time)
      (org-tempus--set-idle-check-baseline))))

(defun org-tempus--handle-idle ()
  "Handle idle checking, including auto clock-out and notifications."
  (let* ((idle-seconds (org-tempus--session-idle-seconds))
         (now (current-time))
         (last-check org-tempus--last-idle-check-time)
         (since-last (and last-check
                          (float-time (time-subtract now last-check)))))
    (when (and (numberp since-last)
               (> since-last (* 2 org-tempus-idle-check-interval)))
      (org-tempus--debug "Reset activity streak after gap: %.1fs" since-last)
      (setq org-tempus--idle-active-streak 0))
    (when (org-tempus--maybe-auto-clock-out-after-suspend now)
      (org-tempus--update-mode-line))
    (org-tempus--set-idle-check-baseline now)
    (when idle-seconds
      (let* ((active (< idle-seconds org-tempus-idle-check-interval))
             (start-time (time-subtract (current-time)
                                        (seconds-to-time
                                         (min idle-seconds
                                              org-tempus-idle-check-interval))))
             (auto-clocked-in nil))
        (when (and active (not (org-clock-is-active)))
          (let ((since-out (and org-clock-out-time
                                (float-time (time-subtract (current-time)
                                                           org-clock-out-time)))))
            (when (and since-out
                       (< since-out org-tempus-idle-active-streak-seconds))
              (org-tempus--debug
               "Skip auto clock-in: recent clock-out %.1fs ago" since-out)
              (setq active nil)))
          (when (and org-tempus-session-starts-on-activity
                     (not org-tempus--session-start-time))
            (setq org-tempus--session-start-time start-time))
          (org-tempus--debug "Active detected (idle %.1fs), attempting auto clock-in"
                             idle-seconds)
          (when active
            (setq auto-clocked-in
                  (or (org-tempus--maybe-auto-clock-in start-time)
                      (org-tempus--maybe-auto-clock-in-default start-time)))))
        (when (and org-tempus-auto-clock-enabled
                   (org-clock-is-active)
                   (> org-tempus-auto-clock-out-seconds 0)
                   (>= idle-seconds org-tempus-auto-clock-out-seconds))
          (org-tempus--debug "Auto clock-out after idle: %.1fs" idle-seconds)
          (setq org-tempus--auto-clock-out-time (current-time))
          (if org-tempus-auto-clock-out-backdate
              (org-clock-out nil t (time-subtract (current-time)
                                                  (seconds-to-time idle-seconds)))
            (org-clock-out nil t))
          (setq org-tempus--session-start-time nil)
          (org-tempus--reset-notification-state)
          (org-tempus--update-mode-line)
          (org-tempus--notify
           (format "Auto clocked out after %s idle."
                   (org-duration-from-minutes
                    (/ idle-seconds 60.0)))))
        (if (and active (not (org-clock-is-active)))
            (when (or (not (numberp since-last))
                      (and (>= since-last org-tempus-idle-check-interval)
                           (<= since-last (* 2 org-tempus-idle-check-interval))))
              (setq org-tempus--idle-active-streak
                    (+ org-tempus--idle-active-streak org-tempus-idle-check-interval)))
          (setq org-tempus--idle-active-streak 0))
        (when (and (not auto-clocked-in)
                   (>= org-tempus--idle-active-streak
                       org-tempus-idle-active-streak-seconds)
                   (not (org-clock-is-active)))
          (when (org-tempus--notification-allowed-p)
            (org-tempus--record-notification)
            (let ((msg "You seem active but no task is clocked in."))
              (org-tempus--debug "Notify idle: %s" msg)
              (org-tempus--notify msg))))))))

(defun org-tempus--clock-in-last (start-time)
  "Clock in to the last task using START-TIME.
Return non-nil when clock-in succeeds."
  (let ((marker (car org-clock-history)))
    (when (and marker (marker-buffer marker))
      (org-clock-clock-in (list marker) nil start-time)
      t)))

(defun org-tempus--auto-clock-in (clock-in-fn start-time msg)
  "Auto clock in using CLOCK-IN-FN at START-TIME and notify with MSG."
  (when (funcall clock-in-fn start-time)
    (org-tempus--debug "%s" msg)
    (org-tempus--reset-auto-clock-state)
    (org-tempus--update-mode-line)
    (org-tempus--notify msg)
    t))

(defun org-tempus--auto-clock-in-message (base)
  "Return notification message using BASE with optional break time."
  (let ((break-seconds (org-tempus--current-break-duration)))
    (if (numberp break-seconds)
        (format "%s after %s break"
                base
                (org-duration-from-minutes (/ break-seconds 60.0)))
      base)))

(defun org-tempus--maybe-auto-clock-in (&optional start-time)
  "Auto clock in to the last task if eligible.
When START-TIME is non-nil, use it as the clock-in time.
Return non-nil when an auto clock-in occurs."
  (when (and org-tempus-auto-clock-enabled
             org-tempus-auto-clock-in-last
             org-tempus--auto-clock-out-time
             (not (org-clock-is-active)))
    (let ((since (float-time (time-subtract (current-time)
                                            org-tempus--auto-clock-out-time))))
      (if (<= since (* 60 org-tempus-auto-clock-in-window-minutes))
          (org-tempus--auto-clock-in
           #'org-tempus--clock-in-last
           start-time
           (org-tempus--auto-clock-in-message
            "Auto clocked in to your last task"))
        (org-tempus--debug "Skipping auto clock-in (last task): window expired (%.1fs)"
                           since)))))

(defun org-tempus--clock-in-default (start-time)
  "Clock in to the default task using START-TIME.
Return non-nil when clock-in succeeds."
  (let ((marker (org-id-find org-tempus-auto-clock-default-task-id 'marker)))
    (when (and marker (marker-buffer marker))
      (org-with-point-at marker
        (org-clock-in nil start-time))
      t)))

(defun org-tempus--maybe-auto-clock-in-default (&optional start-time)
  "Auto clock in to the default task if eligible.
When START-TIME is non-nil, use it as the clock-in time.
Return non-nil when an auto clock-in occurs."
  (when (and org-tempus-auto-clock-enabled
             org-tempus-auto-clock-default-task-id
             (not (org-clock-is-active)))
    (if (org-id-find org-tempus-auto-clock-default-task-id 'marker)
        (org-tempus--auto-clock-in
         #'org-tempus--clock-in-default
         start-time
         (org-tempus--auto-clock-in-message
          "Auto clocked in to your default task"))
      (org-tempus--debug "Skipping auto clock-in (default): Org ID not found"))))

(defun org-tempus--maybe-update-dconf (&optional value)
  "Update dconf with VALUE when `org-tempus-dconf-path' is set."
  (when (and org-tempus-dconf-path
             (> (length org-tempus-dconf-path) 0)
             (executable-find "dconf"))
    (let ((value (or value "")))
      (when (not (equal value org-tempus--last-dconf-value))
        (setq org-tempus--last-dconf-value value)
        ;; Elisp print syntax doubles as a GVariant double-quoted string.
        (call-process "dconf" nil 0 nil
                      "write" org-tempus-dconf-path
                      (prin1-to-string value))))))

(defun org-tempus--update-mode-line ()
  "Update the Org Tempus mode line indicator."
  (org-tempus--maybe-auto-clock-out-after-suspend)
  (let* ((total-minutes (org-tempus--sum-today-minutes))
         (total-seconds (* 60 total-minutes))
         (total-threshold-hit (and (> org-tempus-total-threshold-minutes 0)
                                   (>= total-seconds
                                       (* 60 org-tempus-total-threshold-minutes))))
         (total-str (org-duration-from-minutes total-minutes))
         (total-display (if total-threshold-hit
                            (propertize total-str 'face 'error)
                          total-str))
         (raw (if (org-clock-is-active)
                  (let* ((session-seconds (org-tempus--current-session-duration))
                         (session (org-duration-from-minutes
                                   (/ session-seconds 60.0)))
                         (session-str (if (>= session-seconds
                                              (* 60 org-tempus-session-threshold-minutes))
                                         (propertize session 'face 'error)
                                       session)))
                    (org-tempus--maybe-notify-session-threshold session-seconds)
                    (concat "⏳["
                            (org-tempus--format-mode-line-item "S" session-str)
                            (org-tempus--mode-line-separator)
                            (org-tempus--format-mode-line-item
                             "T" total-display)
                            "] "
                            (let* ((task-minutes (org-clock-get-clocked-time))
                                   (task-time (org-duration-from-minutes task-minutes))
                                   (effort-minutes (org-tempus--current-task-effort-minutes))
                                   (effort (when effort-minutes
                                             (org-duration-from-minutes effort-minutes)))
                                   (task-display (if (and effort-minutes
                                                          (>= task-minutes effort-minutes))
                                                     (propertize task-time 'face 'error)
                                                   task-time)))
                              (concat "<" task-display
                                      (if effort
                                          (concat "/" effort)
                                        "")
                                      "> "))
                            (org-tempus--current-task-name)))
                (let* ((break-seconds (org-tempus--current-break-duration))
                       (break-str (when break-seconds
                                    (org-duration-from-minutes
                                     (/ break-seconds 60.0)))))
                  (concat "⌛️["
                          (org-tempus--format-mode-line-item
                           "T" total-display)
                          (if break-str
                              (concat (org-tempus--mode-line-separator)
                                      (org-tempus--format-mode-line-item
                                       "B" break-str))
                            "")
                          "]"))))
         (str (propertize raw 'help-echo "Org Tempus")))
    (org-tempus--maybe-notify-total-threshold total-seconds)
    (setq org-tempus-mode-line-string str))
  (org-tempus--maybe-update-dconf
   (substring-no-properties org-tempus-mode-line-string))
  (force-mode-line-update t))

(defun org-tempus--disable ()
  "Disable org-tempus integrations and restore previous state."
  (when org-tempus-add-to-global-mode-string
    (or global-mode-string (setq global-mode-string '("")))
    (setq global-mode-string
          (remove org-tempus--mode-line-format global-mode-string))
    (force-mode-line-update))
  (setq org-clock-clocked-in-display org-tempus--saved-org-clock-clocked-in-display)
  (when org-tempus-hide-org-mode-line-string
    ;; No need to restore `org-mode-line-string': both branches below
    ;; overwrite it, via `org-clock-update-mode-line' or with "".
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
  (when org-tempus-dconf-path
    (setq org-tempus--last-dconf-value nil)
    (org-tempus--maybe-update-dconf ""))
  (org-tempus--stop-timers)
  (remove-hook 'org-clock-in-hook #'org-tempus--update-session-start)
  (remove-hook 'org-clock-in-hook #'org-tempus--reset-notification-state)
  (remove-hook 'org-clock-in-hook #'org-tempus--update-mode-line)
  (remove-hook 'org-clock-in-hook #'org-tempus--hide-org-mode-line)
  (remove-hook 'org-clock-out-hook #'org-tempus--hide-org-mode-line)
  (remove-hook 'org-clock-out-hook #'org-tempus--update-mode-line)
  (remove-hook 'org-clock-out-hook #'org-tempus--reset-notification-state)
  (advice-remove 'org-clock-out #'org-tempus--clock-out-advice)
  (advice-remove 'org-clock-update-mode-line #'org-tempus--maybe-hide-org-mode-line))

(defun org-tempus--enable ()
  "Enable org-tempus integrations."
  (org-tempus--reset-notification-state)
  (org-tempus--start-timers)
  (add-hook 'org-clock-in-hook #'org-tempus--update-session-start)
  (add-hook 'org-clock-in-hook #'org-tempus--reset-notification-state)
  (add-hook 'org-clock-in-hook #'org-tempus--update-mode-line t)
  (add-hook 'org-clock-in-hook #'org-tempus--hide-org-mode-line t)
  (add-hook 'org-clock-out-hook #'org-tempus--reset-notification-state)
  (add-hook 'org-clock-out-hook #'org-tempus--update-mode-line)
  (add-hook 'org-clock-out-hook #'org-tempus--hide-org-mode-line)
  (advice-add 'org-clock-out :around #'org-tempus--clock-out-advice)
  (advice-add 'org-clock-update-mode-line :after #'org-tempus--maybe-hide-org-mode-line)
  (when org-tempus-add-to-global-mode-string
    (or global-mode-string (setq global-mode-string '("")))
    (or (memq org-tempus--mode-line-format global-mode-string)
        (setq global-mode-string
              (append global-mode-string (list org-tempus--mode-line-format)))))
  (setq org-tempus--saved-org-clock-clocked-in-display org-clock-clocked-in-display)
  (setq org-clock-clocked-in-display nil)
  (when org-tempus-hide-org-mode-line-string
    (setq org-tempus--saved-org-mode-line-string-present nil)
    (org-tempus--hide-org-mode-line))
  (when (org-clock-is-active)
    (org-tempus--update-session-start))
  (org-tempus--update-mode-line))

(defun org-tempus-unload-function ()
  "Unload `org-tempus' cleanly."
  (org-tempus--disable)
  (setq org-tempus-mode nil)
  nil)

;;;###autoload
(define-minor-mode org-tempus-mode
  "Minor mode to enhance time tracking in ‘org-mode’."
  :lighter " Tempus"
  :global t
  (if org-tempus-mode
      (org-tempus--enable)
    (org-tempus--disable)))

(provide 'org-tempus)
;;; org-tempus.el ends here
