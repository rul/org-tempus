# org-tempus

`org-tempus` is an Emacs package to enhance Org's time tracking
features by setting thresholds for session and daily clocked time, and
sending notifications when they are reached. It optionally auto clocks
in or out from your tasks, and adjusts timestamps accordingly.

## Features
- Thresholds for session and total daily clocked time.
- Mode line integration.
- Auto clocks in to a default task.
- Auto clocks out when idle.
- Supports different sources for detecting idleness: Emacs itself,
  Mutter (for GNOME), and freedesktop.org ScreenSaver.
- Sends notifications to take a break after the session threshold is
  reached.
- Sends notifications after a total daily time is reached.
- Sends notifications when activity is detected, but no task is
  clocked in.
- Optional integration with dconf. This is useful to display
  `org-tempus` values in GNOME's panel, for example.

## Example workflow
- I start using my computer. `org-tempus` detects that the computer is
  not idle and auto clocks in in a default task.
- After spending 15 minutes organizing my work, answering emails,
  etc., I manually clock in to another task.
- After working 15 more minutes in this task, my session reaches a 30
  minutes threshold, so `org-tempus` sends a notification to take a
  break. I stop using my computer and go for a short walk.
- After 4 minutes of inactivity, `org-tempus` auto clocks out from the
  task, and adjust the timestamp accordingly so it doesn't include the
  idle time.
- Some minutes later I come back to my computer. `org-tempus` detects
  that my break was short, so it auto clocks me in to the last task,
  instead of the default.
- (I repeat this pattern throughout the day)
- After reaching 5 hours of focused work, `org-tempus` sends a
  notification to wrap up my tasks.

All thresholds are configurable. By default, the `org-tempus` suggests
work sessions of 30 minutes, and 5 hours of total work.

## Installation

```lisp
(unless (package-installed-p 'org-tempus)
  (package-vc-install "https://github.com/rul/org-tempus.git"))

(use-package org-tempus
  :init
  (org-tempus-mode 1))
```

## Configuration

- `org-tempus-auto-clock-enabled`: master switch for auto clock in/out.
- `org-tempus-auto-clock-default-task-id`: Org ID of the default task for auto clock-in.
- `org-tempus-auto-clock-out-seconds`: idle time before auto clock-out.
- `org-tempus-auto-clock-in-window-minutes`: window after auto clock-out to allow auto clock-in.
- `org-tempus-idle-check-interval`: idle check cadence and activity threshold.
- `org-tempus-session-threshold-minutes`: continuous session notification threshold.
- `org-tempus-total-threshold-minutes`: total daily notification threshold.
- `org-tempus-notifications-enabled`: enable/disable notifications.
- `org-tempus-idle-provider`: idle source (`emacs`, `mutter`, `freedesktop-screensaver`).
- `org-tempus-dconf-path`: write modeline string to dconf (optional GNOME integration).

