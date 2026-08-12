# org-tempus

[![MELPA](https://melpa.org/packages/org-tempus-badge.svg)](https://melpa.org/#/org-tempus)
[![MELPA Stable](https://stable.melpa.org/packages/org-tempus-badge.svg)](https://stable.melpa.org/#/org-tempus)

`org-tempus` is an Emacs package to enhance Org's time tracking
features by setting thresholds for session and daily clocked time,
showing them in the mode line, and sending notifications when they are
reached. It optionally auto clocks in to or out of your tasks, and
adjusts timestamps accordingly.

<img src="https://raw.githubusercontent.com/rul/org-tempus/assets/screenshots/org-tempus-demo-captioned.gif" alt="The mode line through a work session: the session and effort thresholds turning red, an auto clock-out after 4 minutes idle, and an auto clock-in on return">

## Features
- Thresholds for session and total daily clocked time.
- Mode line integration.
- Auto clocks in to a default task.
- Auto clocks out when idle.
- Supports different sources for detecting idleness: Emacs itself,
  Mutter (for GNOME), and freedesktop.org ScreenSaver.
- Sends notifications when the session threshold is reached, when the
  daily total is reached, and when activity is detected while no task
  is clocked in.
- Optional integration with dconf. This is useful to display
  `org-tempus` values in GNOME's panel, for example. It can be used
  with extensions such as
  [simple-message](https://extensions.gnome.org/extension/5018/simple-message/).

## Example workflow
- I start using my computer. `org-tempus` detects that the computer is
  not idle and auto clocks in to a default task.
- After spending 15 minutes organizing my work, answering emails,
  etc., I manually clock in to another task.
- After working 15 more minutes in this task, my session reaches a 30
  minutes threshold, so `org-tempus` sends a notification to take a
  break. I stop using my computer and go for a short walk.
- After 4 minutes of inactivity, `org-tempus` auto clocks out of the
  task, and adjusts the timestamp accordingly so it doesn't include the
  idle time.
- Some minutes later I come back to my computer. `org-tempus` detects
  that my break was short, so it auto clocks me in to the last task,
  instead of the default.
- (I repeat this pattern throughout the day)
- After reaching 5 hours of focused work, `org-tempus` sends a
  notification to wrap up my tasks.

With this workflow, I only have to manually clock in to the tasks I
want to work on. I no longer need to clock out, or adjust timestamps
manually if I forget to clock out when I stop using my computer.

All thresholds are configurable. By default, `org-tempus` suggests
work sessions of 30 minutes, and 5 hours of total work.

## Installation

`org-tempus` is available on [MELPA](https://melpa.org/#/org-tempus):

```
M-x package-install RET org-tempus RET
```

Or with `use-package`:

```lisp
(use-package org-tempus
  :ensure t
  :init
  (org-tempus-mode 1))
```

If you don't have MELPA in your archives yet:

```lisp
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
```

To follow the development version instead of a release:

```lisp
(package-vc-install "https://github.com/rul/org-tempus.git")
```

## Configuration

`org-tempus` has many knobs. They are all documented, and can be
checked out with `M-x customize-group` or `M-x apropos-variable`.

These are the most relevant:
- `org-tempus-session-threshold-minutes`: continuous session notification threshold.
- `org-tempus-total-threshold-minutes`: total daily notification threshold.
- `org-tempus-auto-clock-enabled`: master switch for auto clock in/out. It's off by default.
- `org-tempus-auto-clock-default-task-id`: Org ID of the default task
  for auto clock-in. You can generate one by running
  `org-id-get-create` on a heading.
- `org-tempus-idle-provider`: idle source (`emacs`, `mutter`, `freedesktop-screensaver`).
- `org-tempus-notifications-enabled`: enable/disable notifications.
- `org-tempus-dconf-path`: write mode line string to dconf (optional GNOME integration).

An example minimal configuration could look like this:

```lisp
(setq
 org-tempus-auto-clock-enabled t
 org-tempus-auto-clock-default-task-id "6fc9cfbc-0cf6-4c3f-87a9-cc49a7b6ea7b"
 org-tempus-idle-provider 'mutter
 org-tempus-dconf-path "/org/gnome/shell/extensions/simple-message/message"
 )
```

## Commands

- `org-tempus-reset-session`: start the session (S) over from now,
  without touching the clock. The daily total (T) is unaffected. It
  also clears the notification streak, so the next session and idle
  notifications are sent again. Useful after a break `org-tempus`
  didn't notice, such as a meeting away from the computer while a task
  stayed clocked in.
- `org-tempus-toggle-legend`: show or hide the `S`, `T` and `B` labels.
- `org-tempus-toggle-notifications`: enable or disable notifications.
- `org-tempus-toggle-auto-clock`: enable or disable auto clock in/out.

## Mode line

While a task is clocked in, the mode line shows the session time (S),
the day's total (T), the task's clocked time (against its effort, when
one is set), and its heading:

```
⏳[S 0:20 | T 3:20] <0:47/1:00> Write release notes
```

When no task is clocked in, it shows the day's total and the break
time (B):

```
⌛️[T 3:20 | B 0:12]
```

The session time and the task time turn red once the session threshold
and the task's effort are reached, as in the recording above.

The legend is a reminder of what each number means. Once you no longer
need it, `org-tempus-toggle-legend` drops the labels:

```
⏳[0:20|3:20] <0:47/1:00> Write release notes
```

By default `org-tempus` hides the stock Org mode line indicator and
replaces it with its own entry. This is recommended, but you can toggle
it off with `org-tempus-hide-org-mode-line-string`, and control whether
`org-tempus` is added to the global mode line at all with
`org-tempus-add-to-global-mode-string`.

## Notifications

Notifications are sent when the session threshold is reached, when the
total time threshold is reached, and when activity is detected while no
task is clocked in. They can be toggled off with
`org-tempus-toggle-notifications`.

<img src="https://raw.githubusercontent.com/rul/org-tempus/assets/screenshots/org-tempus-notifications.png" alt="Session threshold, total time threshold, and activity-without-a-clock notifications">

## GNOME panel

The optional dconf integration writes the mode line string where
GNOME's panel can pick it up, so you can see your timers even when you
aren't looking at Emacs.

<img src="https://raw.githubusercontent.com/rul/org-tempus/assets/screenshots/org-tempus-dconf-gnome.png" height="25" alt="GNOME panel integration">
