;;; cae-lib.el --- Library for CAE Doom Emacs modules -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 StrawberryTea
;;
;; Author: StrawberryTea <look@strawberrytea.xyz>
;; Maintainer: StrawberryTea <look@strawberrytea.xyz>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: internal extensions
;; Homepage: https://github.com/LemonBreezes/cae-lib
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Library for CAE Doom Emacs modules
;;
;;; Code:

;;; lisp/cae-lib.el -*- lexical-binding: t; -*-
(defvar cae-config-finished-loading nil)

(defun cae-ignore-errors-a (fun &rest args)
  "Ignore errors in FUN with ARGS."
  (ignore-errors (apply fun args)))

(defun cae-display-graphic-p ()
  (or (display-graphic-p)
      (daemonp)))

(defun cae-tty-disable-unicode-p ()
  (not (cae-display-graphic-p)))

;; Lazy load keymaps from packages.
(defmacro cae-oneshot-keymap (keymap package)
  `(if (featurep ',package)
       (symbol-value ',keymap)
     (lambda () (interactive)
       (and ',package (require ',package))
       (let* ((once t)
              (timer
               (when (featurep 'which-key)
                 (run-with-idle-timer
                  which-key-idle-delay nil
                  (lambda ()
                    (when once
                      (let ((which-key-show-prefix t))
                        (which-key--show-keymap
                         (symbol-name ',keymap)
                         ,keymap
                         nil nil t))))))))
         (set-transient-map (symbol-value ',keymap)
                            (lambda ()
                              (prog1 once
                                (setq once nil)))
                            (lambda ()
                              (cancel-timer timer)))))))

;; A generic adviser for responding yes to yes or no prompts automatically.
(defun cae-always-yes-a (oldfun &rest args)
  (cl-letf (((symbol-function #'yes-or-no-p) (symbol-function #'always))
            ((symbol-function #'y-or-n-p) (symbol-function #'always)))
    (apply oldfun args)))

;; Add timers in an idempotent way.
(defvar cae--timers (make-hash-table :test #'equal)
  "Hash table storing active timers by unique name.")

(defun cae-run-with-idle-timer (seconds repeat name function &rest args)
  "Run an idle timer in an idempotent way.
SECONDS, REPEAT, FUNCTION, and ARGS are passed to `run-with-idle-timer'.
NAME is a unique identifier for the timer.
If a timer with NAME already exists, cancel it before creating a new one."
  (when-let ((existing (gethash name cae--timers)))
    (cancel-timer existing))
  (puthash name (apply #'run-with-idle-timer seconds repeat function args)
           cae--timers))

(defun cae-run-with-timer (seconds repeat name function &rest args)
  "Run a timer in an idempotent way.
SECONDS, REPEAT, FUNCTION, and ARGS are passed to `run-with-timer'.
NAME is a unique identifier for the timer.
If a timer with NAME already exists, cancel it before creating a new one."
  (when-let ((existing (gethash name cae--timers)))
    (cancel-timer existing))
  (puthash name (apply #'run-with-timer seconds repeat function args)
           cae--timers))

(defun cae-terminal-type ()
  (cond
   ;; If Emacs is running in a GUI, you have full Unicode/font support.
   ((cae-display-graphic-p)
    2)
   ((getenv "WT_SESSION")
    1)
   ;; Linux virtual console (tty). The TERM variable is usually "linux"
   ((string-prefix-p "/dev/tty" (terminal-name))
    0)
   ;; Otherwise, if LANG indicates UTF-8 you're probably in a modern terminal emulator.
   ((and (getenv "LANG") (string-match "utf8" (getenv "LANG")))
    1)
   (t 0)))

(defun cae-running-in-ssh-p ()
  "Return non-nil if Emacs is running within an SSH session."
  (or (getenv "SSH_CLIENT")
      (getenv "SSH_TTY")
      (getenv "SSH_CONNECTION")))

(defun cae-remove-cae-advices (symbol &optional new-advice-name)
  "Remove all advices with the 'cae-' prefix from function SYMBOL.
Only show messages when removing advice names different from NEW-ADVICE-NAME.
If NEW-ADVICE-NAME is provided, also print a message about the new advice being added."
  (let (cae-advices)
    ;; First, collect all the "cae-" prefixed advices
    (advice-mapc
     (lambda (advice-function properties)
       (let ((name (or (alist-get 'name properties) advice-function)))
         (when (and (symbolp name)
                    (string-prefix-p "cae-" (symbol-name name)))
           (push name cae-advices))))
     symbol)

    ;; Return the list of removed advices
    cae-advices))

(defmacro cae-defadvice! (symbol arglist &optional docstring &rest body)
  "Like `defadvice!` but removes any existing cae- prefixed advices first.
SYMBOL, ARGLIST, DOCSTRING and BODY are as in `defadvice!`."
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       ;; Define the function
       (defun ,symbol ,arglist ,docstring ,@body)

       ;; For each target function, first remove cae- advices, then add the new one
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           ;; Remove any existing cae- advices
           (when cae-config-finished-loading
             (cae-remove-cae-advices target ',symbol))

           ;; Add the new advice
           (advice-add target (car targets) #',symbol))))))

(defun cae-advice-add (symbol how function &optional props)
  "Like `advice-add` but removes any existing cae- prefixed advices first.
SYMBOL is the function to advise.
HOW determines the position of the advice.
FUNCTION is the advice function.
PROPS is a plist of properties."
  ;; First, remove any existing cae- advices
  (when cae-config-finished-loading
    (let ((advice-name (if (and props (plist-get props :name))
                          (plist-get props :name)
                        function)))
      (cae-remove-cae-advices symbol advice-name)))

  ;; Then add the new advice
  (advice-add symbol how function props))

;; For rotate word packages.
(defalias 'cae-advice-remove 'advice-remove)
(defalias 'cae-undefadvice! 'undefadvice!)
(defalias 'uncae-defadvice! 'undefadvice!)

(defun cae-wm-name ()
  "Return the running window manager's EWMH name, or nil.
Reads `_NET_WM_NAME' of the window pointed to by the root
`_NET_SUPPORTING_WM_CHECK' property.  This works regardless of how
Emacs was launched, so it does not depend on session env vars like
`XDG_CURRENT_DESKTOP' (which are empty under a bare `xinit' session,
e.g. StumpWM)."
  (when (eq 'x (framep (selected-frame)))
    (let ((id (x-window-property "_NET_SUPPORTING_WM_CHECK" nil "WINDOW" 0 nil t)))
      (when (integerp id)
        ;; Some WMs only set the legacy STRING property (e.g. StumpWM, which
        ;; leaves UTF8_STRING empty), others use UTF8_STRING (e.g. EXWM); try
        ;; both and skip empty values.
        (cl-some (lambda (type)
                   (let ((name (x-window-property "_NET_WM_NAME" nil type id nil t)))
                     (and (stringp name) (not (string-empty-p name))
                          (substring-no-properties name))))
                 '("UTF8_STRING" "STRING"))))))

(defvar cae-pdump--building nil
  "Non-nil while the pdump image is being built (set by cli.el).
`cae-after-frame!' checks this to decide whether to run its body now or defer it
to the first real frame at runtime.")

(defun cae-run-on-first-frame (fn)
  "Call FN once, on the first real frame.
On a daemon there is no usable frame until a client connects, so defer to
`server-after-make-frame-hook' (fires once the daemon is connected to);
otherwise the initial `-nw'/GUI frame already exists, so call FN immediately.
The `daemonp' test happens when this runs (at runtime), not when the dump is
built (always batch, never a daemon).  Self-removing in the daemon case."
  (if (daemonp)
      (letrec ((fn* (lambda (&rest _)
                      (remove-hook 'server-after-make-frame-hook fn*)
                      (funcall fn))))
        (add-hook 'server-after-make-frame-hook fn* t))
    (funcall fn)))

(defmacro cae-before-frame! (&rest body)
  "Run BODY now, or — during the pdump build — defer it to `before-init-hook'
at runtime, so it runs OUTSIDE the dumped image (seeing the live environment,
e.g. `CAE_EXWM') and BEFORE the initial frame is created.

The counterpart to `cae-after-frame!': use it when BODY must *precede* the first
frame rather than follow it — enabling EXWM as the window manager is the case in
point.  `before-init-hook' fires inside `command-line' after `early-init.el' but
before `frame-initialize' (see startup.el), which is the earliest runtime point
a dumped hook can reach.  Predicates that pivot on the frame must therefore use
`initial-window-system' (set by then) rather than `(framep (selected-frame))'
\(still the initial terminal frame at this point).

When NOT building the dump, config loads during `init.el', after the initial
frame already exists, so BODY runs inline — the earliest point still reachable,
and EXWM simply takes over the existing frame.  Because the env-/frame-sensitive
decision is deferred into the dump as a hook, the image never bakes in the build
host's environment.  Intended for the non-daemon WM launch; a daemon has no
initial frame to precede."
  `(if (bound-and-true-p cae-pdump--building)
       (add-hook 'before-init-hook (lambda () ,@body) t) ; append → preserve order
     ,@body))

(defmacro cae-after-frame! (&rest body)
  "Run BODY now (a frame already exists), or — during the pdump build — defer it
to the first real frame at runtime so the display/WM/tty predicates see the
actual launch environment.

Deferral fires on the FIRST real frame via `cae-run-on-first-frame': at startup
for a `-nw'/GUI Emacs (the initial frame already exists by `emacs-startup-hook'
time), and on `server-after-make-frame-hook' for a daemon (once a client
connects).  Plain `after-make-frame-functions' would miss the initial frame of a
non-daemon Emacs entirely."
  `(if (bound-and-true-p cae-pdump--building)
       (add-hook 'emacs-startup-hook
                 (lambda () (cae-run-on-first-frame (lambda () ,@body)))
                 t)                     ; append → preserve load order
     ,@body))


(provide 'cae-lib)
;;; cae-lib.el ends here
