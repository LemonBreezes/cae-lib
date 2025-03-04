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

;; For shutting up noisy functions.
(defun cae-shut-up-a (oldfun &rest args)
  (advice-add #'message :override #'ignore)
  (unwind-protect (apply oldfun args)
    (advice-remove #'message #'ignore)))

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
   ;; Otherwise, if LANG indicates UTF-8 you’re probably in a modern terminal emulator.
   ((and (getenv "LANG") (string-match "utf8" (getenv "LANG")))
    1)
   (t 0)))

(defun cae-remove-cae-advices (symbol)
  "Remove all advices with the 'cae-' prefix from function SYMBOL."
  (let ((removed 0))
    (advice-mapc
     (lambda (advice-function properties)
       (let ((name (alist-get 'name properties)))
         (when (and name
                    (symbolp name)
                    (string-prefix-p "cae-" (symbol-name name)))
           (advice-remove symbol name)
           (setq removed (1+ removed)))))
     symbol)
    removed))

(defun cae-remove-cae-advices (symbol &optional new-advice-name)
  "Remove all advices with the 'cae-' prefix from function SYMBOL.
Only show messages when removing advice names different from NEW-ADVICE-NAME."
  (let (cae-advices)
    ;; First, collect all the "cae-" prefixed advices
    (advice-mapc
     (lambda (advice-function properties)
       (let ((name (or (alist-get 'name properties) advice-function)))
         (when (and (symbolp name)
                    (string-prefix-p "cae-" (symbol-name name)))
           (push name cae-advices))))
     symbol)

    ;; Then remove each one
    (dolist (advice cae-advices)
      ;; Only message if the advice name is different from new-advice-name
      (when (or (not new-advice-name)
                (not (equal advice new-advice-name)))
        (message "Removing advice %S from %S" advice symbol))
      (advice-remove symbol advice))

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
             (cae-remove-cae-advices target))

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
    (cae-remove-cae-advices symbol))

  ;; Then add the new advice
  (advice-add symbol how function props))

(provide 'cae-lib)
;;; cae-lib.el ends here
