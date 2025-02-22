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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Library for CAE Doom Emacs modules
;;
;;; Code:

;;; lisp/cae-lib.el -*- lexical-binding: t; -*-

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

(provide 'cae-lib)
;;; cae-lib.el ends here
