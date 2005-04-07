;;; keyboard.el --- keybindings -*- lexical-binding: t; -*-

(keymap-global-unset "C-M-<wheel-down>") ; mouse-wheel-global-text-scale
(keymap-global-unset "C-M-<wheel-up>") ; mouse-wheel-global-text-scale
(keymap-global-unset "C-<wheel-down>") ; mouse-wheel-text-scale
(keymap-global-unset "C-<wheel-up>") ; mouse-wheel-text-scale

(keymap-global-unset "M-l") ; downcase-word
(keymap-global-unset "M-u") ; upcase-word

;; Disable secondary selection mouse bindings
(keymap-global-unset "<mouse-2>") ;; middle mouse button secondary yank
(keymap-global-unset "M-<mouse-1>") ;; set secondary selection start
(keymap-global-unset "M-<mouse-3>") ;; set secondary selection end


(keymap-global-unset "<mouse-2>") ; upcase-word

;;;;;;;;;;;;;;;;;;;;;
;;     GENERAL     ;;
;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "C-s-f" #'toggle-frame-fullscreen)

(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-set "C-0" #'text-scale-adjust)

(keymap-global-set "s-r" #'recentf)

(keymap-global-set "s-<up>" #'beginning-of-buffer)
(keymap-global-set "s-<down>" #'end-of-buffer)

(keymap-global-set "s-<up>" #'backward-paragraph)
(keymap-global-set "s-<down>" #'forward-paragraph)

;;;;;;;;;;;;;;;;;;;;;
;;     EDITING     ;;
;;;;;;;;;;;;;;;;;;;;;
(keymap-global-unset "C-M-<down-mouse-1>")
(keymap-global-set "M-s-<down-mouse-1>" #'mouse-drag-region-rectangle)

(defun my-delete-line-backwards ()
  (interactive)
  (delete-region (point) (line-beginning-position)))
(keymap-global-set "s-<backspace>" #'my-delete-line-backwards)

(keymap-global-unset "s-l")
(keymap-global-set "s-l" #'meow-line)

(keymap-global-set "s-/" #'comment-line)
