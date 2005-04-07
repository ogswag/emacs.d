;;; nord-light-theme.el --- Nord light theme based on modus operandi -*- lexical-binding: t -*-

;; Copyright (C) 2005
;; Author: Alexander Zakharov
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: theme nord light minimal

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A clean, light theme based on Nord palette and darkened for visibility on light background.
;; Designed with modus operandi structure for accessibility and customization.
;;; Code:

(deftheme nord-light
  "Clean light theme based on Nord palette and darkened for visibility on light background.")

(let* ((class '((class color) (min-colors 256)))
       ;; Nord palette - base colors
       (nord0  "#2E3440")   ; Polar Night darkest
       (nord1  "#3B4252")   ; Polar Night dark
       (nord2  "#434C5E")   ; Polar Night medium
       (nord3  "#4C566A")   ; Polar Night light
       (nord4  "#D8DEE9")   ; Snow Storm - origin
       (nord5  "#E5E9F0")   ; Snow Storm - lighter
       (nord6  "#ECEFF4")   ; Snow Storm - lightest
       (nord7  "#5F9392")   ; Frost - cyan
       (nord8  "#427D8E")   ; Frost - cyan lighter
       (nord9  "#4D79A4")   ; Frost - blue
       (nord10 "#35629A")   ; Frost - blue darker
       (nord11 "#B33B47")   ; Aurora - red
       (nord12 "#C66142")   ; Aurora - orange
       (nord13 "#B3914C")   ; Aurora - yellow
       (nord14 "#749855")   ; Aurora - green
       (nord15 "#B35FA4")   ; Aurora - magenta

       (bg-main     "#F9FAFB")
       (bg-dim      "#F3F4F6")
       (bg-alt      "#EBEDF0")
       (fg-main     "#2B3E50")
       (fg-dim      "#6F7F9E")
       (fg-alt      "#718096")

       (accent-blue      "#2F58B6")
       (accent-cyan      "#007197")
       (accent-magenta   "#755DAC")

       ;; Semantic colors
       (red           nord11)
       (red-warmer    "#D64045")
       (red-cooler    "#B24B56")

       (green         nord14)
       (green-warmer  "#9AC94D")
       (green-cooler  "#8EB89F")

       (yellow        nord13)
       (yellow-warmer "#BD8A58")
       (yellow-cooler "#BD8E18")

       (blue          accent-blue)
       (blue-warmer   nord8)
       (blue-cooler   nord10)

       (cyan          accent-cyan)
       (cyan-warmer   nord7)
       (cyan-cooler   "#0B6E99")

       (magenta       accent-magenta)
       (magenta-warmer nord15)
       (magenta-cooler "#7E5BA8")

       ;; UI elements
       (border        "#DDE5ED")
       (border-dim    "#E9ECF1")
       (highlight     "#FEF5E7")
       (hl-line       "#F1F5F9")
       (selection     "#D6EAF8")
       (cursor        nord0)
       )


  (custom-theme-set-faces
   'nord-light

   ;; === BASIC FACES ===
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((,class (:background ,cursor :foreground ,bg-main))))
   `(region ((,class (:background ,selection :foreground unspecified))))
   `(highlight ((,class (:background ,highlight))))
   `(hl-line ((,class (:background ,hl-line))))
   `(fringe ((,class (:background ,bg-main :foreground ,fg-dim))))
   `(line-number ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(line-number-current-line ((,class (:background ,hl-line :foreground ,fg-main :weight bold))))

   ;; === MODELINE ===
   `(mode-line ((,class (:background ,bg-alt :foreground ,fg-main :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,bg-dim :foreground ,fg-dim :box (:line-width 1 :color ,border-dim)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,blue))))
   `(mode-line-emphasis ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:background ,highlight))))

   ;; === FACES FOR SYNTAX HIGHLIGHTING ===
   `(font-lock-comment-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-doc-string-face ((,class (:foreground ,green))))
   `(font-lock-keyword-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-builtin-face ((,class (:foreground ,magenta :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,accent-blue :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,cyan))))
   `(font-lock-type-face ((,class (:foreground ,cyan :weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,magenta))))
   `(font-lock-warning-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,red :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))

   ;; === FACES FOR ERROR/WARNING ===
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,yellow :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; === HEADING FACES ===
   `(heading ((,class (:weight bold))))
   `(org-level-1 ((,class (:foreground ,blue :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,cyan :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,green :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,magenta :weight bold))))
   `(org-level-5 ((,class (:foreground ,accent-blue :weight bold))))
   `(org-level-6 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-7 ((,class (:foreground ,green :weight bold))))
   `(org-level-8 ((,class (:foreground ,magenta :weight bold))))

   ;; === LINK AND UNDERLINE ===
   `(link ((,class (:foreground ,accent-blue :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   `(underline ((,class (:underline t))))

   ;; === WHITESPACE ===
   `(whitespace-space ((,class (:background unspecified :foreground ,fg-alt))))
   `(whitespace-hspace ((,class (:background unspecified :foreground ,fg-alt))))
   `(whitespace-tab ((,class (:background unspecified :foreground ,fg-alt))))
   `(whitespace-newline ((,class (:background unspecified :foreground ,fg-alt))))
   `(whitespace-trailing ((,class (:background ,red :foreground ,fg-main))))
   `(whitespace-line ((,class (:background ,highlight :foreground unspecified))))
   `(whitespace-space-before-tab ((,class (:background ,yellow :foreground ,fg-main))))
   `(whitespace-indentation ((,class (:background unspecified :foreground ,fg-alt))))
   `(whitespace-empty ((,class (:background ,highlight :foreground unspecified))))
   `(whitespace-space-after-tab ((,class (:background ,yellow :foreground ,fg-main))))

   ;; === SEARCH AND HIGHLIGHTING ===
   `(isearch ((,class (:background ,yellow :foreground ,fg-main :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,fg-main))))
   `(lazy-highlight ((,class (:background ,highlight :foreground unspecified))))
   `(match ((,class (:background ,highlight :foreground unspecified :weight bold))))

   ;; === DIFF/PATCH ===
   `(diff-header ((,class (:background ,bg-alt :foreground ,fg-main :weight bold))))
   `(diff-file-header ((,class (:background ,bg-alt :foreground ,fg-main :weight bold))))
   `(diff-function ((,class (:background ,bg-alt :foreground ,accent-blue))))
   `(diff-hunk-header ((,class (:background ,bg-alt :foreground ,magenta))))
   `(diff-added ((,class (:background "#E8F5E9" :foreground ,green))))
   `(diff-removed ((,class (:background "#FFEBEE" :foreground ,red))))
   `(diff-changed ((,class (:background "#FFF3E0" :foreground ,yellow))))
   `(diff-indicator-added ((,class (:foreground ,green :weight bold))))
   `(diff-indicator-removed ((,class (:foreground ,red :weight bold))))
   `(diff-indicator-changed ((,class (:foreground ,yellow :weight bold))))

   ;; === BUFFER/WINDOW NAVIGATION ===
   `(buffer-menu-buffer ((,class (:weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,accent-blue :weight bold))))
   `(minibuffer-noticeable-prompt ((,class (:foreground ,accent-blue :weight bold))))

   ;; === COMPLETION FRAMEWORK ===
   `(completions-annotations ((,class (:foreground ,fg-dim :slant italic))))
   `(completions-common-part ((,class (:foreground ,fg-main :weight bold))))
   `(completions-first-difference ((,class (:foreground ,cyan :weight bold))))

   ;; === VERTICO/SELECTRUM ===
   `(vertico-current ((,class (:background ,selection :foreground ,fg-main :weight bold))))
   `(vertico-group-title ((,class (:foreground ,accent-blue :weight bold))))
   `(vertico-group-separator ((,class (:foreground ,border))))

   ;; === ORDERLESS ===
   `(orderless-match-face-0 ((,class (:foreground ,blue :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,cyan :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,green :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,magenta :weight bold))))

   ;; === CORFU ===
   `(corfu-default ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(corfu-current ((,class (:background ,selection :foreground ,fg-main :weight bold))))
   `(corfu-bar ((,class (:background ,border))))
   `(corfu-border ((,class (:background ,border))))
   `(corfu-annotations ((,class (:foreground ,fg-dim))))
   `(corfu-deprecated ((,class (:strike-through t :foreground ,fg-dim))))

   ;; === FLYMAKE ===
   `(flymake-error ((,class (:underline (:style wave :color ,red) :background unspecified))))
   `(flymake-warning ((,class (:underline (:style wave :color ,yellow) :background unspecified))))
   `(flymake-note ((,class (:underline (:style wave :color ,cyan) :background unspecified))))

   ;; === ELDOC ===
   `(eldoc-highlight-function-argument ((,class (:background ,highlight :foreground ,fg-main :weight bold))))

   ;; === DIRED ===
   `(dired-directory ((,class (:foreground ,accent-blue :weight bold))))
   `(dired-symlink ((,class (:foreground ,cyan :slant italic))))
   `(dired-special ((,class (:foreground ,magenta :weight bold))))
   `(dired-perm-write ((,class (:foreground ,fg-main))))
   `(dired-marked ((,class (:background ,highlight :foreground ,fg-main :weight bold))))
   `(dired-flagged ((,class (:background "#FFEBEE" :foreground ,red))))

   ;; === GIT/MAGIT ===
   `(magit-header ((,class (:weight bold))))
   `(magit-section-heading ((,class (:foreground ,blue :weight bold))))
   `(magit-section-heading-selection ((,class (:background ,selection :foreground ,fg-main :weight bold))))
   `(magit-section-highlight ((,class (:background ,hl-line))))
   `(magit-branch-current ((,class (:foreground ,accent-blue :weight bold))))
   `(magit-branch-local ((,class (:foreground ,cyan))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-tag ((,class (:foreground ,magenta))))
   `(magit-cherry-unmatched ((,class (:foreground ,cyan))))
   `(magit-cherry-equivalent ((,class (:foreground ,magenta))))
   `(magit-dimmed ((,class (:foreground ,fg-dim))))
   `(magit-diff-file-heading ((,class (:weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,hl-line :weight bold))))
   `(magit-diff-file-heading-selection ((,class (:background ,selection :weight bold))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-alt :foreground ,magenta))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,magenta :foreground ,bg-main :weight bold))))
   `(magit-diff-hunk-heading-selection ((,class (:background ,selection :foreground ,magenta))))
   `(magit-diff-lines-heading ((,class (:background ,magenta :foreground ,bg-main))))
   `(magit-diff-context-highlight ((,class (:background unspecified))))
   `(magit-diff-added ((,class (:background "#E8F5E9" :foreground ,green))))
   `(magit-diff-added-highlight ((,class (:background "#E8F5E9" :foreground ,green))))
   `(magit-diff-removed ((,class (:background "#FFEBEE" :foreground ,red))))
   `(magit-diff-removed-highlight ((,class (:background "#FFEBEE" :foreground ,red))))
   `(magit-log-author ((,class (:foreground ,magenta))))
   `(magit-log-date ((,class (:foreground ,fg-dim))))
   `(magit-log-graph ((,class (:foreground ,fg-dim))))

   ;; === ESHELL ===
   `(eshell-prompt ((,class (:foreground ,accent-blue :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,accent-blue :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyan :slant italic))))
   `(eshell-ls-executable ((,class (:foreground ,green :weight bold))))
   `(eshell-ls-readonly ((,class (:foreground ,fg-dim))))
   `(eshell-ls-unreadable ((,class (:foreground ,red))))
   `(eshell-ls-special ((,class (:foreground ,magenta :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,magenta))))
   `(eshell-ls-backup ((,class (:foreground ,yellow))))
   `(eshell-ls-product ((,class (:foreground ,fg-dim))))
   `(eshell-ls-clutter ((,class (:foreground ,fg-dim))))

   ;; === ANSI COLORS ===
   `(ansi-color-black ((,class (:background ,nord0 :foreground ,nord0))))
   `(ansi-color-red ((,class (:background ,red :foreground ,red))))
   `(ansi-color-green ((,class (:background ,green :foreground ,green))))
   `(ansi-color-yellow ((,class (:background ,yellow :foreground ,yellow))))
   `(ansi-color-blue ((,class (:background ,blue :foreground ,blue))))
   `(ansi-color-magenta ((,class (:background ,magenta :foreground ,magenta))))
   `(ansi-color-cyan ((,class (:background ,cyan :foreground ,cyan))))
   `(ansi-color-white ((,class (:background ,nord6 :foreground ,nord6))))
   `(ansi-color-bright-black ((,class (:background ,nord3 :foreground ,nord3))))
   `(ansi-color-bright-red ((,class (:background ,red-warmer :foreground ,red-warmer))))
   `(ansi-color-bright-green ((,class (:background ,green-warmer :foreground ,green-warmer))))
   `(ansi-color-bright-yellow ((,class (:background ,yellow-warmer :foreground ,yellow-warmer))))
   `(ansi-color-bright-blue ((,class (:background ,blue-warmer :foreground ,blue-warmer))))
   `(ansi-color-bright-magenta ((,class (:background ,magenta-warmer :foreground ,magenta-warmer))))
   `(ansi-color-bright-cyan ((,class (:background ,cyan-warmer :foreground ,cyan-warmer))))
   `(ansi-color-bright-white ((,class (:background ,nord6 :foreground ,nord6))))

   ;; === INFO ===
   `(info-header-xref ((,class (:foreground ,accent-blue :underline t))))
   `(info-header-node ((,class (:foreground ,fg-main :weight bold))))
   `(info-index-match ((,class (:background ,highlight :foreground ,fg-main :weight bold))))
   `(info-menu-header ((,class (:foreground ,blue :weight bold))))
   `(info-menu-star ((,class (:foreground ,red))))
   `(info-node ((,class (:weight bold))))
   `(info-xref ((,class (:foreground ,accent-blue :underline t))))
   `(info-xref-visited ((,class (:foreground ,magenta :underline t))))

   ;; === ORG-MODE ===
   `(org-agenda-structure ((,class (:foreground ,blue :weight bold))))
   `(org-agenda-calendar-event ((,class (:foreground ,cyan))))
   `(org-agenda-calendar-sexp ((,class (:foreground ,magenta))))
   `(org-agenda-date ((,class (:foreground ,accent-blue :weight bold))))
   `(org-agenda-date-today ((,class (:background ,hl-line :foreground ,accent-blue :weight bold))))
   `(org-agenda-date-weekend ((,class (:foreground ,fg-dim))))
   `(org-agenda-done ((,class (:foreground ,green :strike-through t))))
   `(org-agenda-clocking ((,class (:background ,highlight :foreground ,blue))))
   `(org-agenda-filter-category ((,class (:foreground ,magenta))))
   `(org-agenda-filter-effort ((,class (:foreground ,yellow))))
   `(org-agenda-filter-regexp ((,class (:foreground ,cyan))))
   `(org-archive ((,class (:foreground ,fg-dim :slant italic))))
   `(org-block ((,class (:background ,bg-alt :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-alt :foreground ,fg-dim :slant italic))))
   `(org-block-end-line ((,class (:background ,bg-alt :foreground ,fg-dim :slant italic))))
   `(org-checkbox ((,class (:foreground ,cyan :weight bold))))
   `(org-checkbox-statistics-done ((,class (:foreground ,green :weight bold))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,yellow :weight bold))))
   `(org-clock-overlay ((,class (:background ,highlight :foreground ,blue))))
   `(org-code ((,class (:background ,bg-alt :foreground ,cyan))))
   `(org-column ((,class (:background ,bg-alt))))
   `(org-column-title ((,class (:background ,bg-alt :foreground ,blue :weight bold))))
   `(org-date ((,class (:foreground ,cyan :underline t))))
   `(org-date-selected ((,class (:background ,selection :foreground ,cyan :weight bold))))
   `(org-document-info ((,class (:foreground ,fg-dim))))
   `(org-document-info-keyword ((,class (:foreground ,fg-dim :slant italic))))
   `(org-document-title ((,class (:foreground ,accent-blue :weight bold :height 1.2))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-drawer ((,class (:foreground ,fg-dim))))
   `(org-ellipsis ((,class (:foreground ,fg-dim))))
   `(org-footnote ((,class (:foreground ,cyan :underline t))))
   `(org-formula ((,class (:foreground ,magenta))))
   `(org-habit-clear-face ((,class (:background ,blue))))
   `(org-habit-clear-future-face ((,class (:background ,blue-cooler))))
   `(org-habit-ready-face ((,class (:background ,green))))
   `(org-habit-ready-future-face ((,class (:background ,green-cooler))))
   `(org-habit-alert-face ((,class (:background ,yellow))))
   `(org-habit-alert-future-face ((,class (:background ,yellow-cooler))))
   `(org-habit-overdue-face ((,class (:background ,red))))
   `(org-habit-overdue-future-face ((,class (:background ,red-cooler))))
   `(org-headline-done ((,class (:foreground ,green :strike-through t))))
   `(org-latex-and-related ((,class (:foreground ,magenta))))
   `(org-link ((,class (:foreground ,accent-blue :underline t))))
   `(org-list-dt ((,class (:foreground ,magenta :weight bold))))
   `(org-macro ((,class (:foreground ,magenta))))
   `(org-mode-line-clock ((,class (:background ,highlight :foreground ,blue))))
   `(org-mode-line-clock-overrun ((,class (:background ,red :foreground ,bg-main :weight bold))))
   `(org-priority ((,class (:foreground ,red :weight bold))))
   `(org-property-value ((,class (:foreground ,fg-dim))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,yellow))))
   `(org-scheduled-today ((,class (:foreground ,cyan))))
   `(org-sexp-date ((,class (:foreground ,cyan))))
   `(org-special-keyword ((,class (:foreground ,magenta :weight bold))))
   `(org-table ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(org-table-header ((,class (:background ,bg-dim :foreground ,blue :weight bold))))
   `(org-tag ((,class (:foreground ,magenta :weight bold))))
   `(org-tag-group ((,class (:foreground ,magenta :weight bold))))
   `(org-target ((,class (:foreground ,cyan :underline t))))
   `(org-time-grid ((,class (:foreground ,fg-dim))))
   `(org-todo ((,class (:foreground ,yellow :weight bold))))
   `(org-upcoming-deadline ((,class (:foreground ,red))))
   `(org-upcoming-distant-deadline ((,class (:foreground ,fg-dim))))
   `(org-verbatim ((,class (:background ,bg-alt :foreground ,green))))
   `(org-verse ((,class (:foreground ,fg-dim :slant italic))))

   ;; === MARKDOWN ===
   `(markdown-blockquote-face ((,class (:foreground ,fg-dim :slant italic))))
   `(markdown-bold-face ((,class (:weight bold))))
   `(markdown-code-face ((,class (:background ,bg-alt :foreground ,cyan))))
   `(markdown-comment-face ((,class (:foreground ,fg-dim))))
   `(markdown-emphasis-face ((,class (:slant italic))))
   `(markdown-gfm-checkbox-face ((,class (:foreground ,cyan))))
   `(markdown-header-delimiter-face ((,class (:foreground ,fg-dim))))
   `(markdown-header-face ((,class (:weight bold))))
   `(markdown-header-face-1 ((,class (:foreground ,blue :weight bold :height 1.3))))
   `(markdown-header-face-2 ((,class (:foreground ,cyan :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground ,green :weight bold :height 1.1))))
   `(markdown-header-face-4 ((,class (:foreground ,magenta :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,accent-blue :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,cyan :weight bold))))
   `(markdown-header-rule-face ((,class (:foreground ,border))))
   `(markdown-highlight-face ((,class (:background ,highlight))))
   `(markdown-html-attr-name-face ((,class (:foreground ,cyan))))
   `(markdown-html-attr-value-face ((,class (:foreground ,green))))
   `(markdown-html-entity-face ((,class (:foreground ,magenta))))
   `(markdown-html-tag-delimiter-face ((,class (:foreground ,fg-dim))))
   `(markdown-html-tag-name-face ((,class (:foreground ,accent-blue))))
   `(markdown-inline-code-face ((,class (:background ,bg-alt :foreground ,cyan))))
   `(markdown-italic-face ((,class (:slant italic))))
   `(markdown-language-keyword-face ((,class (:foreground ,magenta))))
   `(markdown-line-break-face ((,class (:background ,highlight))))
   `(markdown-link-face ((,class (:foreground ,accent-blue :underline t))))
   `(markdown-link-title-face ((,class (:foreground ,cyan))))
   `(markdown-list-face ((,class (:foreground ,magenta))))
   `(markdown-markup-face ((,class (:foreground ,fg-dim))))
   `(markdown-math-face ((,class (:foreground ,magenta))))
   `(markdown-metadata-key-face ((,class (:foreground ,magenta))))
   `(markdown-metadata-value-face ((,class (:foreground ,cyan))))
   `(markdown-missing-link-face ((,class (:foreground ,red))))
   `(markdown-plain-url-face ((,class (:foreground ,cyan :underline t))))
   `(markdown-pre-face ((,class (:background ,bg-alt :foreground ,green))))
   `(markdown-reference-face ((,class (:foreground ,cyan :underline t))))
   `(markdown-strike-through-face ((,class (:strike-through t))))
   `(markdown-table-face ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(markdown-url-face ((,class (:foreground ,accent-blue :underline t))))

   ))

(provide-theme 'nord-light)
;;; nord-light-theme.el ends here
