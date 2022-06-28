;; Emacs Configuration

;; major aims:
;; to have a SWEET clojure dev environment
;; potentially to be evil. (evaluate)
;; have a portable setup you can compress/put in your own git for deps.
;; have an amazing color scheme.
;; have a workflow to switch between projects like you do with tmux tabs.
;; 
;; minor aims:
;; insert git prefix into commits.
;; to be able to commit homedir repo stuff (can't remember the name)
;; possibly setup emacs server to launch under systemd (--user)
;; clean up this file after all of the above

;; do we really want to go full emacs?
;; can we go full nvim/conjure.. etc?
;; what's the best in the long run?
;; if you go evil, then your keybindings will not interfere

;; Mastering Emacs Notes:
;; M-x info ~= :h for reading docs for emacs/packages
;; fyi, emacs is a tiliing window manager
;; C-x s save all files!
;; C-g will reverse the direction of C-/ undo into a redo!
;; Don't underestimate the negative bindings C-M-- & M--
;; S-exp navigation:
;; - C-M-f + C-M-b : forward/backward s-exp
;; - C-M-d + C-M-u : down/up a list (nested s-exps)
;; - C-M-k : kill-sexp (C-M-u + C-M-k combo kills current s-exp)
;; - C-M-n + C-M-p : forward/backward list
;; - M-a/e : forward/backward sentence, though it behaves similarly to C-M-f/b
;; - C-M-a/e : forward/backward defun
;; - C-M-<SPC> : mark s-exp, see also C-M-- + C-M-<SPC> to mark in reverse
;; C-x h marks the entire buffer!
;; C-s searches but C-M-s regexp-searches!
;; M-n/M-p can search through C-s/isearch history.
;; C-M-i is for Tab completion, where tab isn't available e.g C-s.
;; C-s C-w search under word (C-w adds word, C-M-y adds character)
;; M-s o : occur in a grep-like utility that comes with emacs.
;; Occur also activates inside isearch.
;; Imenu is the framework for jumping to points of interest in a buffer. It's not bound to any key.
;; >>>!!! Suggests using Helm. I've used Ivy in the past. What's best?
;;   - try Google Suggest from Helm
;; You can tell emacs to describe everything under a prefix key with help-map e.g C-x c (C-h/S-h)
;; M-s . : search under cursor
;; grep-search : grep,grep-find,lgrep,rgrep,rzgrep
;; mainly you'll want to use rgrep, you can go up/down matches with M-g,M-n/p
;; More Editing:
;;  - C-S-<backspace> : kill current line, no more 1 C-k
;;  - M-k : kill sentenc
;;  - C-M-w : append kill (to clipboard) if next command is a kill.
;;  - M-y : select yank from kill-ring (previously copied/killed text)
;;  - C-t,M-t : transpose characters/words, a.k.a pulling chars/words to the right.
;;  - C-M-t : transpose s-exps
;;  - C-x C-t : transport lines
;;  - there are also transpose-paragraphs and transpose-sentences
;;  - M-q : refill the paragraph
;;  - C-u, M-q : refill & justify text
;;  - C-x f : sets the fill column width (handy for the above M-q)
;;  - C-x . : sets the fill prefix
;;  - auto-fill-mode : potentially similar to what you had in vim, perhaps just for document modes.
;;  - C-M-% (i3 conflict) : query regexp search and replace
;;  - M-% : query search and replace (can also be used inside isearch)
;;  - in search/replace: spc (replace one), ! (replace all), u (undo one), U (under all)
;;  - C-x C-u/l uppercase/lowercase region
;;  - M-c/u/l capitalize/uppercase/lowercase the next word
;;  Filter Category Editing:
;;  - You can count-lines-region, count-matches, count-words, M-= counts-words-region
;;  - You can delete-duplicate-lines (supports C-u C-u C-u for different behaviours)
;;  - You can delete lines by pattern with flush-lines and keep-lines
;;  - M-s o : list lines matching a pattern
;;  - C-x C-o : Deletes all blank lines after point
;;  - M-^ : Joins the line the point is on with the one above, removing prefixes (like vim?)
;;  - delete-trailing-whitespace, cycle-spacing
;;  - M-\ : deletes whitespace around point
;;  Macros:
;;  - C-x (/)/e : start/stop/play macros (there's a LOT more that emacs supports)
;;  Text Expansion/Auto-complete:
;;  - abbrev/dabbrev/hippie expand/skeletons
;;  - skeletons is a templating tool
;;  - YASnippet
;;  - Autoinsert - inserts templates when you create a new file that matches a certain file type.
;;  Sorting/Alignment:
;;  - sort-lines/fields/numeric-fields/columns/paragraphs/regexp-fields (+ C-u!)
;;  - sort-fields is special, M-3 M-x sort-fields would sort on the third field for example.
;;  - sorting fields must be whitespace delimited, to alter use sort-regexp-fields
;;  - align/align-current/align-regexp
;;  M-z + M-S-z : zap/up-to-char
;;  dictionary-search : localhost or dict.org dictionary search!
;;  insert literals with C-q (e.g C-q backspace)
;;  help-map i : refer to the M-x info (manual) to learn more (there's a lot to learn!)
;;  apropos + info-apropos
;;  vcs is in-built, C-x v is the prefix.
;;  C-x p p : switch between projects (part of project.el)
;;  Cross-References:
;;  - M-. : find defs at point
;;  - M-, : pop marker and return after finding a definition
;;  - M-? : find references matching pattern
;;  - C-M-. : find symbols matching pattern
;;  - C-o : show definition (inside xref def buffer)
;;  image-dired is an image thumbnail browser equivalent of dired
;;  EWW emacs web wowser
;;  Shell Commands:
;;  M-! currently blocked by i3
;;  - M-! : shell cmd into new buffer
;;  - C-u M-! : shell cmd into current buffer
;;  - M-& async shell cmd
;;  - M-| pipe region into shell command
;;  - C-u M-| pipe region and replace region with output
;;  Compilation: M-x compile/recompile + (M-g M-n/p) + g (rerun)
;;  C-x p c
;;  Shells:
;;  term is a shell wrapper, ansi-term is a terminal-emulator
;;  eshell is a freak of nature tbh.. (shell implementation written in elisp)
;;  for most cases, use shell and ansi-term for interactive programs (if you must!)
;;  C-c C-z : send stop cmd, everything else is basically readline
;;  C-c C-c : send stop cmd in ansi-term
;;  Conclusion:
;;  - The key to mastering emacs is understanding how to ask Emacs questions
;;  - Even better, have tools (e.g which-key) that will ask questions for you

;; [current. seaarch and replace, p206]


;; diffs between linux + x11 forwarded windows
;; Recursive Mono Linear Static displays extra wide, the 'code' version Rec Mono Linear works just fine though there are no italics.
;; Also make sure you select the keyboard layout want before starting XMing as that will get passed to the X11  forwarded apps and there appears to be no way to change that after the app starts.

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(when (x-list-fonts "Recursive Mono Linear Static")
  (set-face-attribute 'default nil :font "Recursive Mono Linear Static" :height 100)
  (set-frame-font "Recursive Mono Linear Static" nil t))

(electric-pair-mode 1)
(fido-mode 1) ;; TODO perhaps use ivy instead?
(tab-bar-mode 1)
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-tab-to 'rightmost
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil)
(setq-default indent-tabs-mode nil)
;; tab-width? also TODO yasnippet for .dir-locals.el defaults?

(column-number-mode)
(global-display-line-numbers-mode t)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(dolist (mode '(org-mode-hook
		term-mode-hook
		help-mode-hook
		helpful-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; TODO not sure if this will ever refresh contents after the first load?
;; (unless package-archive-contents
;;   (package-refresh-contents))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package paredit)
(use-package clojure-mode)
(use-package cider)
(use-package diminish)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src"))
    (setq projectile-switch-project-action #'projectile-dired)))
(use-package magit) ;; TODO can you configure magit to work with your dotfiles bare repo?
;; counsel?
;; ivy? helm?
;; helpful?
;; rainbow-delimiters?
;; which-key? gonna be important for discovering emacs/plugins on the fly.
;; counsel-projectile?
;; magit-todos
;; git-gutter
;; org-mode? (there's a lot of config in your old config)
;; org-bullets
;; markdown-mode
;; visual-fill-column
;; typescript-mode
;; lsp-mode (or potentailly eglot)
;; company
;; whitespace-mode? (in-built mode to display whitespace) & whitespace-cleanup?
;; flyspell-mode? on the fly spell checking (also flyspell-prog-mode for docstrings/strings only)
;; project.el? p270 allows running cmds based on project root (based on vcs files etc)
;; nov - epub reader for emacs
;; hydra?
;; dump-jump?
;; include code to use ripgrep where available

(defmacro ifn (fn)
  `(lambda () (interactive) ,fn))

(define-prefix-command 'frame-map) ;; prefix for tmux-like actions

(dolist
    (binding
     `(("M-o" . other-window)
       ("C-c i" . ,(ifn (find-file "~/.emacs.d/init.el")))
       ("C-c n" . ,(ifn (find-file  "~/src/texts/core.org")))
       ("C-c l" . ,(ifn (find-file "~/src")))
       ("C-c g" . magit)
       ("C-h" . delete-backward-char)
       ("M-i" . imenu)
       ("M-/" . comment-or-uncomment-region)
       ("M-O" . tab-recent)
       ("M-;" . ,frame-map)
       ("M-; ;" . comment-dwim)
       ("M-; o" . delete-other-windows)
       ("M-; =" . tab-new)
       ("M-; ]" . tab-rename)
       ("M-; v" . ,(ifn (progn (split-window-vertically) (other-window 1))))
       ("M-; s" . ,(ifn (progn (split-window-horizontally) (other-window 1))))
       ("M-; x" . tab-bar-close-tab)
       ("M-; 1" . ,(ifn (tab-bar-select-tab 1)))
       ("M-; 2" . ,(ifn (tab-bar-select-tab 2)))
       ("M-; 3" . ,(ifn (tab-bar-select-tab 3)))
       ("M-; 4" . ,(ifn (tab-bar-select-tab 4)))
       ("M-; 5" . ,(ifn (tab-bar-select-tab 5)))
       ("M-; 6" . ,(ifn (tab-bar-select-tab 6)))
       ("M-; 7" . ,(ifn (tab-bar-select-tab 7)))
       ("M-; 8" . ,(ifn (tab-bar-select-tab 8)))
       ("M-; 9" . ,(ifn (tab-bar-select-tab 9)))
       ("C-x j" . ,(ifn (progn (split-window-vertically) (other-window 1))))
       ("C-x l" . ,(ifn (progn (split-window-horizontally) (other-window 1))))
       ("C-x g" . magit-status)
       ("s-h" . ,help-map) ;; M-H doesnt work, i3 conflict
       ("M-j" . join-line)
       ;; ("M-k" . paredit-forward-barf-sexp)
       ;; ("M-l" . paredit-forward-slurp-sexp)
       ("M-F" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))


(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(load-theme 'doom-molokai t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" default))
 '(package-selected-packages
   '(spacemacs-theme monokai-pro-theme cider paredit paredit-mode which-key visual-fill-column visual-fill use-package typescript-mode rainbow-delimiters org-bullets magit-todos lsp-mode ivy-rich helpful gruvbox-theme git-gutter evil doom-themes doom-modeline diminish counsel-projectile company clojure-mode))
 '(safe-local-variable-values
   '((eval progn
	   (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
	   (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-default-repl . clojure-cli)
     (cider-default-clj-repl . clojure-cli)
     (cider-clojure-cli-aliases "-A:repl"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
