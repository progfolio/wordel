;;; wordel.el --- "Wordle" aka "Lingo" in Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  Nicholas Vollmer
;; Keywords: games

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

;;

;;; Code:
(require 'cl-lib)
(require 'text-property-search)

;;; Custom Options
(defgroup wordel nil
  "A wordel clone for Emacs."
  :group 'org
  :prefix "wordel-")

(defcustom wordel-word-length 5
  "Length of the puzzle word.
If it is an int, words will be that length.
It may also be a cons sell of form: (MIN . MAX)."
  :type (or 'int 'cons))

(defcustom wordel-attempt-limit 6
  "Number of attempts allowed before game over."
  :type 'int)

(defcustom wordel-words-function #'wordel-local-words
  "Function used to retrieve the candidate word list.
It takes no words and returns a list of strings."
  :type 'function)

(defcustom wordel-word-file "/usr/share/dict/words"
  "File containing puzzle word candidates.
Each candidate should be on a separate line."
  :type 'file)

(defcustom wordel-illegal-characters "[^A-Za-z]"
  "Regular expression matching illegal word characters.
These are deleted from a puzzle word character."
  :type 'regexp)

;;;; Variables
(defvar wordel--game-in-progress nil "Whether or not the game is active.")
(defvar wordel-buffer "*wordel*" "Name of the wordel buffer.")

;;;; Faces
(defface wordel-correct
  '((t (:foreground "green")))
  "Face for a guessed letter which matches its position in the puzzle word.")

(defface wordel-almost
  '((t (:foreground "orange")))
  "Face for a guessed letter which is included in the puzzle word.")

(defface wordel-box
  '((t (:box (:line-width 2 :style 'released-button))))
  "Default face for a wordel letter.")

(defface wordel-current-box
  '((t (:box (:line-width 2 :color "yellow" :style 'released-button))))
  "Default face for a wordel letter.")

(defface wordel-spacer
  '((t (:width ultra-condensed :height 0.75)))
  "Face for space between letter boxes.")

(defface wordel-default
  '((t ( :weight ultra-bold
         :height 3.0)))
  "Default face for a wordel letter.")

(defface wordel-error
  '((t ( :inherit compilation-error)))
  "Default face for a wordel error message.")

(defun wordel-legal-p (word)
  "Return t if WORD is a legal word, nil otherwise."
  (let* ((min (if (consp wordel-word-length)
                  (car wordel-word-length)
                wordel-word-length))
         (max (if (consp wordel-word-length)
                  (cdr wordel-word-length)
                wordel-word-length)))
    (and (<= min (length word) max)
         (string-match-p "[AEIOUYaeiouy]" word)
         (not (string-match-p wordel-illegal-characters word)))))

(defun wordel-local-words ()
  "Return a puzzle word from `wordel-word-file'."
  (with-temp-buffer
    (insert-file-contents wordel-word-file)
    (mapcar #'upcase
            (cl-remove-if-not #'wordel-legal-p
                              (split-string (buffer-string) "\n")))))

(defun wordel--word (candidates)
  "Select a random word from CANDIDATES."
  (nth (random (length candidates)) candidates))

(defun wordel--comparison (guess subject)
  "Return propertized GUESS character list compared against SUBJECT."
  (let ((subjects (split-string subject "" 'omit-nulls))
        (guesses  (split-string guess   "" 'omit-nulls))
        (matches  nil))
    (cl-loop for i from 0 to (1- (length guesses))
             for g = (nth i guesses)
             for s = (nth i subjects)
             do (put-text-property
                 0 1 'hint
                 (cond
                  ((string-match-p g s)
                   (push g matches) 'wordel-correct)
                  ((and (string-match-p g subject)
                        (not (string-match-p g guess (+ i 1)))
                        (not (member g matches)))
                   'wordel-almost)
                  (t nil))
                 g)
             collect g)))

(defun wordel--pad (char)
  "Visually pad CHAR."
  (let ((spacer (propertize " " 'display '(space :width 1.5))))
    (concat spacer char spacer)))

(defun wordel--tile (string &optional box)
  "Return a tile from STRING.
If BOX is non-nil, outline the tile with it."
  (let ((face (list :inherit (list 'wordel-default))))
    (when-let ((hint (get-text-property 0 'hint string)))
      (push hint (cadr face)))
    (push (or box 'wordel-box) (cadr face))
    (propertize (wordel--pad string) 'face face)))

(defun wordel--row (chars &optional current)
  "Return a row of tiles from CHARS.
If CURRENT is non-nil, mark row as current."
  (string-join
   (cl-loop for i from 0 to (1- (length chars))
            for c = (nth i chars)
            collect (wordel--tile (if current (propertize c 'index i) c)
                                  (when current 'wordel-current-box)))
   (propertize " " 'face 'wordel-spacer)))

(defun wordel--board (rows)
  "Return a board string from ROWS."
  (mapconcat (lambda (row) (propertize row 'cursor-intangible t))
             rows "\n"))

(defun wordel--position-cursor (column)
  "Position cursor in COLUMN of current-row.
COLUMNs are zero indexed."
  (goto-char (point-min))
  (text-property-search-forward 'current-row)
  (beginning-of-line)
  (let ((box nil))
    (dotimes (_ (1+ column))
      (setq box (text-property-search-forward 'index)))
    (when box (goto-char (prop-match-beginning box)))))

(defun wordel--display-char (char)
  "Display CHAR in current box."
  (with-current-buffer wordel-buffer
    (with-silent-modifications
      (let ((p (point)))
        (put-text-property p (1+ p) 'display char)))))

(defun wordel--current-word ()
  "Return current row's word."
  (save-excursion
    (wordel--position-cursor 0)
    (let ((row (buffer-substring (line-beginning-position) (line-end-position))))
      (mapconcat (lambda (string)
                   (if-let ((char (get-text-property 0 'display string))
                            ((stringp char)))
                       char
                     ""))
                 (split-string row "" 'omit-nils)))))

(defun wordel-quit ()
  "Quit wordel."
  (interactive)
  (when-let ((buffer (get-buffer wordel-buffer))
             ((buffer-live-p buffer))
             (window (get-buffer-window buffer t)))
    (setq wordel--game-in-progress nil)
    (quit-window 'kill window)))

(defun wordel--display-message (string &rest objects)
  "Display a message in the UI message area.
STRING and OBJECTS are passed to `format', which see."
  (save-excursion
    (goto-char (point-max))
    (if-let ((area (text-property-search-backward 'message-area)))
        (with-silent-modifications
          (put-text-property (prop-match-beginning area) (prop-match-end area)
                             'display (apply #'format string objects)))
      (error "Unable to locate message area"))))

(defun wordel--display-error (string &rest objects)
  "Display an error in the UI message area.
STRING and OBJECTS are passed to `format', which see."
  (wordel--display-message
   "%s" (propertize (apply #'format string objects) 'face 'wordel-error)))

(defun wordel-read-word (words)
  "Read word and test against WORDS."
  (let ((index 0)
        done
        result)
    (while (not done)
      (wordel--position-cursor index)
      ;; @HACK: Is there a better way to catch a quit signal from read-event?
      ;; Thought I could wrap the call in a `condition-case', but that doesn't seem
      ;; do the trick on its own. ~ NV 2022-01-14
      (let ((event (let ((inhibit-quit t))
                     (read-event "wordel reading events. Press C-g to quit game."))))
        (wordel--display-message "%s" " ") ;;clear messages
        (pcase event
          (?\C-g  (setq done t result nil))
          ('return
           (let ((word (wordel--current-word)))
             (if (and (wordel-legal-p word)
                      (member word words))
                 (setq done t result word)
               (wordel--display-error "Word not in dictionary: %S" word))))
          ('backspace (wordel--display-char " ")
                      (when (> index 0) (cl-decf index)))
          ((pred characterp)
           (let ((s (char-to-string event)))
             (if (string-match-p wordel-illegal-characters s)
                 (wordel--display-error "Illegal character: %S" s)
               (wordel--display-char (upcase s))
               (when (< index (1- wordel-word-length))
                 (cl-incf index))))))))
    result))

(defun wordel--new-game ()
  "Initialize a new game."
  (let* ((words (or (funcall wordel-words-function)
                    (error "Unable to retrieve candidate words with %S"
                           wordel-words-function)))
         (word (or (wordel--word words)
                   (error "Unable to find a puzzle word")))
         ;;@TODO: count up instead of down
         (attempts wordel-attempt-limit)
         (rows nil)
         (blanks (make-list (length word) " "))
         (wordel--game-in-progress t)
         (outcome nil))
    (with-current-buffer (get-buffer-create wordel-buffer)
      (pop-to-buffer-same-window wordel-buffer)
      (while wordel--game-in-progress
        (wordel-mode)
        (goto-char (point-min))
        (with-silent-modifications
          (erase-buffer)
          (insert (wordel--board
                   (append rows
                           (when (> attempts 0)
                             (append
                              (list (wordel--row blanks 'current))
                              (make-list (1- attempts) (wordel--row blanks))))))
                  "\n\n"
                  (propertize " " 'message-area t)))
        (pcase outcome
          ('win  (wordel--display-message "You WON!")
                 (setq wordel--game-in-progress nil))
          ('lose (wordel--display-message "YOU LOST! Word was %S" word)
                 (setq wordel--game-in-progress nil))
          ('quit (setq wordel--game-in-progress nil)
                 (wordel--display-message "The word was %S, quitter." word))
          (_
           (cond
            ((and rows
                  (string= (replace-regexp-in-string " " "" (car (last rows))) word))
             (setq outcome 'win))
            ((zerop attempts)
             (setq outcome 'lose))
            (t (cl-decf attempts)
               (let ((guess (wordel-read-word words)))
                 (if (null guess)
                     (setq outcome 'quit)
                   (setq rows
                         (append
                          rows
                          (list (wordel--row
                                 (wordel--comparison guess word)))))))))))))))

(define-derived-mode wordel-mode special-mode "Wordel"
  "A word game based on 'Wordle' and/or 'Lingo'.

\\{wordel-mode-map}")

;;; Key bindngs
(define-key wordel-mode-map (kbd "r") 'wordel)

;;@REMOVE:
(evil-make-intercept-map wordel-mode-map)

(defun wordel ()
  "Play wordel."
  (interactive)
  (wordel--new-game))

(provide 'wordel)
;;; wordel.el ends here
