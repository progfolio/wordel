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

(defmacro wordel-dochar (word &rest body)
  "Execute BODY for each character in WORD.
The following anaphoric bindings are provided:
   - `word`  the string represented by WORD.
   - `chars` a list of each character in WORD.
   - `char`  the current character of WORD.
   - `i`     the index of the current character."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let* ((word  ,word)
          (chars (split-string word "" 'omit-nulls)))
     (dotimes (i (length chars))
       (let ((char (nth i chars)))
         ,@body))))

(defun wordel--compare (guess subject &optional box)
  "Return propertized GUESS character list compared against SUBJECT.
IF BOX is non-nil, use that for the character BOX face instead of `wordel-box'."
  (let ((subjects (split-string subject "" 'omit-nulls))
        result
        seen)
    (wordel-dochar guess
      (push
       (propertize
        char 'face
        (list :inherit
              (delq nil (list
                         (or box 'wordel-box)
                         (cond
                          ((or (string= char " ")
                               (and (member char seen)
                                    (not (string-match-p char (nth i subjects)))))
                           nil)
                          ((string-match-p char (nth i subjects))    'wordel-correct)
                          ((string-match-p char subject)             'wordel-almost)
                          (t                                         nil))
                         'wordel-default))))
       result)
      (push char seen))
    (nreverse result)))

(defun wordel--row (chars)
  "Return list of propertized CHARS."
  ;;at this point word will have char matches propertized?
  (string-join
   (mapcar (lambda (c) (format "%s%s" c (propertize " " 'face 'wordel-spacer)))
           chars)))

(defun wordel--insert-board (rows)
  "Insert the board from ROWS.
Each row in ROWS is a character list."
  (dolist (row rows)
    (insert (propertize (concat " " row "\n") 'cursor-intangible t))))

(defvar wordel--game-in-progress nil "Whether or not the game is active.")
(defvar wordel-buffer "*wordel*" "Name of the wordel buffer.")


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
      (string-join
       (mapcar (lambda (char) (get-text-property 0 'display char))
               (split-string row "" 'omit-nils))))))

(defun wordel-quit ()
  "Quit wordel."
  (interactive)
  (when-let ((buffer (get-buffer wordel-buffer))
             ((buffer-live-p buffer))
             (window (get-buffer-window buffer t)))
    (setq wordel--game-in-progress nil)
    (quit-window 'kill window)))

(defalias 'wordel--display-error #'message)
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
             (unless (string-match-p wordel-illegal-characters s)
               (wordel--display-char (upcase s))
               (when (< index (1- wordel-word-length))
                 (cl-incf index))))))))
    result))

(defun wordel--new-game ()
  "Initialize a new game."
  (let* ((words (or (funcall wordel-words-function)
                    (error "Unable to retrieve candidate words with %S"
                           wordel-words-function)))
         (word  (or (wordel--word words) (error "Unable to find a puzzle word")))
         (attempts-left wordel-attempt-limit)
         (attempts  nil)
         (blanks      (make-string (length word) ? ))
         (blank-row   (wordel--row (wordel--compare blanks word)))
         (current (let ((c (substring blanks))
                        result)
                    (wordel-dochar c
                      (push (propertize char 'current-row t 'index i) result))
                    (string-join (nreverse result))))
         (current-row (wordel--row (wordel--compare current word 'wordel-current-box)))
         (wordel--game-in-progress t)
         (outcome nil))
    (with-current-buffer (get-buffer-create wordel-buffer)
      (pop-to-buffer wordel-buffer)
      (while wordel--game-in-progress
        (read-only-mode)
        (goto-char (point-min))
        (with-silent-modifications
          (erase-buffer)
          (wordel--insert-board
           (append (reverse attempts)
                   (when (> attempts-left 0)
                     (append
                      (list current-row)
                      (make-list (1- attempts-left) blank-row))))))
        (pcase outcome
          ('win  (message "HORAY") (setq wordel--game-in-progress nil))
          ('lose (message "YOU LOST. Word was %S" word)
                 (setq wordel--game-in-progress nil)
                 (if (y-or-n-p (format "You LOST! Word was %S. Play again?" word))
                     (wordel--new-game)
                   (wordel-quit)))
          ('quit (setq wordel--game-in-progress nil)
                 (if (y-or-n-p (format "The word was %S, quitter. Play again?"
                                       word))
                     (wordel--new-game)
                   (wordel-quit)))
          (_
           (cond
            ((and attempts
                  (string= (replace-regexp-in-string " " "" (car attempts)) word))
             (setq outcome 'win))
            ((zerop attempts-left)
             (setq outcome 'lose))
            (t
             (cl-decf attempts-left)
             (let ((guess (wordel-read-word words)))
               (if (null guess)
                   (setq outcome 'quit)
                 (push (wordel--row (wordel--compare guess word)) attempts)))))))))))

(defun wordel ()
  "Play wordel."
  (interactive)
  (wordel--new-game))

(provide 'wordel)
;;; wordel.el ends here
