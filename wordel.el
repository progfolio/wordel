;;; wordel.el --- An Elisp implementation of "Wordle" (aka "Lingo")  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  Nicholas Vollmer <iarchivedmywholelife@gmail.com>
;; URL: https://github.com/progfolio/wordel
;; Created: Janurary 13, 2022
;; Keywords: games
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.0

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

;; Have fun!

;;; Code:
(require 'cl-lib)
(require 'text-property-search)

;;; Custom Options
(defgroup wordel nil
  "Elisp implementation of `wordle` aka `lingo`."
  :group 'games
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

(defcustom wordel-word-file
  (expand-file-name "./words.txt" (file-name-directory
                                   (file-truename
                                    (or (buffer-file-name) load-file-name))))
  "File containing puzzle word candidates.
Each candidate should be on a separate line."
  :type 'file)

(defcustom wordel-illegal-characters "[^A-Za-z]"
  "Regular expression matching illegal word characters.
These are deleted from a puzzle word character."
  :type 'regexp)

(defcustom wordel-want-evil-row-navigation t
  "If non-nil, \"H\" and \"L\" move the cursor left and right in the game board."
  :type 'boolean)

;;;; Variables
(defvar wordel-buffer "*wordel*" "Name of the wordel buffer.")
(defvar wordel--last-game nil "Game state of last played game.")
(defvar wordel--last-marathon nil "Game state of last played marathon.")

;;;; Faces
(defface wordel-correct
  '((t (:background "#538D4E")))
  "Face for a guessed letter which matches its position in the puzzle word.")

(defface wordel-almost
  '((t (:background "#bf9f3b")))
  "Face for a guessed letter which is included in the puzzle word.")

(defface wordel-box
  '((t (:box (:line-width -4 :color "black" :style released-button))))
  "Default face for a wordel letter.")

(defface wordel-current-box
  '((t (:box (:line-width -4 :color "orange" :style released-button))))
  "Default face for a wordel letter.")

(defface wordel-spacer
  '((t (:width ultra-condensed :height 0.1 :background nil)))
  "Face for space between letter boxes.")

(defface wordel-default
  '((t ( :weight ultra-bold :background "#3A3A3C" :foreground "#D7DADC" :height 3.0)))
  "Default face for a wordel letter.")

(defface wordel-error
  '((t ( :inherit compilation-error)))
  "Default face for a wordel error message.")

(defun wordel-legal-p (word)
  "Return t if WORD is a legal word, nil otherwise."
  (let* ((min (or (car-safe wordel-word-length) wordel-word-length))
         (max (or (cdr-safe wordel-word-length) wordel-word-length)))
    (and (<= min (length word) max)
         (not (string-match-p wordel-illegal-characters word)))))

(defun wordel-local-words ()
  "Return a puzzle word from `wordel-word-file'."
  (with-temp-buffer
    (insert-file-contents wordel-word-file)
    (mapcar #'upcase (cl-remove-if-not #'wordel-legal-p
                                       (split-string (buffer-string) "\n")))))

(defun wordel--word (candidates)
  "Select a random word from CANDIDATES."
  (nth (random (length candidates)) candidates))

(defun wordel--split-with-spaces (string)
  "Split STRING, keeping its spaces."
  (let ((s (split-string string ""))) (pop s) (butlast s)))

(defun wordel--comparison (guess subject)
  "Return propertized GUESS character list compared against SUBJECT."
  (let* ((subjects (split-string subject "" 'omit-nulls))
         ;; Keep spaces in complete rows that result from pause.
         (guesses  (wordel--split-with-spaces guess))
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

(defun wordel--rules ()
  "Return the rules string."
  (propertize
   (string-join
    (list
     (propertize "How to Play" 'face 'wordel-default)
     "Type a letter into each box to guess the secret word."
     (concat "Press " (propertize "RETURN" 'face 'help-key-binding) " to submit your guess.")
     "Each letter in your guess will be color coded to give you hints:"
     "\n"
     (mapconcat
      (lambda (cell)
        (format "  %s  %s" (car cell) (propertize (cdr cell) 'display '(raise 0.50))))
      `((,(wordel--tile (propertize "X" 'hint 'wordel-almost))
         . "The letter appears in the word at least once.")
        (,(wordel--tile (propertize "X" 'hint 'wordel-correct))
         . "The letter is in this exact spot in the word.")
        (,(wordel--tile "X") . "The letter is in not in the word."))
      "\n")
     "\n"
     "You get as many guesses as there are rows in the game table.")
    "\n")
   'wordel-rules t))

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
      (let ((p (point))) (put-text-property p (1+ p) 'display char)))))

(defun wordel--row-to-word (row)
  "Return character display properties of ROW."
  (mapconcat (lambda (string)
               (cond
                ((get-text-property 0 'index string)
                 (or (get-text-property 0 'display string) " "))
                ((not (string-match-p " " string)) string)))
             (wordel--split-with-spaces row)))

(defun wordel--current-word ()
  "Return current row's word."
  (save-excursion
    (wordel--position-cursor 0)
    (wordel--row-to-word
     (buffer-substring (line-beginning-position) (line-end-position)))))

(defun wordel--display-message (string &rest objects)
  "Display a message in the UI message area.
STRING and OBJECTS are passed to `format', which see."
  (with-current-buffer wordel-buffer
    (save-excursion
      (goto-char (point-min))
      (if-let ((area (text-property-search-forward 'message-area)))
          (with-silent-modifications
            (put-text-property (prop-match-beginning area) (prop-match-end area)
                               'display (apply #'format string objects)))
        (error "Unable to locate message area")))))

(defun wordel--display-error (string &rest objects)
  "Display an error in the UI message area.
STRING and OBJECTS are passed to `format', which see."
  (wordel--display-message
   "%s" (propertize (apply #'format string objects) 'face 'wordel-error)))

(defun wordel--read-inputs (words &optional index)
  "Read user inputs during game loop.
If a valid input word is given, it is compared against WORDS.
If INDEX is non-nil, start at that column of current row."
  (let ((index      (or index 0))
        (navigators (append '(left right)
                            (when wordel-want-evil-row-navigation '(?H ?L))))
        (prev-header header-line-format)
        (header     (mapconcat
                     (lambda (c)
                       (concat (propertize (car c) 'face 'help-key-binding) " " (cdr c)))
                     '(("C-g" . "quit game")
                       ("C-p" . "pause game")
                       ("C-h" . "toggle help"))
                     " "))
        help
        done
        result)
    (setq header-line-format header)
    (while (not done)
      (wordel--position-cursor index)
      ;; @HACK: Is there a better way to catch a quit signal from read-event?
      ;; Thought I could wrap the call in a `condition-case', but that doesn't seem
      ;; do the trick on its own. ~ NV 2022-01-14
      (let ((event (let ((inhibit-quit t)) (read-event " "))))
        (wordel--display-message "%s" " ") ;;clear messages
        (pcase event
          (?\C-g  (setq done t result 'quit))
          (?\C-p  (setq done t result (wordel--split-with-spaces (wordel--current-word))))
          (?\C-h (if (setq help (not help))
                     (wordel-help header)
                   (with-current-buffer (concat wordel-buffer "<help>")
                     (quit-window))))
          ((pred (lambda (e) (member e navigators)))
           (pcase event
             ((or 'left  ?H) (when (> index 0)                        (cl-decf index)))
             ((or 'right ?L) (when (< index (1- wordel-word-length)) (cl-incf index)))))
          ('return
           (let ((word (wordel--current-word)))
             (if (and (wordel-legal-p word)
                      (member word words))
                 (setq done t result word)
               (wordel--display-error
                (if (string-match-p wordel-illegal-characters word)
                    "Not enough letters: %S"
                  "Word not in dictionary: %S")
                word))))
          ('backspace (wordel--display-char " ")
                      (when (> index 0) (cl-decf index)))
          ((pred characterp)
           (if (and help (eq event ?q))
               (progn (setq help nil) (quit-window))
             (let ((s (char-to-string event)))
               (if (string-match-p wordel-illegal-characters s)
                   (wordel--display-error "Illegal character: %S" s)
                 (wordel--display-char (upcase s))
                 (when (< index (1- wordel-word-length))
                   (cl-incf index)))))))))
    (setq header-line-format prev-header)
    result))

(defun wordel--print-board (rows limit)
  "Print board ROWS up to LIMIT.
After ROWS are exhausted, blank rows are printed.
The first blank row is marked current."
  (let ((empty (make-list wordel-word-length " "))
        (len   (length rows)))
    (insert
     (wordel--board (append
                     (reverse rows)
                     (when (< len limit)
                       (append (list (wordel--row empty 'current))
                               (make-list (- limit (1+ len)) (wordel--row empty)))))))))

(defun wordel--unpause (inputs)
  "Print stored INPUTS on board.
Return index of needed input in current row."
  (dotimes (i (length inputs))
    (let ((c (nth i inputs)))
      (wordel--position-cursor i)
      (wordel--display-char c)))
  (when inputs
    (or (string-match-p wordel-illegal-characters (string-join inputs))
        (1- (length inputs)))))

(defun wordel--end-game (state)
  "End game according to STATE."
  (apply #'wordel--display-message
         (pcase (plist-get state :outcome)
           ('win   (list "You WON!"))
           ('lose  (list "YOU LOST! Word was %S"     (plist-get state :word)))
           ('quit  (list "The word was %S, quitter." (plist-get state :word)))
           ('pause (list "Game Paused. Press \"r\" to resume")))))

(defun wordel--words ()
  "Return a word list using `wordel-words-funtcion'."
  (or (funcall wordel-words-function)
      (error "Unable to retrieve candidate words with %S"
             wordel-words-function)))

(defun wordel--game (&optional state)
  "Initialize a new game.
If STATE is non-nil, it's properties are used in lieu of defaults.
Return a state plist."
  (cl-destructuring-bind (&key (words (wordel--words))
                               (word  (or (wordel--word words)
                                          (error "Unable to find a puzzle word")))
                               (limit wordel-attempt-limit)
                               rows inputs (attempts 0)
                               &allow-other-keys &aux
                               (outcome  nil)
                               ;;@TODO: fix when resuming from pause
                               (start-time (current-time))
                               (wordel-word-length (length word)))
      (copy-tree state)
    (with-current-buffer (get-buffer-create wordel-buffer)
      (pop-to-buffer-same-window wordel-buffer)
      (unless (derived-mode-p 'wordel-mode) (wordel-mode))
      (while (not outcome)
        (with-silent-modifications
          (erase-buffer)
          (goto-char (point-min))
          (wordel--print-board rows limit)
          (insert "\n\n" (propertize " " 'message-area t) "\n\n"))
        (cond
         ((when-let ((r (car rows))) (string= (wordel--row-to-word r) word))
          (setq outcome 'win))
         ((>= attempts limit)
          (setq outcome 'lose))
         (t
          (let ((index (wordel--unpause inputs)))
            (setq inputs nil)
            (cl-incf attempts)
            (pcase (wordel--read-inputs words index)
              ('quit                 (setq outcome 'quit))
              ((and (pred listp) it) (setq outcome 'pause inputs it) (cl-decf attempts))
              (it                    (push (wordel--row (wordel--comparison it word))
                                           rows)))))))
      (let ((state (list :outcome  outcome
                         :attempts attempts
                         :limit limit
                         :inputs inputs
                         :word word
                         :rows rows
                         :start-time start-time
                         :end-time (current-time)
                         :words words)))
        (wordel--end-game state)
        ;; Leaving cursor in the board gives false impression that game is on.
        (goto-char (point-max))
        (setq wordel--last-game state)))))

;;;###autoload
(defun wordel (&optional new)
  "Play wordel.
IF NEW is non-nil, abandon paused game, if any."
  (interactive "P")
  (wordel--game (unless new
                  (when (eq (plist-get wordel--last-game :outcome) 'pause)
                    wordel--last-game))))

(defun wordel--marathon (&optional states)
  "Initialize a marathon of wordel games.
If STATES is non-nil, it's first element initializes the state of the marathon."
  (let* ((states  (copy-tree  states))
         (state   (car states))
         (resume  (eq (plist-get state :outcome) 'pause))
         (wordlen (if-let ((w (plist-get state :word))) (length w) 3))
         (limit   (or (plist-get state :limit) 10))
         (rounds  (if resume (length states) 0))
         ;; Bound here so we don't interfere with non-marathon games.
         wordel--last-game)
    (while (or resume (not (member (plist-get (car states) :outcome)
                                   '(quit pause lose champion))))
      (setq wordel--last-marathon
            (push (let ((wordel-word-length wordlen)
                        (wordel-attempt-limit limit))
                    (condition-case _ ; The dictionary has been exhausted.
                        (wordel--game
                         (when resume (prog1 (car states) (setq resume nil))))
                      ((error) (setf (car states)
                                     (plist-put (car states) :outcome 'champion)))))
                  states))
      (cl-incf rounds)
      (cl-incf wordlen)
      (if (zerop (mod rounds 3)) (cl-decf limit)))
    ;;@TODO: print more stats about marathon
    (apply #'wordel--display-message
           (let* ((game    (car states))
                  (word    (plist-get game :word))
                  (outcome (plist-get game :outcome)))
             (pcase outcome
               ('quit     (list "Had enough, eh? Word was %S.\nFinal Score: %d" word
                                (cl-decf rounds)))
               ('lose     (list "Sorry. Word was %S.\nFinal Score: %d" word rounds))
               ('champion (list "YOU BEAT THE DICTIONARY!\nFinal Score: %d" rounds))
               ('pause    (list "Marathon paused. Press \"m\" to resume.")))))
    (unless (eq (plist-get (car states) :outcome) 'pause)
      (setq wordel--last-marathon nil))))

;;@TODO: This and the `wordel` command are essentially the same...
;;;###autoload
(defun wordel-marathon (&optional new)
  "Play a wordel marathon.
IF NEW is non-nil, abandon paused marathon, if any."
  (interactive "P")
  (wordel--marathon (unless new
                      (when-let ((last (car wordel--last-marathon))
                                 ((eq (plist-get last :outcome) 'pause)))
                        wordel--last-marathon))))

(defun wordel--commands-text ()
  "Return commands text."
  (substitute-command-keys
   (concat "\\<wordel-mode-map>"
           "Wordel! "
           "\\[wordel] to play again. "
           "\\[wordel-marathon] to play in marathon mode. "
           "\\[wordel-help] to display help. ")))

;;;###autoload
(defun wordel-help (&optional header)
  "Display game rules.
If HEADER is non-nil use it for `header-line-format'."
  (interactive)
  (let ((b (concat wordel-buffer "<help>")))
    (with-current-buffer (get-buffer-create b)
      (with-silent-modifications
        (erase-buffer)
        (insert
         (wordel--rules)
         "\nYou can quit this window by pressing "
         (propertize (substitute-command-keys "\\<special-mode-map>\\[quit-window]."))
         "\n"))
      (special-mode)
      (setq header-line-format header)
      (pop-to-buffer-same-window (current-buffer)))))

(define-derived-mode wordel-mode special-mode "Wordel"
  "A word game based on 'Wordle' and/or 'Lingo'.

    \\{wordel-mode-map}"
  (setq header-line-format (wordel--commands-text)))

;;; Key bindngs
(define-key wordel-mode-map (kbd "r") 'wordel)
(define-key wordel-mode-map (kbd "m") 'wordel-marathon)
(define-key wordel-mode-map (kbd "h") 'wordel-help)

(provide 'wordel)
;;; wordel.el ends here
