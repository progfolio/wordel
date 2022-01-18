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
  "If non-nil, the following motions are available during `wordel-input-mode'.
- `H`        move cursor left
- `L`        move cursor right
- `0` or `^` move cursor to start of word
- `$` or `E` move cursor to end of word"
  :type 'boolean)

;;;; Variables
(defvar wordel-buffer "*wordel*" "Name of the wordel buffer.")
(defvar wordel--last-game nil "Game state of last played game.")
(defvar wordel--last-marathon nil "Game state of last played marathon.")
(defvar wordel--game nil "State of the game.")

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

(defun wordel--rules ()
  "Return the rules string."
  (string-join
   (list
    (propertize "How to Play" 'face 'wordel-default)
    "Type a letter into each box to guess the secret word."
    (concat "Press " (substitute-command-keys "\\<wordel-input-mode-map>\\[wordel-submit-guess]") " to submit your guess.")
    "Each letter in your guess will be color coded to give you hints:"
    "\n"
    (wordel--row (wordel--comparison "ZIPPO" "EMACS"))
    "None of the guessed letters match the word."
    "\n"
    (wordel--row (wordel--comparison "CLUED" "EMACS"))
    "The letters \"C\" and \"E\" are in the word, but not in the right spot."
    "\n"
    (wordel--row (wordel--comparison "MACES" "EMACS"))
    "The letters \"S\" is in the word and in the correct spot."
    "\n"
    (wordel--row (wordel--comparison "EMACS" "EMACS"))
    "The word was guessed correctly."
    "\n"
    "You get as many guesses as there are rows in the game table.")
   "\n"))

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

(defun wordel--random-word (candidates)
  "Select a random word from CANDIDATES."
  (nth (random (length candidates)) candidates))

(defun wordel--split-with-spaces (string)
  "Split STRING, keeping its spaces."
  (let ((s (split-string string ""))) (pop s) (butlast s)))

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

(defun wordel--row (strings &optional current)
  "Return a row of tiles from STRINGS.
If CURRENT is non-nil, mark row as current."
  (string-join
   (cl-loop for i from 0 to (1- (length strings))
            for c = (nth i strings)
            collect (wordel--tile (if current (propertize c 'index i) c)
                                  (when current 'wordel-current-box)))
   (propertize " " 'face 'wordel-spacer)))

(defun wordel--board (rows limit)
  "Return board with ROWS up to LIMIT.
After ROWS are exhausted, blank rows are printed.
The first blank row is marked current."
  (let ((empty (make-list wordel-word-length " "))
        (len   (length rows)))
    (propertize
     (string-join
      (append rows (when (< len limit)
                     (append (list (wordel--row empty 'current))
                             (make-list (- limit (1+ len)) (wordel--row empty)))))
      "\n")
     'read-only t 'cursor-intangible t)))

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

(defun wordel--row-to-word (row)
  "Return character display properties of ROW."
  (mapconcat (lambda (string)
               (cond
                ((get-text-property 0 'index string)
                 (or (get-text-property 0 'display string) " "))
                ((not (string-match-p " " string)) string)))
             (wordel--split-with-spaces row)
             ""))

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

(defun wordel--random-words ()
  "Return a word list using `wordel-words-funtcion'."
  (or (funcall wordel-words-function)
      (error "Unable to retrieve candidate words with %S"
             wordel-words-function)))

(defun wordel--state (&optional props)
  "Return game state plist.
If PROPS are non-nil, they are used in place of default values."
  (cl-destructuring-bind (&key (words (wordel--random-words))
                               (word  (or (wordel--random-word words)
                                          (error "Unable to find a puzzle word")))
                               (limit wordel-attempt-limit)
                               rows (attempts 0)
                               &allow-other-keys &aux
                               ;;@TODO: fix when resuming from pause
                               (start-time (current-time)))
      (copy-tree props)
    (list :attempts attempts
          :index    0
          :limit    limit
          :outcome  nil
          :rows     rows
          :start    start-time
          :word     word
          :words    words)))

(defmacro wordel--with-state (state &rest body)
  "Provides anaphoric bindings to variable STATE during BODY."
  (declare (indent 1) (debug t))
  `(let ((state ,state))
     (cl-destructuring-bind
         (&key attempts index limit outcome rows start word words &allow-other-keys)
         ,state
       (ignore state attempts index limit outcome rows start word words) ;pacify bytecompiler
       ,@body)))

(defun wordel--insert-board ()
  "Insert the game board."
  (with-current-buffer (get-buffer-create wordel-buffer)
    (unless (derived-mode-p 'wordel-mode) (wordel-mode))
    (wordel--with-state wordel--game
      (with-silent-modifications
        (erase-buffer)
        (goto-char (point-min))
        (insert (wordel--board (reverse rows) limit))
        (insert (propertize (format "\n\n%s\n\n" (propertize " " 'message-area t))
                            'cursor-intangible t)
                (propertize "\n " 'read-only t))))))

;; (defun wordel--marathon (&optional states)
;;   "Initialize a marathon of wordel games.
;; If STATES is non-nil, it's first element initializes the state of the marathon."
;;   (let* ((states  (copy-tree  states))
;;          (state   (car states))
;;          (resume  (eq (plist-get state :outcome) 'pause))
;;          (wordlen (if-let ((w (plist-get state :word))) (length w) 3))
;;          (limit   (or (plist-get state :limit) 10))
;;          (rounds  (if resume (length states) 0))
;;          ;; Bound here so we don't interfere with non-marathon games.
;;          wordel--last-game)
;;     (while (or resume (not (member (plist-get (car states) :outcome)
;;                                    '(quit pause lose champion))))
;;       (setq wordel--last-marathon
;;             (push (let ((wordel-word-length wordlen)
;;                         (wordel-attempt-limit limit))
;;                     (condition-case _ ; The dictionary has been exhausted.
;;                         (wordel
;;                          (when resume (prog1 (car states) (setq resume nil))))
;;                       ((error) (setf (car states)
;;                                      (plist-put (car states) :outcome 'champion)))))
;;                   states))
;;       (cl-incf rounds)
;;       (cl-incf wordlen)
;;       (if (zerop (mod rounds 3)) (cl-decf limit)))
;;     ;;@TODO: print more stats about marathon
;;     (apply #'wordel--display-message
;;            (let* ((game    (car states))
;;                   (word    (plist-get game :word))
;;                   (outcome (plist-get game :outcome)))
;;              (pcase outcome
;;                ('quit     (list "Had enough, eh? Word was %S.\nFinal Score: %d" word
;;                                 (cl-decf rounds)))
;;                ('lose     (list "Sorry. Word was %S.\nFinal Score: %d" word rounds))
;;                ('champion (list "YOU BEAT THE DICTIONARY!\nFinal Score: %d" rounds))
;;                ('pause    (list "Marathon paused. Press \"m\" to resume.")))))
;;     (unless (eq (plist-get (car states) :outcome) 'pause)
;;       (setq wordel--last-marathon nil))))

;;@TODO: This and the `wordel` command are essentially the same...
;;;###autoload
;; (defun wordel-marathon (&optional new)
;;   "Play a wordel marathon.
;; IF NEW is non-nil, abandon paused marathon, if any."
;;   (interactive "P")
;;   (wordel--marathon (unless new
;;                       (when-let ((last (car wordel--last-marathon))
;;                                  ((eq (plist-get last :outcome) 'pause)))
;;                         wordel--last-marathon))))



;;;###autoload
(defun wordel (&optional state)
  "Initialize a new game.
If STATE is non-nil, it is used in lieu of `wordel--game'."
  (interactive)
  (wordel--with-state (setq wordel--game (or state (wordel--state)))
    (wordel--insert-board)
    (with-current-buffer wordel-buffer
      (wordel-input-mode)
      (wordel--position-cursor index)
      (pop-to-buffer-same-window (current-buffer)))))

;;;###autoload
(defun wordel-help (&optional header)
  "Display game rules.
If HEADER is non-nil use it for `header-line-format'."
  (interactive)
  (let ((b (concat wordel-buffer "<help>")))
    (with-current-buffer (get-buffer-create b)
      (special-mode)
      (with-silent-modifications
        (erase-buffer)
        (insert
         (wordel--rules)
         "\n\nYou can quit this window by pressing "
         (propertize (substitute-command-keys "\\[quit-window]."))
         "\n"))
      (setq header-line-format header)
      (pop-to-buffer-same-window (current-buffer)))))

(defun wordel--clamp (n min max)
  "Return N if it is between MIN and MAX.
Otherwise whichever is closer."
  (if (<= min n max) n (min (max n min) max)))

(defun wordel--filter-inputs ()
  "Filter inputs during game play."
  (when-let (((eq real-this-command 'self-insert-command))
             (input (this-command-keys))
             ((or (not (string-match-p wordel-illegal-characters input))
                  (member input '(" ")))))
    (setq this-command 'ignore)
    (wordel--input-char (upcase input))))

(defun wordel-quit-game ()
  "Quit the current game."
  (interactive)
  (wordel--with-state wordel--game
    (setf (plist-get state :outcome) 'quit)
    (wordel--display-message "The word was %S, quitter." word)
    (setq wordel--game nil)
    ;; Leaving point on the board gives impression that input can still be given.
    (goto-char (point-max))
    (wordel-input-mode -1)))

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

(defun wordel-submit-guess ()
  "Submit the current guess."
  (interactive)
  (wordel--with-state wordel--game
    (let ((guess (wordel--current-word)))
      (if (not (and (wordel-legal-p guess)
                    (member guess words)))
          (wordel--display-error
           (if (string-match-p wordel-illegal-characters guess)
               "Not enough letters: %S"
             "Word not in word list: %S")
           guess)
        (setf (plist-get state :attempts) (cl-incf attempts)
              (plist-get state :rows)
              (push (wordel--row (wordel--comparison guess word)) rows))
        (wordel--insert-board)
        (cond
         ((equal guess word)
          (setf (plist-get state :outcome) 'win)
          (wordel--display-message "You WON!")
          (wordel-input-mode -1))
         ((>= attempts limit)
          (setf (plist-get state :outcome) 'lose)
          (wordel--display-message "YOU LOST! Word was %S" word)
          (wordel-input-mode -1))
         (t (wordel--position-cursor (setf (plist-get state :index) 0))))))))

(defun wordel--display-char (char)
  "Display CHAR in current box."
  (with-current-buffer wordel-buffer
    (with-silent-modifications
      (let ((p (point))) (put-text-property p (1+ p) 'display char)))))

(defun wordel--input-char (character)
  "Input CHARACTER in current column."
  (wordel--with-state wordel--game
    (wordel--position-cursor index)
    (wordel--display-char character)
    (setf index (wordel--clamp (cl-incf index) 0 (1- (length word)))
          (plist-get state :index) index)
    (wordel--position-cursor index)))

(defun wordel-delete-char ()
  "Delete the character in the current column.
Move point to previous column."
  (interactive)
  (wordel--with-state wordel--game
    (wordel--display-char " ")
    (setf index (wordel--clamp (cl-decf index) 0 (1- (length word)))
          (plist-get state :index) index)
    (wordel--position-cursor index)))

(defun wordel--change-col (direction n)
  "Move cursor in DIRECTION N columns."
  (wordel--with-state wordel--game
    (let ((index (wordel--clamp
                  (if (eq direction 'prev) (cl-decf index n) (cl-incf index n))
                  0 (1- (length word)))))
      (setf (plist-get state :index) index)
      (wordel--position-cursor index))))

(defun wordel-next-column (&optional n)
  "Move forward N columns."
  (interactive "p")
  (wordel--change-col 'next n))

(defun wordel-prev-column (&optional n)
  "Move forward N columnss."
  (interactive "p")
  (wordel--change-col 'prev n))

(defun wordel-last-column ()
  "Move cursor to last column."
  (interactive)
  (wordel-next-column most-positive-fixnum))

(defun wordel-first-column ()
  "Move cursor to first column."
  (interactive)
  (wordel-prev-column most-positive-fixnum))

(define-derived-mode wordel-mode text-mode "Wordel"
  "A word game based on 'Wordle' and/or 'Lingo'.

    \\{wordel-mode-map}"
  (setq header-line-format (wordel--commands-text))
  (read-only-mode)
  (cursor-intangible-mode))

;;; Key bindngs
(define-key wordel-mode-map (kbd "q") 'quit-window)
(define-key wordel-mode-map (kbd "r") 'wordel)
(define-key wordel-mode-map (kbd "m") 'wordel-marathon)
(define-key wordel-mode-map (kbd "h") 'wordel-help)

(define-minor-mode wordel-input-mode "Read inputs for wordel game."
  :lighter " Wordel-i"
  :keymap (let ((map (make-sparse-keymap)))
            (mapc (lambda (cell) (define-key map (car cell) (cdr cell)))
                  (append (list (cons (kbd "C-h")     'wordel-help)
                                (cons (kbd "C-r")     'wordel)
                                (cons (kbd "C-g")     'wordel-quit-game)
                                (cons (kbd "RET")     'wordel-submit-guess)
                                (cons (kbd "DEL")     'wordel-delete-char)
                                (cons (kbd "q")       'self-insert-command)
                                (cons (kbd "r")       'self-insert-command)
                                (cons (kbd "m")       'self-insert-command)
                                (cons (kbd "h")       'self-insert-command)
                                (cons (kbd "<up>")    'wordel-last-column)
                                (cons (kbd "<down>")  'wordel-first-column)
                                (cons (kbd "<left>")  'wordel-prev-column)
                                (cons (kbd "<right>") 'wordel-next-column))
                          (when wordel-want-evil-row-navigation
                            (list (cons (kbd "H") 'wordel-prev-column)
                                  (cons (kbd "L") 'wordel-next-column)
                                  (cons (kbd "0") 'wordel-first-column)
                                  (cons (kbd "^") 'wordel-first-column)
                                  (cons (kbd "$") 'wordel-last-column)
                                  (cons (kbd "E") 'wordel-last-column)))))
            map)
  (cond
   (wordel-input-mode
    (unless (equal (buffer-name) wordel-buffer)
      (wordel-input-mode -1)
      (user-error "Minor mode not applicable outside of wordel-buffer"))
    (add-hook 'pre-command-hook #'wordel--filter-inputs nil t)
    (setq header-line-format (wordel--commands-text))
    (cursor-intangible-mode -1))
   (t (remove-hook 'pre-command-hook #'wordel--filter-inputs t)
      (setq header-line-format (wordel--commands-text))
      (cursor-intangible-mode))))

(defun wordel--commands-text ()
  "Return commands text for MAP."
  (substitute-command-keys
   (string-join
    (delq nil
          (list (when wordel-input-mode "Quit Game: \\[wordel-quit-game]")
                "New Game: \\[wordel]"
                "Help: \\[wordel-help]"))
    " ")))

(provide 'wordel)
;;; wordel.el ends here
