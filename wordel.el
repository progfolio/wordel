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
(eval-when-compile (require 'subr-x))

;;; Custom Options
(defgroup wordel nil
  "Elisp implementation of `wordle` aka `lingo`."
  :group 'games
  :prefix "wordel-")

(defcustom wordel-show-letter-info t
  "When non-nil, the alphabet is displayed below the game board.
Each tile is shown with its hint face."
  :type 'boolean)

(defcustom wordel-word-length 5
  "Length of the puzzle word.
If it is an int, words will be that length.
It may also be a cons sell of form: (MIN . MAX)."
  :type (or 'int 'cons))

(defcustom wordel-attempt-limit 6
  "Number of attempts allowed before game over."
  :type 'int)

(defcustom wordel-words-function #'wordel-local-words
  "Function which returns a list of candidate word strings.
It should accept one argument, the desired length of the words."
  :type 'function)

(defconst wordel--source-dir
  (file-name-directory (file-truename (or load-file-name (buffer-file-name))))
  "Directory where wordel source files are stored.")

(defcustom wordel-words-dir (expand-file-name "words/" wordel--source-dir)
  "Directory where word lists are stored.
Wordel ships with multiple word lists.  If you change this directory,
you are responsible for populating it with word lists."
  :type 'directory)

(defcustom wordel-word-list-file (expand-file-name "scrabble" wordel-words-dir)
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

(defcustom wordel-i-am-a-cheater nil
  "When non-nil, you are a cheater."
  :type 'boolean)

;;;; Variables
(defvar wordel-buffer    "*wordel*" "Name of the wordel buffer.")
(defvar wordel--game nil "State of the game.")

;;;; Faces
(defface wordel-correct
  '((t (:background "#538D4E")))
  "Face for a guessed letter which matches its position in the puzzle word.")

(defface wordel-guessed
  '((t (:background "#202020")))
  "Face for a guessed letter which has no other hint information.")

(defface wordel-almost
  '((t (:background "#BF9F3B")))
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
  '((t (:weight ultra-bold :background "#3A3A3C" :foreground "#D7DADC" :height 3.0)))
  "Default face for a wordel letter.")

(defface wordel-error
  '((t (:inherit compilation-error)))
  "Default face for a wordel error message.")

(define-minor-mode wordel-marathon-mode "Variation of wordel with multiple rounds."
  :lighter " Wordel-m"
  (with-current-buffer wordel-buffer
    (if wordel-marathon-mode
        (add-hook 'wordel-mode-hook #'wordel-marathon-mode)
      (remove-hook 'wordel-mode-hook #'wordel-marathon-mode))
    (setq header-line-format (wordel--commands-text))))

(defun wordel--commands-text ()
  "Return commands text."
  (let ((word-list (file-name-nondirectory wordel-word-list-file)))
    (concat
     (propertize (concat "WORDEL" (when wordel-marathon-mode " MARATHON"))
                 'face '(:weight bold))
     " "
     (substitute-command-keys
      (string-join
       (delq
        nil
        (list (when wordel--game "Quit Game: \\[wordel-quit-game]")
              (unless wordel--game "New Game: \\[wordel]")
              "Help: \\[wordel-help]"
              (unless wordel--game "Toggle Marathon: \\[wordel-marathon-mode]")
              (if wordel--game
                  (propertize
                   (format "Word list: %S" word-list)
                   'face '(:weight bold))
                (format "Choose Word list (%s): \\[wordel-choose-word-list]" word-list))))
       " ")))))

(defmacro wordel--with-state (state &rest body)
  "Provide anaphoric bindings to variable STATE during BODY."
  (declare (indent 1) (debug t))
  `(let ((state! ,state))
     (cl-destructuring-bind
         (&key attempts! index! limit! rows! rounds!
               score! start! word! words! wordlen!
               &allow-other-keys)
         ,state
       (ignore state! attempts! index! limit! rows! rounds!
               start! score! word! words! wordlen!) ;pacify bytecompiler
       ,@body)))

(defun wordel--legal-word-p (string length)
  "Return t if STRING is a legal word of LENGTH, nil otherwise."
  (and (= (length string) length)
       (not (string-match-p wordel-illegal-characters string))))

(defun wordel-local-words (n)
  "Return a puzzle word N letters long from `wordel-word-file'."
  (with-temp-buffer
    (insert-file-contents wordel-word-list-file)
    (cl-delete-if-not (lambda (word) (wordel--legal-word-p word n))
                      (split-string (upcase (buffer-string)) "\n"))))

(defun wordel--random-word (candidates)
  "Select a random word from CANDIDATES."
  (nth (random (length candidates)) candidates))

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
            for s = (nth i strings)
            collect (wordel--tile (if current (propertize s 'index i) s)
                                  (when current 'wordel-current-box)))
   (propertize " " 'face 'wordel-spacer)))

(defun wordel--board (rows n limit)
  "Return board with ROWS of length N up to LIMIT.
After ROWS are exhausted, blank rows are printed.
The first blank row is marked current."
  (let ((empty (make-list n " "))
        (len   (length rows)))
    (propertize
     (string-join
      (append rows
              (when (< len limit)
                (append (list (wordel--row empty 'current))
                        (make-list (- limit (1+ len)) (wordel--row empty)))))
      "\n")
     'read-only t)))

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

(defun wordel--words (n)
  "Return a list of words of length N using `wordel-words-function'."
  (or (funcall wordel-words-function n)
      (error "Unable to retrieve candidate words with %S" wordel-words-function)))

(defun wordel--wordlen ()
  "Return an int from `wordel-word-length' spec.
If `wordel-word-length' is a range, return a random int in that range."
  (pcase wordel-word-length
    ((pred numberp) wordel-word-length)
    (`(,min . ,max) (+ min (random (1+ (- max min)))))
    (_ (signal 'wrong-type-error '((numberp consp) wordel-word-length)))))

(defun wordel--state (&optional props)
  "Return game state plist.
If PROPS are non-nil, they are used in place of default values."
  (cl-destructuring-bind (&key
                          (wordlen!  (wordel--wordlen))
                          (words!    (wordel--words wordlen!))
                          (word!     (or (wordel--random-word words!)
                                         (error "Unable to find a puzzle word")))
                          (limit!    wordel-attempt-limit)
                          (attempts! 0)
                          (rounds!   0)
                          (score!    0)
                          rows!
                          &allow-other-keys &aux
                          (start! (current-time)))
      (copy-tree props)
    (list :attempts! attempts!
          :index!    0
          :limit!    limit!
          :rows!     rows!
          :rounds!   rounds!
          :score!    score!
          :start!    start!
          :word!     word!
          :words!    words!
          :wordlen!  wordlen!)))

(defun wordel--filter-inputs ()
  "Filter inputs during game play."
  (when-let (wordel--game
             ((eq real-this-command 'self-insert-command))
             (input (this-command-keys))
             ((or (not (string-match-p wordel-illegal-characters input))
                  (member input '(" ")))))
    (setq this-command 'ignore)
    (wordel--input-char (upcase input))))

;;; Key bindings
(defvar wordel-mode-map (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "M-h")     'wordel-help)
                          (define-key map (kbd "M-q")     'wordel-quit-game)
                          (define-key map (kbd "RET")     'wordel-submit-guess)
                          (define-key map (kbd "DEL")     'wordel-delete-char)
                          (define-key map (kbd "<up>")    'wordel-last-column)
                          (define-key map (kbd "<down>")  'wordel-first-column)
                          (define-key map (kbd "<left>")  'wordel-prev-column)
                          (define-key map (kbd "<right>") 'wordel-next-column)
                          (when wordel-want-evil-row-navigation
                            (define-key map (kbd "H") 'wordel-prev-column)
                            (define-key map (kbd "L") 'wordel-next-column)
                            (define-key map (kbd "0") 'wordel-first-column)
                            (define-key map (kbd "^") 'wordel-first-column)
                            (define-key map (kbd "$") 'wordel-last-column)
                            (define-key map (kbd "E") 'wordel-last-column))
                          map)
  "Keymap for wordle-mode.")

(define-derived-mode wordel-mode text-mode "Wordel"
  "A word game based on 'Wordle' and/or 'Lingo'.

    \\{wordel-mode-map}"
  (add-hook 'pre-command-hook #'wordel--filter-inputs nil t)
  (setq header-line-format (wordel--commands-text))
  (read-only-mode))

(defvar wordel-select-mode-map (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "g") 'wordel)
                                 (define-key map (kbd "h") 'wordel-help)
                                 (define-key map (kbd "m") 'wordel-marathon-mode)
                                 (define-key map (kbd "w") 'wordel-choose-word-list)
                                 map)
  "Keymap for `wordel-select-mode'.")

(define-derived-mode wordel-select-mode special-mode "Wordel-s"
  "Mode to select the type of wordel game to play.

    \\{wordel-mode-map}"
  (setq header-line-format (wordel--commands-text)))

(defun wordel--letter-info (alphabet)
  "Return list of hinted tiles for every letter in the ALPHABET."
  (wordel--with-state wordel--game
    (let ((guessed
           (apply #'append
                  (mapcar (lambda (row)
                            (split-string (wordel--row-to-word row) "" 'omit-nulls))
                          rows!)))
          (alphabet (split-string alphabet "" 'omit-nulls)))
      (cl-loop for letter in alphabet
               collect (wordel--tile
                        (if-let ((guess (member letter guessed)))
                            (let* ((duplicates (cl-remove-if-not
                                                (lambda (it) (equal (car guess) it))
                                                guess))
                                   (hints
                                    (mapcar (lambda (guess)
                                              (get-text-property 0 'hint guess))
                                            duplicates)))
                              (propertize (car duplicates) 'hint
                                          (cond
                                           ((member 'wordel-correct hints) 'wordel-correct)
                                           ((member 'wordel-almost hints)  'wordel-almost)
                                           (t                              'wordel-guessed))))
                          (wordel--tile letter)))))))

(defun wordel--insert-board ()
  "Insert the game board."
  (with-current-buffer (get-buffer-create wordel-buffer)
    (unless (derived-mode-p 'wordel-mode) (wordel-mode))
    (wordel--with-state wordel--game
      (with-silent-modifications
        (erase-buffer)
        (goto-char (point-min))
        (insert (wordel--board (reverse rows!) wordlen! limit!))
        (insert (propertize (format "\n\n%s\n\n" (propertize " " 'message-area t)))
                (propertize "\n " 'read-only t))
        (when wordel-show-letter-info
          (let ((letters (wordel--letter-info "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
            (dotimes (i (length letters))
              (when (zerop (% i 13)) (insert (propertize "\n" 'readonly t)))
              (insert (propertize (concat (nth i letters) (propertize " " 'face 'wordel-spacer)) 'read-only t)))))))))

(defun wordel--split-with-spaces (string)
  "Split STRING, keeping its spaces."
  (let ((s (split-string string ""))) (pop s) (butlast s)))

(defun wordel--comparison (guess subject)
  "Return propertized GUESS character list compared against SUBJECT."
  (let* ((subjects (split-string subject "" 'omit-nulls))
         ;; Keep spaces in complete rows that result from pause.
         (guesses  (wordel--split-with-spaces guess))
         (matches nil))
    (cl-loop for i from 0 to (1- (length guesses))
             for g = (nth i guesses)
             for s = (nth i subjects)
             collect (propertize g 'hint
                                 (cond
                                  ((string-match-p g s)
                                   (push g matches) 'wordel-correct)
                                  ((and (string-match-p g subject)
                                        (not (string-match-p g guess (1+ i)))
                                        (not (member g matches)))
                                   'wordel-almost)
                                  (t 'wordel-guessed))))))

(defun wordel--rules ()
  "Return the rules string."
  (string-join
   (list
    (propertize "How to Play" 'face 'wordel-default)
    "Type a letter into each box to guess the secret word."
    (concat "Press "
            (substitute-command-keys "\\<wordel-mode-map>\\[wordel-submit-guess]")
            " to submit your guess.")
    "Each letter in your guess will be color coded to give you hints. For example:"
    "\n"
    (wordel--row (wordel--comparison "ZIPPO" "EMACS"))
    "None of the guessed letters match the word."
    "\n"
    (wordel--row (wordel--comparison "CLUED" "EMACS"))
    "The letters \"C\" and \"E\" are in the word, but not in the right spot."
    "\n"
    (wordel--row (wordel--comparison "MACES" "EMACS"))
    "The letter \"S\" is in the word and in the correct spot."
    "\n"
    (wordel--row (wordel--comparison "EMACS" "EMACS"))
    "The word was guessed correctly."
    "\n"
    "The game is over when the word is guessed correctly, \
or the player runs out of table rows to guess in."
    "\n"
    (propertize "Marathon Mode" 'face 'wordel-default)
    "Each round is a puzzle as described above."
    "The length of the word increases by one each round."
    "Every 3 rounds, the number of guesses is reduced by one."
    "Rounds continue until the puzzle word length exceeds the longest \
candidate word returned by wordel-words-function.")
   "\n"))

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

(defun wordel--valid-guess-p (guess length words)
  "Return t if GUESS is valid.
It's LENGTH and content are checked via `wordel--legal-word-p'.
It also must be a `member' of WORDS."
  (or (and (wordel--legal-word-p guess length)
           (member guess words))
      (wordel--display-error
       (if (string-match-p wordel-illegal-characters guess)
           "Not enough letters: %S"
         "Word not in word list: %S")
       guess)))

;;;###autoload
(defun wordel (&optional state)
  "Start a new game.
If STATE is non-nil, it is used in lieu of `wordel--game'."
  (interactive)
  (wordel--with-state (setq wordel--game (wordel--state state))
    (wordel--insert-board)
    (with-current-buffer wordel-buffer
      (unless wordel-i-am-a-cheater (wordel-integrity-mode))
      (wordel--position-cursor index!)
      (pop-to-buffer-same-window (current-buffer)))))

;;;###autoload
(defun wordel-marathon ()
  "Start a new marathon."
  (interactive)
  (wordel)
  (with-current-buffer wordel-buffer (wordel-marathon-mode)))

;;;###autoload
(defun wordel-help ()
  "Display game rules."
  (interactive)
  (let ((b (concat wordel-buffer "<help>")))
    (with-current-buffer (get-buffer-create b)
      (special-mode)
      (with-silent-modifications
        (erase-buffer)
        (insert (wordel--rules)))
      (goto-char (point-min))
      (pop-to-buffer-same-window (current-buffer)))))

;;;###autoload
(defun wordel-choose-word-list (path)
  "Set `wordel-word-list-file' to file at PATH."
  (interactive (list (read-file-name "Word list: "
                                     wordel-words-dir wordel-word-list-file t)))
  (setq wordel-word-list-file path)
  (if (equal (buffer-name) wordel-buffer)
      (setq header-line-format (wordel--commands-text))
    (message "Wordel wordlist set to %S" (file-name-nondirectory path))))

(defun wordel--row-to-word (row)
  "Return character display properties of ROW."
  (mapconcat
   (lambda (string)
     (cond ((get-text-property 0 'index string)
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

(defun wordel--clean-up ()
  "Disable gameplay modes; clean state."
  (wordel-integrity-mode -1)
  (setq wordel--game nil)
  ;; Leaving point on the board gives impression that input can still be given.
  (goto-char (point-max))
  (wordel-select-mode))

(defun wordel-submit-guess ()
  "Submit the current guess."
  (interactive)
  (wordel--with-state wordel--game
    (when-let ((guess (wordel--current-word))
               ((wordel--valid-guess-p guess wordlen! words!)))
      (setf (cl-getf state! :attempts!) (cl-incf attempts!)
            (cl-getf state! :rows!)
            (push (wordel--row (wordel--comparison guess word!)) rows!))
      (wordel--insert-board)
      (cond
       ((equal guess word!)
        ;; Prevent current row from being inserted
        (setf (cl-getf state! :rows!)
              (append (make-list (- limit! attempts!)
                                 (wordel--row (make-list wordlen! " ")))
                      rows!))
        (wordel--insert-board)
        (if wordel-marathon-mode
            (condition-case-unless-debug _
                (wordel (list :rounds!  (cl-incf rounds!)
                              :wordlen! (cl-incf wordlen!)
                              :limit!   (if (zerop (mod rounds! 3))
                                            (cl-decf limit!)
                                          limit!)
                              :score! (+ rounds! (- limit! attempts!))))
              ((error)
               (wordel--display-message
                "MARATHON COMPLETE! Final Score: %d" score!)
               (wordel--clean-up)))
          (wordel--display-message "You WON!"))
        (unless wordel-marathon-mode (wordel--clean-up)))
       ((>= attempts! limit!)
        (apply #'wordel--display-message
               `(,(concat "YOU LOST! The word was %S."
                          (when wordel-marathon-mode " Final Score: %d"))
                 ,@(delq nil (list word! (when wordel-marathon-mode score!)))))
        (wordel--clean-up))
       (t (wordel--position-cursor (setf (cl-getf state! :index!) 0)))))))

(defun wordel--display-char (char)
  "Display CHAR in current box."
  (with-current-buffer wordel-buffer
    (with-silent-modifications
      (let ((p (point))) (put-text-property p (1+ p) 'display char)))))

(defun wordel--clamp (n min max)
  "Return N if it is between MIN and MAX.
Otherwise whichever is closer."
  (min (max n min) max))

(defun wordel--input-char (character)
  "Input CHARACTER in current column."
  (wordel--with-state wordel--game
    (wordel--position-cursor index!)
    (wordel--display-char character)
    (setf index! (wordel--clamp (cl-incf index!) 0 (1- (length word!)))
          (cl-getf state! :index!) index!)
    (wordel--position-cursor index!)))

(defun wordel-delete-char ()
  "Delete the character in the current column.
Move point to previous column."
  (interactive)
  (wordel--with-state wordel--game
    (wordel--display-char " ")
    (setf index! (wordel--clamp (cl-decf index!) 0 (1- (length word!)))
          (cl-getf state! :index!) index!)
    (wordel--position-cursor index!)))

(defun wordel--change-col (direction n)
  "Move cursor in DIRECTION N columns."
  (wordel--with-state wordel--game
    (let ((index! (wordel--clamp
                   (if (eq direction 'prev) (cl-decf index! n) (cl-incf index! n))
                   0 (1- (length word!)))))
      (setf (cl-getf state! :index!) index!)
      (wordel--position-cursor index!))))

(defun wordel-next-column (&optional n)
  "Move forward N columns."
  (interactive "p")
  (wordel--change-col 'next n))

(defun wordel-prev-column (&optional n)
  "Move forward N columns."
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

(defun wordel-quit-game ()
  "Quit the current game."
  (interactive)
  (when-let ((word! (plist-get wordel--game :word!)))
    (apply #'wordel--display-message
           (if wordel-marathon-mode
               (list "You're not in marathon shape yet. The word was %S. Final Score: %d"
                 word! (plist-get wordel--game :score!))
             (list "The word was %S, quitter." word!)))
    (wordel--clean-up)))

(defun wordel-integrity-p (player &rest integrity)
  "Return t if the PLAYER has INTEGRITY, nil otherwise."
  (if (eq (car integrity) 'wordel--game)
      (with-current-buffer (get-buffer-create "*HELP!!!!*")
        (with-silent-modifications
          (erase-buffer)
          (insert
           (format
            "wordel--game is a variable defined in ‘%s’."
            (propertize "home/users/cheater/couldnt-help-myself.el" 'face 'button))))
        (help-mode)
        (display-buffer (current-buffer)))
    (apply player integrity)))

(define-minor-mode wordel-integrity-mode
  "Disabling this mode may cause spiritual damage."
  :lighter " wordel-integrity"
  (if wordel-integrity-mode
      (advice-add #'describe-variable :around #'wordel-integrity-p)
    (advice-remove #'describe-variable #'wordel-integrity-p)))

(provide 'wordel)
;;; wordel.el ends here
