;;; gpt.el --- GPT-3 frontend for Emacs

;; Copyright (C) Damien Gonot
;; Author: Damien Gonot <damien.gonot@gmail.com>

;;; Commentary:
;; Basic commands to use the OpenAI API and let us interact with GPT-3 within Emacs

;;; Code:
(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'env)
  (require 'json))

(defgroup gpt nil
  "GPT-3 frontend."
  :group 'convenience
  :prefix "gpt-")

(defcustom gpt-max-tokens 1024
  "Upper limit on the number of tokens the API will return."
  :type 'integer)

(defvar gpt-buffer "*GPT-3*"
  "Title of the buffer used to store the results of an OpenAI API query.")

(define-error 'gpt-error "An error related to gpt.el")

(define-error 'gpt-parsing-error
  "An error caused by a failure to parse an OpenAI API Response")

(defmacro gpt-show-results-buffer-if-active ()
  "Show the results in other window if necessary."
  `(if (and (not ;; visible
             (get-buffer-window gpt-buffer))
	    (called-interactively-p 'interactive))
       (lambda (&optional buf) (ignore buf)
         (with-current-buffer buf
           (view-mode t))
         (switch-to-buffer-other-window gpt-buffer))
     #'identity))

;;;###autoload
(defun gpt-prompt (prompt callback)
  "Query OpenAI with PROMPT calling the CALLBACK function on the resulting buffer.
Returns buffer containing the text from this query"
  (interactive (list (read-string "Prompt GPT-3 with: ")
                     (lambda (buf) (with-current-buffer buf
                                (view-mode t))
                       (switch-to-buffer-other-window gpt-buffer))))
  (gpt--query-open-api prompt
                           (lambda (results)
                             (with-current-buffer (get-buffer-create gpt-buffer)
                               ;; Erase contents of buffer after receiving response
                               (read-only-mode -1)
                               (erase-buffer)
                               (insert results)
                               ;; Return the gpt output buffer for non interactive usage
                               (funcall callback (current-buffer))))))

;;;###autoload
(defun gpt-fix-region (BEG END)
  "Takes a region from BEG to END and asks GPT-3 to explain what's wrong with it.
It then displays the answer in the `gpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (gpt-prompt (gpt--append-to-prompt
                     current-code
                     "Why doesn't this code work?")
                    (gpt-show-results-buffer-if-active))))

;;;###autoload
(defun gpt-explain-region (BEG END)
  "Takes a region from BEG to END and asks GPT-3 what it does.
The answer in the displays in `gpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (gpt-prompt (gpt--append-to-prompt
                     current-code
                     "What does this code do?")
                    (gpt-show-results-buffer-if-active))))

;;;###autoload
(defun gpt-gen-tests-for-region (BEG END)
  "Takes a region from BEG to END and asks GPT-3 to write a test for it.
It then displays the answer in the `gpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (gpt-prompt (gpt--append-to-prompt
                     current-code
                     "Write me a tests for this code")
                    (gpt-show-results-buffer-if-active))))

;; TODO currently just says what changed but doesn't wanna show the code itself
;; (defun gpt-optimize-region (BEG END)
;;   "Takes a region from BEG to END asks GPT-3 to optimize it for speed.
;; It then displays the answer in the `gpt-buffer'."
;;   (interactive "r")
;;   (let ((current-code         (buffer-substring BEG END)))
;;     (gpt-prompt (gpt--append-to-prompt
;;                      current-code
;;                      "Refactor this code for speed and tell me what you changed and why it's faster")
;;                     (gpt-show-results-buffer-if-active))))

;;;###autoload
(defun gpt-refactor-region (BEG END)
  "Takes a region from BEG to END and asks GPT-3 to refactor it.
It then displays the answer in the `gpt-buffer'."
  (interactive "r")
  (let ((current-code         (buffer-substring BEG END)))
    (gpt-prompt (gpt--append-to-prompt
                     current-code
                     "Refactor this code and tell me what you changed")
                    (gpt-show-results-buffer-if-active))))

;;;###autoload
(defun gpt-prompt-region (BEG END)
  "Prompt GPT-3 with the region from BEG to END.
It then displays the results in a separate buffer `gpt-buffer'."
  (interactive "r")
  (gpt-prompt (buffer-substring BEG END)
                  ;; Show the results if not already being viewed
                  (gpt-show-results-buffer-if-active)))

;;;###autoload
(defun gpt-prompt-region-and-replace (BEG END)
  "Replace region from BEG to END with the response from the GPT-3 API.

The region is BEG and until END"
  (interactive "r")

  (let ((og-buf (current-buffer)))
    (gpt-prompt (buffer-substring BEG END)
                    (lambda (buf)
                      (save-excursion
                        (with-current-buffer og-buf
                          (delete-region BEG END)
                          (goto-char BEG)
                          (insert (with-current-buffer buf (buffer-string)))))))))
(defun gpt--append-to-prompt (prompt comment-str)
  "Append the string COMMENT-STR extra information to a PROMPT as a comment."
  (concat prompt
          "\n"
	  comment-start
          " "
	  comment-str))

(defun gpt--extract-text-from-query (query-result)
  "Extract the resulting text from a given OpenAI response QUERY-RESULT."
  (condition-case err
      (thread-last query-result
                   (assoc-default 'choices)
                   seq-first
                   (assoc-default 'text)
                   string-trim)
    (error
     (signal 'gpt-parsing-error err))))

(defun gpt--parse-response (status callback)
  "Ignoring STATUS and parse the response executing the CALLBACK function
on the resulting string."
  (ignore status)
  ;; All this is ran inside the buffer containing the response
  (goto-char 0)
  (re-search-forward "^$")
  (funcall callback (gpt--extract-text-from-query (json-read))))

(defun gpt--query-open-api (prompt callback)
  "Send a string PROMPT to OpenAI API and pass the resulting buffer to CALLBACK.
The environment variable OPENAI_API_KEY is used as your API key

You can register an account here
https://beta.openai.com/docs/introduction/key-concepts"
  (let* ((api-key (getenv "OPENAI_API_KEY"))
         (url-request-method (encode-coding-string "POST" 'us-ascii))
	 (url-request-extra-headers `(("Content-Type" . "application/json")
				      ("Authorization" . ,(format "Bearer %s" api-key))))
         (url-request-data (json-encode
			    `(("model" . "text-davinci-003")
			      ("prompt" . ,prompt)
			      ("max_tokens" . ,gpt-max-tokens)
			      ("temperature" . 0)))))
    (cl-assert (not (string= "" api-key))
               t
               "Current contents of the environmental variable OPENAI_API_KEY
are '%s' which is not an appropriate OpenAI token please ensure
you have the correctly set the OPENAI_API_KEY variable"
               api-key)
    (url-retrieve
     "https://api.openai.com/v1/completions"
     'gpt--parse-response
     (list callback))))

(provide 'gpt)
;;; gpt.el ends here
