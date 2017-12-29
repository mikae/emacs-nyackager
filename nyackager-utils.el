;; nyackager-utils.el --- 
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL: 
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; .
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun nyackager--path-join (&rest args)
  (cl-reduce (lambda (acc it)
               (concat (file-name-as-directory acc)
                       it))
             args))

(defmacro nyackager--plist-add (plist key value)
  `(setq ,plist
         (plist-put ,plist ,key ,value)))

(defmacro nyackager--plist-add* (plist &rest args)
  `(let ((--list (list ,@args)))
     (cl-loop for key   in --list       by #'cddr
              for value in (cdr --list) by #'cddr
              do
              (nyackager--plist-add ,plist key value))))

(defun nyackager--read-sexps (filepath)
  "Read list of sexps from a file FILEPATH."
  (if (not (file-readable-p filepath))
      (error "read-sexp: file %s is not readable" filepath)
    (with-temp-buffer
      (save-excursion
        (insert "(progn\n")
        (insert-file-contents filepath)
        (goto-char (point-max))
        (insert "\n)\n"))
      (cdr (read (current-buffer)))))) ;; cdr is needed for dropping the progn

(defun nyackager--write-sexps (filepath sexps)
  "Write SEXPS to file FILEPATH.
SEXPS is a list of s-exps."
  (with-temp-file filepath
    (dolist (sexp sexps)
      (prin1 sexp (current-buffer)))))

(defmacro nyackager--chain (base &rest funcs)
  "Represents concept of sequential transferring data to the list of executers.
BASE - is the basic data.
FUNCS - is the list of list forms, where result of execution of the previous form will be added.
It's able to use placeholders(sign _) to tell, as which argument previous result should be interpreted.
All placeholder will be replaced as previous result.
If no placeholder is used, result is added to the end of the form."
  `(cl-reduce (lambda (acc func)
                (let ((func-name (car func))
                      (func-args (cdr func)))
                  (setq func-args
                        (if (cl-find '_ func-args)
                            func-args
                          (append func-args '(_))))
                  (apply func-name (cl-mapcar (lambda (form)
                                                (if (eq form '_)
                                                    acc
                                                  (eval form)))
                                              func-args))))
              ',funcs
              :initial-value ,base))

(defun nyackager--version->= (version1 version2)
  "Parses V1 and V2 and returns the latest version."
  (nyackager--chain (cl-mapcar (lambda (elem1 elem2)
                                 (>= elem1 elem2))
                               (cl-mapcar #'string-to-number
                                          (split-string version1))
                               (cl-mapcar #'string-to-number
                                          (split-string version2)))
                    (cl-reduce (lambda (acc res)
                                 (and acc res)))))

(defun nyackager--recreate-directory (dirpath)
  "Remove and create directory DIRPATH."
  (when (and (file-exists-p dirpath))
    (if (file-accessible-directory-p dirpath)
        (delete-directory dirpath :recursive)
      (error "nyackager--recreate-directory: not a directory: %s" dirpath)))

  (mkdir dirpath :parents))

(defun nyackager--touch-file (filepath)
  "Touch file FILEPATH."
  (when (and (not (file-exists-p filepath))
             (file-accessible-directory-p (file-name-directory filepath)))
    (with-temp-buffer
      (write-file filepath))))

(defun nyackager--recipe-dependencies-p (recipe-dependencies)
  "Return t, if dependencies are correct list"
  (> (length recipe-dependencies)
     0))

(provide 'nyackager-utils)
;;; nyackager-utils.el ends here
