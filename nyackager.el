;; nyackager.el ---
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

(require 'cl-lib)
(require 'nyackager-utils)
(require 'nyackager-repositories)
(require 'nyackager-git)

(defconst nyackager-subscription-default-priority 0
  "Default priority for recipe repositories.")

(defconst nyackager-subscription-minimal-priority -9999
  "Minimal priority for recipe repositories.")

(defconst nyackager-subscription-maximal-priority 9999
  "Maximal priority for recipe repositories.")

(defconst nyackager-build-directory (concat (file-name-as-directory "/")
                                            (file-name-as-directory "tmp")
                                            (file-name-as-directory ".nyackager")
                                            "build")
  "Nyackager build directory")

(defvar nyackager-recipe-directory (concat (file-name-as-directory user-emacs-directory)
                                           (file-name-as-directory ".nyackager")
                                           "recipes")
  "Directory, where recipe would be downloaded.")

(defvar nyackager-results-directory (concat (file-name-as-directory user-emacs-directory)
                                            (file-name-as-directory ".nyackager")
                                            "results")
  "Directory, where results of recipe execution will be contained.")

(defvar nyackager-lock-file (concat (file-name-as-directory user-emacs-directory)
                                    (file-name-as-directory ".nyackager")
                                    "lockfile.nyalock")
  "Lockfile for recipes.")

(defvar nyackager--subscriptions nil
  "List of subscriptions.")

(cl-defun nyackager-subscribe (&key (repo     nil repo-p)
                                    (path     nil path-p)
                                    (priority nyackager-subscription-default-priority))
  "Subsribe to recipe repository."
  (unless (and repo-p
               path-p)
    (error "nyackager-subscribe: options aren't correct."))

  (add-to-list 'nyackager--subscriptions
               (cond
                ((eq repo
                     'github)
                 (nyackager--subscribe-github path priority)))))

(defun nyackager-clear-subscriptions ()
  "Clear subscriptions."
  (setq nyackager--subscriptions nil))

(defun nyackager-get-subscriptions ()
  "Get subscriptions."
  nyackager--subscriptions)

(defun nyackager--manifest-destination (sub)
  "Return manifest destination path."
  (nyackager--path-join nyackager-recipe-directory
                        (plist-get sub 'name)
                        "manifest.nyaifest"))

(defun nyackager--subscription-priority (sub)
  "Return subscription priority."
  (plist-get sub 'priority))

(defun nyackager-update-tree ()
  "Updates manifest files and downloads recipes."
  (nyackager--recreate-directory nyackager-recipe-directory)
  (nyackager--touch-file nyackager-lock-file)

  (dolist (--sub (nyackager-get-subscriptions))
    (let ((--repo              (plist-get --sub 'repo)))
      (cond
       ((eq --repo
            'github)
        (nyackager--download-repo-github --sub))))))

(defun nyackager--execute-recipe (repo-name recipe-name recipe-version)
  "Execute recipe from REPO-NAME with RECIPE-NAME and RECIPE-VERSION."
  (let* ((--recipe-path (nyackager--path-join nyackager-recipe-directory
                                              repo-name
                                              recipe-name
                                              (concat recipe-version
                                                      ".nyacipe")))
         (--result-directory (nyackager--path-join nyackager-results-directory
                                                   recipe-name))
         (--recipe-build-directory (nyackager--path-join nyackager-build-directory
                                                         emacs-version
                                                         repo-name
                                                         recipe-name
                                                         recipe-version))
         (--recipe (nyackager--read-sexps --recipe-path))
         ;; todo parse more effectively
         ;; write general sexp parser function
         (--recipe-fetcher       (cadr (assoc 'fetcher      --recipe)))
         (--recipe-uri           (cadr (assoc 'uri          --recipe)))
         (--recipe-rev           (cadr (assoc 'rev          --recipe)))
         (--recipe-dependencies  (cdr  (assoc 'dependencies --recipe)))
         (--recipe-clean-steps   (cdr  (assoc 'clean        --recipe)))
         (--recipe-compile-steps (cdr  (assoc 'compile      --recipe)))
         (--recipe-install-steps (cdr  (assoc 'install      --recipe))))
    (nyackager--recreate-directory --recipe-build-directory)
    (nyackager--recreate-directory --result-directory)

    (when (nyackager--recipe-dependencies-p --recipe-dependencies)
      (dolist (--dependency --recipe-dependencies)
        (let ((--dependency-name    (cl-getf --dependency :name))
              (--dependency-version (cl-getf --dependency :version)))
          (when --dependency-name
            (nyackager-execute-recipe --dependency-name
                                      --dependency-version)))))

    (cond
     ((eq --recipe-fetcher 'git)
      (nyackager--git-clone :uri         --recipe-uri
                            :destination --recipe-build-directory
                            :revision    --recipe-rev)))

    (dolist (--compile-step --recipe-compile-steps)
      (cond
       ((eq (car --compile-step)
            'byte-compile)
        (dolist (--file (cl-getf (cdr --compile-step)
                                 :files))
          (byte-compile-file (nyackager--path-join --recipe-build-directory
                                                   --file))))))

    (dolist (--install-step --recipe-install-steps)
      (cond
       ((eq (car --install-step)
            'copy)
        (dolist (--file (cl-getf (cdr --install-step)
                                 :what))
          (copy-file (nyackager--path-join --recipe-build-directory
                                           --file)
                     (nyackager--path-join --result-directory
                                           --file))))))

    (add-to-list 'load-path --result-directory)))

(defun nyackager--select-latest-version (versions)
  "Select latest version of VERSIONS
`versions' is the list of strings.
If string `\"latest\"' in the `versions', then it would be returned."
  (or (cl-find "latest" versions :test #'equal)
      (cl-reduce (lambda (acc item)
                   (if (nyackager--version->= acc item)
                       acc
                     item))
                 versions)))

(defun nyackager-execute-recipe (recipe-name &optional recipe-version)
  "Execute recipe with RECIPE-NAME.
It will fetch data, compile, and install it into target directory."
  ;; todo add error checking
  (nyackager--chain (nyackager-get-subscriptions)
                    ;; parse repository file structure
                    (cl-mapcar (lambda (sub)
                                 (let* ((--repo-name (plist-get sub 'name))
                                        (--repo-path (nyackager--path-join nyackager-recipe-directory
                                                                           --repo-name)))
                                   (list (nyackager--subscription-priority sub)
                                         (nyackager--chain (nyackager--subdirectories --repo-path :both)
                                                           (cl-mapcar (lambda (lst)
                                                                        (list :name     (cadr lst)
                                                                              :versions (nyackager--chain (nyackager--subfiles (car lst) :relative)
                                                                                                          (cl-mapcar (lambda (filename)
                                                                                                                       (file-name-base filename))))))))
                                         --repo-name))))
                    ;; remove repos that doesn't have target recipe
                    (cl-remove-if-not (lambda (item)
                                        (nyackager--chain item
                                                          (nth 1)
                                                          (cl-mapcar (lambda (recipe-form)
                                                                       (equal (cl-getf recipe-form :name)
                                                                              recipe-name)))
                                                          (cl-reduce (lambda (acc has)
                                                                       (or acc has))))))
                    ;; sort by priority
                    (cl-sort _ (lambda (item)
                                 (nth 0 item)))
                    ;; take the most prioritized repo
                    (car)
                    ;; get rid of priority
                    (cdr)
                    ;; execute recipe
                    (funcall (lambda (lst)
                               (let* ((item      (car lst))
                                      (repo-name (cadr lst))
                                      (recipe    (cl-find-if (lambda (--recipe)
                                                               (equal (cl-getf --recipe :name)
                                                                      recipe-name))
                                                             item))
                                      (versions  (cl-getf recipe :versions))
                                      (target-version (or recipe-version
                                                          (nyackager--select-latest-version versions))))
                                 (nyackager--execute-recipe repo-name
                                                            recipe-name
                                                            target-version)
                                 (list repo-name
                                       recipe-name
                                       target-version))))
                    ;; update lock file
                    (funcall (lambda (lst)
                               (cl-destructuring-bind (repo-name recipe-name recipe-version) lst
                                 (nyackager--chain (nyackager--load-lock-file)
                                                   (nyackager--remove-lock recipe-name)
                                                   (nyackager--add-lock repo-name recipe-name recipe-version)
                                                   (nyackager--save-lock-file)))))))

(defun nyackager--find-lock (recipe-name lock)
  "Find lock item that represents recipe RECIPE-NAME."
  (cl-find-if (lambda (lock-item)
                (equal (cl-getf lock-item :name)
                       recipe-name))
              lock))

(defun nyackager--remove-lock (recipe-name lock)
  "Remove lock in lock file."
  (cl-remove-if (lambda (item)
                  (equal (cl-getf item :name)
                         recipe-name))
                lock))

(defun nyackager--add-lock (repo-name recipe-name recipe-version lock)
  "Returns new lock file"
  (cons (list :repo repo-name
              :name recipe-name
              :version recipe-version)
        lock))

(defun nyackager--load-lock-file ()
  "Load lock file and return it as list of sexps."
  ;; car is needed because read-sexp-list return list of actual contents
  (nyackager--read-sexps nyackager-lock-file))

(defun nyackager--save-lock-file (lock)
  "Clear lock-file and save LOCK to lock-file."
  (nyackager--write-sexps nyackager-lock-file lock))

(defun nyackager-recipe-installed (recipe-name)
  "Return t, if recipe RECIPE-NAME is installed."
  (not (null (nyackager--find-lock recipe-name
                                   (nyackager--load-lock-file)))))

(provide 'nyackager)
;;; nyackager.el ends here
