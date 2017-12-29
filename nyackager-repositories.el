;; nyackager-github.el --- 
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

(require 'nyackager-git)
(require 'nyackager-utils)

(defun nyackager--github-repo-p (author name branch)
  "Returns t if "
  (let ((page (format "https://raw.githubusercontent.com/%s/%s/%s/manifest.nyaifest"
                      author name branch)))
    (with-current-buffer (url-retrieve-synchronously page)
      (goto-char (point-min))
      (not (string-match "HTTP/1.1 404 Not Found"
                         (buffer-substring (line-beginning-position)
                                           (line-end-position)))))))

(defun nyackager--subscribe-github (path priority)
  ""
  (unless (string-match "\\([a-zA-Z0-9]+/[a-zA-Z0-9]+\\|[a-zA-Z0-9]+/[a-zA-Z0-9]+/[a-zA-Z0-9]+\\)"
                        path)
    (error "nyackager--subscribe-github: incorrect path format: %s" path))

  (let* ((--splitted (split-string path "/"))
         (--author   (nth 0 --splitted))
         (--name     (nth 1 --splitted))
         (--branch   (or (nth 2 --splitted)
                         "master"))
         (--sub      ()))

    (when (nyackager--github-repo-p --author --name --branch)
      (nyackager--plist-add* --sub
                             'repo     'github
                             'author   --author
                             'name     --name
                             'branch   --branch
                             'priority priority))
    --sub))

(defun nyackager--download-repo-github (sub)
  "Download rep from github."
  (let* ((--repo-author (plist-get sub 'author))
         (--repo-branch (plist-get sub 'branch))
         (--repo-name   (plist-get sub 'name))
         (--repo-source (nyackager--path-join "https://github.com"
                                              --repo-author
                                              --repo-name))
         (--repo-dest   (nyackager--path-join nyackager-recipe-directory
                                              --repo-name)))
    (nyackager--git-clone :uri         --repo-source
                          :destination --repo-dest
                          :branch      --repo-branch)))

(defun nyackager--download-recipes-github (sub manifest)
  "Download recipes from git"
  (dolist (--form manifest)
    (when (eq (car --form)
              'packages)
      (dolist (--package (cdr --form))
        (nyackager--download-recipe-github sub --package)))))

(provide 'nyackager-repositories)
;;; nyackager-repositories.el ends here
