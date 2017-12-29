;; nyackager-git.el --- 
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

(cl-defun nyackager--git-clone (&key (uri         nil uri-p)
                                     (destination nil destination-p)
                                     (revision    nil branch-p)
                                     (branch      nil revision-p))
  "Fetch repo from SOURCE-URI with DESTINATION-DIR"
  (unless (and uri-p destination-p)
    (error "nyackager--git-clone: uri and destination are required"))

  (when (and branch-p revision-p)
    (error "nyackager--git-clone: branch xor revision should be provided"))

  (shell-command-to-string (format "git clone %s %s"
                                   uri
                                   destination))
  (when branch
    (shell-command-to-string (format "cd %s; git branch %s"
                                     destination
                                     branch)))

  (when revision
    (shell-command-to-string (format "cd %s; git checkout %s"
                                     destination
                                     revision))))

(provide 'nyackager-git)
;;; nyackager-git.el ends here
