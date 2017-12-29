;; nyackager-test.el --- 
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

(require 'buttercup)
(require 'f)

(require 'nyackager)

(defconst default-load-path load-path)
(defmacro setup-variables ()
  "Setup `nyackager-' variables."
  '(before-each
     (setq nyackager-recipe-directory "/tmp/.nyackager/recipes")
     (setq nyackager-results-directory "/tmp/.nyackager/results")
     (setq nyackager-lock-file "/tmp/.nyackager/lock.nyalock")

     (when (f-dir-p nyackager-recipe-directory)
       (f-delete nyackager-recipe-directory :force))

     (when (f-dir-p nyackager-results-directory)
       (f-delete nyackager-results-directory :force))

     (when (f-file-p nyackager-lock-file)
       (f-delete nyackager-lock-file :force))

     (setq load-path default-load-path)
     (nyackager-clear-subscriptions)

     (dolist (--feature '(test-package
                          test-package-2
                          very-test-package))
       (when (featurep --feature)
         (unload-feature --feature)))))

(describe "Subscriptions"
  (describe "nyackager-get-subscriptions"
    (it "Return nil, when no subscriptions"
      (expect (nyackager-get-subscriptions)
              :to-be nil)))

  (describe "nyackager-subscribe"
    (describe "Raises error if subscription options aren't correct"
      (it "fetcher is not selected"
        (expect (nyackager-subscribe :path "nyackagertester/nyackager-repo")
                :to-throw))

      (it "path is not selected"
        (expect (nyackager-subscribe :repo 'github)
                :to-throw))

      (it "path is not correct"
        (expect (nyackager-subscribe :repo 'github
                                     :path "ttt")
                :to-throw)))

    (it "Adds subscription"
      (nyackager-subscribe :repo     'github
                           :path     "nyackagertester/nyackager-repo"
                           :priority 101)
      (let ((--sub (car (nyackager-get-subscriptions))))
        (expect (plist-get --sub 'repo)
                :to-be 'github)

        (expect (plist-get --sub 'author)
                :to-equal "nyackagertester")

        (expect (plist-get --sub 'name)
                :to-equal "nyackager-repo")

        (expect (plist-get --sub 'branch)
                :to-equal "master")

        (expect (plist-get --sub 'priority)
                :to-equal 101)))

    (it "Doesn't add a subscription if repo doesn't exist"
      (nyackager-subscribe :repo     'github
                           :path     "nyackagertester/test-not-repo"
                           :priority 101)
      (let ((--sub (car (nyackager-get-subscriptions))))
        (expect --sub
                :to-be nil)))))

(describe "nyackager-update-tree"
  (setup-variables)

  (it "Downloads manifest files"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (expect (f-exists-p (f-join nyackager-recipe-directory
                                "nyackager-repo"
                                "manifest.nyaifest"))
            :to-be t))

  (it "Downloads recipes"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (expect (f-exists-p (f-join nyackager-recipe-directory
                                "nyackager-repo"
                                "test-package"
                                "0.1.nyacipe"))
            :to-be t))
  )

(describe "nyackager-execute-recipe"
  (setup-variables)

  (it "Executes recipe"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (nyackager-execute-recipe "test-package" "0.2")
    (expect (f-exists-p (f-join nyackager-results-directory
                                "test-package"
                                "test-package.elc")))
    (require 'test-package)
    (expect test-package-version
            :to-equal 0.2))

  (it "Executes recipe by recipe version"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (nyackager-execute-recipe "test-package" "0.1")
    (expect (f-exists-p (f-join nyackager-results-directory
                                "test-package"
                                "test-package.elc")))
    (require 'test-package)
    (expect test-package-version
            :to-equal 0.1))

  (it "Executes recipe and it's dependencies"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (nyackager-execute-recipe "very-test-package")
    (expect (nyackager-recipe-installed "test-package")
            :to-be t)
    (expect (nyackager-recipe-installed "very-test-package")
            :to-be t)
    (expect (require 'test-package)
            :to-be 'test-package)
    (expect (require 'very-test-package)
            :to-be 'very-test-package))

  (it "Executes dependencies by versions"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (nyackager-execute-recipe "very-test-package" "0.1")
    (require 'test-package)
    (require 'very-test-package)
    (expect test-package-version
            :to-equal 0.1))
  )

(describe "nyackager-recipe-installed-p"
  (setup-variables)

  (it "Returns nil if no package is installed"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (expect (nyackager-recipe-installed "test-package")
            :to-be nil))

  (it "Returns t if package is installed"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (nyackager-execute-recipe "test-package")
    (expect (nyackager-recipe-installed "test-package")
            :to-be t))

  (it "Integrity 1"
    (nyackager-subscribe :repo     'github
                         :path     "nyackagertester/nyackager-repo"
                         :priority 101)

    (nyackager-update-tree)
    (nyackager-execute-recipe "test-package")
    (nyackager-execute-recipe "test-package-2")
    (expect (nyackager-recipe-installed "test-package")
            :to-be t)
    (expect (nyackager-recipe-installed "test-package-2")
            :to-be t)
    )
  )

;; Local Variables:
;; eval: (put 'describe    'lisp-indent-function 'defun)
;; eval: (put 'it          'lisp-indent-function 'defun)
;; eval: (put 'before-each 'lisp-indent-function 'defun)
;; eval: (put 'after-each  'lisp-indent-function 'defun)
;; eval: (put 'before-all  'lisp-indent-function 'defun)
;; eval: (put 'after-all   'lisp-indent-function 'defun)
;; End:
