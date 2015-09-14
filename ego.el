;;; ego.el --- static site generator based on org mode

;; Copyright (C)  2015 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/emacs-china/ego

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ego is a static site generator based on org mode.

;; 1. Sources:   https://github.com/emacs-china/ego
;; 2. Documents: http://emacs-china.github.io/ego

;; ego is a fork of Kelvin H's org-page (https://github.com/kelvinh/org-page),
;; and provides similar features as org-page, the main differents is as follow:

;; 1. org-page focus on personal blog while ego is main used to
;;    generate small project website.
;; 2. org-page use many customizable variables to configure org-page
;;    while org-website use an `org-publish-project-alist' style
;;    alist to adjust org-website's behaver.

;;    Managing multi-site configs in an emacs session with org-website is more
;;    simple than with org-page.
;; 3. org-website can deal with "increment" or "inherit" themes.
;;
;;    A "increment" theme is a mod theme which only include changed template,
;;    css and other files, the files same with base theme doesn't include.

;;    ego autosearch the same files from base theme when use "increment"
;;    theme.
;; 4. org-website include a tiny emacs web server, which can be used to test publish.


;;; Code:

(require 'ox)
(require 'ht)
(require 'ego-util)
(require 'ego-config)
(require 'ego-git)
(require 'ego-resource)
(require 'ego-export)
(require 'ego-web-server)
(require 'cl-lib)
(require 'seq)

(defconst ego-version "0.9")

(defun ego/do-publication (&optional project-name
                                     force-all
                                     base-git-commit
                                     test-and-not-publish
                                     checkin-all
                                     publish-all)
  "The main entrance of ego. The entire procedure is:
1) verify configuration
2) read changed files on \"org branch\" of \"repository directory\",
   the definition of 'changed files' is:
   1. if FORCE-ALL is non-nil, then all files will be published
      will be published.
   2. if FORCE-ALL is nil, the changed files will be obtained based on
      BASE-GIT-COMMIT
   3. if BASE-GIT-COMMIT is nil or omitted, the changed files will be obtained based on previous commit
3) publish org files to html,
   if TEST-AND-NOT-PUBLISH is t, test the generated html files by the web-server,
   otherwise html files will be published on \"html-branch\" of \"repository directory\" and pushed to the remote repository.
4) CHECKIN-ALL will checkin all the org-files, input 'n' if you have done it or don't want to checkin all.
5) PUBLISH-ALL will publish all branch in the repository, input 'n' if you don't want to publish all. "
  (interactive
   (let* ((j (or ego/default-project-name
                 (completing-read "Which project do you want to publish? "
                                  (delete-dups
                                   (mapcar 'car ego/project-config-alist))
                                  nil t nil nil ego/last-project-name)))
          (f (y-or-n-p (format "Publish all org files of \"%s\" project? (input 'n' if you want to publish partially)" j)))
          (b (unless f (read-string "Base git commit: " "HEAD~1")))
          (p (y-or-n-p "Publish to:  [Yes] Web server to test, [No] Original repo and publish remote. "))
          (c (y-or-n-p "checkin all org files? (input 'n' if you have done it)"))
          (a (unless p (y-or-n-p "publish all branch? (input 'n' if you only want to publish html)"))))
     (list j f b p c a)))

  (setq ego/current-project-name project-name)
  (setq ego/last-project-name project-name)

  (let ((preparation-function
         (ego/get-config-option :preparation-function)))
    (when preparation-function
      (run-hooks 'preparation-function)))

  (ego/verify-configuration)
  (setq ego/item-cache nil)
  (let* ((repo-dir (ego/get-repository-directory))
         (org-branch (ego/get-config-option :repository-org-branch))
         (html-branch (ego/get-config-option :repository-html-branch))
         (repo-files-function (ego/get-config-option :repo-files-function))
         (addition-files-function (ego/get-config-option :addition-files-function))
         (orig-branch (ego/git-branch-name repo-dir))
         (to-repo (not test-and-not-publish))
         (test-dir (expand-file-name (ego/get-config-option :web-server-docroot)))
         (store-dir (if (not base-git-commit)
                        test-dir
                      "~/.ego-tmp/")) ; TODO customization
         (base-git-commit-test (if base-git-commit 1 2))
         repo-files addition-files changed-files remote-repos)
    (ego/git-change-branch repo-dir org-branch)
    (setq repo-files
          (when (functionp repo-files-function)
            (funcall repo-files-function repo-dir)))
    (setq addition-files
          (when (functionp addition-files-function)
            (funcall addition-files-function repo-dir)))
    (when checkin-all
      (ego/git-commit-changes repo-dir "checkin all org files by EGO"))
    (when (or (not (equal base-git-commit-test ego/publish-without-org-to-html))
              test-and-not-publish)
      (setq changed-files (if force-all
                              `(:update ,repo-files :delete nil)
                            (message "Getting all changed files, just waiting...")
                            (ego/git-files-changed repo-dir (or base-git-commit "HEAD~1"))))
      (setq ego/publish-to-repository to-repo) ;make relative-to-absolute link, temporarily set it unusable.
      (when (file-directory-p store-dir)
        (delete-directory store-dir t t))
      (make-directory store-dir t)
      (ego/prepare-theme-resources store-dir)
      (message "Pre-publish all files needed to be publish, waiting...")
      (ego/publish-changes repo-files addition-files changed-files store-dir)
      (message "Pre-publish finished, output directory: %s." store-dir)
      (setq ego/publish-without-org-to-html nil))
    (cond (test-and-not-publish
           (unless (file-directory-p test-dir)
             (make-directory test-dir t))
           ;; when (called-interactively-p 'any)
           (if (not base-git-commit)
               (setq ego/publish-without-org-to-html 2)
             (copy-directory store-dir test-dir t t t)
             (setq ego/publish-without-org-to-html 1))
           (message "test the generated htmls in %s." test-dir)
           (ego/web-server-browse))
          (to-repo
           (message "pre-publish accomplished ~ begin real publish")
           ;;left the part below for async
           (ego/git-change-branch repo-dir html-branch)
           (push '("\\(?:\\.htm\\|\\.html\\)" . ego/copy-file-handler) file-name-handler-alist); register ego/copy-file-handler to tackle relative-url-to-absolute problem
           (copy-directory store-dir repo-dir t t t)
           (setq file-name-handler-alist
                 (delete '("\\(?:\\.htm\\|\\.html\\)" . ego/copy-file-handler) file-name-handler-alist)); unregister ego/copy-file-handler
           (ego/git-commit-changes repo-dir (concat "Update published html files, "
                                                    "committed by EGO."))
           (ego/git-change-branch repo-dir orig-branch)
           ;;left the part above for async

           (message "Local publish finished, see *EGO output* buffer to get more information (Such as \"remote publish condition\")")
           (setq remote-repos (ego/git-remote-name repo-dir))
           (if (not remote-repos)
               (message "No valid remote repository found.")
             (let (repo)
               (if (> (length remote-repos) 1)
                   (setq repo (read-string
                               (format "Which repo to push %s: "
                                       (prin1-to-string remote-repos))
                               (car remote-repos)))
                 (setq repo (car remote-repos)))
               (if (not (member repo remote-repos))
                   (message "Invalid remote repository '%s'." repo)
                 (ego/git-push-remote repo-dir
                                      repo
                                      html-branch publish-all))))
           (message "Publication finished: on branch '%s' of repository '%s'." html-branch repo-dir)))
    (setq ego/current-project-name nil)))

(defun ego/new-repository (repo-dir)
  "Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by EGO. In order to construct a real repository,
you must customize the variable `ego/project-config-alist' according to the readme file of EGO project."
  (interactive
   (list (read-directory-name
          "Specify a directory to become the repository: " nil nil nil)))
  (ego/git-init-repo repo-dir)
  (ego/generate-readme repo-dir)
  (ego/git-commit-changes repo-dir "initial commit")
  (ego/git-new-branch repo-dir (ego/get-config-option :repository-html-branch))
  (ego/git-new-branch repo-dir (ego/get-config-option :repository-org-branch))
  (ego/generate-index repo-dir)
  (ego/git-commit-changes repo-dir "add source index.org")
  (ego/generate-about repo-dir)
  (ego/git-commit-changes repo-dir "add source about.org"))

(defun ego/verify-configuration ()
  "Ensure all required configuration fields are properly configured, include:
1.  `:repository-directory': <required>
2.  `:site-domain': <required>
3.  `:personal-disqus-shortname': <optional>
4.  `:personal-duoshuo-shortname': <optional>
5.  `:repository-org-branch': [optional] (but customization recommended)
6.  `:repository-html-branch': [optional] (but customization recommended)
7.  `:site-main-title': [optional] (but customization recommanded)
8.  `:site-sub-title': [optional] (but customization recommanded)
9.  `:personal-github-link': [optional] (but customization recommended)
10. `:personal-google-analytics-id': [optional] (but customization recommended)
11. `:theme': [optional]"
  (unless (member ego/current-project-name
                  (mapcar 'car ego/project-config-alist))
    (error "Can't find project: \"%s\"" ego/current-project-name))
  (let ((repo-dir (ego/get-repository-directory))
        (site-domain (ego/get-site-domain)))
    (unless (and repo-dir (file-directory-p repo-dir))
      (error "Repository directory is not properly configured."))
    (unless site-domain
      (error "Site domain is not properly configured."))))

(defun ego/generate-readme (save-dir)
  "Generate README for `ego/new-repository'. SAVE-DIR is the directory where to
save generated README."
  (ego/string-to-file
   (concat
    (format "Personal site of %s, managed by emacs, org mode, git and ego."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by ego \"ego/new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by ego.")
   (expand-file-name "README" save-dir)))

(defun ego/generate-index (save-dir)
  "Generate index.org for `ego/new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (ego/string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun ego/generate-about (save-dir)
  "Generate about.org for `ego/new-repository'. SAVE-DIR is the directory where
to save generated about.org."
  (ego/string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by ego.")
   (expand-file-name "about.org" save-dir)))

(defun ego/insert-options-template (&optional title uri
                                              keywords tags description)
  "Insert a template into current buffer with information for exporting.

TITLE: the title of this post
URI: the uri of this post, usually looks like: /2013/12/27/the-post-title,
the following parameters could be used:
    %y: to represent the year of creation date
    %m: to represent the month of creation date
    %d: to represent the day of creation date
KEYWORDS: the keywords of this post, used by search engine
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed

Note that this function does not verify the input parameters, it is users'
responsibility to guarantee these parameters are valid."
  (interactive
   (let* ((i (read-string "Title: "))
          (u (read-string "URI(%y, %m and %d can be used to represent year, \
month and day): " (unless (string= i "")
                    (format-spec "/%c/%y/%m/%d/%t"
                                 `((?c . ,(ego/get-config-option :default-category))
                                   (?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(ego/encode-string-to-url i)))))))
          (k (read-string "Keywords(separated by comma and space [, ]): "))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u k a d)))
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s

# #+URI:         %s
# #+KEYWORDS:    %s
# #+TAGS:        %s
# #+DESCRIPTION: %s

#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
           (if (string= keywords "")
               "<TODO: insert your keywords here>"
             keywords)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           (if (string= description "")
               "<TODO: insert your description here>"
             description)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps)))

(defun ego/new-post (&optional project-name category filename insert-fallback-template)
  "Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive
   (let* ((p (or ego/default-project-name
                 (completing-read "Which project do you want post? "
                                  (delete-dups
                                   (mapcar 'car ego/project-config-alist))
                                  nil t nil nil ego/last-project-name)))
          (c (read-string (format "Category of \"%s\" project: " p)
                          (progn (setq ego/current-project-name p)
                                 (setq ego/last-project-name p)
                                 (ego/get-config-option :default-category))))
          (f (read-string (format "Filename of \"%s\" project: " p) "new-post.org" p))
          (d (yes-or-no-p "Insert fallback template? ")))
     (list p c f d)))
  (if (string= category "")
      (setq category (ego/get-config-option :default-category)))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (ego/string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((repo-dir (ego/get-repository-directory))
         (dir (concat (file-name-as-directory repo-dir)
                      (file-name-as-directory category)))
         (path (concat dir filename)))
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (if (and (not insert-fallback-template)
             (called-interactively-p 'any))
        (call-interactively 'ego/insert-options-template)
      (ego/insert-options-template "<Insert Your Title Here>"
                                   (format "/%s/%%y/%%m/%%d/%%t/ Or /%s/%%t/"
                                           category category)
                                   "keyword1, keyword2, keyword3"
                                   "tag1, tag2, tag3"
                                   "<Add description here>"))
    (save-buffer))
  (setq ego/current-project-name nil))


(provide 'ego)

;;; ego.el ends here
