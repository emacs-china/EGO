;;; ego.el --- static site generator based on Emacs, Git and Org-mode

;; Copyright (C)  2015 Feng Shu, Kuangdash
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;;         Kuangdash <kuangdash AT 163.com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/emacs-china/EGO

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

;; EGO is a static site generator based on Emacs, Git and Org mode.

;; 1. Sources:   https://github.com/emacs-china/EGO
;; 2. Documents: http://emacs-china.github.io/EGO


;;; Code:

(require 'ox-html)
(require 'ht)
(require 'ego-util)
(require 'ego-config)
(require 'ego-git)
(require 'ego-resource)
(require 'ego-export)
(require 'cl-lib)
(require 'subr-x)

(defconst ego-version "0.9")

;;;###autoload
(defun ego-do-publication (&optional project-name
                                     force-all
                                     base-git-commit
                                     checkin-all)
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
   html files will be published on \"html-branch\" of \"repository directory\" and pushed to the remote repository.
4) CHECKIN-ALL checkin all the org-files, with the CHECKIN-ALL you input as the COMMIT STRING."
  (interactive (list (ego--select-project)
                     (yes-or-no-p "Full publish?(y/n)")
                     nil
                     (when ego-auto-commit
                       (read-string "checkin message (won't show in 'git log' if you have committed all): "))))
  (let* ((ego-current-project-name (or project-name
                                       (ego--select-project)))
         (repo-dir (ego--get-repository-directory))
         (store-dir (expand-file-name (ego--get-config-option :store-dir)))
         (org-branch (ego--get-config-option :repository-org-branch))
         (html-branch (ego--get-config-option :repository-html-branch))
         (base-git-commit (or base-git-commit
                              (unless force-all
                                (read-string "Base git commit: " (or (ego--get-first-commit-before-publish repo-dir org-branch store-dir html-branch)
                                                                     "HEAD~1")))))
         (preparation-function (ego--get-config-option :preparation-function)))
    (when preparation-function
      (run-hooks 'preparation-function))
    (message "EGO: verify configuration")
    (ego--verify-configuration)
    (setq ego--item-cache nil)
    (let* ((repo-files-function (ego--get-config-option :repo-files-function))
           (addition-files-function (ego--get-config-option :addition-files-function))
           (orig-repo-branch (ego--git-branch-name repo-dir))
           repo-stashed-p)
      (message "EGO: Git branch operation and get changed files")
      (unless (ego-git-repo-up2date-p repo-dir)
        (if checkin-all
            (ego--git-commit-changes repo-dir (concat checkin-all "--Committed by EGO"))
          (ego--git-stash-changes repo-dir "EGO")
          (setq repo-stashed-p t))) ; TODO 使用stash代替commit应该会好点
      (ego--git-change-branch repo-dir org-branch)
      (ego--git-pull-remote repo-dir org-branch)
      (let* ((repo-files
              (-filter `(lambda (string)
                          (not (string-match ,(ego--get-config-option :ignore-file-name-regexp) string)))
                       (when (functionp repo-files-function)
                         (funcall repo-files-function repo-dir))))
             (addition-files            ;addition-files一般为repo之外的附加文件
              (when (functionp addition-files-function)
                (funcall addition-files-function repo-dir)))
             (changed-files
              (if force-all
                  `(:update ,repo-files :delete nil)
                (message "EGO: Getting all changed files, just waiting...")
                (ego--git-files-changed repo-dir base-git-commit))))
        (progn
          (message "EGO: Create necessary directory and prepare theme!")
          (unless (file-directory-p store-dir)
            (ego--init-repository store-dir html-branch))
          (ego--git-change-branch store-dir html-branch)
          (ego--git-pull-remote store-dir html-branch)
          (ego--prepare-theme-resources store-dir)
          (message "EGO: Pre-publish all files needed to be publish, waiting...")
          (ego--publish-changes repo-files addition-files changed-files store-dir)
          (message "EGO: Pre-publish finished, output directory: %s." store-dir))
        (let ((file-name-handler-alist (cons '("\\(?:\\.htm\\|\\.html\\)" . ego--copy-file-handler) file-name-handler-alist)) ; register ego--copy-file-handler to tackle relative-url-to-absolute problem
              )
          (message "EGO: pre-publish accomplished ~ begin real publish")
          (ego--git-commit-changes repo-dir (concat "Update ego-db-file,committed by EGO.") (list ego-db-file-name))
          (ego--git-commit-changes store-dir (concat "Update published html files,committed by EGO."))
          (ego--git-change-branch repo-dir orig-repo-branch)
          (when repo-stashed-p
            (vc-git-stash-pop "EGO"))
          (message "EGO: Local Publication finished, see *EGO output* buffer to get more information.")

          ;; publish remote
          (ego--git-push-remote repo-dir org-branch)
          (ego--git-push-remote store-dir html-branch)
          (message "EGO: Remote Publication finished.\nSee *EGO OUTPUT* buffer for remote publication situation."))))))

(defun ego--init-repository (repo-dir branch)
  (ego--git-init-repo repo-dir)
  (ego--git-new-empty-branch repo-dir branch))

;;;###autoload
(defun ego-new-repository (&optional repo-dir org-branch store-dir html-branch )
  "Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by EGO. In order to construct a real repository,
you must customize the variable `ego-project-config-alist' according to the readme file of EGO project."
  (interactive)
  (let ((repo-dir (or repo-dir
                      (read-directory-name
                       "Specify a directory to become the repository: " nil nil nil)))
        (org-branch (or org-branch
                        (read-string "Input the branch name of 'source' branch: " "source" nil "source")))
        (store-dir (or store-dir
                      (read-directory-name
                       "Specify a directory(not the same as repo directory) to store the html: " nil nil nil)))
        (html-branch (or html-branch
                         (read-string "Input the branch name of 'html' branch: " "master" nil "master"))))
    (when (equal repo-dir store-dir)
      (error "Repo-dir(%s) and Store-dir(%s) should not the same" repo-dir store-dir))
    (ego--init-repository repo-dir org-branch)
    (ego--init-repository store-dir html-branch)
    (ego--generate-readme repo-dir)
    (ego--git-commit-changes repo-dir "initial commit")
    (ego--generate-index repo-dir)
    (ego--git-commit-changes repo-dir "add source index.org")
    (ego--generate-about repo-dir)
    (ego--git-commit-changes repo-dir "add source about.org")))

(defun ego--verify-configuration ()
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
  (unless (member ego-current-project-name
                  (mapcar 'car ego-project-config-alist))
    (error "Can't find project: \"%s\"" ego-current-project-name))
  (let ((repo-dir (ego--get-repository-directory))
        (site-domain (ego--get-site-domain)))
    (unless (and repo-dir (file-directory-p repo-dir))
      (error "Repository directory is not properly configured."))
    (unless site-domain
      (error "Site domain is not properly configured."))))

(defun ego--generate-readme (save-dir)
  "Generate README for `ego-new-repository'. SAVE-DIR is the directory where to
save generated README."
  (ego--string-to-file
   (concat
    (format "Personal site of %s, managed by EGO."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by ego \"ego-new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by ego.")
   (expand-file-name "README" save-dir)))

(defun ego--generate-index (save-dir)
  "Generate index.org for `ego-new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (ego--string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun ego--generate-about (save-dir)
  "Generate about.org for `ego-new-repository'. SAVE-DIR is the directory where
to save generated about.org."
  (ego--string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by ego.")
   (expand-file-name "about.org" save-dir)))

(defun ego--insert-options-template (&optional title uri
                                               tags description)
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
                                 `((?c . ,(ego--get-config-option :default-category))
                                   (?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(ego--encode-string-to-url i)))))))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u a d)))
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s

#+URI:         %s
#+TAGS:        %s
#+DESCRIPTION: %s

#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
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

;;;###autoload
(defun ego-new-post (&optional project-name category filename insert-fallback-template)
  "Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive)
  (let* ((ego-current-project-name (or project-name
                                        (ego--select-project)))
         (category (or category
                       (let* ((prompt (format "Category of \"%s\" project: " p))
                              (categories (mapcar #'file-name-nondirectory (cl-remove-if-not #'file-directory-p (directory-files (ego--get-config-option :repository-directory) t))))
                              (init-category (unless categories
                                               (ego--get-config-option :default-category))))
                         (completing-read prompt categories nil 'confirm init-category nil))))
         (filename (or filename
                       (read-string (format "Filename of \"%s\" project: " ego-current-project-name) "new-post.org" p)))
         (insert-fallback-template (or insert-fallback-template
                                       (yes-or-no-p "Insert fallback template? "))))
  (if (string= category "")
      (setq category (ego--get-config-option :default-category)))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((repo-dir (ego--get-repository-directory))
         (dir (concat (file-name-as-directory repo-dir)
                      (file-name-as-directory category)))
         (path (concat dir filename)))
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (erase-buffer)
    (if (and (not insert-fallback-template)
             (called-interactively-p 'any))
        (call-interactively 'ego--insert-options-template)
      (ego--insert-options-template "<Insert Your Title Here>"
                                    (format "/%s/%%y/%%m/%%d/%%t/ Or /%s/%%t/"
                                            category category)
                                    "tag1, tag2, tag3"
                                    "<Add description here>"))
    (save-buffer))))

(defun ego--get-first-commit-after-publish (&optional repo-dir org-branch store-dir html-branch)
  "Return the first commit after publish in `REPO-DIR',return nil if no commit after publish"
  (let* ((repo-dir (or repo-dir (ego--get-repository-directory)))
         (store-dir (or store-dir (expand-file-name (ego--get-config-option :store-dir))))
         (org-branch (or org-branch
                         (ego--get-config-option :repository-org-branch)
                         "source"))
         (html-branch (or html-branch
                          (ego--get-config-option :repository-html-branch)
                          "master"))
         (publish-time (string-trim (ego--git-command store-dir
                                                      (concat "log -n 1 --pretty='%cd' " html-branch))))
         (commits-after-publish (string-trim (ego--git-command repo-dir
                                                               (format "log --pretty='%%H' --since '%s' %s" publish-time org-branch))))
         (commits-after-publish (split-string commits-after-publish))
         (first-commit-after-publish (car (last commits-after-publish))))
    (if (string-blank-p first-commit-after-publish)
        nil
      first-commit-after-publish)))

(defun ego--get-first-commit-before-publish (&optional repo-dir org-branch store-dir html-branch)
  "Return the first commit after publish in `REPO-DIR',return nil if no commit after publish"
  (let* ((repo-dir (or repo-dir (ego--get-repository-directory)))
         (store-dir (or store-dir (expand-file-name (ego--get-config-option :store-dir))))
         (org-branch (or org-branch
                         (ego--get-config-option :repository-org-branch)
                         "source"))
         (html-branch (or html-branch
                          (ego--get-config-option :repository-html-branch)
                          "master"))
         (publish-time (string-trim (ego--git-command store-dir
                                                      (concat "log -n 1 --pretty='%cd' " html-branch))))
         (first-commit-before-publish (string-trim (ego--git-command repo-dir
                                                               (format "log -n 1 --pretty='%%H' --until '%s' %s" publish-time org-branch)))))
    (if (string-blank-p first-commit-before-publish)
        nil
      first-commit-before-publish)))

(defun ego--select-project ()
  "Select a project defined in `ego-project-config-alist'"
  (completing-read "Which project theme do you want to use? "
                   (delete-dups
                    (mapcar 'car ego-project-config-alist))
                   nil t nil nil ego-current-project-name))

(provide 'ego)

;;; ego.el ends here
