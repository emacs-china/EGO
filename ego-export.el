;;; ego-export.el --- Publication related functions required by ego

;; Copyright (C)  2005 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: convenience
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

;; org source publication related functions

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'dash)
(require 'ego-util)
(require 'ego-config)
(require 'ego-git)
(require 'ego-template)


(defun ego/publish-changes (files-list addition-list change-plist pub-root-dir)
  "This function is for:
1. publish changed org files to html
2. delete html files which are relevant to deleted org files (NOT implemented)
3. update index pages
4. regenerate tag pages
`files-list' and `addition-list' contain paths of org files, `change-plist'
contains two properties, one is :update for files to be updated, another is :delete
for files to be deleted. `pub-root-dir' is the root publication directory."
  (let* ((repo-dir (ego/get-repository-directory))
         (files-list (delete-dups (append files-list addition-list)))
         (upd-list (delete-dups
                    (append (plist-get change-plist :update)
                            addition-list)))
         (del-list (plist-get change-plist :delete))
         visiting file-buffer attr-cell file-attr-list)
    (when (or upd-list del-list)
      (mapc
       #'(lambda (org-file)
           (setq visiting (find-buffer-visiting org-file))
           (with-current-buffer (setq file-buffer
                                      (or visiting (find-file org-file)))
             (setq attr-cell (ego/get-org-file-options
                              pub-root-dir
                              (member org-file upd-list)))
             (setq file-attr-list (cons (car attr-cell) file-attr-list))
             (when (member org-file upd-list)
               (ego/publish-modified-file (cdr attr-cell)
                                          (plist-get (car attr-cell) :pub-dir)))
             (when (member org-file del-list)
               (ego/handle-deleted-file org-file)))
           (or visiting (kill-buffer file-buffer)))
       files-list)
      (unless (member
               (expand-file-name "index.org" repo-dir)
               files-list)
        (ego/generate-default-index file-attr-list pub-root-dir))
      (when (and (ego/get-config-option :about)
                 (not (member
                       (expand-file-name "about.org" repo-dir)
                       files-list)))
        (ego/generate-default-about pub-root-dir))
      (ego/update-category-index file-attr-list pub-root-dir)
      (when (ego/get-config-option :rss)
        (ego/update-rss file-attr-list pub-root-dir))
      (mapc
       #'(lambda (name)
           (ego/update-summary file-attr-list pub-root-dir name))
       (mapcar #'car (ego/get-config-option :summary))))))

(defun ego/get-org-file-options (pub-root-dir do-pub)
  "Retrieve all needed options for org file opened in current buffer.
PUB-ROOT-DIR is the root directory of published files, if DO-PUB is t, the
content of the buffer will be converted into html."
  (let* ((repo-dir (ego/get-repository-directory))
         (filename (buffer-file-name))
         (attr-plist `(:title ,(funcall (ego/get-config-option :get-title-function))
                              :date ,(ego/fix-timestamp-string
                                      (or (ego/read-org-option "DATE")
                                          (format-time-string "%Y-%m-%d")))
                              :mod-date ,(if (not filename)
                                             (format-time-string "%Y-%m-%d")
                                           (or (ego/git-last-change-date
                                                repo-dir
                                                filename)
                                               (format-time-string
                                                "%Y-%m-%d"
                                                (nth 5 (file-attributes filename)))))
                              :description ,(or (ego/read-org-option "DESCRIPTION")
                                                "No Description")
                              :thumb ,(ego/read-org-option "THUMBNAIL")))
         assets-dir post-content
         asset-path asset-abs-path pub-abs-path converted-path
         component-table tags category cat-config)
    (setq tags (ego/read-org-option "TAGS"))
    (when tags
      (plist-put
       attr-plist :tags (delete "" (mapcar 'ego/trim-string
                                           (split-string tags "[:,]+" t)))))
    (setq authors (ego/read-org-option "AUTHOR"))
    (when authors
      (plist-put
       attr-plist :authors (delete "" (mapcar 'ego/trim-string
                                              (split-string authors "[:,]+" t)))))
    (setq category (ego/get-category filename))
    (plist-put attr-plist :category category)
    (setq cat-config (cdr (or (assoc category ego/category-config-alist)
                              (ego/get-category-setting
                               (ego/get-config-option :default-category)))))
    (plist-put attr-plist :uri (funcall (plist-get cat-config :uri-generator)
                                        (plist-get cat-config :uri-template)
                                        (plist-get attr-plist :date)
                                        (plist-get attr-plist :title)))
    (plist-put attr-plist :pub-dir (file-name-as-directory
                                    (concat
                                     (file-name-as-directory pub-root-dir)
                                     (replace-regexp-in-string
                                      "\\`/" ""
                                      (plist-get attr-plist :uri)))))
    (when do-pub
      (princ attr-plist)
      (setq post-content (ego/render-content))
      (setq assets-dir (file-name-as-directory
                        (concat (file-name-as-directory pub-root-dir)
                                "assets/"
                                (replace-regexp-in-string
                                 "\\`" "" (plist-get attr-plist :uri)))))
      (with-temp-buffer
        (insert post-content)
        (goto-char (point-min))
        (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
                ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
                "<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^\"]+\\)\"[^>]*>" nil t)
          (setq asset-path (match-string 2))
          (when (not (or (string-prefix-p "http://" asset-path)
                         (string-prefix-p "https://" asset-path)
                         (string-prefix-p "mailto:" asset-path)
                         (string-prefix-p "ftp://" asset-path)
                         (string-prefix-p "#" asset-path)
                         ;; TODO add more here
                         ))
            (setq asset-abs-path
                  (expand-file-name asset-path (file-name-directory filename)))
            (if (not (file-exists-p asset-abs-path))
                (message "[WARN] File %s in hyper link does not exist, org \
file: %s." asset-path filename)
              (unless (file-directory-p assets-dir)
                (mkdir assets-dir t))
              (copy-file asset-abs-path assets-dir t t t t)
              (setq pub-abs-path (concat assets-dir
                                         (file-name-nondirectory asset-path)))
              (unless (string-prefix-p pub-root-dir pub-abs-path)
                (message "[WARN] The publication root directory %s is not an \
ancestor directory of assets directory %s." pub-root-dir assets-dir))
              (setq converted-path
                    (concat "/" (file-relative-name pub-abs-path pub-root-dir)))
              (setq post-content
                    (replace-regexp-in-string
                     (regexp-quote asset-path) converted-path post-content))))))
      (setq component-table (ht ("header" (ego/render-header))
                                ("nav" (ego/render-navigation-bar))
                                ("content" post-content)
                                ("footer" (ego/render-footer)))))
    (cons attr-plist component-table)))

(defun ego/read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun ego/generate-uri (default-uri-template creation-date title)
  "Generate URI of org file opened in current buffer. It will be firstly created
by #+URI option, if it is nil, DEFAULT-URI-TEMPLATE will be used to generate the
uri. If CREATION-DATE is nil, current date will be used. The uri template option
can contain following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date
%t: title of current buffer"
  (let ((uri-template (or (ego/read-org-option "URI")
                          default-uri-template))
        (date-list (split-string (if creation-date
                                     (ego/fix-timestamp-string creation-date)
                                   (format-time-string "%Y-%m-%d"))
                                 "-"))
        (encoded-title (ego/encode-string-to-url title)))
    (format-spec uri-template `((?y . ,(car date-list))
                                (?m . ,(cadr date-list))
                                (?d . ,(caddr date-list))
                                (?t . ,encoded-title)))))


(defun ego/get-file-category (org-file)
  "This is the default function used to get a file's category,
see ego config option 'retrieve-category-function. How to judge a
file's category is based on its name and its root folder name."
  (let ((repo-dir (ego/get-repository-directory))
        (default-category (ego/get-config-option :default-category))
        (category-ignore-list (ego/get-config-option :category-ignore-list)))
    (cond ((not org-file)
           (let ((cat-list `("index" "about" ,(ego/get-config-option :default-category)))) ;; 3 default categories
             (dolist (f (directory-files repo-dir))
               (when (and (not (equal f "."))
                          (not (equal f ".."))
                          (not (equal f ".git"))
                          (not (member f category-ignore-list))
                          (not (equal f default-category))
                          (file-directory-p
                           (expand-file-name f repo-dir)))
                 (setq cat-list (cons f cat-list))))
             cat-list))
          ((string= (expand-file-name "index.org" repo-dir)
                    (expand-file-name org-file)) "index")
          ((string= (expand-file-name "about.org" repo-dir)
                    (expand-file-name org-file)) "about")
          ((string= (file-name-directory (expand-file-name org-file))
                    repo-dir) default-category)
          (t (car (split-string (file-relative-name
                                 (expand-file-name org-file) repo-dir)
                                "[/\\\\]+"))))))

(defun ego/relative-url-to-absolute (html-content)
  "Force convert relative url of `html-content' to absolute url."
  (let ((site-domain (ego/get-site-domain))
        url)
    (with-temp-buffer
      (insert html-content)
      (goto-char (point-min))
      (when (and ego/publish-to-repository
                 (ego/get-config-option :force-absolute-url))
        (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
                ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
                "\\(<[a-zA-Z]+[^/>]+\\)\\(src\\|href\\)\\(=\"\\)\\([^\"]+\\)\\(\"[^>]*>\\)" nil t)
          (setq url (match-string 4))
          (when (string-prefix-p "/" url)
            (setq url (concat
                       (match-string 1)
                       (match-string 2)
                       (match-string 3)
                       site-domain url
                       (match-string 5)))
            (replace-match url))))
      (buffer-string))))

(defun ego/publish-modified-file (component-table pub-dir)
  "Publish org file opened in current buffer. COMPONENT-TABLE is the hash table
used to render the template, PUB-DIR is the directory for published html file.
If COMPONENT-TABLE is nil, the publication will be skipped."
  (when component-table
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (ego/string-to-file
     (ego/relative-url-to-absolute
      (mustache-render
       (ego/get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (ego/file-to-string (ego/get-template-file "container.mustache")))
       component-table))
     (concat (file-name-as-directory pub-dir) "index.html") ;; 'html-mode ;; do NOT indent the code
     )))

(defun ego/handle-deleted-file (org-file-path)
  "TODO: add logic for this function, maybe a little complex."
  )

(defun ego/rearrange-category-sorted (file-attr-list)
  "Rearrange and sort attribute property lists from FILE-ATTR-LIST. Rearrange
according to category, and sort according to :sort-by property defined in
`ego/category-config-alist', if category is not in `ego/category-config-alist',
by default, category which set by config option `:default-category' will be used.
For sorting, later lies headmost."
  (let ((default-category (ego/get-config-option :default-category))
        cat-alist cat-list)
    (mapc
     #'(lambda (plist)
         (setq cat-list (cdr (assoc (plist-get plist :category) cat-alist)))
         (if cat-list
             (nconc cat-list (list plist))
           (setq cat-alist (cons (cons (plist-get plist :category)
                                       (list plist))
                                 cat-alist))))
     file-attr-list)
    (mapc
     #'(lambda (cell)
         (setcdr
          cell
          (sort (cdr cell)
                #'(lambda (plist1 plist2)
                    (<= (ego/compare-standard-date
                         (ego/fix-timestamp-string
                          (plist-get
                           plist1
                           (plist-get
                            (cdr (or (assoc (plist-get plist1 :category)
                                            ego/category-config-alist)
                                     (ego/get-category-setting default-category)))
                            :sort-by)))
                         (ego/fix-timestamp-string
                          (plist-get
                           plist2
                           (plist-get
                            (cdr (or (assoc (plist-get plist2 :category)
                                            ego/category-config-alist)
                                     (ego/get-category-setting default-category)))
                            :sort-by))))
                        0)))))
     cat-alist)))

(defun ego/update-category-index (file-attr-list pub-base-dir)
  "Update index page of different categories. FILE-ATTR-LIST is the list of all
file attribute property lists. PUB-BASE-DIR is the root publication directory."
  (let* ((sort-alist (ego/rearrange-category-sorted file-attr-list))
         (default-category (ego/get-config-option :default-category))
         cat-dir)
    (mapc
     #'(lambda (cat-list)
         (unless (not (plist-get (cdr (or (assoc (car cat-list)
                                                 ego/category-config-alist)
                                          (ego/get-category-setting default-category)))
                                 :category-index))
           (setq cat-dir (file-name-as-directory
                          (concat (file-name-as-directory pub-base-dir)
                                  (ego/encode-string-to-url (car cat-list)))))
           (unless (file-directory-p cat-dir)
             (mkdir cat-dir t))
           (ego/string-to-file
            (ego/relative-url-to-absolute
             (mustache-render
              (ego/get-cache-create
               :container-template
               (message "Read container.mustache from file")
               (ego/file-to-string (ego/get-template-file "container.mustache")))
              (ht ("header"
                   (ego/render-header
                    (ht ("page-title" (concat (capitalize (car cat-list))
                                              " Index - "
                                              (ego/get-config-option :site-main-title)))
                        ("author" (or user-full-name "Unknown Author")))))
                  ("nav" (ego/render-navigation-bar))
                  ("content"
                   (ego/render-content
                    "category-index.mustache"
                    (ht ("cat-name" (capitalize (car cat-list)))
                        ("posts"
                         (mapcar
                          #'(lambda (attr-plist)
                              (ht ("date"
                                   (plist-get
                                    attr-plist
                                    (plist-get
                                     (cdr (or (assoc
                                               (plist-get attr-plist :category)
                                               ego/category-config-alist)
                                              (ego/get-category-setting default-category)))
                                     :sort-by)))
                                  ("post-uri" (plist-get attr-plist :uri))
                                  ("post-title" (plist-get attr-plist :title))))
                          (cdr cat-list))))))
                  ("footer"
                   (ego/render-footer
                    (ht ("show-meta" nil)
                        ("show-comment" nil)
                        ("author" (or user-full-name "Unknown Author"))
                        ("google-analytics" (ego/get-config-option :personal-google-analytics-id))
                        ("google-analytics-id" (ego/get-config-option :personal-google-analytics-id))
                        ("creator-info" (ego/get-html-creator-string))
                        ("email" (ego/confound-email-address (or user-mail-address
                                                                 "Unknown Email")))))))))
            (concat cat-dir "index.html") 'html-mode)))
     sort-alist)))

(defun ego/generate-default-index (file-attr-list pub-base-dir)
  "Generate default index page, only if index.org does not exist. FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the root
publication directory."
  (let ((sort-alist (ego/rearrange-category-sorted file-attr-list))
        (id 0))
    (ego/string-to-file
     (ego/relative-url-to-absolute
      (mustache-render
       (ego/get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (ego/file-to-string (ego/get-template-file "container.mustache")))
       (ht ("header"
            (ego/render-header
             (ht ("page-title" (concat "Index - " (ego/get-config-option :site-main-title)))
                 ("author" (or user-full-name "Unknown Author")))))
           ("nav" (ego/render-navigation-bar))
           ("content"
            (ego/render-content
             "index.mustache"
             (ht ("categories"
                  (mapcar
                   #'(lambda (cell)
                       (ht ("id" (setq id (+ id 1)))
                           ("category" (car cell))
                           ("posts" (mapcar
                                     #'(lambda (plist)
                                         (ht ("post-uri"
                                              (plist-get plist :uri))
                                             ("post-title"
                                              (plist-get plist :title))
                                             ("post-desc"
                                              (plist-get plist :description))
                                             ("post-date"
                                              (plist-get plist :date))
                                             ("post-thumb"
                                              (or (plist-get plist :thumb) ""))))
                                     (cdr cell)))))
                   sort-alist)))))
           ("footer"
            (ego/render-footer
             (ht ("show-meta" nil)
                 ("show-comment" nil)
                 ("author" (or user-full-name "Unknown Author"))
                 ("google-analytics" (ego/get-config-option :personal-google-analytics-id))
                 ("google-analytics-id" (ego/get-config-option :personal-google-analytics-id))
                 ("creator-info" (ego/get-html-creator-string))
                 ("email" (ego/confound-email-address (or user-mail-address
                                                          "Unknown Email")))))))))
     (concat (file-name-as-directory pub-base-dir) "index.html") 'html-mode)))

(defun ego/generate-default-about (pub-base-dir)
  "Generate default about page, only if about.org does not exist. PUB-BASE-DIR
is the root publication directory."
  (let* ((about-sub-dir
          (replace-regexp-in-string
           "^/" ""
           (car (cdr (ego/get-config-option :about)))))
         (pub-dir (file-name-as-directory
                   (expand-file-name about-sub-dir pub-base-dir))))
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (ego/string-to-file
     (ego/relative-url-to-absolute
      (mustache-render
       (ego/get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (ego/file-to-string (ego/get-template-file "container.mustache")))
       (ht ("header"
            (ego/render-header
             (ht ("page-title" (concat "About - " (ego/get-config-option :site-main-title)))
                 ("author" (or user-full-name "Unknown Author")))))
           ("nav" (ego/render-navigation-bar))
           ("content"
            (ego/render-content
             "about.mustache"
             (ht ("author" (or user-full-name "Unknown Author")))))
           ("footer"
            (ego/render-footer
             (ht ("show-meta" nil)
                 ("show-comment" nil)
                 ("author" (or user-full-name "Unknown Author"))
                 ("google-analytics" (ego/get-config-option :personal-google-analytics-id))
                 ("google-analytics-id" (ego/get-config-option :personal-google-analytics-id))
                 ("creator-info" (ego/get-html-creator-string))
                 ("email" (ego/confound-email-address (or user-mail-address
                                                          "Unknown Email")))))))))
     (concat pub-dir "index.html") 'html-mode)))

(defun ego/generate-summary-uri (summary-name summary-item-name)
  "Generate summary uri based on `summary-name' and `summary-item-name'."
  (concat "/" summary-name "/" (ego/encode-string-to-url summary-item-name) "/"))

(defun ego/update-summary (file-attr-list pub-base-dir summary-name)
  "Update summary pages which name is `summary-name', FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the
root publication directory.

TODO: improve this function."
  (let* ((summary-base-dir (expand-file-name
                            (concat summary-name "/")
                            pub-base-dir))
         summary-alist summary-list summary-dir)
    (mapc
     #'(lambda (attr-plist)
         (mapc
          #'(lambda (name)
              (setq summary-list (assoc name summary-alist))
              (unless summary-list
                (add-to-list 'summary-alist (setq summary-list `(,name))))
              (nconc summary-list (list attr-plist)))
          (let* ((summary-attr (car (cdr (assoc summary-name (ego/get-config-option :summary)))))
                 (elem (plist-get attr-plist summary-attr)))
            (if (listp elem) elem (list elem)))))
     file-attr-list)
    (unless (file-directory-p summary-base-dir)
      (mkdir summary-base-dir t))
    (ego/string-to-file
     (ego/relative-url-to-absolute
      (mustache-render
       (ego/get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (ego/file-to-string (ego/get-template-file "container.mustache")))
       (ht ("header"
            (ego/render-header
             (ht ("page-title" (concat (capitalize summary-name)
                                       " Index - "
                                       (ego/get-config-option :site-main-title)))
                 ("author" (or user-full-name "Unknown Author")))))
           ("nav" (ego/render-navigation-bar))
           ("content"
            (ego/render-content
             "summary-index.mustache"
             (ht ("summary-name" (capitalize summary-name))
                 ("summary"
                  (mapcar
                   #'(lambda (summary-list)
                       (ht ("summary-item-name" (car summary-list))
                           ("summary-item-uri" (ego/generate-summary-uri summary-name (car summary-list)))
                           ("count" (number-to-string (length (cdr summary-list))))))
                   summary-alist)))))
           ("footer"
            (ego/render-footer
             (ht ("show-meta" nil)
                 ("show-comment" nil)
                 ("author" (or user-full-name "Unknown Author"))
                 ("google-analytics" (ego/get-config-option :personal-google-analytics-id))
                 ("google-analytics-id" (ego/get-config-option :personal-google-analytics-id))
                 ("creator-info" (ego/get-html-creator-string))
                 ("email" (ego/confound-email-address (or user-mail-address
                                                          "Unknown Email")))))))))
     (concat summary-base-dir "index.html") 'html-mode)
    (mapc
     #'(lambda (summary-list)
         (setq summary-dir (file-name-as-directory
                            (concat summary-base-dir
                                    (ego/encode-string-to-url (car summary-list)))))
         (unless (file-directory-p summary-dir)
           (mkdir summary-dir t))
         (ego/string-to-file
          (ego/relative-url-to-absolute
           (mustache-render
            (ego/get-cache-create
             :container-template
             (message "Read container.mustache from file")
             (ego/file-to-string (ego/get-template-file "container.mustache")))
            (ht ("header"
                 (ego/render-header
                  (ht ("page-title" (concat (capitalize summary-name) ": " (car summary-list)
                                            " - " (ego/get-config-option :site-main-title)))
                      ("author" (or user-full-name "Unknown Author")))))
                ("nav" (ego/render-navigation-bar))
                ("content"
                 (ego/render-content
                  "summary.mustache"
                  (ht ("summary-name" (capitalize summary-name))
                      ("summary-item-name" (car summary-list))
                      ("posts"
                       (mapcar
                        #'(lambda (attr-plist)
                            (ht ("post-uri" (plist-get attr-plist :uri))
                                ("post-title" (plist-get attr-plist :title))
                                ("post-date" (plist-get attr-plist :date))))
                        (cdr summary-list))))))
                ("footer"
                 (ego/render-footer
                  (ht ("show-meta" nil)
                      ("show-comment" nil)
                      ("author" (or user-full-name "Unknown Author"))
                      ("google-analytics" (ego/get-config-option :personal-google-analytics-id))
                      ("google-analytics-id" (ego/get-config-option :personal-google-analytics-id))
                      ("creator-info" (ego/get-html-creator-string))
                      ("email" (ego/confound-email-address (or user-mail-address
                                                               "Unknown Email")))))))))
          (concat summary-dir "index.html") 'html-mode))
     summary-alist)))

(defun ego/update-rss (file-attr-list pub-base-dir)
  "Update RSS. FILE-ATTR-LIST is the list of all file attribute property lists.
PUB-BASE-DIR is the root publication directory."
  (let* ((rss-file-name
          (replace-regexp-in-string
           "^/" ""
           (car (cdr (ego/get-config-option :rss)))))
         (rss-file
          (concat (file-name-as-directory pub-base-dir) rss-file-name))
         (rss-base-dir
          (file-name-directory rss-file))
         (last-10-posts
          (-take 10 (--sort (>= 0 (ego/compare-standard-date
                                   (ego/fix-timestamp-string
                                    (plist-get it :mod-date))
                                   (ego/fix-timestamp-string
                                    (plist-get other :mod-date))))
                            (--filter (not (or
                                            (string= (plist-get it :category)
                                                     "index")
                                            (string= (plist-get it :category)
                                                     "about")))
                                      file-attr-list)))))
    (unless (file-directory-p rss-base-dir)
      (mkdir rss-base-dir t))
    (ego/string-to-file
     (ego/relative-url-to-absolute
      (mustache-render
       ego/rss-template
       (ht ("title" (ego/get-config-option :site-main-title))
           ("link" (ego/get-site-domain))
           ("description" (ego/get-config-option :site-sub-title))
           ("date" (format-time-string "%a, %d %b %Y %T %Z"))
           ("items" (--map (ht ("item-title" (plist-get it :title))
                               ("item-link" (ego/get-full-url (plist-get it :uri)))
                               ("item-description" (plist-get it :description))
                               ("item-update-date" (plist-get it :mod-date)))
                           last-10-posts)))))
     rss-file)))


(provide 'ego-export)

;;; ego-export.el ends here
