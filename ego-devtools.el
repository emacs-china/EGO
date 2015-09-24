;;; ego-devtools.el --- Functions used to develop EGO

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu <tumashu AT 163 DOT com>
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

;; ego-config.el contains functions used to develop EGO.

;;; Code:
(require 'org)
(require 'ego-config)

(defun ego/devtools-update-ego-config ()
  (interactive)
  (add-to-list
   'ego/project-config-alist
   `("EGO"
     :repository-directory ,ego/load-directory
     :site-domain "http://emacs-china.github.io/EGO"
     :site-main-title "EGO"
     :site-sub-title "Static site senerator based on Emacs, Git and Org-mode"
     :theme (default)
     :force-absolute-url t
     :source-browse-url ("GitHub" "https://github.com/emacs-china/ego")
     :repository-org-branch "master"
     :repository-html-branch "gh-pages"
     :summary nil
     :confound-email nil
     :ignore-file-name-regexp "readme.org"
     :web-server-docroot ,(expand-file-name "~/webRoot/EGO")
     :web-server-port 4321)))

(ego/devtools-update-ego-config)

(provide 'ego-devtools)

;;; ego-devtools.el ends here
