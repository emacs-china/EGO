;;; ego-web-server.el --- Test web server required by ego

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

;; ego-web-server.el is a web server used to test ego.

;;; Code:
(require 'url-util)
(require 'browse-url)
(require 'web-server)
(require 'ego-config)

(defvar ego/web-server nil)

(defun ego/web-server-get-url ()
  (file-name-as-directory
   (format "http://localhost:%s"
           (number-to-string
            (ego/get-config-option :web-server-port)))))

(defun ego/web-server-start ()
  (interactive)
  (lexical-let ((docroot
                 (expand-file-name
                  (ego/get-config-option :web-server-docroot)))
                (port (ego/get-config-option :web-server-port)))
    (when (and (not ego/web-server)
               docroot port)
      (setq ego/web-server
            (ws-start
             (lambda (request)
               (with-slots (process headers) request
                 (let* ((path (substring (decode-coding-string
                                          (url-unhex-string (cdr (assoc :GET headers)))
                                          'utf-8) 1))
                        (path-expand (expand-file-name path docroot))
                        (path-index-file (concat (file-name-as-directory path-expand)
                                                 "index.html")))
                   (if (or (ws-in-directory-p docroot path-expand)
                           (< (length path) 1))
                       (cond
                        ((file-exists-p path-index-file)
                         (ws-send-file process path-index-file))
                        ((and (file-exists-p path-expand)
                              (not (file-directory-p path-expand)))
                         (ws-send-file process path-expand))
                        ((file-directory-p path-expand)
                         (ws-send-directory-list process path-expand))
                        (t (ws-send-404 process)))
                     (ws-send-404 process)))))
             (ego/get-config-option :web-server-port))))))

(defun ego/web-server-stop ()
  (interactive)
  (when ego/web-server
    (ws-stop ego/web-server)
    (setq ego/web-server nil)))

(defun ego/web-server-browse ()
  (interactive)
  (ego/web-server-stop)
  (ego/web-server-start)
  (when ego/web-server
    (browse-url-default-browser
     (ego/web-server-get-url))))

(provide 'ego-web-server)

;;; ego-web-server.el ends here
