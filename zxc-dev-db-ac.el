;;; zxc-dev-db-ac.el --- HTTP client using the url library -*- lexical-binding: t -*-

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: tools emacs sql client
;; version: 1.0.0

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

;; Prompt database table name.

;;; Code:

(require 'auto-complete)
(require 'url)
(require 'deferred)
(require 'json)


(defvar zxc-dev-db-ac-tablename-url "%s/service/rest/dbMeta/%s/%s" "Host,alias,tablename service url.")

(make-variable-buffer-local
 (defvar zxc-dev-db-ac-db-alias "1" "Database alias."))

(make-variable-buffer-local
 (defvar zxc-dev-db-ac-host "http://localhost:9990"
   "Backend host."))

(defvar zxc-dev-db-ac-table-name-candidates nil "Tablename candidates.")

(defvar zxc-dev-db-ac-default-sources ac-sources "Default ac sources.")

(defvar zxc-dev-db-ac-source
  '((candidates . zxc-dev-db-ac-tablename-candidates))
  "Table name candidates.")

(defun zxc-dev-db-ac-get-tables ()
  "Get table list from server.
Return json format [{tableSchema:\"schema\",tableName:\"tablename\"},...]."
  (deferred:$
    (deferred:url-get (format zxc-dev-db-ac-tablename-url zxc-dev-db-ac-host zxc-dev-db-ac-db-alias (zxc-dev-db-ac-previous-word)))
    (deferred:nextc it
      (lambda (buf)
	(let ((data (with-current-buffer buf (buffer-string)))
	      (json-object-type 'plist)
	      (json-array-type 'list)
	      (json-false nil))
	  (kill-buffer buf)
	  (condition-case error
	      (json-read-from-string data)
	    (error "Error data : %s" error)))))
    (deferred:nextc it
      (lambda (result)
	(when (> (length result) 0)
	  (setf zxc-dev-db-ac-table-name-candidates result))))))

(defun zxc-dev-db-ac-previous-word ()
  "Get previous word as tablename prefix."
  (let ((start (point)))
    (save-excursion
      (backward-word)
      (when (and (> (- (point) 1) 0)
		 (equal (buffer-substring-no-properties (point) (- (point) 1))  "."))
	(backward-word))
      (buffer-substring-no-properties start (point)))))

(defun zxc-dev-db-ac-tablename-candidates ()
  "Get table candidates."
  (zxc-dev-db-ac-get-tables)
  zxc-dev-db-ac-table-name-candidates)

(defun zxc-dev-db-ac-toggle ()
  "Toggle ac db sources."
  (interactive)
  (if (equal ac-sources '(zxc-dev-db-ac-source))
      (progn (setq ac-sources zxc-dev-db-ac-default-sources)
	     (message "reset ac"))
    (setq ac-sources '(zxc-dev-db-ac-source))
    (auto-complete-mode t)
    (message "enable db-ac")))

(provide 'zxc-dev-db-ac)


;;; zxc-dev-db-ac.el ends here
