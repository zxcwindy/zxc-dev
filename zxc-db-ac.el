;;; zxc-db-ac.el --- HTTP client using the url library

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: emacs sql client
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

(require 'deferred)

(defvar zxc-db-ac-tablename-url "%s/service/rest/dbMeta/%s/%s" "host,alias,tablename service url")

(make-variable-buffer-local
 (defvar zxc-db-ac-db-alias "1" "database alias"))

(defvar zxc-db-ac-table-name-candidates nil "tablename candidates")

(defvar zxc-db-ac-default-sources ac-sources "default ac sources")

(defvar zxc-db-ac-source
  '((candidates . zxc-db-ac-tablename-candidates))
  "table name candidates")


(defun zxc-db-ac-set-db-alias (alias)
  "database alias"
  (interactive "sInput Database Alias：")
  (setf zxc-mode-lighter (format "Zxc[%s]" alias))
  (setf zxc-db-ac-db-alias alias))

(defun zxc-db-ac-get-tables ()
  "get table list from server
return json format [{tableSchema:\"schema\",tableName:\"tablename\"},...]
"
  (deferred:$
    (deferred:url-get (format zxc-db-ac-tablename-url zxc-db-host zxc-db-ac-db-alias (zxc-db-ac-previous-word)))
    (deferred:nextc it
      (lambda (buf)
	(let ((data (with-current-buffer buf (buffer-string)))
	      (json-object-type 'plist)
	      (json-array-type 'list)
	      (json-false nil))
	  (kill-buffer buf)
	  (condition-case error
	      (json-read-from-string data)
	    (error "error data : %s" error)))))
    (deferred:nextc it
      (lambda (result)
	(when (> (length result) 0)
	  (setf zxc-db-ac-table-name-candidates result))))))

(defun zxc-db-ac-previous-word ()
  "get previous word as tablename prefix"
  (let ((start (point)))
    (save-excursion
      (backward-word)
      (when (and (> (- (point) 1) 0)
		 (equal (buffer-substring-no-properties (point) (- (point) 1))  "."))
	(backward-word))
      (buffer-substring-no-properties start (point)))))

(defun zxc-db-ac-tablename-candidates ()
  "get table candidates"
  (zxc-db-ac-get-tables)
  zxc-db-ac-table-name-candidates)

(defun zxc-db-ac-toggle ()
  "toggle ac db sources"
  (interactive)
  (if (equal ac-sources '(zxc-db-ac-source))
      (progn (setq ac-sources zxc-db-ac-default-sources)
	     (message "reset ac"))
    (setq ac-sources '(zxc-db-ac-source))
    (auto-complete-mode t)
    (message "enable db-ac")))

(provide 'zxc-db-ac)
