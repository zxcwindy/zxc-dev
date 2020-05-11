;;; zxc-dev-db.el --- Emacs database client -*- lexical-binding: t -*-

;; Version: 1.0.0
;; URL: https://github.com/zxcwindy/zxc-dev

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

;; Emacs sql client.

;;; Code:

(require 'url)
(require 'json)
(require 'ctable)
(require 'zxc-dev-db-ac)
(require 'deferred)
(require 'cl-extra)

(defvar zxc-dev-db-query-param nil "Query parameters.")

(defvar zxc-dev-db-exec-param nil "Execute parameters.")

(defvar zxc-dev-db-get-create-sql-url "%s/service/rest/dbMeta/getCreateSql/%s/%s" "Tablename service url.")

(defvar zxc-dev-db-result nil
  "Result dataset.")

(defun zxc-dev-db-send (uri object zxc-dev-db-callback)
  "Send OBJECT to URL as an HTTP POST request.
returning the response and response headers.
object is an json,
eg {key:value} they are encoded using CHARSET,which defaults to 'utf-8
Argument URI url."
  (lexical-let ((zxc-dev-db-callback zxc-dev-db-callback))
    (deferred:$
      (deferred:url-post (format "%s/service/rest/data/%s/%s" zxc-dev-db-ac-host uri zxc-dev-db-ac-db-alias) object)
      (deferred:nextc it
	(lambda (buf)
	  (let ((data (with-current-buffer buf (buffer-string)))
		(json-object-type 'plist)
		(json-array-type 'list)
		(json-false nil))
	    (kill-buffer buf)
	    (setf zxc-dev-db-result (json-read-from-string (decode-coding-string data 'utf-8))))))
      (deferred:nextc it
	(lambda (_response)
	  (funcall zxc-dev-db-callback))))))

;;temp-func
(defun zxc-dev-db-get (uri zxc-dev-db-callback)
  "Send object to URL as an HTTP GET request.
returning the response and response headers, object is an text.
Argument URI url.
Argument ZXC-DEV-DB-CALLBACK callback function."
  (lexical-let ((zxc-dev-db-callback zxc-dev-db-callback))
    (deferred:$
      (deferred:url-get uri)
      (deferred:nextc it
	(lambda (buf)
	  (let ((data (with-current-buffer buf (buffer-string))))
	    (kill-buffer buf)
	    (setf zxc-dev-db-result (decode-coding-string data 'utf-8)))))
      (deferred:nextc it
	(lambda (_response)
	  (funcall zxc-dev-db-callback))))))

(defun zxc-dev-db-create-column ()
  "Create table header."
  (mapcar (lambda (meta-info)
	    (make-ctbl:cmodel :title meta-info :align 'center :sorter nil))
	  (cl-getf zxc-dev-db-result :metadata)))

(defun zxc-dev-db-create-table-buffer ()
  "Create result table."
  (let ((cp
	 (ctbl:create-table-component-buffer
	  :width nil :height nil
	  :model
	  (make-ctbl:model
	   :column-model (zxc-dev-db-create-column)
	   :data (cl-getf zxc-dev-db-result :data))))
	(pre-10-tbl (get-buffer (format "*Table: %d*" (- ctbl:uid 10)))))
    (delete-other-windows)		;fix windows bug
    (display-buffer (ctbl:cp-get-buffer cp))
    (when pre-10-tbl
      (kill-buffer pre-10-tbl))))

(defun zxc-dev-db-get-table-name ()
  "Get table name."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((start (save-excursion
		   (forward-char)
		   (backward-word)
		   (point)))
	  (end (save-excursion
		 (forward-word)
		 (point))))
      (buffer-substring-no-properties start end))))

(defun zxc-dev-db-query-callback ()
  "Query callback."
  (let ((error-msg (cl-getf zxc-dev-db-result :errorMsg)))
    (if (null error-msg)
	(save-excursion
	  (zxc-dev-db-create-table-buffer))
      (with-current-buffer (get-buffer-create "*zxc-dev-db-log*")
	(goto-char (point-max))
	(insert (concat "\n" error-msg))
	(goto-char (point-max)))
      (display-buffer "*zxc-dev-db-log*"))))

(defun zxc-dev-db-exec-callback ()
  "Executer callback."
  (let ((error-msg (cl-getf zxc-dev-db-result :errorMsg)))
    (if (null error-msg)
	(message "update %s" (cl-getf zxc-dev-db-result :result))
      (with-current-buffer (get-buffer-create "*zxc-dev-db-log*")
	(goto-char (point-max))
	(insert (decode-coding-string (concat "\n" error-msg) 'utf-8))
	(goto-char (point-max)))
      (display-buffer "*zxc-dev-db-log*"))))

(defun zxc-dev-db-get-callback ()
  "Http get callback."
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (zxc-dev-db-delete-current-word))
  (save-excursion
    (insert zxc-dev-db-result)))


(defun zxc-dev-db-send-region-query ()
  "Execute the Query SQL statement of the current region."
  (interactive)
  (zxc-dev-db-send "query" (list (cons "sql" (zxc-dev-db-util-get-region-or-paragraph-string))) #'zxc-dev-db-query-callback))

(defun zxc-dev-db-send-region-exec ()
  "Execute the DML SQL statement of the current region."
  (interactive)
  (zxc-dev-db-send "exec" (list (cons "sql" (zxc-dev-db-util-get-region-or-paragraph-string))) #'zxc-dev-db-exec-callback))

(defun zxc-dev-db-get-data (func)
  "Query data and execute callback function.
Argument FUNC callback."
  (zxc-dev-db-get (format "%s/service/rest/dbMeta/%s/%s/%s" zxc-dev-db-ac-host func zxc-dev-db-ac-db-alias (zxc-dev-db-get-table-name))  #'zxc-dev-db-get-callback))


(defun zxc-dev-db-get-table-sql ()
  "Get table creation statement."
  (interactive)
  (zxc-dev-db-get-data "getCreateSql"))

(defun zxc-dev-db-get-select-sql ()
  "Get query sql statement."
  (interactive)
  (zxc-dev-db-get-data "getSelectSql"))

(defun zxc-dev-db-util-get-region-or-paragraph-string ()
  "Get the text content of the current selected area or current paragraph."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((start (save-excursion
		   (backward-paragraph)
		   (point)))
	  (end (save-excursion
		 (forward-paragraph)
		 (point))))
      (buffer-substring-no-properties start end))))

(defun zxc-dev-db-delete-current-word()
  "Delete current word."
  (backward-word)
  (kill-word 1))

(provide 'zxc-dev-db)

;;; zxc-dev-db.el ends here
