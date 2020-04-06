;;; zxc-db.el --- database client

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

;;

(require 'ctable)
(require 'zxc-db-ac)

(make-variable-buffer-local
 (defvar zxc-db-host "http://localhost:9990"
   "backend host"))

(defvar zxc-db-query-param nil "query parameters")

(defvar zxc-db-exec-param nil "execute parameters")

(defvar zxc-db-get-create-sql-url "%s/service/rest/dbMeta/getCreateSql/%s/%s" "tablename service url")

(defvar zxc-db-result nil
  "result dataset")

(defun zxc-db-send (uri object zxc-db-callback)
  "Send object to URL as an HTTP POST request, returning the response
and response headers.
object is an json, eg {key:value} they are encoded using CHARSET,
which defaults to 'utf-8"
  (lexical-let ((zxc-db-callback zxc-db-callback))
    (deferred:$
      (deferred:url-post (format "%s/service/rest/data/%s/%s" zxc-db-host uri zxc-db-ac-db-alias) object)
      (deferred:nextc it
	(lambda (buf)
	  (let ((data (with-current-buffer buf (buffer-string)))
		(json-object-type 'plist)
		(json-array-type 'list)
		(json-false nil))
	    (kill-buffer buf)
	    (setf zxc-db-result (json-read-from-string (decode-coding-string data 'utf-8))))))
      (deferred:nextc it
	(lambda (response)
	  (funcall zxc-db-callback))))))

;;temp-func
(defun zxc-db-get (uri zxc-db-callback)
  "Send object to URL as an HTTP GET request, returning the response
and response headers, object is an text."
  (lexical-let ((zxc-db-callback zxc-db-callback))
    (deferred:$
      (deferred:url-get uri)
      (deferred:nextc it
	(lambda (buf)
	  (let ((data (with-current-buffer buf (buffer-string))))
	    (kill-buffer buf)
	    (setf zxc-db-result (decode-coding-string data 'utf-8)))))
      (deferred:nextc it
	(lambda (response)
	  (funcall zxc-db-callback))))))

(defun zxc-db-create-column ()
  "创建表头"
  (mapcar #'(lambda (meta-info)
	      (make-ctbl:cmodel :title meta-info :align 'center :sorter nil))
	  (getf zxc-db-result :metadata)))

(defun zxc-db-create-table-buffer ()
  "创建结果表格"
  (let ((cp
	 (ctbl:create-table-component-buffer
	  :width nil :height nil
	  :model
	  (make-ctbl:model
	   :column-model (zxc-db-create-column)
	   :data (getf zxc-db-result :data))))
	(pre-10-tbl (get-buffer (format "*Table: %d*" (- ctbl:uid 10)))))
    (delete-other-windows)		;fix windows bug
    (display-buffer (ctbl:cp-get-buffer cp))
    (when pre-10-tbl
      (kill-buffer pre-10-tbl))))

(defun zxc-db-get-table-name ()
  "get table name"
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

(defun zxc-db-query-callback ()
  "query callback"
  (let ((error-msg (getf zxc-db-result :errorMsg)))
    (if (null error-msg)
	(save-excursion
	  (zxc-db-create-table-buffer))
      (with-current-buffer (get-buffer-create "*zxc-db-log*")
	(goto-char (point-max))
	(insert (concat "\n" error-msg))
	(goto-char (point-max)))
      (display-buffer "*zxc-db-log*"))))

(defun zxc-db-exec-callback ()
  "executer callback"
  (let ((error-msg (getf zxc-db-result :errorMsg)))
    (if (null error-msg)
	(message "update %s" (getf zxc-db-result :result))
      (with-current-buffer (get-buffer-create "*zxc-db-log*")
	(goto-char (point-max))
	(insert (decode-coding-string (concat "\n" error-msg) 'utf-8))
	(goto-char (point-max)))
      (display-buffer "*zxc-db-log*"))))

(defun zxc-db-get-callback ()
  "http get callback"
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (zxc-delete-current-word))
  (save-excursion
    (insert zxc-db-result)))


(defun zxc-db-send-region-query ()
  "Execute the Query SQL statement of the current region"
  (interactive)
  (zxc-db-send "query" (list (cons "sql" (zxc-util-get-region-or-paragraph-string))) #'zxc-db-query-callback))

(defun zxc-db-send-region-exec ()
  "Execute the DML SQL statement of the current region"
  (interactive)
  (zxc-db-send "exec" (list (cons "sql" (zxc-util-get-region-or-paragraph-string))) #'zxc-db-exec-callback))

(defun zxc-db-get-data (func)
  (zxc-db-get (format "%s/service/rest/dbMeta/%s/%s/%s" zxc-db-host func zxc-db-ac-db-alias (zxc-db-get-table-name))  #'zxc-db-get-callback))


(defun zxc-db-get-table-sql ()
  "Get table creation statement"
  (interactive)
  (zxc-db-get-data "getCreateSql"))

(defun zxc-db-get-select-sql ()
  "get query sql statement"
  (interactive)
  (zxc-db-get-data "getSelectSql"))

(provide 'zxc-db)
