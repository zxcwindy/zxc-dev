;;; zxc-dev.el --- File tag system and sql client -*- lexical-binding: t -*-

;; Copyright (C) 2020 zhengxc

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: tools, sql client and file tag system
;; Version: 1.0.0
;; URL: https://github.com/zxcwindy/zxc-dev
;; Package-Requires: ((emacs "26.1") (ctable "0.1.2") (deferred "0.5.1") (auto-complete "1.5.1") (http-post-simple "1.0") (page-break-lines "20181221.2308") (switch-window "1.6.1"))

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

;; Emacs SQL client and File tag System, using springboot and JDBC as back-end services.
;;
;; First add database info in backend/database.properties
;;
;; For Example
;; jdbc-0.proxool.alias=abc
;; jdbc-0.proxool.driver-url=jdbc:mysql://localhost:3306/dbname?useUnicode=true&characterEncoding=UTF-8
;; jdbc-0.proxool.driver-class=com.mysql.jdbc.Driver
;; jdbc-0.user=root
;; jdbc-0.password=root
;; jdbc-0.proxool.maximum-connection-count=10
;; jdbc-0.proxool.house-keeping-test-sql=select CURRENT_DATE

;; Init:
;; (require 'zxc-dev)
;; (zxc-dev-set-local-config-dir "~/.emacs.d/zxc-dev-example")
;; (zxc-dev-start)
;; set jdbc-56123.proxool.driver-url=jdbc:h2:~/.emacs.d/zxc-dev-example/tagdb/tags.db

;; Usage:

;; SQL Client
;; "C-; aa" Set the backend database connection corresponding to the current buffer.The alias should be consistent with the alias in database.properties.For example, if you set it to abc, you will see that Zxc[abc] is displayed in the lighter
;; "C-; cs" Gets the query statement containing all fields for the selected table name
;; "C-; de" Send the insert, update, delete, create, drop and other statements of the current paragraph or selected area to execute
;; "C-; ds" Send the query statement of the current paragraph or selected area to execute
;; "C-; dt" Get the table creation statement of the selected table name
;; "C-; ac" To start the automatic completion of table name, you need to configure it in backend/interval.properties.  The format is {alias}-interval=20. 20 indicates that the cache information of the table name is updated every 20 seconds (Optional)

;; File tag
;; "C-; t" display all tags
;; "C-u C-; t" create tags
;; "b" display all tag in current dir
;; "e" edit tag when in dired mode
;; "C-u <backspace>" quit edit tag when in dired mode


;;; Code:

(require 'find-func)
(require 'zxc-db)
(require 'zxc-ft)

;;;; Minor Mode Definition

(defvar zxc-dev-mode-map (make-sparse-keymap)
  "Keymap for the zxc minor mode.")

(make-variable-buffer-local
 (defvar zxc-dev-mode-lighter "Zxc"))

(define-minor-mode zxc-dev-mode
  "Minor mode for Zxc dev"
  :lighter (" " zxc-dev-mode-lighter))

(defvar zxc-dev-local-config-folder nil
  "Local configuration folder.")

(defvar zxc-dev-template-path nil
  "Template configuration path.")

(global-set-key (kbd "C-; C-;") 'zxc-dev-mode)
(define-key zxc-dev-mode-map (kbd  "C-; cs") #'zxc-dev-db-get-select-sql)
(define-key zxc-dev-mode-map (kbd  "C-; de") #'zxc-dev-db-send-region-exec)
(define-key zxc-dev-mode-map (kbd  "C-; ds") #'zxc-dev-db-send-region-query)
(define-key zxc-dev-mode-map (kbd  "C-; dt") #'zxc-dev-db-get-table-sql)
(define-key zxc-dev-mode-map (kbd  "C-; aa") #'zxc-dev-db-ac-set-db-alias)
(define-key zxc-dev-mode-map (kbd  "C-; ac") #'zxc-dev-db-ac-toggle)
(global-set-key (kbd "C-; t") 'zxc-ft)

(mapc #'(lambda (mode-hook)
	  (add-hook mode-hook 'zxc-dev-mode))
      (list 'shell-mode-hook 'sql-mode-hook))

(defun zxc-dev-db-ac-set-db-alias (alias)
  "Database ALIAS."
  (interactive "sInput Database Alias：")
  (setf zxc-mode-lighter (format "Zxc[%s]" alias))
  (setf zxc-dev-db-ac-db-alias alias))

(defun zxc-dev-set-local-config-dir (dir-path)
  "Set local configuration folder.
copy resources from the template to the folder.
Argument DIR-PATH Custom folder."
  (unless (file-directory-p dir-path)
    (make-directory dir-path))
  (setq zxc-dev-local-config-folder (concat (directory-file-name dir-path) "/"))
  (setq zxc-dev-template-path (concat (file-name-directory (cdr (find-function-library 'zxc-dev-db-send-region-query))) "backend/"))
  (loop for config-dir in (list "conf/" "jdbclib/" "velocity/" "tagdb/")
	do
	(let ((source-dir (concat zxc-dev-template-path config-dir))
	      (target-dir (concat zxc-dev-local-config-folder config-dir)))
	  (ignore-errors (make-directory target-dir))
	  (mapc #'(lambda (f)
		    (unless (file-exists-p (concat target-dir f))
		      (copy-file (concat source-dir f) target-dir)))
		(seq-filter #'(lambda (file-name)
				(and (not (equal file-name ".")) (not (equal file-name ".."))))
			    (directory-files source-dir))))))

(defun zxc-dev-start ()
  "Startup springboot backend."
  (save-excursion
    (make-thread #'(lambda ()
		     (let ((buffer-name "*zxc-backend*"))
		       (if (eq nil (get-buffer buffer-name))
			   (progn
			     (shell buffer-name)
			     (comint-simple-send (get-buffer-process (get-buffer buffer-name)) (concat zxc-dev-template-path "run.sh " zxc-dev-local-config-folder)))
			 (shell buffer-name)))))))

(provide 'zxc-dev)

;;; zxc-dev.el ends here
