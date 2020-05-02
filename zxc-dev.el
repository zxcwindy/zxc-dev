;; zxc.el --- my util tools collection

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: sql client; file tag system
;; version: 1.0.0
;; URL: https://github.com/zxcwindy/zxc-db
;; Package-Requires: ((ctable "0.1.2") (deferred "0.5.1") (auto-complete "1.5.1"))

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

;; Emacs SQL client, using springboot and JDBC as back-end services.
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
;; (zxc-dev-set-local-config-dir "~/.emacs.d/zxc-dev-example")  ;; Initialize only once
;; (zxc-dev-start)

;; Usage:

;; "C-; aa" Set the backend database connection corresponding to the current buffer.The alias should be consistent with the alias in database.properties.For example, if you set it to abc, you will see that Zxc[abc] is displayed in the lighter
;; "C-; cs" Gets the query statement containing all fields for the selected table name
;; "C-; de" Send the insert, update, delete, create, drop and other statements of the current paragraph or selected area to execute
;; "C-; ds" Send the query statement of the current paragraph or selected area to execute
;; "C-; dt" Get the table creation statement of the selected table name
;; "C-; ac" To start the automatic completion of table name, you need to configure it in backend/interval.properties. The format is {alias}-interval=20. 20 indicates that the cache information of the table name is updated every 20 seconds (Optional)

;;; Code:

(require 'zxc-db)

;;;; Minor Mode Definition

(defvar zxc-dev-mode-map (make-sparse-keymap)
  "Keymap for the zxc minor mode.")

(make-variable-buffer-local
 (defvar zxc-dev-mode-lighter "Zxc"))

(define-minor-mode zxc-dev-mode
  "Minor mode for Zxc dev"
  :lighter (" " zxc-dev-mode-lighter))

(defvar zxc-dev-local-config-folder nil
  "local configuration folder")

(defvar zxc-dev-template-path nil
  "template configuration path")

(global-set-key (kbd "C-; C-;") 'zxc-dev-mode)
(define-key zxc-dev-mode-map (kbd  "C-; cs") #'zxc-db-get-select-sql)
(define-key zxc-dev-mode-map (kbd  "C-; cq") #'zxc-util-convert-table-to-sql)
(define-key zxc-dev-mode-map (kbd  "C-; de") #'zxc-db-send-region-exec)
(define-key zxc-dev-mode-map (kbd  "C-; ds") #'zxc-db-send-region-query)
(define-key zxc-dev-mode-map (kbd  "C-; dt") #'zxc-db-get-table-sql)
(define-key zxc-dev-mode-map (kbd  "C-; aa") #'zxc-db-ac-set-db-alias)
(define-key zxc-dev-mode-map (kbd  "C-; ac") #'zxc-db-ac-toggle)

(mapc #'(lambda (mode-hook)
	  (add-hook mode-hook 'zxc-dev-mode))
      (list 'shell-mode-hook 'sql-mode-hook))

(defun zxc-dev-set-local-config-dir (dir-path)
  "set local configuration folder,copy resources from the template to the folder"
  (unless (file-directory-p dir-path)
      (make-directory dir-path))
  (setq zxc-dev-local-config-folder (concat (directory-file-name dir-path) "/"))
  (setq zxc-dev-template-path (concat (file-name-directory (cdr (find-function-library 'zxc-db-send-region-query))) "backend/"))
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
  "startup springboot backend"
  (save-excursion
    (make-thread #'(lambda ()
		     (let ((buffer-name "*zxc-backend*"))
		       (if (eq nil (get-buffer buffer-name))
			   (progn
			     (shell buffer-name)
			     (comint-simple-send (get-buffer-process (get-buffer buffer-name)) (concat zxc-dev-template-path "run.sh")))
			 (shell buffer-name)))))))


(provide 'zxc-dev)
