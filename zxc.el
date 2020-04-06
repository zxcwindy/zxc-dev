;; zxc.el --- my util tools collection

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: zxc dev mode
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

(require 'zxc-db)

;;;; Minor Mode Definition

(defvar zxc-mode-map (make-sparse-keymap)
  "Keymap for the zxc minor mode.")

(make-variable-buffer-local
 (defvar zxc-mode-lighter "Zxc"))

(define-minor-mode zxc-mode
  "Minor mode for Zxc"
  :lighter (" " zxc-mode-lighter))

(global-set-key (kbd "C-; C-;") 'zxc-mode)
(define-key zxc-mode-map (kbd  "C-; cs") #'zxc-db-get-select-sql)
(define-key zxc-mode-map (kbd  "C-; cq") #'zxc-util-convert-table-to-sql)
(define-key zxc-mode-map (kbd  "C-; de") #'zxc-db-send-region-exec)
(define-key zxc-mode-map (kbd  "C-; ds") #'zxc-db-send-region-query)
(define-key zxc-mode-map (kbd  "C-; dt") #'zxc-db-get-table-sql)
(define-key zxc-mode-map (kbd  "C-; aa") #'zxc-db-ac-set-db-alias)
(define-key zxc-mode-map (kbd  "C-; ac") #'zxc-db-ac-toggle)

(mapc #'(lambda (mode-hook)
	  (add-hook mode-hook 'zxc-mode))
      (list 'shell-mode-hook 'sql-mode-hook))

(defun zxc-backend-init ()
  "startup springboot backend"
  (make-thread #'(lambda ()
		   (let ((buffer-name "*zxc-backend*")
			 (script-path (file-name-directory (cdr (find-function-library 'zxc-db-send-region-query)))))
		     (if (eq nil (get-buffer buffer-name))
			 (progn
			   (shell buffer-name)
			   (comint-simple-send (get-buffer-process (get-buffer buffer-name)) (concat script-path "backend/run.sh")))
		       (shell buffer-name))))))


(provide 'zxc)
