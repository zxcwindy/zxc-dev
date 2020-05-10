;;; zxc-dev-ft.el --- Quick access to files by tags -*- lexical-binding: t -*-

;; Copyright (C) 2020-2020.

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: file, tags, tools
;; Version: 1.0.0
;; URL: https://github.com/zxcwindy/zxc-dev
;; Package-Requires: ((emacs "26.1"))

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

;; File tag System

;;; Code:

;; create table tags_(
;;    id int auto_increment primary key,
;;;   short_name char(2) ,
;;    tag_name varchar(64),
;;    p_id int,
;;;   level_ int default 1
;; );

;; create table files_(
;;    id int auto_increment primary key,
;;    file_name varchar(128),
;;    file_path varchar(2048),
;;    update_time int
;; );

;; create table tag_file (
;;      tag_id int ,
;;      file_id int
;; );

;; create index tag_file_tag_id on tag_file (tag_id);
;; create index tag_file_file_id on tag_file (file_id);


(require 'http-post-simple)
(require 'page-break-lines)
(require 'json)
(require 'zxc-db)
(require 'dired)
(require 'cl-extra)

(defvar zxc-dev-ft-buffer nil
  "The buffer displaying the file tags.")

(defvar zxc-dev-ft-query-url "http://localhost:9990/service/rest/data/query/tag"
  "Background query service.")

(defvar zxc-dev-ft-exec-url "http://localhost:9990/service/rest/data/exec/tag"
  "Background update service.")

(defvar zxc-dev-ft-tag-widget nil "Display tag input.")

(defvar zxc-dev-ft-action-keys
  (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
	"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
	"A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
  "Tag action.")

(defvar zxc-dev-ft-tags-all (make-hash-table :test 'equal) "Key:shortName,value:collection.")

(defvar zxc-dev-ft-tags-search nil "Query tag.")

(defvar zxc-dev-ft-begin-ol nil "Starting point of tag.")

(defvar zxc-dev-ft-sub-tags-keys
  (nconc (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
	 (loop for i from 0 to 9 collect i))
  "When a shortname corresponds to more than one label, the shortcut keys are reordered in natural order.")

(defvar zxc-dev-ft-is-sub-view nil "Is it a child tag view.")

(defvar zxc-dev-ft-sub-tags-tmp nil "Temporary sub tag collection.")

(defvar zxc-dev-ft-current-action nil "Nil->query, t->manage.")

(defface zxc-dev-ft-action-face-foreground
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "color of keys"
  :group 'zxc-dev-mode)

(defvar zxc-dev-ft-init-p nil)

(defvar zxc-dev-ft-http-data nil "Response text.")

(defun zxc-dev-ft-http-method (url method &optional fields)
  "GET or POST METHOD for http request.
Return the response head,status code,data.
Argument URL http url."
  (if (string= (upcase method) "POST")
      (http-post-simple url fields)
    (http-get-simple url fields)))

(defun zxc-dev-ft-http-json-2-lisp (lst)
  "When the response status is 200 and it's data is a json string.
convert a json string to plist object
Argument LST list of data head status."
  (multiple-value-bind (data head status) lst
    (setf data (decode-coding-string data 'utf-8))
    (if (= status 200)
	(condition-case err
	    (let ((json-object-type 'plist)
		  (json-array-type 'list)
		  (json-false nil))
	      (setf zxc-dev-ft-http-data (json-read-from-string data)))
	  (json-readtable-error
	   (message "error json format:%s" data)))
      (minibuffer-message status))))

(defun zxc-dev-ft-http-post (url &optional fields)
  "POST method.
Argument URL url.
Optional argument FIELDS params."
  (zxc-dev-ft-http-json-2-lisp (zxc-dev-ft-http-method url "POST" fields)))

(defun zxc-dev-ft-mode-action-str (mode-map newstr &optional func-or-shortcut)
  "If func-or-shortcut is non-nil and if it is a function.
call it when STR is clicked ;
if FUNC-OR-SHORTCUT is a string.
execute the corresponding keyboard action when it is clicked.
Argument MODE-MAP mode-map.
Argument NEWSTR action key."
  (let ((func (if (functionp func-or-shortcut)
		  func-or-shortcut
		(if (stringp func-or-shortcut)
		    (lexical-let ((macro func-or-shortcut))
		      (lambda()(interactive)
			(execute-kbd-macro macro)))))))
    (define-key mode-map (kbd newstr) func)))

(defun zxc-dev-ft (arg)
  "Start file tag system.
Argument ARG prefix."
  (interactive "P")
  (if (null arg)
      (progn
	(zxc-dev-ft-init)
	(switch-to-buffer zxc-dev-ft-buffer))
    (zxc-dev-ft-create-new-tag)))


(define-derived-mode zxc-dev-ft-mode special-mode "FT"
  "Reorganize files with tags"
  (page-break-lines-mode)
  (mapc (lambda (key)
	  (zxc-dev-ft-mode-action-str zxc-dev-ft-mode-map key 'zxc-dev-ft-tag-choose)) zxc-dev-ft-action-keys)
  (define-key zxc-dev-ft-mode-map (kbd "<backspace>") 'zxc-dev-ft-reset-main-view)
  (define-key zxc-dev-ft-mode-map (kbd "RET") 'zxc-dev-ft-tag-action))

(defun zxc-dev-ft-init ()
  "Initialize."
  (setq zxc-dev-ft-buffer (get-buffer-create "*ft*"))
  (set-buffer zxc-dev-ft-buffer)
  (clrhash zxc-dev-ft-tags-all)
  (setq zxc-dev-ft-tags-search nil)
  (setq zxc-dev-ft-is-sub-view nil)
  (setq zxc-dev-ft-tags-search nil)
  (setq zxc-dev-ft-current-action nil)
  (zxc-dev-ft-main-view)
  (zxc-dev-ft-mode))

(defun zxc-dev-ft-main-view ()
  "Display tag main view."
  (with-current-buffer zxc-dev-ft-buffer
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((top-tags (zxc-dev-ft-get-top-tags)))
      (remove-overlays)
      (insert "\n\t\t")
      (setq zxc-dev-ft-tag-widget (widget-create
				   'editable-field
				   :size 100
				   :format "Currently selected tag: %v "))
      (insert "\n\n")
      (setq zxc-dev-ft-begin-ol (make-overlay (point) (point)))
      (zxc-dev-ft-view-create-all-tags top-tags))
    (widget-setup)))

(defun zxc-dev-ft-view-create-all-tags (top-tags)
  "Display tag classification.
Argument TOP-TAGS level = 0."
  (loop for tag in top-tags
	do (progn (insert "[" (zxc-dev-ft-get-action-key (nth 1 tag)) "]" " " (nth 2 tag) "\n\n")
		  (let ((child-tags (zxc-dev-ft-get-child-tags (nth 0 tag)))
			(start-point (point)))
		    (loop for i from 0 to (- (length child-tags) 1)
			  do (progn
			       (when (and (= (% i 8) 0) (/= i 0))
				 (insert "\n"))
			       (insert "[" (zxc-dev-ft-get-action-key (nth 1 (nth i child-tags))) "]" (nth 2 (nth i child-tags)) "\t")))
		    (align-regexp start-point (point) "\\(\\s-*\\)\\s-" 1 1 t))
		  (insert "\n\n"))))

(defun zxc-dev-ft-view-create-sub-tags (tags)
  "Create a tag view of the same shortname.
Argument TAGS list tags object."
  (let ((inhibit-read-only t))
    (delete-region (overlay-start zxc-dev-ft-begin-ol) (point-max))
    (goto-char (point-max))
    (loop for i from 0 to (- (length tags) 1)
	  do (progn
	       (when (and (= (% i 8) 0) (/= i 0))
		 (insert "\n"))
	       (insert "[" (zxc-dev-ft-get-action-key (nth i zxc-dev-ft-sub-tags-keys)) "]" (nth 2 (nth i tags)) "\t")))
    (align-regexp (overlay-start zxc-dev-ft-begin-ol) (point) "\\(\\s-*\\)\\s-" 1 1 t)))

(defun zxc-dev-ft-reset-main-view (arg)
  "Return to view showing all tags.
Argument ARG prefix."
  (interactive "P")
  (if (or (null zxc-dev-ft-current-action) (null arg))
      (when zxc-dev-ft-is-sub-view
	(let ((inhibit-read-only t))
	  (delete-region (overlay-start zxc-dev-ft-begin-ol) (point-max))
	  (clrhash zxc-dev-ft-tags-all)
	  (goto-char (point-max))
	  (zxc-dev-ft-view-create-all-tags (zxc-dev-ft-get-top-tags))
	  (setq zxc-dev-ft-sub-tags-tmp nil)
	  (setq zxc-dev-ft-is-sub-view nil)
	  (goto-char (point-min))))
    (delete-window)))

(defun zxc-dev-ft-get-top-tags ()
  "Get all label groups with level 0."
  (let ((temp-tags (zxc-dev-ft-query-data '((sql . "select id,short_name,tag_name from tags_ where level_=0")))))
    (zxc-dev-ft-add-tags temp-tags)
    temp-tags))

(defun zxc-dev-ft-get-child-tags (pid)
  "Get child tag of parent tag.
Argument PID parent id in database."
  (let ((temp-tags (zxc-dev-ft-query-data (list (cons 'sql (concat "select id,short_name,tag_name from tags_ where p_id=" (int-to-string pid)))))))
    (zxc-dev-ft-add-tags temp-tags)
    temp-tags))

(defun zxc-dev-ft-query-data (param)
  "Data query service of tag.
Argument PARAM post params."
  (cl-getf (zxc-dev-ft-http-post zxc-dev-ft-query-url
				 param)
	   :data))

(defun zxc-dev-ft-exec-data (sql)
  "Data update service for tag.
Argument SQL dml sql."
  (zxc-dev-ft-http-post zxc-dev-ft-exec-url
			(list (cons 'sql sql))))

(defun zxc-dev-ft-get-action-key (key-str)
  "Action key face.
Argument KEY-STR action key."
  (propertize key-str 'face 'zxc-template-face-foreground))

(defun zxc-dev-ft-add-tags (tags)
  "Store TAGS in the map as shortname and collection."
  (loop for tag in tags
	do (puthash (nth 1 tag) (cons tag (gethash (nth 1 tag) zxc-dev-ft-tags-all)) zxc-dev-ft-tags-all)))

(defun zxc-dev-ft-build-query (tag)
  "Build TAG query criteria, add when it does not exist in the query, and cancel if it exists."
  (if (not (member tag zxc-dev-ft-tags-search))
      (setq zxc-dev-ft-tags-search (cons tag zxc-dev-ft-tags-search))
    (setq zxc-dev-ft-tags-search (delete tag zxc-dev-ft-tags-search)))
  (zxc-dev-ft-widget-update)
  (goto-char (point-min)))

(defun zxc-dev-ft-widget-update ()
  "Set wideget value from search."
  (widget-value-set zxc-dev-ft-tag-widget "")
  (widget-value-set zxc-dev-ft-tag-widget
		    (mapconcat (lambda (temp-tag)
				 (nth 2 temp-tag))
			       zxc-dev-ft-tags-search
			       " + ")))

(defun zxc-dev-ft-tag-choose ()
  "Select the label according to the selected key and output it to input."
  (interactive)
  (let ((short-cut (this-command-keys)))
    (if zxc-dev-ft-is-sub-view
	(let* ((t-index (-elem-index short-cut zxc-dev-ft-sub-tags-keys))
	       (tag (nth t-index zxc-dev-ft-sub-tags-tmp)))
	  (if tag
	      (zxc-dev-ft-build-query tag)
	    (message "tag not found")))
      (let ((tags (gethash short-cut zxc-dev-ft-tags-all)))
	(if tags
	    (if (= (length tags) 1)
		(zxc-dev-ft-build-query (nth 0 tags))
	      (progn (setq zxc-dev-ft-sub-tags-tmp tags)
		     (setq zxc-dev-ft-is-sub-view t)
		     (zxc-dev-ft-view-create-sub-tags zxc-dev-ft-sub-tags-tmp)))
	  (message "tag not found"))))))

(defun zxc-dev-ft-tag-action ()
  "Query when triggered in zxc ft mode, and manage tags when triggered in dired mode."
  (interactive)
  (if (null zxc-dev-ft-current-action)
      (zxc-dev-ft-tag-query)
    (zxc-dev-ft-dired-make-tags)))

(defun zxc-dev-ft-tag-query ()
  "Query files by tag."
  (if zxc-dev-ft-tags-search
      (let ((buffer-name (format "*ft result %s*" (widget-value zxc-dev-ft-tag-widget))))
	(with-current-buffer (get-buffer-create buffer-name)
	  (zxc-dev-ft-result-mode)
	  (let ((results (zxc-dev-ft-query-data (list
						 (cons 'sql
						       (format
							"select a.id,a.file_name,a.file_path,'n/a' update_time from files_ a,(select file_id from tag_file where tag_id in (%s) group by file_id having count(1) = %s )b where a.id = b.file_id"
							(mapconcat (lambda (tag) (int-to-string (nth 0 tag))) zxc-dev-ft-tags-search ",")
							(length zxc-dev-ft-tags-search)))
						 (cons 'limit "1000")))))
	    (setq tabulated-list-entries
		  (zxc-dev-ft-result-2-entries results)
		  zxc-dev-ft-tags-search nil)
	    (widget-value-set zxc-dev-ft-tag-widget "")
	    (tabulated-list-print t)))
	(goto-char (point-max))
	(switch-to-buffer buffer-name))
    (message "Please select the tag first")))

(defun zxc-dev-ft-result-2-entries (results)
  "Return result entry in tabulated-list view.
Argument RESULTS search result."
  (mapcar (lambda (r)
	    (list (nth 0 r) `[,(nth 1 r) ,(nth 2 r) ,(nth 3 r)]))
	  results))




(define-derived-mode zxc-dev-ft-result-mode tabulated-list-mode "FT Result"
  "display search results"
  (define-key zxc-dev-ft-result-mode-map (kbd "RET") 'zxc-dev-ft-result-view-open)
  (setq tabulated-list-format
	`[("Filename" 85)
	  ("Directory" 55 nil)
	  ;; todo
	  ("UpdateTime" 5 nil)
	  ])
  (setq tabulated-list-padding 3)
  (setq tabulated-list-sort-key (cons "UpdateTime" nil))
  (tabulated-list-init-header))

(defun zxc-dev-ft-result-view-open ()
  "Open file with current line."
  (interactive)
  (find-file (concat (aref (tabulated-list-get-entry) 1) "/" (aref (tabulated-list-get-entry) 0))))



(make-variable-buffer-local
 (defvar zxc-dev-ft-short-name nil
   "Short name."))

(make-variable-buffer-local
 (defvar zxc-dev-ft-tag-name nil
   "Tag name."))

(make-variable-buffer-local
 (defvar zxc-dev-ft-tag-pid nil
   "Tag parent id."))


(defun zxc-dev-ft-create-new-tag ()
  "Create a new tag."
  (switch-to-buffer "*ft add tag*")
  (kill-all-local-variables)
  (setq zxc-dev-ft-short-name nil)
  (setq zxc-dev-ft-tag-name nil)
  (setq zxc-dev-ft-tag-pid nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-create 'editable-field
		 :size 13
		 :format "ShortName: %v "
		 :notify (lambda (widget &rest ignore)
			   (setf zxc-dev-ft-short-name (widget-value widget))))
  (widget-create 'editable-field
		 :size 13
		 :format "FullName: %v "
		 :notify (lambda (widget &rest ignore)
			   (setf zxc-dev-ft-tag-name (widget-value widget))))
  (widget-insert "\n")
  (apply 'widget-create
	 'menu-choice
	 :tag "ParentNode"
	 :value nil
	 :notify (lambda (widget &rest ignore)
		   (setq zxc-dev-ft-tag-pid (widget-value widget)))
	 '(item :tag "No Parent" :value nil)
	 (mapcar (lambda (label) (list 'item :tag (nth 2 label) :value (nth 0 label)))
		 (zxc-dev-ft-query-data '((sql . "select id,short_name,tag_name from tags_ where level_=0")))))
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (if (and zxc-dev-ft-short-name zxc-dev-ft-tag-name)
			       (progn
				 (if (null zxc-dev-ft-tag-pid)
				     (zxc-dev-ft-exec-data (format "insert into tags_ (short_name,tag_name,level_) values ('%s','%s',0) " zxc-dev-ft-short-name zxc-dev-ft-tag-name))
				   (zxc-dev-ft-exec-data (format "insert into tags_ (short_name,tag_name,p_id,level_) values ('%s','%s',%d, 1) " zxc-dev-ft-short-name zxc-dev-ft-tag-name zxc-dev-ft-tag-pid)))
				 (message "tag %s-%s created successfully" zxc-dev-ft-short-name zxc-dev-ft-tag-name))
			     (message "Short and full names are required")))
		 "Save")
  (widget-insert " ")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))


;;; dired mode 扩展
(make-variable-buffer-local
 (defvar zxc-dev-ft-dired-file-pos nil
   "Store file pos and tags."))

(defun zxc-dev-ft-dired-list-tags ()
  "Display all tags of all files in the current folder."
  (interactive)
  (save-excursion
    (remove-overlays)
    (set 'zxc-dev-ft-dired-file-pos (make-hash-table :test 'equal))
    (let ((results (zxc-dev-ft-query-data (list (cons 'sql (format "select b.tag_id,b.file_id,c.tag_name,a.file_name from (select id,FILE_NAME from FILES_ where FILE_PATH = '%s') a, tag_file b , tags_ c where a.id = b.file_id and c.id = b.tag_id" (substring (dired-current-directory) 0 -1)))))))
      (zxc-dev-ft-dired-show-tags results))))

(defun zxc-dev-ft-dired-show-tags (tag-infos)
  "TAG-INFOS is a two-dimensional array，The order of each row is tagid、fileid，tagname、filename."
  (loop for tag-info in tag-infos
	do (let* ((file-name (nth 3 tag-info))
		  (pos (or (gethash file-name zxc-dev-ft-dired-file-pos)
			   (progn
			     (goto-char (point-min))
			     (search-forward (nth 3 tag-info) nil t)))))
	     (when pos
	       (let ((ol (or (car (overlays-in pos pos)) (make-overlay pos pos))))
		 (puthash file-name pos zxc-dev-ft-dired-file-pos)
		 (overlay-put ol 'after-string (propertize (concat (overlay-get ol 'after-string)  " " (nth 2 tag-info)) 'face 'font-lock-doc-face)))))))

(defun zxc-dev-ft-dired-mark-tags ()
  "Tag the selected files in the current folder."
  (interactive)
  (save-excursion
    (unless (buffer-live-p zxc-dev-ft-buffer)
      (zxc-dev-ft-init)))
  (setq zxc-dev-ft-current-action t)
  (save-excursion
    (setq  zxc-dev-ft-tags-search (zxc-dev-ft-query-data (list (cons 'sql (format "select a.id,a.short_name,a.tag_name from tags_ a ,(select tag_id from files_ b ,tag_file c where file_name = '%s' and file_path = '%s' and c.file_id = b.id) d where a.id = d.tag_id" (file-name-nondirectory (dired-get-file-for-visit)) (substring (dired-current-directory) 0 -1))))))
    (zxc-dev-ft-widget-update)
    (display-buffer zxc-dev-ft-buffer)
    (switch-window)
    (goto-char (point-min))))

(defun zxc-dev-ft-dired-make-tags ()
  "Update tag."
  (switch-window)
  (let* ((file-path (substring (dired-current-directory) 0 -1))
	 (file-name (file-name-nondirectory (dired-get-file-for-visit)))
	 (file-info (zxc-dev-ft-query-file-info file-path file-name))
	 (file-id nil))
    (if file-info
	(setf file-id (nth 0 (car file-info)))
      (progn
	(zxc-dev-ft-exec-data (format "insert into files_ (file_name,file_path) values ('%s','%s')" file-name file-path))
	(setf file-id (nth 0 (car (zxc-dev-ft-query-file-info file-path file-name))))))
    (zxc-dev-ft-exec-data (format "delete from tag_file where file_id = %d" file-id))
    (mapc (lambda (tag)
	    (zxc-dev-ft-exec-data (format "insert into tag_file (tag_id,file_id) values (%d,%d)" (nth 0 tag) file-id)))
	  zxc-dev-ft-tags-search)
    (delete-window (get-buffer-window zxc-dev-ft-buffer))
    (setq zxc-dev-ft-current-action nil))
  (zxc-dev-ft-dired-list-tags))

(defun zxc-dev-ft-query-file-info (file-path file-name)
  "Query file info from database.
Argument FILE-PATH directory.
Argument FILE-NAME filename."
  (zxc-dev-ft-query-data (list (cons 'sql (format "select id,file_name,file_path from files_ where file_path = '%s' and file_name = '%s'" file-path  file-name)))))

(defun zxc-dev-ft-dired-revert-buffer ()
  "Clean tags，revert dired buffer."
  (interactive)
  (remove-overlays)
  (revert-buffer))

(defun zxc-dev-ft-dired-sort-buffer (&optional arg)
  "Clean tags，sort dired buffer.
Optional argument ARG prefix."
  (interactive "P")
  (remove-overlays)
  (dired-sort-toggle-or-edit arg))

(add-hook 'dired-mode-hook #'(lambda ()
			       (define-key dired-mode-map (kbd "b") 'zxc-dev-ft-dired-list-tags)
			       (define-key dired-mode-map (kbd "g") 'zxc-dev-ft-dired-revert-buffer)
			       (define-key dired-mode-map (kbd "s") 'zxc-dev-ft-dired-sort-buffer)
			       (define-key dired-mode-map (kbd "e") 'zxc-dev-ft-dired-mark-tags)))

(provide 'zxc-dev-ft)

;;; zxc-dev-ft.el ends here
