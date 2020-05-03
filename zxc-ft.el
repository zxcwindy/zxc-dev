;;; zxc-ft --- quick access to files by tags

;; Copyright (C) 2020-2020.

;; Author: zhengxc <zh_xi_ch@126.com>
;; Keywords: file, tags, tools

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

(defvar zxc-ft-buffer nil
  "The buffer displaying the file tags.")

(defvar zxc-ft-query-url "http://localhost:9990/service/rest/data/query/tag"
  " background query service")

(defvar zxc-ft-exec-url "http://localhost:9990/service/rest/data/exec/tag"
  " background update service")

(defvar zxc-ft-tag-widget nil "display tag input")

(defvar zxc-ft-action-keys
  (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
	"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
	"A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
  "tag action")

(defvar zxc-ft-tags-all (make-hash-table :test 'equal) "key:shortName,value:collection")

(defvar zxc-ft-tags-search nil "query tag")

(defvar zxc-ft-begin-ol nil "Starting point of tag")

(defvar zxc-ft-sub-tags-keys
  (nconc (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
	 (loop for i from 0 to 9 collect i))
  "When a shortname corresponds to more than one label, the shortcut keys are reordered in natural order")

(defvar zxc-ft-is-sub-view nil "Is it a child tag view")

(defvar zxc-ft-sub-tags-tmp nil "temporary sub tag collection")

(defvar zxc-ft-current-action nil "nil->query, t->manage")

(defface zxc-ft-action-face-foreground
  '((((class color)) (:foreground "red"))
    (((background dark)) (:foreground "gray100"))
    (((background light)) (:foreground "gray0"))
    (t (:foreground "gray100")))
  "color of keys")

(defvar zxc-ft-init-p nil)

(defvar http-data nil "response text")

(defun http-method (url method &optional fields)
  "GET or POST method for http request,returning the response head,status code,data"
  (if (string= (upcase method) "POST")
      (http-post-simple url fields)
    (http-get-simple url fields)))

(defun http-json-2-lisp (lst)
  "when the response status is 200 and it's data is a json string,
convert a json string to plist object"
  (multiple-value-bind (data head status) lst
    (setf data (decode-coding-string data 'utf-8))
    (if (= status 200)
	(condition-case err
	    (let ((json-object-type 'plist)
		  (json-array-type 'list)
		  (json-false nil))
	      (setf http-data (json-read-from-string data)))
	  (json-readtable-error
	   (message "error json format:%s" data)))
      (minibuffer-message status))))

(defun http-post (url &optional fields)
  "POST method"
  (http-json-2-lisp (http-method url "POST" fields)))

(defun zxc-mode-action-str (mode-map newstr &optional func-or-shortcut)
  "If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked ; if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  (let ((func (if (functionp func-or-shortcut)
		  func-or-shortcut
		(if (stringp func-or-shortcut)
		    (lexical-let ((macro func-or-shortcut))
		      (lambda()(interactive)
			(execute-kbd-macro macro)))))))
    (define-key mode-map (kbd newstr) func)))

(defun zxc-ft (arg)
  "Start file tag system"
  (interactive "P")
  (if (null arg)
      (progn
	(zxc-ft-init)
	(switch-to-buffer zxc-ft-buffer))
    (zxc-ft-create-new-tag)))


(define-derived-mode zxc-ft-mode fundamental-mode "FT"
  "Reorganize files with tags"
  (page-break-lines-mode)
  (mapc #'(lambda (key)
	    (zxc-mode-action-str zxc-ft-mode-map key 'zxc-ft-tag-choose)) zxc-ft-action-keys)
  (define-key zxc-ft-mode-map (kbd "<backspace>") 'zxc-ft-reset-main-view)
  (define-key zxc-ft-mode-map (kbd "RET") 'zxc-ft-tag-action))

(defun zxc-ft-init ()
  "Initialize"
  (setq zxc-ft-buffer (get-buffer-create "*ft*"))
  (set-buffer zxc-ft-buffer)
  (clrhash zxc-ft-tags-all)
  (setq zxc-ft-tags-search nil)
  (setq zxc-ft-is-sub-view nil)
  (setq zxc-ft-tags-search nil)
  (setq zxc-ft-current-action nil)
  (zxc-ft-main-view)
  (zxc-ft-mode))

(defun zxc-ft-main-view ()
  (with-current-buffer zxc-ft-buffer
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((top-tags (zxc-ft-get-top-tags)))
      (remove-overlays)
      (insert "\n\t\t")
      (setq zxc-ft-tag-widget (widget-create
			       'editable-field
			       :size 100
			       :format "Currently selected tag: %v "))
      (insert "\n\n")
      (setq zxc-ft-begin-ol (make-overlay (point) (point)))
      (zxc-ft-view-create-all-tags top-tags))
    (widget-setup)))

(defun zxc-ft-view-create-all-tags (top-tags)
  "Display tag classification"
  (loop for tag in top-tags
	do (progn (insert "[" (zxc-get-action-key (nth 1 tag)) "]" " " (nth 2 tag) "\n\n")
		  (let ((child-tags (zxc-ft-get-child-tags (nth 0 tag)))
			(start-point (point)))
		    (loop for i from 0 to (- (length child-tags) 1)
			  do (progn
			       (when (and (= (% i 8) 0) (/= i 0))
				 (insert "\n"))
			       (insert "[" (zxc-get-action-key (nth 1 (nth i child-tags))) "]" (nth 2 (nth i child-tags)) "\t")))
		    (align-regexp start-point (point) "\\(\\s-*\\)\\s-" 1 1 t))
		  (insert "\n\n"))))

(defun zxc-ft-view-create-sub-tags (tags)
  "Create a tag view of the same shortname"
  (let ((inhibit-read-only t))
    (delete-region (overlay-start zxc-ft-begin-ol) (point-max))
    (goto-char (point-max))
    (loop for i from 0 to (- (length tags) 1)
	  do (progn
	       (when (and (= (% i 8) 0) (/= i 0))
		 (insert "\n"))
	       (insert "[" (zxc-get-action-key (nth i zxc-ft-sub-tags-keys)) "]" (nth 2 (nth i tags)) "\t")))
    (align-regexp (overlay-start zxc-ft-begin-ol) (point) "\\(\\s-*\\)\\s-" 1 1 t)))

(defun zxc-ft-reset-main-view (arg)
  "Return to view showing all tags"
  (interactive "P")
  (if (or (null zxc-ft-current-action) (null arg))
      (when zxc-ft-is-sub-view
	(let ((inhibit-read-only t))
	  (delete-region (overlay-start zxc-ft-begin-ol) (point-max))
	  (clrhash zxc-ft-tags-all)
	  (goto-char (point-max))
	  (zxc-ft-view-create-all-tags (zxc-ft-get-top-tags))
	  (setq zxc-ft-sub-tags-tmp nil)
	  (setq zxc-ft-is-sub-view nil)
	  (goto-char (point-min))))
    (delete-window)))

(defun zxc-ft-get-top-tags ()
  "Get all label groups with level 0"
  (let ((temp-tags (zxc-ft-query-data '((sql . "select id,short_name,tag_name from tags_ where level_=0")))))
    (zxc-ft-add-tags temp-tags)
    temp-tags))

(defun zxc-ft-get-child-tags (pid)
  "Get child tag of parent tag"
  (let ((temp-tags (zxc-ft-query-data (list (cons 'sql (concat "select id,short_name,tag_name from tags_ where p_id=" (int-to-string pid)))))))
    (zxc-ft-add-tags temp-tags)
    temp-tags))

(defun zxc-ft-query-data (param)
  "Data query service of tag"
  (getf (http-post zxc-ft-query-url
		   param)
	:data))

(defun zxc-ft-exec-data (sql)
  "Data update service for tag"
  (http-post zxc-ft-exec-url
	     (list (cons 'sql sql))))

(defun zxc-get-action-key (key-str)
  (propertize key-str 'face 'zxc-template-face-foreground))

(defun zxc-ft-add-tags (tags)
  "Store tags in the map as shortname and collection"
  (loop for tag in tags
	do (puthash (nth 1 tag) (cons tag (gethash (nth 1 tag) zxc-ft-tags-all)) zxc-ft-tags-all)))

(defun zxc-ft-build-query (tag)
  "Build tag query criteria, add when it does not exist in the query, and cancel if it exists"
  (if (not (member tag zxc-ft-tags-search))
      (setq zxc-ft-tags-search (cons tag zxc-ft-tags-search))
    (setq zxc-ft-tags-search (delete tag zxc-ft-tags-search)))
  (zxc-ft-widget-update)
  (goto-char (point-min)))

(defun zxc-ft-widget-update ()
  (widget-value-set zxc-ft-tag-widget "")
  (widget-value-set zxc-ft-tag-widget
		    (mapconcat #'(lambda (temp-tag)
				   (nth 2 temp-tag))
			       zxc-ft-tags-search
			       " + ")))

(defun zxc-ft-tag-choose ()
  "Select the label according to the selected key and output it to input"
  (interactive)
  (let ((short-cut (this-command-keys)))
    (if zxc-ft-is-sub-view
	(let* ((t-index (-elem-index short-cut zxc-ft-sub-tags-keys))
	       (tag (nth t-index zxc-ft-sub-tags-tmp)))
	  (if tag
	      (zxc-ft-build-query tag)
	    (message "tag not found")))
      (let ((tags (gethash short-cut zxc-ft-tags-all)))
	(if tags
	    (if (= (length tags) 1)
		(zxc-ft-build-query (nth 0 tags))
	      (progn (setq zxc-ft-sub-tags-tmp tags)
		     (setq zxc-ft-is-sub-view t)
		     (zxc-ft-view-create-sub-tags zxc-ft-sub-tags-tmp)))
	  (message "tag not found"))))))

(defun zxc-ft-tag-action ()
  "Query when triggered in zxc ft mode, and manage tags when triggered in dired mode"
  (interactive)
  (if (null zxc-ft-current-action)
      (zxc-ft-tag-query)
    (zxc-ft-dired-make-tags)))

(defun zxc-ft-tag-query ()
  "Query files by tag"
  (if zxc-ft-tags-search
      (let ((buffer-name (format "*ft result %s*" (widget-value zxc-ft-tag-widget))))
	(with-current-buffer (get-buffer-create buffer-name)
	  (zxc-ft-result-mode)
	  (let ((results (zxc-ft-query-data (list
					     (cons 'sql
						   (format
						    "select a.id,a.file_name,a.file_path,'n/a' update_time from files_ a,(select file_id from tag_file where tag_id in (%s) group by file_id having count(1) = %s )b where a.id = b.file_id"
						    (mapconcat #'(lambda (tag) (int-to-string (nth 0 tag))) zxc-ft-tags-search ",")
						    (length zxc-ft-tags-search)))
					     (cons 'limit "1000")))))
	    (setq tabulated-list-entries
		  (zxc-ft-result-2-entries results)
		  zxc-ft-tags-search nil)
	    (widget-value-set zxc-ft-tag-widget "")
	    (tabulated-list-print t)))
	(goto-char (point-max))
	(switch-to-buffer buffer-name))
    (message "Please select the tag first")))

(defun zxc-ft-result-2-entries (results)
  (mapcar #'(lambda (r)
	      (list (nth 0 r) `[,(nth 1 r) ,(nth 2 r) ,(nth 3 r)]))
	  results))




(define-derived-mode zxc-ft-result-mode tabulated-list-mode "FT Result"
  "display search results"
  (define-key zxc-ft-result-mode-map (kbd "RET") 'zxc-ft-result-view-open)
  (setq tabulated-list-format
	`[("Filename" 85)
	  ("Directory" 55 nil)
	  ;; todo
	  ("UpdateTime" 5 nil)
	  ])
  (setq tabulated-list-padding 3)
  (setq tabulated-list-sort-key (cons "UpdateTime" nil))
  (tabulated-list-init-header))

(defun zxc-ft-result-view-open ()
  "open file with current line"
  (interactive)
  (find-file (concat (aref (tabulated-list-get-entry) 1) "/" (aref (tabulated-list-get-entry) 0))))


(defun zxc-ft-create-new-tag ()
  "Create a new tag."
  (switch-to-buffer "*ft add tag*")
  (kill-all-local-variables)
  (set (make-local-variable 'short-name) nil)
  (set (make-local-variable 'tag-name) nil)
  (set (make-local-variable 'tag-pid) nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-create 'editable-field
		 :size 13
		 :format "ShortName: %v "
		 :notify (lambda (widget &rest ignore)
			   (setf short-name (widget-value widget))))
  (widget-create 'editable-field
		 :size 13
		 :format "FullName: %v "
		 :notify (lambda (widget &rest ignore)
			   (setf tag-name (widget-value widget))))
  (widget-insert "\n")
  (apply 'widget-create
	 'menu-choice
	 :tag "ParentNode"
	 :value nil
	 :notify (lambda (widget &rest ignore)
		   (setq tag-pid (widget-value widget)))
	 '(item :tag "No Parent" :value nil)
	 (mapcar (lambda (label) (list 'item :tag (nth 2 label) :value (nth 0 label)))
		 (zxc-ft-query-data '((sql . "select id,short_name,tag_name from tags_ where level_=0")))))
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (if (and short-name tag-name)
			       (progn
				 (if (null tag-pid)
				     (zxc-ft-exec-data (format "insert into tags_ (short_name,tag_name,level_) values ('%s','%s',0) " short-name tag-name))
				   (zxc-ft-exec-data (format "insert into tags_ (short_name,tag_name,p_id,level_) values ('%s','%s',%d, 1) " short-name tag-name tag-pid)))
				 (message "tag %s-%s created successfully" short-name tag-name))
			     (message "Short and full names are required")))
		 "Save")
  (widget-insert " ")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))


;;; dired mode 扩展

(make-local-variable 'zxc-ft-dired-file-pos )

(defun zxc-ft-dired-list-tags ()
  "Display all tags of all files in the current folder"
  (interactive)
  (save-excursion
    (remove-overlays)
    (set 'zxc-ft-dired-file-pos (make-hash-table :test 'equal))
    (let ((results (zxc-ft-query-data (list (cons 'sql (format "select b.tag_id,b.file_id,c.tag_name,a.file_name from (select id,FILE_NAME from FILES_ where FILE_PATH = '%s') a, tag_file b , tags_ c where a.id = b.file_id and c.id = b.tag_id" (substring (dired-current-directory) 0 -1)))))))
      (zxc-ft-dired-show-tags results))))

(defun zxc-ft-dired-show-tags (tag-infos)
  "tag-infos is a two-dimensional array，The order of each row is tagid、fileid，tagname、filename"
  (loop for tag-info in tag-infos
	do (let* ((file-name (nth 3 tag-info))
		  (pos (or (gethash file-name zxc-ft-dired-file-pos)
			   (progn
			     (goto-char (point-min))
			     (search-forward (nth 3 tag-info) nil t)))))
	     (when pos
	       (let ((ol (or (car (overlays-in pos pos)) (make-overlay pos pos))))
		 (puthash file-name pos zxc-ft-dired-file-pos)
		 (overlay-put ol 'after-string (propertize (concat (overlay-get ol 'after-string)  " " (nth 2 tag-info)) 'face 'font-lock-doc-face)))))))

(defun zxc-ft-dired-mark-tags ()
  "tag the selected files in the current folder"
  (interactive)
  (save-excursion
    (when (not (buffer-live-p zxc-ft-buffer))
      (zxc-ft-init)))
  (setq zxc-ft-current-action t)
  (save-excursion
    (setq  zxc-ft-tags-search (zxc-ft-query-data (list (cons 'sql (format "select a.id,a.short_name,a.tag_name from tags_ a ,(select tag_id from files_ b ,tag_file c where file_name = '%s' and file_path = '%s' and c.file_id = b.id) d where a.id = d.tag_id" (file-name-nondirectory (dired-get-file-for-visit)) (substring (dired-current-directory) 0 -1))))))
    (zxc-ft-widget-update)
    (display-buffer zxc-ft-buffer)
    (switch-window)
    (goto-char (point-min))))

(defun zxc-ft-dired-make-tags ()
  "update tag"
  (switch-window)
  (let* ((file-path (substring (dired-current-directory) 0 -1))
	 (file-name (file-name-nondirectory (dired-get-file-for-visit)))
	 (file-info (zxc-ft-query-file-info file-path file-name))
	 (file-id nil))
    (if file-info
	(setf file-id (nth 0 (car file-info)))
      (progn
	(zxc-ft-exec-data (format "insert into files_ (file_name,file_path) values ('%s','%s')" file-name file-path))
	(setf file-id (nth 0 (car (zxc-ft-query-file-info file-path file-name))))))
    (zxc-ft-exec-data (format "delete from tag_file where file_id = %d" file-id))
    (mapc #'(lambda (tag)
	      (zxc-ft-exec-data (format "insert into tag_file (tag_id,file_id) values (%d,%d)" (nth 0 tag) file-id)))
	  zxc-ft-tags-search)
    (delete-window (get-buffer-window zxc-ft-buffer))
    (setq zxc-ft-current-action nil))
  (zxc-ft-dired-list-tags))

(defun zxc-ft-query-file-info (file-path file-name)
  (zxc-ft-query-data (list (cons 'sql (format "select id,file_name,file_path from files_ where file_path = '%s' and file_name = '%s'" file-path  file-name)))))

(defun zxc-ft-dired-revert-buffer ()
  "clean tags，revert dired buffer"
  (interactive)
  (remove-overlays)
  (revert-buffer))

(defun zxc-ft-dired-sort-buffer (&optional arg)
  "clean tags，sort dired buffer"
  (interactive "P")
  (remove-overlays)
  (dired-sort-toggle-or-edit arg))

(add-hook 'dired-mode-hook #'(lambda ()
			       (define-key dired-mode-map (kbd "b") 'zxc-ft-dired-list-tags)
			       (define-key dired-mode-map (kbd "g") 'zxc-ft-dired-revert-buffer)
			       (define-key dired-mode-map (kbd "s") 'zxc-ft-dired-sort-buffer)
			       (define-key dired-mode-map (kbd "e") 'zxc-ft-dired-mark-tags)))

(provide 'zxc-ft)
