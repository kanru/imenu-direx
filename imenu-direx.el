;;; imenu-direx.el --- Tree style source code viewer for any IMenu compatible buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Kan-Ru Chen (陳侃如)

;; Author: Kan-Ru Chen (陳侃如) <kanru@kanru.info>
;; Keywords: convenience

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

;;; Code:

(require 'direx)
(require 'imenu)

(defgroup imenu-direx nil
  "Tree style source code viewer for IMenu compatible buffer"
  :group 'programming
  :prefix "imenu-direx")

(defclass imenu-direx--file (direx:tree direx:node)
  ((buffer :initarg :buffer
           :accessor imenu-direx--imenu-buffer)
   (file-name :initarg :file-name
              :accessor direx:file-full-name)
   (cache :initarg :cache
          :accessor imenu-direx--imenu-cache)))

(defclass imenu-direx--node (direx:node)
  ((children :initarg :children
             :accessor imenu-direx--node-children)))

(defclass imenu-direx--leaf (direx:leaf)
  ((marker :initarg :marker
           :accessor imenu-direx--node-marker)))

(defun imenu-direx--treep (node)
  (consp (cdr node)))

(defun imenu-direx--make-node (node)
  (if (imenu-direx--treep node)
      (make-instance 'imenu-direx--node
                     :children (cdr node)
                     :name (car node))
    (make-instance 'imenu-direx--leaf
                   :name (car node)
                   :marker (cdr node))))

(cl-defmethod direx:node-children ((node imenu-direx--file))
  (mapcar #'imenu-direx--make-node
          (delete-if (lambda (node)
                       (string= "*Rescan*" (car node)))
                     (imenu-direx--imenu-cache node))))

(cl-defmethod direx:node-children ((node imenu-direx--node))
  (mapcar #'imenu-direx--make-node
          (imenu-direx--node-children node)))

(defclass imenu-direx--item (direx:item)
  ())

(cl-defmethod direx:make-item ((node imenu-direx--node) parent)
  (let ((item (make-instance 'imenu-direx--item :tree node :parent parent)))
    (setf (direx:item-face item) 'imenu-direx-node-face)
    item))

(cl-defmethod direx:make-item ((node imenu-direx--leaf) parent)
  (let ((item (make-instance 'imenu-direx--item :tree node :parent parent)))
    (setf (direx:item-face item) 'imenu-direx-leaf-face)
    item))

(cl-defmethod direx:generic-find-item ((item imenu-direx--item) not-this-window)
  (let* ((node (direx:item-tree item))
         (root (direx:item-tree (direx:item-root item))))
    (pop-to-buffer (imenu-direx--imenu-buffer root))
    (goto-char (imenu-direx--node-marker node))))

;;; Face

(defface imenu-direx-node-face
  '((t :inherit 'font-lock-type-face))
  "Face for category name in direx tree"
  :group 'imenu-direx)

(defface imenu-direx-leaf-face
  '((t :inherit font-lock-function-name-face))
  "Face for method name in direx tree"
  :group 'imenu-direx)

;;; Command

(defun imenu-direx--make-buffer ()
  (direx:ensure-buffer-for-root
   (make-instance 'imenu-direx--file
                  :name (format "*direx-imenu: %s*" (buffer-name))
                  :buffer (current-buffer)
                  :file-name (buffer-file-name)
                  :cache (imenu--make-index-alist))))

(defun imenu-direx ()
  (interactive)
  (pop-to-buffer (imenu-direx--make-buffer)))

(provide 'imenu-direx)
;;; imenu-direx.el ends here
