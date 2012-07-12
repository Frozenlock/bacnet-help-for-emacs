;    Copyright 2012 Frozenlock
;
;    Filename: bhelp.el
;    Version: 1.0
;    Author: Frozenlock <frozenlock AT gmail DOT com>
;    Keywords: bacnet-help json emacs
;    Description: Retrieve bacnet device info from bacnet-help
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;
;=====================================================
; The program begins here
;=====================================================

;;; Code:


(require 'json)

(defun bhelp-get-json (url)
  "Return a usable emacs list from a json taken from url"
  (let ((buffer (url-retrieve-synchronously url))
	(json nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))
  

(defun bhelp-get-device-info-json (project-id device-id)
  "Return a plist of the DEVICE-ID (device instance) from the
  PROJECT-ID. Connect to BACnet Help to retrieve this info"
  (bhelp-get-json (concat "https://bacnethelp.com/api/json/project/" 
			  project-id"/"
			  device-id)))

(defun bhelp-get-project-info-json (project-id)
  "Return an info plist for a PROJECT-ID. Connect to BACnet Help
to retrieve this info"
  (bhelp-get-json (concat "https://bacnethelp.com/api/json/project/" 
			  project-id)))

(defun bhelp-get-project-info (project-id)
  "Return an info plist for a PROJECT-ID."
  (let ((json-object-type 'plist))
    (bhelp-listify (json-read-from-string 
		    (bhelp-get-project-info-json project-id)))))

(defun bhelp-listify (arg)
  "Convert any remaining vector into a list"
  (mapcar (lambda (x) (if (or (vectorp x) (listp x))
			  (bhelp-listify x)
			x)) arg))


(defun bhelp-get-device-info (project-id device-id)
  "Convert a json into a plist"
  (let ((json-object-type 'plist))
    (bhelp-listify (json-read-from-string 
		    (bhelp-get-device-info-json project-id device-id)))))


(defun bhelp-get-keys (list)
  "Takes every other item in a list. In case of an object-plist,
return every properties."
  (let ((remains (cddr list)))  
    (append (list (car list))
	    (when remains
	      (bhelp-get-keys remains)))))



;; The Bacnet Help API return a plist of the following form:

;; (object-integer
;;    (object-instance
;;      (key1 property1 key2 property2 ...)
;;    )
;; )

(defun bhelp-get-IOs (device-plist)
  "From a device plist, return only the IOs and their properties"
  (let ((result '()))
    (dolist (io-type '(:0 :1 :3 :4))
      (let ((instances (plist-get device-plist io-type)))
	(when instances (setq result (append result (list io-type instances))))))
    result))

(defun bhelp-get-object-device (device-plist)
  "Return the device 'object'"
  (plist-get device-plist :8))

(provide 'bhelp)
