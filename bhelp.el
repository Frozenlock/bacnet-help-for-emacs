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

(defun bhelp-get-project-info (project-id)
  "Return an info plist for a PROJECT-ID."
  (bhelp-get-json (concat "https://bacnethelp.com/api/json/project/" 
			  project-id)))

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

(defun bhelp-get-IOs (device-plist)
  "From a device plist, return only the IOs and their properties"
  (remove-if-not 
   '(lambda (object)
      (let ((object-int (plist-get object :object-int)))
	(or (= object-int 0) ;analog-input
	    (= object-int 1) ;analog-ouput
	    (= object-int 3) ;binary-input
	    (= object-int 4)))) ;binary-output
   device-plist))

(defun bhelp-get-object-property (prop-int object-plist)
  "Return the value of the property from the OBJECT-PLIST. The
property is identified by an integer,as defined in the BACnet
protocol. As a reminder:
28: Description
77: Object name
81: Out of service
117: Units"
  (let* ((properties (plist-get object-plist :object-properties))
	 (property-plist (first (remove-if-not
				(lambda (property) (= prop-int (plist-get property :prop-int)))
				properties))))
    (plist-get property-plist :prop-value)))

(defun bhelp-get-object-device (device-plist)
  "Return the device 'object'"
  (first (remove-if-not
	  (lambda (object) 
	    (= 8 (plist-get object :object-int))) device-plist)))

(provide 'bhelp)
