;    Copyright 2012 Christian Fortin
;
;    Filename: bhelp.el
;    Version: 1.0
;    Author: Christian Fortin <frozenlock AT gmail DOT com>
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

(defun bhelp-get-device-info-json (project-id device-id)
  "Return a plist of the DEVICE-ID (device instance) from the
  PROJECT-ID. Connect to BACnet Help to retrieve this info"
  (let ((buffer (url-retrieve-synchronously
		 (concat "http://bacnethelp.dnsd.me/api/json/project/" 
			 project-id"/"
			 device-id)))
	(json nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

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