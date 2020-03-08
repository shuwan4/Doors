;;;; Copyright (C) 2020  Andrea De Michele
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
;;;; USA

(in-package :doors)

;;;; Wireless
(defun wireless ()
  (let* ((iwoutput (uiop:run-program "iwconfig" :output :string))
         (regex-scan (nth-value 1 (cl-ppcre:scan-to-strings
                                   (cl-ppcre:create-scanner
                                    "ESSID:\"(.*)\".*Link Quality=([0-9]*/[0-9]*)"
                                    :single-line-mode t)
                                   iwoutput)))
         (name (if regex-scan
                   (aref regex-scan 0)
                   "No Link"))
         (quality (* 100
                     (if regex-scan
                      (read-from-string
                       (aref regex-scan 1))
                      0))))
    (format nil "(~a: ~d%)" name (round quality))))

;;;; RAM
(defun ram-usage ()
  (let ((usage (uiop:run-program "sh /home/ryan/scripts/mem.sh" :output :string)))
    (format nil "~a" usage)))

;;;; Volume
(defun vol ()
  (let ((v (uiop:run-program "sh /home/ryan/scripts/volume.sh" :output :string)))
    (format nil "Volume: ~a%" v)))

;;;; Weather
(defun weather ()
  (let ((w (uiop:run-program "sh /home/ryan/scripts/weather.sh" :output :string)))
    (format nil "~a" w)))

;;;; CPU
(defun cpu-usage ()
  (let ((c (uiop:run-program "sh /home/ryan/scripts/cpu.sh" :output :string)))
    (format nil "CPU: ~a%" c)))

;;;; Synergy
(defun synergy-running ()
  (let ((s (uiop:run-program "sh /home/ryan/scripts/synergy-running.sh" :output :string)))
    (format nil "~a" s)))

(defun display-info (frame pane)
  (format pane " " )
  (multiple-value-bind (sec min h d m y) (decode-universal-time (get-universal-time))
    (format pane "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d " y m d h min sec))
  (format pane "~a|" (wireless))
  (format pane "~a|" (ram-usage))
  (format pane "~a|" (vol))
  (format pane "~a|" (weather))
  (format pane "~a|" (cpu-usage))
  (format pane "~a|" (synergy-running))
  (loop for frame in (managed-frames)
     when (typep frame 'application-frame)
     do
       (if (eql frame (active-frame (port *wm-application*)))
             (with-text-face (pane :bold)
               (present frame 'application-frame))
             (present frame 'application-frame))))
