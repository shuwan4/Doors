;;;; Doors a window manager based on McCLIM.
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

(asdf:defsystem #:doors
  ;; :defsystem-depends-on (:deploy)
  :build-operation "program-op"
  :build-pathname "doors"
  :entry-point "doors:doors"
  
  :description "A X11 window manager based on McCLIM"
  :author "Andrea De Michele <andrea.demichele@gmail.com>"
  :license "LGPL-2.1+"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:mcclim #:mcclim-doors
               #:clim-listener #:climacs #:clim-debugger #:xembed)
  :serial t
  :components ((:file "package")
               (:file "doors-tray")
               (:file "doors")
               (:file "info-line")))

(defsystem #:mcclim-doors
  :depends-on (#:mcclim-clx
               #:mcclim-clx/truetype)
  :components((:file "package")
              (:file "patch" )
              (:file "graft" :depends-on ("package"))
              (:file "port" :depends-on ("package" "graft"))
              (:file "input" :depends-on ("port"))
              (:file "frame-manager" :depends-on ( "port" ))
              (:file "foreign-application" :depends-on ( "frame-manager"))))

