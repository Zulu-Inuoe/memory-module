;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;Copyright © 2013 Wilfredo Velázquez-Rodríguez
;;;
;;;This software is provided ‘as-is’, without any express or implied 
;;;warranty. In no event will the authors be held liable for any damages
;;;arising from the use of this software.
;;;
;;;Permission is granted to anyone to use this software for any purpose,
;;;including commercial applications, and to alter it and redistribute 
;;;it freely, subject to the following restrictions:
;;;
;;;The origin of this software must not be misrepresented; you must not
;;;claim that you wrote the original software. If you use this software
;;;in a product, an acknowledgment in the product documentation would 
;;;be appreciated but is not required.
;;;
;;;Altered source versions must be plainly marked as such, and must not 
;;;be misrepresented as being the original software.
;;;
;;;This notice may not be removed or altered from any source distribution.

(in-package #:cl-user)

(defpackage #:memory-module
  (:nicknames #:memory-module)
  (:use #:alexandria #:cffi #:cl)
  (:shadow #:byte)
  (:export 
   #:memory-free-library
   #:memory-get-proc-address
   #:memory-load-library))

(in-package #:memory-module)
