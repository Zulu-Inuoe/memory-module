;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MEMORY-MODULE; Base: 10 -*-
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

(in-package #:memory-module)

(defvar *debug-print* nil)

(defmacro debug-print (format-string &rest args)
  `(if *debug-print*
       (format t ,format-string ,@args)))

(declaim (optimize (debug 3) (safety 3) (space 0) (speed 0)))

;;;;WinAPI constants
(defconstant +page-noaccess+ #x01)
(defconstant +page-readonly+ #x02)
(defconstant +page-readwrite+ #x04)
(defconstant +page-writecopy+ #x08)
(defconstant +page-execute+ #x10)
(defconstant +page-execute-read+ #x20)
(defconstant +page-execute-readwrite+ #x40)
(defconstant +page-execute-writecopy+ #x80)
(defconstant +page-nocache+ #x200)
(defconstant +mem-commit+ #x1000)
(defconstant +mem-reserve+ #x2000)
(defconstant +mem-decommit+ #x4000)
(defconstant +mem-release+ #x8000)
(defconstant +image-directory-entry-export+ 0)
(defconstant +image-directory-entry-import+ 1)
(defconstant +image-directory-entry-basereloc+ 5)
(defconstant +image-sizeof-base-relocation+ 8)
(defconstant +image-rel-based-absolute+ 0)
(defconstant +image-rel-based-highlow+ 3)
(defconstant +image-ordginal-flag32+ #x80000000)
(defconstant +dll-process-attach+ 1)
(defconstant +dll-thread-attach+ 2)
(defconstant +dll-thread-detach+ 3)
(defconstant +dll-process-detach+ 0)
(defconstant +image-scn-cnt-initialized-data+ #x00000040)
(defconstant +image-scn-cnt-uninitialized-data+ #x00000080)
(defconstant +image-scn-mem-discardable+ #x02000000)
(defconstant +image-scn-mem-not-cached+ #x04000000)
(defconstant +image-scn-mem-execute+ #x20000000)
(defconstant +image-scn-mem-read+ #x40000000)
(defconstant +image-scn-mem-write+ #x80000000)

(defun logandbitp (val mask)
  (not (zerop (logand val mask))))

;;;;WinAPI types
(defctype long :int32)
(defctype byte :uint8)
(defctype word :uint16)
(defctype dword :uint32)

(defcstruct image-dos-header
  (e-magic word)
  (e-cblp word)
  (e-cp word)
  (e-crlc word)
  (e-cparhdr word)
  (e-minalloc word)
  (e-maxalloc word)
  (e-ss word)
  (e-sp word)
  (e-csum word)
  (e-ip word)
  (e-cs word)
  (e-lfarlc word)
  (e-ovno word)
  (e-res word :count 4)
  (e-oemid word)
  (e-oeminfo word)
  (e-res2 word :count 10)
  (e-lfanew long))

(defcstruct image-file-header
  (machine word)
  (number-of-sections word)
  (time-data-stamp dword)
  (pointer-to-symbol-table dword)
  (number-of-symbols dword)
  (size-of-optional-header word)
  (characteristics word))

(defcstruct image-data-directory
  (virtual-address dword)
  (size dword))

(defcstruct image-export-directory
  (characteristics dword)
  (time-date-stamp dword)
  (major-version word)
  (minor-version word)
  (name dword)
  (base dword)
  (number-of-functions dword)
  (number-of-names dword)
  (address-of-functions dword)
  (address-of-names dword)
  (address-of-name-ordinals dword))

(defcstruct image-optional-header32
  (magic word)
  (major-linker-version byte)
  (minor-linker-version byte)
  (size-of-code dword)
  (size-of-initialized-data dword)
  (size-of-uninitialized-data dword)
  (address-of-entry-point dword)
  (base-of-code dword)
  (base-of-data dword)
  (image-base dword)
  (section-alignment dword)
  (file-alignment dword)
  (major-operating-system-version word)
  (minor-operating-system-version word)
  (major-image-version word)
  (minor-image-version word)
  (major-subsystem-version word)
  (minor-subsystem-version word)
  (win32-version-value dword)
  (size-of-image dword)
  (size-of-headers dword)
  (checksum dword)
  (subsystem word)
  (dll-characteristics word)
  (size-of-stack-reserve dword)
  (size-of-stack-commit dword)
  (size-of-heap-reserve dword)
  (size-of-heap-commit dword)
  (loader-flags dword)
  (number-of-rva-and-sizes dword)
  (data-directory (:struct image-data-directory) :count 16))

(defcstruct image-nt-headers32
  (signature dword)
  (file-header (:struct image-file-header))
  (optional-header (:struct image-optional-header32)))

(defcstruct image-section-header
  (name byte :count 8 )
  (virtual-size dword)
  (virtual-address dword)
  (size-of-raw-data dword)
  (pointer-to-raw-data dword)
  (pointer-to-relocations dword)
  (pointer-to-line-numbers dword)
  (number-of-relocations word)
  (number-of-line-numbers word)
  (characteristics dword))

(defcstruct image-import-descriptor
  (original-first-thunk dword)
  (time-date-stamp dword)
  (forwarder-chain dword)
  (name dword)
  (first-thunk dword))

(defcstruct image-import-by-name
  (hint word)
  (name byte :count 1))

;;;;Various WinAPI functions from the kernel and user subsystems
(defcfun (virtual-alloc "VirtualAlloc") :pointer
  (address :pointer)
  (size :uint32)
  (allocation-type :uint32)
  (protect :uint32))

(defcfun (virtual-free "VirtualFree") :int
  (address :pointer)
  (size :uint32)
  (free-type :uint32))

(defcfun (virtual-protect "VirtualProtect") :int
  (address :pointer)
  (size :uint32)
  (new-protect dword)
  (old-protect (:pointer dword)))

;;;TODO: Replace with LoadLibraryW for unicode support
(defcfun (load-library "LoadLibraryW") :pointer
  (name (:string :encoding :utf8)))

(defcfun (free-library "FreeLibrary") :int
  (lib :pointer))

(defcfun (get-proc-address "GetProcAddress") :pointer
  (lib :pointer)
  (proc-name :string))

(defcfun (is-bad-read-ptr "IsBadReadPtr") :int
  (lp :pointer)
  (ucb dword))

;;;Could accomplish the same with CL standard string-lessp and friends,
;;;but using this prevents the conversion from foreign to lisp
(defcfun (str-i-cmp "_stricmp") :int
  (str1 :string)
  (str2 :string))

;;;C standard library functions (assumes char is 8-bit, and size_t 32-bit)
(defcfun (memcpy "memcpy") :pointer
  (dst :pointer)
  (src :pointer)
  (size :uint32))

(defcfun (memset "memset") :pointer
  (dst :pointer)
  (value :uint8)
  (size :uint32))

;;;;Some short names for commonly used functions
(defmacro defalias (sym fn)
  "Define a function to be an alias for another."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-function (quote ,sym)) (function ,fn))))

(defmacro & (object type field)
  `(foreign-slot-pointer ,object ',type ',field))

(defmacro -> (object type field)
  `(foreign-slot-value ,object ',type ',field))

(defmacro sizeof (type)
  `(foreign-type-size ',type))

(defalias pa pointer-address)
(defalias fsv foreign-slot-value)
(defalias fsp foreign-slot-pointer)
(defalias fso foreign-slot-offset)
(defalias fts foreign-type-size)

(defstruct memory-module
  "Contains information for a module (shared library) in memory."
  headers
  code-base
  modules
  initialized)

(defun to-c-array (vector &optional (loc (null-pointer) loc-p))
  "Mirrors the contents of VECTOR which should be of type (unsigned-byte 8) in foreign memory, and returns a pointer to the region."
  (cond
    (loc-p
     (loop :for byte :across vector
           :doing 
              (setf (mem-ref loc 'byte) byte)
              (incf-pointer loc)))
    (t
     (setf loc (foreign-alloc :uint8 :count (length vector) :initial-contents vector))))
  loc)

(defmacro with-c-array ((var vector) &body body)
  `(let ((,var (to-c-array ,vector)))
     (unwind-protect
          (progn
            ,@body)
       (foreign-free ,var))))

(defun image-first-section (nt-header)
  (inc-pointer nt-header
               (+ (fso '(:struct image-nt-headers32) 'optional-header)
                  (-> (& nt-header (:struct image-nt-headers32) file-header)
                      (:struct image-file-header)
                      size-of-optional-header))))

(defun map-sections (fn pe-headers)
  "Applies fn to each section in module.
fn should be a function of 1 argument, the section."
  (loop :with file-header := (& pe-headers (:struct image-nt-headers32) file-header)
        :with num-sections := (-> file-header (:struct image-file-header) number-of-sections)
        :repeat num-sections
        :for section := (image-first-section pe-headers)
          :then (inc-pointer section (sizeof (:struct image-section-header)))
        :doing
           (funcall fn section)))

(defun image-snap-by-ordinal (ordinal)
  (not (zerop (logand ordinal +image-ordginal-flag32+))))

(defun image-ordinal (ordinal)
  (logand ordinal #xFFFF))

(defun copy-sections (buf pe-header module)
  (let ((section-alignment (-> (& pe-header (:struct image-nt-headers32) file-header)
                               (:struct image-optional-header32) 
                               section-alignment))
        (code-base (memory-module-code-base module)))
    (map-sections
     (lambda (section)
       (with-foreign-slots ((size-of-raw-data pointer-to-raw-data virtual-address virtual-size) section (:struct image-section-header))
         (let ((data-size size-of-raw-data) 
               (data-offset pointer-to-raw-data)
               (new-section nil))
           (cond
             ((plusp data-size)
              ;;Commit memory block and copy data from memory-buffer
              (debug-print "~&Allocating new section of size ~D with offset ~D" data-size data-offset)
              (setf new-section (virtual-alloc (inc-pointer code-base virtual-address) data-size +mem-commit+ +page-readwrite+))
              
              (let ((src (inc-pointer buf data-offset)))
                (debug-print "~&Copying section from ~X to ~X, size ~D~%" (pointer-address src) (pointer-address new-section) data-size)
                (memcpy new-section src data-size))
              
              (setf virtual-size (pointer-address new-section)))
             ((plusp section-alignment)
              (debug-print "~&Allocating new empty section with alignment ~D" section-alignment)
              (setf new-section (virtual-alloc (inc-pointer code-base virtual-address) section-alignment +mem-commit+ +page-readwrite+))
              (debug-print "~&Zeroing out section at ~X, size ~D~%" (pointer-address new-section) section-alignment)
              (memset new-section 0 section-alignment)
              (setf virtual-size (pointer-address new-section)))
             (t
              (warn "Section @ ~X has ~A section size, and section alignment for the image is ~A.." section data-size section-alignment))))))
     (memory-module-headers module))))

(defun get-header-dictionary (module index)
  (mem-aptr 
   (& 
    (&
     (memory-module-headers module)
     (:struct image-nt-headers32)
     optional-header)
    (:struct image-optional-header32)
    data-directory)
   '(:struct image-data-directory)
   index))

(defun get-entry-point (module 
                        &aux 
                          (base (memory-module-code-base module)) 
                          (pe-header (memory-module-headers module)))
  (inc-pointer base (-> (& pe-header (:struct image-nt-headers32) optional-header) 
                        (:struct image-optional-header32) address-of-entry-point)))

(defun perform-base-relocation (module delta 
                                &aux 
                                  (code-base (memory-module-code-base module))
                                  (directory (get-header-dictionary module +IMAGE-DIRECTORY-ENTRY-BASERELOC+)))
  (when (plusp (-> directory (:struct  image-data-directory) size))
    (loop :for relocation := (inc-pointer code-base (-> directory (:struct  image-data-directory) virtual-address))
            :then (inc-pointer relocation (-> relocation (:struct image-data-directory) size))
          :while (plusp (-> relocation (:struct  image-data-directory) virtual-address))
          :do
             (loop :repeat (truncate (- (-> relocation (:struct image-data-directory) size) 
                                        +image-sizeof-base-relocation+) 
                                     2)
                   :with dest := (inc-pointer code-base (-> relocation (:struct  image-data-directory) virtual-address))
                   :for rel-info := (inc-pointer relocation +image-sizeof-base-relocation+)
                     :then (inc-pointer rel-info (fts 'word))
                   :for type := (ldb (cl:byte 4 12) (mem-ref rel-info 'word))
                   :for offset := (ldb (cl:byte 12 0) (mem-ref rel-info 'word))
                   :do
                      (ecase type
                        (#.+image-rel-based-absolute+
                         ;;Do nothing
                         )
                        (#.+image-rel-based-highlow+
                         (incf (mem-ref (inc-pointer dest offset) 'dword) delta)))))))

(defun build-import-table (module 
                           &aux 
                             (code-base (memory-module-code-base module))
                             (directory (get-header-dictionary module +image-directory-entry-import+)))
  (when (plusp (-> directory (:struct  image-data-directory) size))
    (loop :for import-desc := (inc-pointer code-base (-> directory (:struct image-data-directory) virtual-address))
            :then (inc-pointer import-desc (sizeof (:struct image-import-descriptor)))
          :while (and (zerop (is-bad-read-ptr import-desc (fts '(:struct image-import-descriptor))))
                      (not (zerop (-> import-desc (:struct image-import-descriptor) name))))
          :for original-first-thunk := (-> import-desc (:struct image-import-descriptor) original-first-thunk)
          :for first-thunk := (-> import-desc (:struct image-import-descriptor) first-thunk)
          :for lib-name := (inc-pointer code-base (-> import-desc (:struct image-import-descriptor) name))
          :for lib := (load-library lib-name)
          :do 
             (debug-print "~&Importing library \"~A\"~%" (foreign-string-to-lisp lib-name))
             (push lib (memory-module-modules module))
             
             (loop :for thunk-ref := (inc-pointer code-base (if (zerop original-first-thunk) first-thunk original-first-thunk))
                     :then (inc-pointer thunk-ref (fts :pointer))
                   :for func-ref := (inc-pointer code-base first-thunk)
                     :then (inc-pointer func-ref (fts 'dword))
                   :until (zerop (mem-ref thunk-ref 'dword))
                   :for proc-name := (if (image-snap-by-ordinal (mem-ref thunk-ref 'dword))
                                         (make-pointer (image-ordinal (mem-ref thunk-ref 'dword)))
                                         (& (inc-pointer code-base (mem-ref thunk-ref 'dword))
                                            (:struct image-import-by-name)
                                            name))
                   :do
                      (debug-print "~&Importing function with name \"~A\"~% into \"~A\"" (foreign-string-to-lisp proc-name) func-ref)
                      (setf (mem-ref func-ref :pointer) (get-proc-address lib proc-name))))))

(defun make-protection-flags (characteristics)
  (let ((executable (logandbitp characteristics +image-scn-mem-execute+))
        (readable (logandbitp characteristics +image-scn-mem-read+))
        (writeable (logandbitp characteristics +image-scn-mem-write+))
        (not-cached (logandbitp characteristics +image-scn-mem-not-cached+)))
    (logior (if not-cached +page-nocache+ 0)
            (if executable
                (if readable
                    (if writeable
                        +page-execute-readwrite+
                        +page-execute-read+)
                    (if writeable
                        +page-execute-writecopy+
                        +page-execute+))
                (if readable
                    (if writeable
                        +page-readwrite+
                        +page-readonly+)
                    (if writeable
                        +page-writecopy+
                        +page-noaccess+))))))

(defun finalize-sections (module)
  (let* ((pe-headers (memory-module-headers module))
         (optional-header (& pe-headers (:struct image-nt-headers32) optional-header))
         (size-of-initialized-data (-> optional-header (:struct image-optional-header32) size-of-initialized-data))
         (size-of-uninitialized-data (-> optional-header (:struct image-optional-header32) size-of-uninitialized-data)))
    (map-sections
     (lambda (section)
       (let* ((size (-> section (:struct image-section-header) size-of-raw-data))
              (address (make-pointer (-> section (:struct image-section-header) virtual-size))) ;TODO 64-bit
              (characteristics (-> section (:struct image-section-header) characteristics))
              (discardable (logandbitp characteristics +image-scn-mem-discardable+))
              (contains-initialized-data (logandbitp characteristics +image-scn-cnt-initialized-data+))
              (contains-uninitialized-data (logandbitp characteristics +image-scn-cnt-uninitialized-data+))
              (protection-flags (make-protection-flags characteristics)))
         (debug-print "~&Finalizing section ~A. Section is ~:[not discardable~;discardable~]" section discardable)
         (if discardable ;We can get rid of it 
             (virtual-free address size +mem-decommit+)
             (progn ;Otherwise keep going
               (when (zerop size)
                 (cond
                   (contains-initialized-data
                    (setf size size-of-initialized-data))
                   (contains-uninitialized-data
                    (setf size size-of-uninitialized-data))))
               (debug-print "~&Setting protection flags of section ~A to: ~A~%" section protection-flags)
               (when (plusp size)
                 (with-foreign-object (old-protect 'dword)
                   (when (zerop (virtual-protect address size protection-flags old-protect))
                       (warn t "Failed to protect section ~A!" section))))))))
     pe-headers)))

(defun memory-get-proc-address (module name)
  (let* ((code-base (memory-module-code-base module))
         (directory (get-header-dictionary module +image-directory-entry-export+))
         (exports (inc-pointer code-base (-> directory (:struct image-data-directory) virtual-address))))
    (debug-print "~&Attempting to load function ~A from ~A~%" name module)
    (debug-print "~&Directory: ~A, exports:~A~%" directory exports)
    (with-foreign-slots ((number-of-names address-of-names address-of-name-ordinals address-of-functions) exports (:struct image-export-directory))      
      (loop :repeat number-of-names
            :for name-ref := (inc-pointer code-base address-of-names)
              :then (inc-pointer name-ref (fts 'dword))
            :for ordinal := (inc-pointer code-base address-of-name-ordinals)
              :then (inc-pointer ordinal (fts 'word))
            :for name-ptr := (inc-pointer code-base (mem-ref name-ref 'dword))
            :do (debug-print "~&Comparing with ~A~%" (foreign-string-to-lisp name-ptr))
            :if (zerop (str-i-cmp name name-ptr))
              :do (debug-print "~&Offset: ~A~%" (mem-ref (inc-pointer code-base (+ address-of-functions (* 4 (mem-ref ordinal 'word)))) 'dword))
              :and
                :return (inc-pointer code-base (mem-ref (inc-pointer code-base (+ address-of-functions (* 4 (mem-ref ordinal 'word)))) 'dword))))))

(defun memory-load-library (buf)
  (with-c-array (buf buf)
    (let* ((dos-header buf)
           (pe-offset (-> dos-header (:struct image-dos-header) e-lfanew))
           (pe-header (inc-pointer dos-header pe-offset))
           (optional-header (& pe-header (:struct image-nt-headers32) optional-header))
           (image-base (-> optional-header (:struct image-optional-header32) image-base))
           (image-size (-> optional-header (:struct image-optional-header32) size-of-image))
           (headers-size (-> optional-header (:struct image-optional-header32) size-of-headers))
           (pe-size (+ pe-offset headers-size))
           code
           headers
           module)

      (debug-print "~&Allocating ~D bytes @ ~X~%" image-size image-base)
      ;;Allocate memory for the library at the desired location
      (setf code (virtual-alloc (make-pointer image-base) image-size (logior +mem-reserve+ +mem-commit+) +page-readwrite+))
      ;;But if it fails, try at a random location
      (when (null-pointer-p code)
        (debug-print "~&Address unavailable. Re-Attempting allocation~%")
        (setf code (virtual-alloc (null-pointer) image-size (logior +mem-reserve+ +mem-commit+) +page-readwrite+))
        (when (null-pointer-p code)
          (error "Out of memory")))
      
      (debug-print "~&Allocated ~D bytes @ ~X~%" image-size (pointer-address code))
      
      ;;Allocate memory for the headers
      (debug-print "~&Allocating ~D bytes for headers~%" headers-size)
      (setf headers (virtual-alloc code headers-size +mem-commit+ +page-readwrite+)
            headers (inc-pointer headers pe-offset))
      
      (debug-print "~&Headers established @ ~X~%" (pointer-address headers))
      
      (debug-print "~&Copying pe-headers from ~X to ~X, size ~D" (pointer-address dos-header) (pointer-address  code) pe-size)
      ;;Copy PE header to code
      (memcpy code dos-header pe-size)
      
      (setf module (make-memory-module 
                    :headers headers 
                    :code-base code 
                    :modules nil
                    :initialized nil))
      (unwind-protect
           (progn
             ;;Copy sections from DLL memory buffer to code
             (debug-print "~&Copying sections~%")
             (copy-sections buf pe-header module)
             
             ;;Adjust base address of imported data
             (let ((delta (- (pointer-address code) image-base)))
               (when (not (zerop delta))
                 (debug-print "~&Relocating base by ~D delta" delta)
                 (perform-base-relocation module delta)))

             (debug-print "~&Building import table~%")
             (build-import-table module)
             (debug-print "~&Finalizing sections~%")
             (finalize-sections module)
             (debug-print "~&Calling entry point~%")
             (let ((entry-point (get-entry-point module)))
               (debug-print "~&Entry point is @ ~X" (pointer-address entry-point))
               (let ((val (foreign-funcall-pointer entry-point (:convention :stdcall) :pointer code dword +dll-process-attach+ dword 0 :int)))
                 (when (zerop val)
                   (warn "Initialization failed with value ~A!" val)))))
        (memory-free-libary module))

      module)))
  
(defun memory-free-libary (module &aux 
                                    (code-base (memory-module-code-base module)) 
                                    (modules (memory-module-modules module)))
  (when (memory-module-initialized module)
    (let ((entry-point (get-entry-point module)))
      (let ((val (foreign-funcall-pointer entry-point (:convention :stdcall) 
                                          :pointer code-base dword +dll-process-detach+ dword 0 
                                          :int)))
        (when (zerop val)
          (warn "Dettach call failed library failed.")))))
  
  (dolist (module modules)
    (free-library module))
  
  (unless (null-pointer-p code-base)
    (virtual-free code-base 0 +mem-release+))
  
  (setf (memory-module-modules module) ()
        (memory-module-headers module) (null-pointer)
        (memory-module-code-base module) (null-pointer)
        (memory-module-initialized module) nil)
  
  module)

(defun test-memory-load (path)
  (memory-load-library (read-file-into-byte-vector path)))

;;;;Issues
;;1)
;;;Saw a crash with exception code c0000005 (ACCESS_VIOLATION) after loading a library.. 
;;;An exception was thrown from SBCL on an undefined variable at the REPL when it happened
;;;Could it be the exception mechanism passing through the foreign library? Do I have some leak somewhere?
;;;I had invoked an earlier version of that library (dll) before freeing it and then loaded it the new one..
