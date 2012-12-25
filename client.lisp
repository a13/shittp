(require :usocket)
(require :babel)

;; TODO: move to common package

(defun command-line ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun nth-arg (n &optional (default-value nil))
  (or (nth n (command-line))
      (format nil "~a" default-value)))

(defmacro make-proto-seq (size)
  `(make-array ,size :element-type '(unsigned-byte 8)))

;; converts string to size in hex
(defun proto-to-size (seq)
  (parse-integer (babel:octets-to-string seq)
               :radix 16 :junk-allowed t))

;; converts hexified size to string
(defun size-to-proto (size)
  (babel:string-to-octets
   (format nil "~16x" size)))


;;; client code starts here

;; returns requested file size
(defun handshake (name stream)
    (let* ((name-seq (babel:string-to-octets name))
           ;; converting name len to hexified string
           (nsize-seq (size-to-proto (length name)))
           (fsize-seq (make-proto-seq 16)))
      (write-sequence nsize-seq stream)
      (write-sequence name-seq stream)
      (force-output stream)
      (read-sequence fsize-seq stream)
      (proto-to-size fsize-seq)))


(defun get-file (host port name)
  (let* ((socket (usocket:socket-connect host port
                                         :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket))
         (fsize (handshake name stream))
         (seq (make-proto-seq fsize)))
      (read-sequence seq stream)
      (close stream)
      (usocket:socket-close socket)
      seq))


;;; arguments
;;; ./client hostname port filename
(defun client-main ()
  (let ((host (nth-arg 1 "127.0.0.1"))
        (port (parse-integer (nth-arg 2 8000) :junk-allowed t))
        (name (nth-arg 3 "test.dmp")))
    (with-open-file (stream (file-namestring name)
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (write-sequence (get-file host port name) stream))))


;;; TODO: move to build proc

#+win32
(defvar *binary* "client.exe")
#-win32
(defvar *binary* "client")

(sb-ext:save-lisp-and-die *binary*
                          :toplevel (lambda ()
                                      (client-main)
                                      0)
                          :executable t
                          :compression t)
