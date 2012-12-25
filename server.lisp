;; (defpackage :dk.test-srv)

(require :usocket)
(require :babel)
(require :bordeaux-threads)

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

;;; server starts here

(defvar *server* nil)

(defun try-make-thread (name function)
  #+bordeaux-threads
  (bt:make-thread function :name name)
  #-bordeaux-threads
  (funcall function))

(defun read-seq-from-stream (size stream)
  (let ((seq (make-proto-seq size)))
    (read-sequence seq stream)
    seq))

(defun handle-request (stream)
  (let* ((seq-nsize (read-seq-from-stream 
                     16 stream))
         (seq-name (read-seq-from-stream 
                    (proto-to-size seq-nsize) stream))
         (file (babel:octets-to-string seq-name)))
    (with-open-file (fstream file
                             :if-does-not-exist nil
                             :direction :input
                             :element-type '(unsigned-byte 8))
      (let* ((fsize (if fstream (file-length fstream) 0))
             (seq-fsize (size-to-proto fsize))
             (seq (make-proto-seq fsize)))
        (write-sequence seq-fsize stream)
        (force-output stream)
        ;; read file only if exist
        (when fstream (read-sequence seq fstream))
        (write-sequence seq stream)))))

(defun run-server (socket)
  (loop
     (usocket:wait-for-input socket)
     (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
       (try-make-thread (format nil "handler for ~s" stream)
                        (lambda ()
                          (with-open-stream (stream stream)
                            (handle-request stream)))))))

(defun start-server (port)
  (let ((socket (usocket:socket-listen usocket:*wildcard-host*
                                       port
                                       :reuse-address t
                                       :element-type '(unsigned-byte 8))))
    (setf *server*
          (try-make-thread (format nil "port ~a server" port)
                           (lambda ()
                             (unwind-protect
                                  (run-server socket)
                               (usocket:socket-close socket)))))))

#+bordeaux-threads
(defun stop-server ()
  (let ((server (shiftf *server* nil)))
    (when server
      (bt:destroy-thread server))))

(defun srv-main ()
  (let* ((port-arg (nth-arg 1 ""))
         (port (or (parse-integer port-arg :junk-allowed t)
                   8000)))
    (format #.*standard-output* 
            "starting server on port ~s~%" port)    
    (start-server port))
  (read-char)
  #+bordeaux-threads
  (stop-server))

;;; TODO: move to build proc

#+win32
(defvar *binary* "server.exe")
#-win32
(defvar *binary* "server")

(sb-ext:save-lisp-and-die *binary* 
                          :toplevel (lambda ()
                                      (srv-main)
                                      0)
                          :executable t
                          :compression t)
