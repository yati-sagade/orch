;; orch.el
;; 
;;
;; We start a very simple HTTP server (by default on port 4000), which has two endpoints:
;;
;; - /ping   (GET)     Responds with "pong" (text/plain). Can be used to server liveness check.
;;
;; - /insert (POST)    Inserts an SVG image, given by the "file" POST parameter of the incoming
;;                     request. The request body must encoded using the HTTP multipart POST data
;;                     format
;;                      
;;                     If all goes well, this endpoint responds with "OK" (text/plain). Otherwise,
;;                     the error message is returned, also in text/plain.
;;
;; 
(ws-start
 ;; The following list defines the two handlers for the two endpoints
 ;; mentioned above.  Each element of the list is a cons cell,
 ;; specified in the dotted pair syntax
 ;; (https://www.gnu.org/software/emacs/manual/html_node/elisp/Dotted-Pair-Notation.html)
 ;; with the CAR being yet another cons cell with the method and the
 ;; endpoint path, and the CDR being a function that handles the
 ;; request to this endpoint.
 '(((:GET . "/ping") .
     (lambda (request)
       ;; with-slots pulls out "slots" (or object
       ;; attributes/properties/members) from an object and binds them
       ;; to lexical variables of the same name, valid within the body
       ;; of the with-slots invocation
       (with-slots (process headers) request
         ;; process is the Emacs process object, that also encapsulates network I/O.
         (ws-response-header process 200 '("Content-type" . "text/plain"))
         (process-send-string process "pong"))))
   
   ((:POST . "/insert") .
    (lambda (request)
      ;; An "assoc list" is just a list of (key value) pairs. Calling
      ;; (assoc "key" assoclist) will returns the *pair* (key value),
      ;; if it exists in the assoclist, nil otherwise.  headers is
      ;; such an assoclist, constructed by parsing the request headers
      ;; and the request body. We are expecting the contents of the
      ;; incoming file to be in the assoc list with key "file".
      (with-slots (process headers) request
        (let ((file (cdr (assoc "file" headers))))
          (if (null file)
              (respond-text/plain process 400 "Missing file")
            ;; write-to-sha1-file generates a filename for our SVG
            ;; content by taking a SHA1 sum of it and then adding any
            ;; extension string that we give it, and writes our
            ;; content to that filename under the directory returned
            ;; by (image-dir), returning us the absolute path of the
            ;; file.
            (let ((image-path (write-to-sha1-file (cdr (assoc 'content file)) "svg")))
              (print (concat "Wrote " image-path))
              (respond-text/plain process 200 "OK")
              image-path)))))))
 ;; Port
 4000)

;; Stops all servers by calling ws-stop on the global list of all
;; server objects.
(mapc #'ws-stop ws-servers)

(assoc "lma" '(nil ("lama" . "olala")))

(defun image-dir () "~/Pictures/orch")

(defun respond-text/plain (process status-code message)
  "Write an HTTP message to `process` with the given status code. The
   Content-type header is set to text/plain"
  (ws-response-header process status-code '("Content-type" . "text/plain"))
  (process-send-string process message))

(defun construct-svg-file-text (svg-path)
  (concat
   "<svg width=\"500\" height=\"500\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">"
   (format "<path d=\"%s\" stroke=\"black\" fill=\"none\" />" svg-path)
   "</svg>"))

(defun write-to-sha1-file (text &optional extn)
  "Computes the SHA1 hash of text and writes text to the filename,
   obtained by appending extn, if given, to this hash. The file will
   be written under the directory whose name is returned by
   (image-file).

   Returns the absolute path of the written file."
  (let* ((file (sha1-filename text extn)))
    ;; with-temp-file opens a temporary *buffer*, upon which normal
    ;; emacs actions can be done, and then it writes this temp buffer
    ;; to the given filename (the first argument. It returns the value
    ;; of the last expression in its body.
    (with-temp-file (sha1-filename text extn)
      (insert text))
    file))

(defun sha1-filename (object &optional extension)
  "Builds a filename from an object by appending the SHA1 hash of the
   object to the directory name returned by (image-dir), optionally
   followed by an extension."
  (let ((h (secure-hash 'sha1 object))
        (ext (if (null extension)
                 ""
               (concat "." extension))))
    (expand-file-name (concat h ext) (image-dir))))

(sha1-filename "olala" "txt")

(with-temp-file "/tmp/out.el"
  (insert "lama"))

(secure-hash 'sha1 "lamalsd")
(expand-file-name "lama" "~/Pictures")
