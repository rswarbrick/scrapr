(defpackage :scrapr
  (:use :cl :drakma :cl-ppcre)
  (:export
   :flickr-user-p
   :user-sets
   :set-photos
   :original-photo-link-from-photo-url
   :original-photo-urls))

(in-package :scrapr)

(defparameter *root-url* "http://www.flickr.com/photos/")
(defparameter *setdiv-regexp*
  "<div class=\"SetCase\">.*?href=\"([^\"]+)\".*?alt=\"([^\"]+)\"")
(defparameter *set-next-page-regexp*
  "<a href=\"([^\"]+)\" class=\"Next\">next &rarr;</a>")
(defparameter *set-photo-regexp*
  "<a href=\"(/photos/[^\"]+)\"[^>]+class=\"image_link\">")
(defparameter *original-photo-link-regexp*
  "<img src=\"(http://farm[^\"]+_o.[^.]+?)\"")

(defun flickr-user-p (name)
  (= 200 (nth-value
          1 (http-request (concatenate 'string *root-url* name "/")))))

(defun set-pairs (text)
  (let ((acc))
    (do-register-groups (url alt) (*setdiv-regexp* text)
      (push (cons alt url) acc))
    (nreverse acc)))

(defun user-sets (name)
  (set-pairs (http-request (concatenate 'string *root-url* name "/sets/"))))

(defun next-page-from-set (text)
  (multiple-value-bind (hit arr) (scan-to-strings *set-next-page-regexp* text)
    (when hit (aref arr 0))))

(defun photos-list-from-set (text)
  (let ((acc))
    (do-register-groups (url) (*set-photo-regexp* text)
      (push url acc))
    (nreverse acc)))

(defun flickr-resolve (url)
  (cond
    ((and (> (length url) 7) (string= (subseq url 0 7) "http://")) url)
    ;; To allow us to pass nil as a url without needing to be careful elsewhere.
    ((null url) nil)
    ((equal #\/ (aref url 0)) (concatenate 'string "http://www.flickr.com" url))
    (t
     (error "Can't resolve relative url."))))

(defun set-photos (set-url)
  (do* ((url (flickr-resolve set-url)
             (flickr-resolve (next-page-from-set text)))
        (text (http-request url) (when url (http-request url)))
        (acc))
       ((not url) (remove-duplicates acc :test #'string=))
    (setf acc (nconc acc (photos-list-from-set text)))))

(defun photo-string-root (photo-url)
  "Get the bit up to the id number. For example,
http://www.flickr.com/photos/whiternoise/4581821046"
  (or (flickr-resolve (scan-to-strings "^[^0-9]+[0-9]+" photo-url))
      (error "Cannot get a photo string out of '~A'." photo-url)))

(defun photo-url-to-orig-size-page (photo-url)
  (concatenate 'string (photo-string-root photo-url) "/sizes/o"))

(defun original-photo-link-from-page (text)
  (multiple-value-bind (hit arr)
      (scan-to-strings *original-photo-link-regexp* text)
    (unless hit
      (error "Couldn't find photo link in text."))
    (aref arr 0)))

(defun original-photo-link-from-photo-url (url)
  (original-photo-link-from-page
   (http-request (photo-url-to-orig-size-page url))))

(defun original-photo-urls (user setregexp outpath)
  (labels ((logstr (&rest args) (apply #'format t args) (force-output))
           (fail (&rest args) (logstr "FAIL~%") (apply #'error args))
           (success () (logstr "SUCCESS~%")))
    (let ((sets) (set) (photos))
      (logstr "Finding user... ")
      (unless (flickr-user-p user)
        (fail "User '~A' does not seem to exist." user))
      (success)

      (logstr "Looking for set... ")
      (setf sets (user-sets user))
      (logstr "(found ~D) " (length sets))
      (setf set (find-if (lambda (s) (scan setregexp s)) sets :key #'car))
      (unless set
        (fail "Couldn't find a set matching '~A'.~%Available sets: ~A"
              setregexp (mapcar #'car sets)))
      (success)

      (logstr "Getting list of photos... ")
      (setf photos (set-photos (cdr set)))
      (logstr "(found ~D) " (length photos))
      (success)

      (logstr "Opening list output file... ")
      (with-open-file (s outpath :direction :output)
        (success)
        
        (logstr "Finding urls for photos...~%")
        (loop
           for photo in photos
           for i from 1
           do
             (logstr "  ~A / ~A... " i (length photos))
             (format s "~A~%"
                     (original-photo-link-from-photo-url photo))
             (force-output s)
             (success)))
      (logstr "~%Finished. The url list should be stored at ~A" outpath))))
