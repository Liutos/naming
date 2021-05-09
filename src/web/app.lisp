(in-package #:cl-user)

(defpackage #:naming.web.app
  (:use #:cl)
  (:import-from #:ningle
                #:<app>))

(in-package #:naming.web.app)

(defvar *app*
  (make-instance '<app>))
