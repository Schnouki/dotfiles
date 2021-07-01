;;; 16-wealf --- with-eval-after-load-feature
;;; Commentary:
;;
;; An alternative to with-eval-after-load with fine compilation
;; and no errors when using macros that are loaded later.
;;
;;; Acknowledgements:
;;
;; - Original version: https://github.com/tarao/with-eval-after-load-feature-el/
;;
;;; Code:
;; Code:

(eval-when-compile (require 'cl))
(eval '(eval-when-compile (require 'cl)))

(defun with-eval-after-load-feature-preload-1 (feature)
  (let ((after-load-alist nil))
    (unless (or (and (stringp feature)
                     (load feature :no-message :no-error))
                (and (symbolp feature)
                     (require feature nil :no-error)))
      (message "Cannot find %s" feature)
      'fail)))

(defun with-eval-after-load-feature-preload (feature-list)
  (loop for f in feature-list
        for fail = (with-eval-after-load-feature-preload-1 f)
        when fail
        collect fail))

(defun with-eval-after-load-feature-transform (feature-list body)
  (if (null feature-list)
      body
    (let ((feature (car feature-list)) (rest (cdr feature-list)))
    `((with-eval-after-load ',feature
       ,@(with-eval-after-load-feature-transform rest body))))))

(defmacro with-eval-after-load-feature (feature &rest body)
  (declare (indent 1) (debug t))
  (let* ((feature (if (and (listp feature) (eq (car-safe feature) 'quote))
                      (cdr feature) feature))
         (fs (if (listp feature) feature (list feature)))
         (form (or (and (eval '(eval-when (compile)
                                 (with-eval-after-load-feature-preload fs)))
                        'with-no-warnings)
                   'progn)))
    `(,form ,@(with-eval-after-load-feature-transform fs body))))

;;; init-16-wealf.el ends here
