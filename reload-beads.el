;;; reload-beads.el --- Reload beads.el modules for development

;;; Commentary:
;; Evaluate this file to reload all beads modules during development:
;; M-x load-file RET reload-beads.el RET

;;; Code:

(let ((lisp-dir (expand-file-name "lisp"
                                   (file-name-directory
                                    (or load-file-name buffer-file-name)))))

  ;; Unload all beads modules
  (message "Unloading beads modules...")
  (dolist (module '(beads-delete beads-list beads-show beads-create
                    beads-update beads-close beads-main beads-dep
                    beads-stats beads-misc beads-graph beads))
    (when (featurep module)
      (unload-feature module t)))

  ;; Add to load path
  (add-to-list 'load-path lisp-dir)

  ;; Reload main module (which loads autoloads)
  (message "Reloading beads.el...")
  (load-file (expand-file-name "beads.el" lisp-dir))

  ;; Load commonly used modules
  (message "Loading beads modules...")
  (require 'beads-delete)
  (require 'beads-list)
  (require 'beads-main)

  (message "Beads modules reloaded successfully!"))

(provide 'reload-beads)
;;; reload-beads.el ends here
