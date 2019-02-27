;;; rfinz-server.el --- Start server  -*- Mode: Emacs-Lisp -*-
;;; Commentary:

;;; Code:
(require 'server)
(unless (server-running-p)
(server-start))


(provide 'rfinz-server)
;;; rfinz-server.el ends here
