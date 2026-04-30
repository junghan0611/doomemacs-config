;;; agent-server-socket.el --- Agent server socket bootstrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;;; Commentary:

;; Socket/startup-only part of agent-server.
;; Keep this separate so the main API file can be loaded without necessarily
;; creating a server socket.

;;; Code:

(defun agent-server-start-socket ()
  "Print READY logs and start the server socket when enabled."
  (message "[agent-server] ========================================")
  (message "[agent-server] READY — v%s" agent-server-version)
  (message "[agent-server] Server: %s" agent-server-name)
  (message "[agent-server] Org: %s" (org-version))
  (message "[agent-server] Denote dir: %s" denote-directory)
  (message "[agent-server] API: agent-server-status, agent-org-read-file,")
  (message "[agent-server]      agent-org-get-headings, agent-org-get-properties,")
  (message "[agent-server]      agent-denote-search, agent-citar-lookup,")
  (message "[agent-server]      agent-denote-rename-by-front-matter,")
  (message "[agent-server]      agent-denote-rename-bulk,")
  (message "[agent-server]      agent-denote-set-front-matter,")
  (message "[agent-server]      agent-org-dblock-update,")
  (message "[agent-server]      agent-org-agenda-day, agent-org-agenda-week,")
  (message "[agent-server]      agent-org-agenda-tags, agent-org-agenda-todos,")
  (message "[agent-server]      agent-denote-keywords, agent-denote-add-history,")
  (message "[agent-server]      agent-denote-add-heading, agent-denote-add-link")
  (message "[agent-server] REPL: emacs_eval for runtime extension")
  (message "[agent-server] ========================================")
  (when (and agent-server-enable-socket
             (not noninteractive))
    (server-start)
    (message "[agent-server] Socket created")))

(provide 'agent-server-socket)
;;; agent-server-socket.el ends here
