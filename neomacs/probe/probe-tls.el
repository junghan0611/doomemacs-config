;;; neomacs/probe/probe-tls.el --- TLS and package-archive probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Settles upstream #121, which is the single assumption the whole minimal
;; set rests on: "gnutls does not work, therefore no ELPA, therefore
;; builtins only".
;;
;; The issue is still OPEN, but the code underneath was replaced in June
;; 2026 — Neomacs now carries a rustls-backed TLS facade
;; (neovm-core/src/emacs_core/tls.rs, ~700 lines) plus upstream tests that
;; fetch a package archive and install via quelpa.  So the issue state no
;; longer describes the runtime, and only a live run can say which is true.
;;
;; If this probe passes, issue #8's scope must widen: external packages
;; become reachable and "builtins only" stops being a constraint.
;; If it fails, the output is a ready-made reproduction for #121.
;;
;; NOTE: this probe makes outbound HTTPS requests to the GNU ELPA archive.
;; It is the only probe that touches the network, and it never installs
;; anything — it fetches the archive index and stops.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'probe-lib)

(my/probe-banner "TLS capability (upstream #121)")

(my/probe-check "gnutls-available-p"
  (if (fboundp 'gnutls-available-p)
      (let ((caps (gnutls-available-p)))
        (if caps (format "%s" caps) "returned nil -- TLS UNAVAILABLE"))
    (cons 'skip "gnutls-available-p unbound")))

(my/probe-check "gnutls symbol surface"
  (let ((present (seq-filter #'fboundp
                             '(gnutls-boot gnutls-boot-parameters
                               gnutls-peer-status gnutls-errorp
                               gnutls-error-string gnutls-negotiate))))
    (format "%d/6: %s" (length present)
            (mapconcat #'symbol-name present " "))))

(my/probe-check "open-network-stream plain TCP"
  ;; Separates "no network" from "no TLS": if this fails too, the finding
  ;; is the process/network layer, not the TLS stack.
  (let ((proc (ignore-errors
                (open-network-stream "probe-plain" nil "elpa.gnu.org" 80
                                     :type 'plain))))
    (if (processp proc)
        (prog1 (format "connected, status=%s" (process-status proc))
          (delete-process proc))
      "COULD NOT CONNECT on port 80")))

(my/probe-check "open-network-stream TLS handshake"
  (let ((proc (open-network-stream "probe-tls" nil "elpa.gnu.org" 443
                                   :type 'tls)))
    (if (processp proc)
        (prog1 (format "handshake ok, status=%s" (process-status proc))
          (delete-process proc))
      "NO PROCESS returned")))

(my/probe-check "TLS peer certificate"
  (let ((proc (open-network-stream "probe-cert" nil "elpa.gnu.org" 443
                                   :type 'tls)))
    (unwind-protect
        (if (fboundp 'gnutls-peer-status)
            (let ((status (gnutls-peer-status proc)))
              (if status
                  (format "peer status keys: %s"
                          (mapconcat (lambda (k) (format "%s" k))
                                     (map-keys status) " "))
                "peer status nil"))
          (cons 'skip "gnutls-peer-status unbound"))
      (when (processp proc) (delete-process proc)))))

(my/probe-banner "TLS + :nowait — the root cause of #121")

;; Measured 2026-07-19 on Neomacs 0.0.13 vs GNU Emacs 31.0.50, same profile:
;;
;;   :type 'tls              both -> tls=yes, HTTP/1.1 200 OK
;;   :type 'tls :nowait t    GNU  -> tls=yes, HTTP/1.1 200 OK
;;                           Neo  -> error "Process not found"
;;
;; url-http opens every connection with :nowait t.  When the asynchronous
;; TLS connect fails, the request goes out over the plain fallback socket,
;; the server answers "You're speaking plain HTTP to an SSL-enabled server
;; port" (400, 561 bytes), and package.el reports an empty archive.  That
;; is the whole of #121 as it stands today — the handshake itself was
;; already fixed by the June rustls facade.

(my/probe-check "TLS connect, blocking"
  (let ((proc (open-network-stream "probe-tls-block" nil "elpa.gnu.org" 443
                                   :type 'tls)))
    (unwind-protect
        (format "status=%s tls=%s" (process-status proc)
                (if (gnutls-peer-status proc) "yes" "NO"))
      (when (processp proc) (delete-process proc)))))

(my/probe-check "TLS connect, :nowait t"
  (let ((proc (open-network-stream "probe-tls-nowait" nil "elpa.gnu.org" 443
                                   :type 'tls :nowait t)))
    (let ((n 0))
      (while (and (eq (process-status proc) 'connect) (< n 100))
        (accept-process-output proc 0.1)
        (setq n (1+ n))))
    (unwind-protect
        ;; A connection that opens without negotiating TLS is the failure,
        ;; not a pass — the socket is live but speaking plaintext.
        (if (gnutls-peer-status proc)
            (format "status=%s tls=yes" (process-status proc))
          (error "status=%s but NO TLS negotiated -- #121 root cause reproduces"
                 (process-status proc)))
      (when (processp proc) (delete-process proc)))))

(my/probe-banner "url.el over HTTPS")

(my/probe-check "url-retrieve-synchronously https"
  (require 'url)
  (let ((buf (url-retrieve-synchronously
              "https://elpa.gnu.org/packages/archive-contents" t t 30)))
    (unless buf (error "nil buffer -- retrieval failed"))
    (unwind-protect
        (with-current-buffer buf
          ;; archive-contents is ~180KB. A few hundred bytes means the
          ;; server answered an error page over the plaintext fallback.
          (if (< (buffer-size) 10000)
              (error "only %d bytes: %s" (buffer-size)
                     (car (split-string (buffer-string) "\n")))
            (format "%d bytes retrieved" (buffer-size))))
      (kill-buffer buf))))

(my/probe-banner "package.el archive refresh (the #121 shape)")

(my/probe-check "package-refresh-contents against GNU ELPA"
  ;; The exact operation #121 reports as broken.  Both package-user-dir and
  ;; package-gnupghome-dir are redirected to a temp dir — the latter matters
  ;; because package.el creates a GPG keyring on first refresh, which would
  ;; otherwise leave an elpa/ tree inside this builtin-only profile.
  (require 'package)
  (let* ((tmp (make-temp-file "neomacs-probe-elpa" t))
         (package-user-dir tmp)
         (package-gnupghome-dir (expand-file-name "gnupg" tmp))
         (package-archives '(("gnu" . "https://elpa.gnu.org/packages/"))))
    (unwind-protect
        (progn
          (package-refresh-contents)
          (let ((n (length package-archive-contents)))
            (if (> n 0)
                (format "%d packages in archive-contents -- #121 RESOLVED" n)
              (error "archive-contents EMPTY -- #121 still reproduces"))))
      (delete-directory tmp t))))

(my/probe-summary)

;;; probe-tls.el ends here
