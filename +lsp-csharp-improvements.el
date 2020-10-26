;;; +lsp-csharp-improvements.el -*- lexical-binding: t; -*-

(after! lsp-csharp
  (lsp-interface (omnisharp:TestMessageEvent (:MessageLevel :Message)))
  (lsp-interface (omnisharp:DotNetTestResult (:MethodName :Outcome :ErrorMessage :ErrorStackTrace :StandardOutput :StandardError)))

  (let ((test-buffer-name "*lsp-csharp unit test run*"))
    (defun lsp-csharp--reset-test-buffer (present-buffer)
      "Creates new or reuses existing unit test result output buffer."
      (let ((existing-buffer (get-buffer test-buffer-name))
            (project-root-dir (lsp--suggest-project-root)))
        (if existing-buffer
            (progn
              (with-current-buffer existing-buffer
                (setq buffer-read-only nil)
                (erase-buffer)
                (setq buffer-read-only t)
                (setq default-directory project-root-dir))
              existing-buffer)
          (let ((buffer (get-buffer-create test-buffer-name)))
            (with-current-buffer buffer
              (setq default-directory project-root-dir)
              (compilation-mode)
              buffer))))

      (if present-buffer
          (display-buffer test-buffer-name)))

    (defun lsp-csharp--test-message (message)
      (let ((existing-buffer (get-buffer test-buffer-name)))
        (if existing-buffer
            (with-current-buffer existing-buffer
              (setq buffer-read-only nil)
              (goto-char (point-max))
              (insert message)
              (insert "\n")
              (setq buffer-read-only t))))))

  (defun lsp-csharp-run-test-at-point ()
    "Starts test run at current position (if any)."
    (interactive)
    (lsp-csharp--reset-test-buffer t)
    (lsp-send-execute-command "omnisharp/runTestMethod"
                              (vector (ht ("textDocumentUri" (lsp--buffer-uri))
                                          ("position" (lsp--cur-position))))))

  (defun lsp-csharp-run-all-tests-in-buffer ()
    "Starts test run for all test methods in current buffer."
    (interactive)
    (lsp-csharp--reset-test-buffer t)
    (lsp-send-execute-command "omnisharp/runTestMethod"
                              (vector (ht ("textDocumentUri" (lsp--buffer-uri))))))

  (lsp-defun lsp-csharp--handle-os-error (_workspace (&omnisharp:ErrorMessage :file-name :text))
    "Handle the 'o#/error' (interop) notification by displaying a message with lsp-warn."
    (lsp-warn "%s: %s" file-name text))

  (lsp-defun lsp-csharp--handle-os-testmessage (_workspace (&omnisharp:TestMessageEvent :message-level :message))
    (lsp-csharp--test-message message))

  (lsp-defun lsp-csharp--handle-os-testcompleted (_workspace (&omnisharp:DotNetTestResult
                                                              :method-name
                                                              :outcome
                                                              :error-message
                                                              :error-stack-trace
                                                              :standard-output
                                                              :standard-error))
    (let ((passed (string-equal "passed" outcome)))
      (lsp-csharp--test-message
       (format "[%s] %s "
               (propertize
                (upcase outcome)
                'font-lock-face (if passed
                                    '(:foreground "green" :weight bold)
                                  '(:foreground "red" :weight bold)))
               method-name))

      (unless passed
        (lsp-csharp--test-message error-message)

        (if error-stack-trace
            (lsp-csharp--test-message error-stack-trace))

        (unless (= (seq-length standard-output) 0)
            (lsp-csharp--test-message "STANDARD OUTPUT:")
            (seq-doseq (stdout-line standard-output)
              (lsp-csharp--test-message stdout-line)))

        (unless (= (seq-length standard-error) 0)
            (lsp-csharp--test-message "STANDARD ERROR:")
            (seq-doseq (stderr-line standard-error)
              (lsp-csharp--test-message stderr-line))))))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     #'lsp-csharp--language-server-command
                                     (lambda ()
                                       (when-let ((binary (lsp-csharp--language-server-path)))
                                         (f-exists? binary))))

                    :major-modes '(csharp-mode)
                    :server-id 'csharp
                    :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                    :notification-handlers (ht ("o#/projectadded" 'ignore)
                                               ("o#/projectchanged" 'ignore)
                                               ("o#/projectremoved" 'ignore)
                                               ("o#/packagerestorestarted" 'ignore)
                                               ("o#/msbuildprojectdiagnostics" 'ignore)
                                               ("o#/packagerestorefinished" 'ignore)
                                               ("o#/unresolveddependencies" 'ignore)
                                               ("o#/error" 'lsp-csharp--handle-os-error)
                                               ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                               ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                               ("o#/projectconfiguration" 'ignore)
                                               ("o#/projectdiagnosticstatus" 'ignore))
                    :download-server-fn
                    (lambda (_client callback error-callback _update?)
                      (condition-case err
                          (progn
                            (lsp-csharp--install-server nil nil)
                            (funcall callback))
                        (error (funcall error-callback (error-message-string err))))))))

(after! lsp-mode
  ;;
  ;; razor server
  ;;

  (lsp-interface (omnisharp-rzls:BufferSpan (:start :end :length :isEmpty)))
  (lsp-interface (omnisharp-rzls:BufferChange (:newText :span)))
  (lsp-interface (omnisharp-rzls:BufferChanges (:changes :hostDocumentFilePath :hostDocumentVersion)))

  (lsp-interface (omnisharp-rzls:RazorCodeActionParams (:range :textDocument :context)))
  (lsp-interface (omnisharp-rzls:RazorCodeAction (:title :edit :data)))

  ;; razor server commands:
  ;;  - "razor/mapToDocumentRanges"
  ;;    - request: { kind, projectedRanges, razorDocumentUri }
  ;;      - kind: CSharp: 1, Html: 2, Razor: 3
  ;;      - projectedRanges: [ { start: { line, character }, end: {xxx} } ]
  ;;    - response: { ranges: [ { start: { line, character }, end: {xxx} ] }

  (lsp-defun lsp-csharp--rzls-update-csharp-buffer (_workspace
                                                    (&omnisharp-rzls:BufferChanges
                                                     :changes
                                                     :host-document-file-path
                                                     :host-document-version))
    (message "update csharp buffer; host file path %s" host-document-file-path)
    (when (= 1 (length changes))
      (let* ((change (seq-first changes))
             ;(span (lsp-get change "span"))
             ;(span-is-empty (lsp-get span "isEmpty"))
             (new-text (ht-get change "newText"))
             (csharp-ws (lsp-find-workspace 'csharp)))
        (when csharp-ws
          (with-lsp-workspace csharp-ws
            (lsp-notify "textDocument/didChange"
                        (list :textDocument (ht ("uri" host-document-file-path)
                                                ("version" 1))
                              :contentChanges (vector (ht ("text" new-text)))))))))
    nil)

  (lsp-defun lsp-csharp--rzls-provide-code-actions (_workspace
                                                    (&omnisharp-rzls:RazorCodeActionParams
                                                     :range
                                                     :text-document
                                                     :context))
    (message "provide code actions; range: %s; text-document: %s; context: %s"
             (json-encode range)
             (json-encode text-document)
             (json-encode context))
    nil)

  (lsp-defun lsp-csharp--rzls-run-code-action (_workspace _params)
    nil)

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     (lambda ()
                       (list (concat (getenv "HOME") "/src/csharp/rzls/6.0.0-alpha.1.20479.2/rzls")
                             "-lsp"
                             "--trace")))

                    :major-modes '(web-mode)
                    :server-id 'csharp-rzls
                    :request-handlers (ht ("razor/provideCodeActions" 'lsp-csharp--rzls-provide-code-actions)
                                          ("razor/runCodeAction" 'lsp-csharp--rzls-run-code-action)
                                          ("razor/updateCSharpBuffer" 'lsp-csharp--rzls-update-csharp-buffer)
                                          ("razor/updateHtmlBuffer" 'ignore)))))
