;;; +lsp-csharp-improvements.el -*- lexical-binding: t; -*-

(after! lsp-mode

  (lsp-interface (omnisharp-rzls:BufferSpan (:start :end :length :isEmpty)))
  (lsp-interface (omnisharp-rzls:BufferChange (:newText :span)))
  (lsp-interface (omnisharp-rzls:BufferChanges (:changes :hostDocumentFilePath :hostDocumentVersion)))

  (lsp-interface (omnisharp-rzls:RazorCodeActionParams (:range :textDocument :context)))
  (lsp-interface (omnisharp-rzls:RazorCodeAction (:title :edit :data)))

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
                       (list "/Users/bob/src/csharp/rzls/6.0.0-alpha.1.20479.2/rzls"
                             "-lsp"
                             "--trace")))

                    :major-modes '(web-mode)
                    :server-id 'csharp-rzls
                    :request-handlers (ht ("razor/provideCodeActions" 'lsp-csharp--rzls-provide-code-actions)
                                          ("razor/runCodeAction" 'lsp-csharp--rzls-run-code-action)
                                          ("razor/updateCSharpBuffer" 'lsp-csharp--rzls-update-csharp-buffer)
                                          ("razor/updateHtmlBuffer" 'ignore)))))
