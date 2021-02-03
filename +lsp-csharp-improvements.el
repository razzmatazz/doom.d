;;; +lsp-csharp-improvements.el -*- lexical-binding: t; -*-

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

(defun sm-csharp-stuff ()

  (with-lsp-workspace (lsp-find-workspace 'csharp)
    (lsp-request-async "o#/checkalivestatus"
                       lsp--empty-ht
                       (lambda (_)
                         (lsp--info "alive!"))))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/classA.cs")
    (lsp-request-async "o#/checkalivestatus" (list) (lambda (value) (lsp--info (concat "alive!: value=" (json-encode value))))))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/classA.cs")
    (lsp-request-async "o#/reanalyze" (list) (lambda (_) (lsp--info "alive!"))))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/classA.cs")
    (lsp-request-async "o#/something" (list) (lambda (_) (lsp--info "alive!"))))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/classA.cs")
    (lsp-request-async "o#/something-2" (list) (lambda (_) (lsp--info "alive!"))))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/classA.cs")
    (lsp-request-async "o#/something-non-existent" (list) (lambda (_) (lsp--info "alive!"))))

  (with-lsp-workspace (lsp-find-workspace 'csharp)
    (lsp-request "o#/checkreadystatus" (list)))

  (with-lsp-workspace (lsp-find-workspace 'csharp)
    (lsp-request "o#/checkreadystatusXXX" (list)))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/classA.cs")
    (lsp-request-async "o#/something-non-existent" (list) (lambda (_) (lsp--info "alive!"))))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/TestClass.cs")
    (lsp-request "o#/project" (lsp-make-omnisharp-project-information-request
                               :file-name "/Users/bob/src/omnisharp/test/TestClass.cs")))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/TestClass.cs")
    (lsp-request-async "o#/gotodefinition"
                       `(:WantMetadata t
                         :FileName "/Users/bob/src/omnisharp/test/TestClass.cs"
                         :Line 11
                         :Column 21
                         :Buffer nil)
                       (lambda (value)
                         (let* ((metadata-source (lsp-get value :MetadataSource)))
                           (lsp--info (json-encode metadata-source))

                           (lsp-request-async "o#/metadata"
                                              metadata-source
                                              (lambda (resp)
                                                (lsp--info (json-encode resp))))))))

  (with-lsp-workspace (lsp-find-workspace 'csharp)
    (lsp-request "o#/stopserver" (list)))


  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/TestClass.cs")
    (lsp-request "o#/checkalivestatus" (list)))

  (with-lsp-workspace (lsp-find-workspace 'csharp "/Users/bob/src/omnisharp/test/TestClass.cs")
    (lsp-feature? "codeAction/resolve")
    )

  )
