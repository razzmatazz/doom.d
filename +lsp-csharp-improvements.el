;;; +lsp-csharp-improvements.el -*- lexical-binding: t; -*-

(after! lsp-csharp
  (lsp-interface (omnisharp:ProjectInformationRequest (:FileName)))

  (lsp-interface (omnisharp:MsBuildProject (:IsUnitProject
                                            :IsExe
                                            :Platform
                                            :Configuration
                                            :IntermediateOutputPath
                                            :OutputPath
                                            :TargetFrameworks
                                            :SourceFiles
                                            :TargetFramework
                                            :TargetPath
                                            :AssemblyName
                                            :Path
                                            :ProjectGuid)))

  (lsp-interface (omnisharp:ProjectInformation (:ScriptProject :MsBuildProject)))
  (lsp-interface (omnisharp:MetadataRequest (:AssemblyName :TypeName :ProjectName :VersionNumber :Language)))
  (lsp-interface (omnisharp:MetadataResponse (:SourceName :Source)))

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

  (lsp-defun lsp-csharp-open-project-file ()
    (interactive)
    (-let* ((project-info-req (lsp-make-omnisharp-project-information-request :file-name (buffer-file-name)))
            (project-info (lsp-request "o#/project" project-info-req))
            ((&omnisharp:ProjectInformation :ms-build-project) project-info)
            ((&omnisharp:MsBuildProject :is-exe :path) ms-build-project))
      (find-file path)))

  (lsp-defun lsp-csharp--osmd-uri-handler (uri)
    (string-match "^osmd:/Project/\\(.+\\)/Assembly/\\(.+\\)/Symbol/\\(.+\\)\.cs$" uri)
    (-when-let* ((project-name (match-string 1 uri))
                 (assembly-name (match-string 2 uri))
                 (type-name (match-string 3 uri))
                 (metadata-req (lsp-make-omnisharp-metadata-request :project-name project-name
                                                                    :assembly-name assembly-name
                                                                    :type-name type-name))
                 (metadata (lsp-request "o#/metadata" metadata-req))
                 ((&omnisharp:MetadataResponse :source-name :source) metadata)
                 (filename (f-join ".cache"
                                   "lsp-csharp"
                                   "metadata"
                                   "projects" project-name
                                   "assemblies" assembly-name
                                   "types" (concat type-name ".cs")))
                 (file-location (expand-file-name filename (lsp--suggest-project-root)))
                 (metadata-file-location (concat file-location ".metadata-uri"))
                 (path (f-dirname file-location)))

        (unless (find-buffer-visiting file-location)
          (unless (file-directory-p path)
            (make-directory path t))

          (with-temp-file metadata-file-location
            (insert uri))

          (with-temp-file file-location
            (insert source)))

        file-location))

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

                    :uri-handlers (lsp-ht ("osmd" #'lsp-csharp--osmd-uri-handler))
                    :before-file-open-fn (lambda (_workspace)
                                           (let ((metadata-file-name (concat buffer-file-name ".metadata-uri")))
                                             (setq-local lsp-buffer-uri
                                                         (when (file-exists-p metadata-file-name)
                                                           (with-temp-buffer (insert-file-contents metadata-file-name)
                                                                             (buffer-string))))))
                    :download-server-fn
                    (lambda (_client callback error-callback _update?)
                      (condition-case err
                          (progn
                            (lsp-csharp--install-server nil nil)
                            (funcall callback))
                        (error (funcall error-callback (error-message-string err)))))))
  )

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

  )
