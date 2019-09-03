(when (featurep 'yaml-mode)

  (define-derived-mode cfn-mode yaml-mode
    "Cloudformation"
    "Cloudformation template mode.")
  
  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-mode))

  (when (featurep 'flycheck)
    (flycheck-define-checker cfn-lint
      "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

      :command ("cfn-lint" "-f" "parseable" source)
      :error-patterns ((warning line-start (file-name) ":" line ":" column
                                ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                (id "W" (one-or-more digit)) ":" (message) line-end)
                       (error line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "E" (one-or-more digit)) ":" (message) line-end))
      :modes (cfn-mode))

    (add-to-list 'flycheck-checkers 'cfn-lint)
    (add-hook 'cfn-mode-hook 'flycheck-mode)))
