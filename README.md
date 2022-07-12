`capf-bibtex` provides a backend using Emacs's native `completion-at-point`
function for the completion of bibtex keys in modes used for prose writing. This
backend activates for citation styles used by `pandoc-mode` (@), `latex-mode`
(\cite{}), and `org-mode` (cite:@). This package is largely a port of
`company-bibtex` to Emacs's native completion framework. It works particularly
well with [corfu](https://github.com/minad/corfu), which enhances completion at
point with a small completion popup.

# Initialization 

Load the package and add `capf-bibtex` to `capf-backends` (there is a minor mode
-- `capf-bibtex-mode` -- for easy activation and deactivation of the capf) using
use-package and straight:

``` elisp
(use-package capf-bibtex
  :straight (:type git :host github :repo "mclear-tools/capf-bibtex")
  :hook ((org-mode markdown-mode tex-mode latex-mode reftex-mode) . capf-bibtex-mode)
  :custom
  (capf-bibtex-bibliography
   '("path/to/bib/file.bib")))
```

# Customization

`capf-bibtex` reads from a bibliography file or files specified in
`capf-bibtex-bibliography`:

```elisp
(setq capf-bibtex-bibliography
	'("/home/cooluser/thesis/thesis1.bib"
	  "/home/cooluser/thesis/thesi2.bib"))
```

# Alternative Packages 

Since this is Emacs, there are other ways to satisfy the user's need for
citation completion at point. Here are two notable ones:

- https://github.com/gbgar/company-bibtex
- https://github.com/emacs-citar/citar


