library(roxygen2)
roxygen2::roxygenize()

library(pkgdown)
pkgdown::build_site_github_pages()

library(styler)
styler::style_pkg()

library(lintr)
lintr::lint_dir()
