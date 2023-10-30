Please run 

```{r}
library(roxygen2)
roxygen2::roxygenize()

library(pkgdown)
pkgdown::build_site_github_pages()
```

or `RScript update_docs.R` before every commit.