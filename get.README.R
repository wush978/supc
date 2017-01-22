library(bibtex)
library(magrittr)
library(git2r)

get.matches <- function(x, pattern) {
  regmatches(x, regexec(pattern, x))
}

header <- "
[![Travis-ci Status](https://travis-ci.org/wush978/supc.svg?branch=master)](https://travis-ci.org/wush978/supc)
[![Appveyor status](https://ci.appveyor.com/api/projects/status/ov2xlvx7edswtki7/branch/master?svg=true)](https://ci.appveyor.com/project/wush978/supc)
"

src <- readLines("vignettes/supc.md")
bib <- read.bib("vignettes/supc.bib")

# bib

.cited.bib <- c()
for(name in names(bib)) {
  if (sum(get.matches(src, name) %>% sapply(length) > 0) == 0) next
  src <- gsub(sprintf("@%s", name), replacement = cite(name, bib), src, fixed = TRUE)
  .cited.bib <- append(.cited.bib, name)
}
  
stopifnot(get.matches(src, "@([a-zA-Z0-9]+)") %>% sapply(length) == 0)
bib <- bib[.cited.bib]

# graphics

targets <- get.matches(src, "!\\[\\]\\(([^\\)]+)\\)") %>% Filter(f = function(x) length(x) > 0)
img.src <- sapply(targets, "[[", 2)
gh <- "gh-pages"
if (file.exists(gh)) unlink(gh, recursive = TRUE)
dir.create(gh)
repo <- git2r::init(path = gh)
img.mapping <- new.env()
lapply(img.src, function(path) {
  file.copy(file.path("vignettes", path), dst <- file.path(gh, basename(path)))
  img.mapping[[path]] <- dst
})
lapply(as.list(img.mapping), git2r::add, repo = repo)
commit <- git2r::commit(repo, message = "init figures")
git2r::remote_add(repo, "origin", "git@github.com:wush978/supc.git")
git2r::branch_rename(git2r::branches(repo)[["master"]], gh)
git2r::push(repo, "origin", "refs/heads/gh-pages", force = TRUE)
lapply(targets, function(target) {
  pattern <- target[1]
  replacement <- sprintf("![](http://wush978.github.io/supc/%s)", basename(target[2]))
  src <<- gsub(pattern, replacement, src, fixed = TRUE)  
}) %>% invisible()

write(c(header, src, "", format(bib, "html")), file = "README.md")
