## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE, fig.width=8, fig.height=4)
opts_knit$set(out.format = "latex")
theme_options <- knit_theme$get("edit-emacs")
theme_options$background = "#f4f3ef"; 
knit_theme$set(theme_options)

## ----echo=FALSE, results='hide'--------------------------------
options(width=65, prompt = "R> ", continue = "+  ", useFancyQuotes = FALSE)

## ----pandocfilters---------------------------------------------
library("pandocfilters")

## ----pandoc_version--------------------------------------------
system2("pandoc", "--version", stdout = TRUE, stderr = TRUE)

## ----rprofile_unix, eval=FALSE---------------------------------
#  Sys.setenv(PANDOC_HOME="/home/florian/bin/pandoc/pandoc_221/bin/pandoc")

## ----rprofile_win, eval=FALSE----------------------------------
#  Sys.setenv(PANDOC_HOME="C:/Users/Florian/AppData/Local/Pandoc/pandoc.exe")

## ----eval=FALSE------------------------------------------------
#  Emph(list(Str("some text")))

## ----eval=FALSE------------------------------------------------
#  Emph(Str("some text"))

## ----eval=FALSE------------------------------------------------
#  Emph("some text")

## ----eval=FALSE------------------------------------------------
#  Emph(list(Str("some text")))
#  Emph(Str("some text"))
#  Emph("some text")

## ----echo=FALSE, results='hide'--------------------------------
class(pandoc_to_json) <- c("pfun", class(pandoc_to_json))
class(pandoc_from_json) <- c("pfun", class(pandoc_from_json))
class(filter) <- c("pfun", class(filter))
print.pfun <- function(x) writeLines(head(deparse(args(x)), 1))

## ----functions-------------------------------------------------
pandoc_to_json
pandoc_from_json
filter

## ----read_ex1, cache = FALSE-----------------------------------
ex1_file <- system.file(package = "pandocfilters", 
                        "examples", "lower_case.md")
readLines(ex1_file)

## ----ex1_setup_text_connections--------------------------------
icon <- textConnection(pandoc_to_json(ex1_file))
ocon <- textConnection("modified_ast", open = "w")

## ----lower_function--------------------------------------------
lower <- function(key, value, ...) {
    if (key == "Str") Str(tolower(value)) else NULL
}

## ----ex1_filter------------------------------------------------
filter(lower, input = icon, output = ocon)

## ----ex1_from_json---------------------------------------------
pandoc_from_json(modified_ast, to ="markdown")

## ----ex1_close_connnections------------------------------------
close(icon)
close(ocon)

