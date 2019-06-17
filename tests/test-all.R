
library(pandocfilters)

find_tests <- function(pkg) {
    test_files_path <- file.path(getwd(), "test_files")
    if ( dir.exists(test_files_path) ) test_files_path else stop("couldn't find tests")
}

## Remove testthat since it add too much dependencies!
## I use anyhow only expact_that equals
context <- function(msg) logger <<- c(logger, list(structure(msg, class = "context")))
header <- function(msg) logger <<- c(logger, list(structure(msg, class = "header")))

equals <- function(x) x
expect_that <- function(x, y) {
    z <- all.equal(x, y)
    if ( !isTRUE(z) ) {
        msg <- sprintf("%s in `%s`", z, deparse(sys.call()))
        logger <<- c(logger, list(structure(msg, class = "complaint")))
    }
}

is.context <- function(x) inherits(x, "context")
is.header <- function(x) inherits(x, "header")
is.complaint <- function(x) inherits(x, "complaint")

build_report <- function(logger) {
    report <- character()
    count_errors <- 0L
    for (i in seq_along(logger)) {
        msg <- ""
        loge <- logger[[i]]
        next_loge <- if ( i < length(logger) ) logger[[i + 1]] else loge

        if ( is.header(loge) ) {
            report <- c(report, as.character(loge))
        } else if ( is.context(loge) ) {
            if ( is.context(next_loge) ) {
                ## report <- c(report, sprintf("  %s: OK!", as.character(loge)))
            } else {
                count_errors <- count_errors + 1L
                report <- c(report, sprintf("  %s: ERROR!", as.character(loge)))
            }
        } else {
            report <- c(report, sprintf("    %s", as.character(loge)))
        }
    }
    if ( count_errors == 0L ) {
        report <- c(report, "  No Errors occured!")
    }
    report
}

## for interactive tests I assume that tests is the working directory
test_pkg <- function(pkg) {
    test_files_path <- find_tests(pkg)
    test_files <- dir(test_files_path, full.names = TRUE)
    for (i in seq_along(test_files)) {
        source(test_files[i], local = TRUE)
    }
}

test_that <- function(description, expression) {
    local(expression)
}

print(getwd())
print(system.file(package = "pandocfilters"))

## to test multiple versions the path to this versions can be
## set in the system variable "PANDOC_PATHS"
## e.g. one can put 
## Sys.setenv(PANDOC_PATHS = 
##   paste(sprintf("/home/florian/bin/pandoc/pandoc_%i/bin/pandoc", 
##                 c(116, 203, 211)), collapse=",")) 
## into the .Rprofile file.
if ( pandocfilters:::is.pandoc_installed() ) {
    TEST_ALL_VERSIONS <- TRUE ## just to speed up things when developing
    if ( nchar(Sys.getenv("PANDOC_PATHS")) & TEST_ALL_VERSIONS ) {
        pandoc_paths <- unlist(strsplit(Sys.getenv("PANDOC_PATHS"), ",", fixed = TRUE))
        logger <- list()
        reports <- list()
        for (i in seq_along(pandoc_paths)) {
            set_pandoc_path(pandoc_paths[i])
            header(sprintf("\nTest pandoc version: %s\n", as.character(get_pandoc_version())))
            test_pkg("pandocfilters")
            reports[[i]] <- build_report(logger)
            logger <- list()
        }
        writeLines(do.call(c, reports))
    } else {
        logger <- list()
        header(sprintf("\nTest pandoc version: %s\n", as.character(get_pandoc_version())))
        test_pkg("pandocfilters")
        writeLines(build_report(logger))
    }
} else {
    writeLines("\nPandoc is not registered in the path so the tests are skipped!\n")
}
