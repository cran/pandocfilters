
is.windows <- function() {
    isTRUE(.Platform$OS.type == "windows")
}

## a constructor for named lists
nlist <- function(...) {
    x <- list(...)
    names(x) <- character(0)
    x
}

toUTF8 <- function(x, from = localeToCharset()) {
    if (!is.windows()) return(x)
    from <- head(from, 1)
    iconv(x, from = from, to = "UTF-8")
}

## ----------------------------------------------------------------------------- 
##
##   Not Exported Function for Testing
##
## -----------------------------------------------------------------------------
sys_call <- function(cmd, args) {
    tryCatch(system2(cmd, args, stdout=TRUE, stderr=TRUE), error=function(e) NULL)
}

is.pandoc_installed <- function() {
    !is.null(sys_call(get_pandoc_path(), "--version"))
}

build_args <- function(args) {
    paste(sprintf("--%s=%s", names(args), args), collapse = " ")
}

pandoc_call <- function(input, control) {
    args <- sprintf("%s %s", build_args(control), input)
    system2(get_pandoc_path(), args, stdout = TRUE, stderr = TRUE)
}

#' Utility functions for testing filters
#' 
#' @param file file name
#' @param from markdown, html, latex or native
#' @param json a \code{JSON} representation of the \code{AST} to be passed 
#'             to \code{pandoc}
#' @param to markdown, html, latex or native
#' @param exchange a character string 
#' 
#' @rdname utility_functions
#' 
#' @export
pandoc_to_json <- function(file, from = "markdown") {
    stopifnot(file.exists(file))
    pandoc_call(file, c(from = from, to = "json"))
}

#' @rdname utility_functions
#' @export
pandoc_from_json <- function(json, to = "markdown", exchange = c("file", "arg")) {
    if ( match.arg(exchange) == "file" ) {
        input <- tempfile(fileext = ".txt")
        on.exit(unlink(input))
        writeLines(json, toUTF8(input), useBytes = TRUE)
    } else {
        input <- shQuote(toUTF8(as.character(json)))
    }
    pandoc_call(input, c(from = "json", to = to))
}

#  -----------------------------------------------------------
#  write.pandoc
#  ============
#' @title Write the JSON-formatted AST to a connection
#' @description Write the JSON-formatted AST to a connection.
#' @param json a JSON representation of the AST to be written out
#' @param file a connection object or a character string to which the JSON-formatted AST is written
#' @param format a character string giving the format (e.g. \code{"latex"}, \code{"html"})
#' @param exchange a character string 
#' @details If you want to apply a filter to the document before it get's written out, or your
#'          pandoc installation is not registered in the \code{PATH} it can be favorable to provide your
#'          own writer function to the document class.
#' @export
#  -----------------------------------------------------------
write.pandoc <- function(json, file, format, exchange = c("arg", "file")) {
    x <- pandoc_from_json(json, to = format, exchange = exchange)
    writeLines(x, con = file)
}

to_pandoc_json <- function(x, meta = nlist()) {
    if( get_pandoc_version() < 1.18 ) {
        z <- list(list(unMeta = meta), x)
    } else {
        z <- list(blocks = x, `pandoc-api-version` = c(1, 17, 0), meta = meta)
    }
    jsonlite::toJSON(z, auto_unbox = TRUE)
}

test <- function(x, to = "html") {
    paste(pandoc_from_json(to_pandoc_json(x), to = to), collapse = "\n")
}

detect_pandoc_version <- function() {
    x <- sys_call(get_pandoc_path(), "--version")
    x <- gsub(".exe", "", x, fixed = TRUE)
    b <- grepl("pandoc\\s+\\d\\.\\d+", x)
    if ( !any(b) ) {
        packageStartupMessage("\nInfo message:\n\tCouldn't find 'pandoc'!")
        return(NA_character_)
    }
    version <- gsub("[[:alpha:] ]", "", x[b][1])
    ## returns the detailed version number e.g. "1.16.0.2"
    version
}

detect_pandoc_types_version <- function() {
    x <- sys_call(get_pandoc_path(), "--version")
    x <- unlist(regmatches(x, gregexpr("pandoc-types\\s+\\d+\\.\\d+\\.\\d+", x)))
    gsub("[[:alpha:] -]", "", x)
}

get_pandoc <- function() {
    getNamespace("pandocfilters")$pandoc
}

#  -----------------------------------------------------------
#  get_pandoc_version
#  ==================
#' @title Get Pandoc Version
#' @description Get the version of pandoc.
#' @param type a character giving the type of the return value.
#' @examples
#' get_pandoc_version()
#' @export
#  -----------------------------------------------------------
get_pandoc_version <- function(type = c("numeric", "character")) {
    type <- match.arg(type)
    version <- get_pandoc()$version
    if ( is.na(version) ) {
        if ( type == "numeric" ) 0 else version
    } else {
        if ( type == "numeric" ) version_to_numeric(version) else version
    }
}

#  -----------------------------------------------------------
#  get_pandoc_types_version
#  ========================
#' @title Get Pandoc-Types Version
#' @description Get the version of pandoc-types.
#' @param type a character giving the type of the return value.
#' @examples
#' get_pandoc_types_version()
#' @export
#  -----------------------------------------------------------
get_pandoc_types_version <- function(type = c("numeric", "character")) {
    type <- match.arg(type)
    version <- get_pandoc()$types_version
    if ( is.na(version) ) {
      if ( type == "numeric" ) 0 else version
    } else {
      if ( type == "numeric" ) version_to_numeric(version) else version
    }
}

version_to_numeric <- function(x) {
    as.numeric(regmatches(x, regexpr("\\d+\\.\\d+", x)))
}

#  -----------------------------------------------------------
#  set_pandoc_version
#  ==================
#  @title Set Pandoc Version
#  @description Set the version version pandoc.
#  @param x a numeric giving the pandoc version (e.g. 1.14 or 1.15 or 1.16 or 1.17)
#  -----------------------------------------------------------
set_pandoc_version <- function(x) {    
    assign("version", x, envir = get_pandoc())
}

set_pandoc_types_version <- function(x) {    
  assign("types_version", x, envir = get_pandoc())
}

#  -----------------------------------------------------------
#  get_pandoc_path
#  ===============
#' @title Get Pandoc Path
#' @description Get the path of pandoc.
#' @export
#  -----------------------------------------------------------
get_pandoc_path <- function() {
    get_pandoc()$path
}

#  -----------------------------------------------------------
#  set_pandoc_path
#  ===============
#' @title Set Pandoc Path
#' @description Set the path to pandoc.
#' @param path a character giving the location of pandoc 
#'        (default is \code{"pandoc"} which uses the pandoc set in the system path).
#' @export
#  -----------------------------------------------------------
set_pandoc_path <- function(path = "pandoc") {
    stopifnot(is.character(path))
    stopifnot(isTRUE(path == "pandoc") | file.exists(path))
    assign("path", path, envir = get_pandoc())
    set_pandoc_version(detect_pandoc_version())
    set_pandoc_types_version(detect_pandoc_types_version())
}
