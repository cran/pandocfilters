.onLoad <- function( libname, pkgname ) {
    pandoc_home <- Sys.getenv("PANDOC_HOME")
    if ( nchar(pandoc_home) ) {
        if ( file.exists((unix_path <- file.path(pandoc_home, "pandoc"))) ) {
            assign("path", unix_path, envir = get_pandoc())
        } else if ( file.exists((windows_path <- file.path(pandoc_home, "pandoc.exe"))) ) {
            assign("path", windows_path, envir = get_pandoc())
        } else {
            msg <- sprintf("'PANDOC_HOME' is set but neither '%s' nor '%s' exists", 
                           unix_path, windows_path)
            warning(msg)
        }
    } 
    set_pandoc_version(detect_pandoc_version())
    set_pandoc_types_version(detect_pandoc_types_version())
}
