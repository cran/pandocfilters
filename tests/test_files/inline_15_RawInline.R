context("RawInline")

test_that("RawInline", {

    if ( get_pandoc_version() < 2.8 ) {
        y <- c("some RawInline", "==============")
    } else {
        y <- c("`some RawInline`{=latex}", "========================")
    }
    inline <- RawInline("latex", "some RawInline")

    ## Test Space with Header
    x <- pandocfilters:::test(list(Header(list(inline))), "markdown")
    expect_that(x, equals(paste(y, collapse = "\n")))

    x <- pandocfilters:::test(list(Header(inline)), "markdown")
    expect_that(x, equals(paste(y, collapse = "\n")))

    ## Test Space with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))), "markdown")
    expect_that(x, equals(y[1]))

    x <- pandocfilters:::test(list(Plain(inline)), "markdown")
    expect_that(x, equals(y[1]))

} )
