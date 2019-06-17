context("Span")

test_that("Span", {

    attr <- Attr("A", c("B"), list(c("C", "D")))
    inline <- Span(attr, list(Str("some inline string")))

    if ( get_pandoc_version() < 2 ) {
        y <- "<h1><span id=\"A\" class=\"B\" C=\"D\">some inline string</span></h1>"
    } else {
        y <- "<h1><span id=\"A\" class=\"B\" data-C=\"D\">some inline string</span></h1>"
    }

    ## Test Image with Header
    x <- pandocfilters:::test(list(Header(inline)))
    expect_that(x, equals(y))

    if ( get_pandoc_version() < 2 ) {
        y <- "<span id=\"A\" class=\"B\" C=\"D\">some inline string</span>"
    } else {
        y <- "<span id=\"A\" class=\"B\" data-C=\"D\">some inline string</span>"
    }

    ## Test Image with Plain
    x <- pandocfilters:::test(list(Plain(list(inline))))
    expect_that(x, equals(y))

} )