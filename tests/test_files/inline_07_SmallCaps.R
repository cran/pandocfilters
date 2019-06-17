context("SmallCaps")

test_that("SmallCaps", {

    ## Test SmallCaps with Header
    if ( get_pandoc_version() < 2 ) {
        y <- "<h1><span style=\"font-variant: small-caps;\">Hello R!</span></h1>"
    } else {
        y <- "<h1><span class=\"smallcaps\">Hello R!</span></h1>"
    }

    x <- pandocfilters:::test(list(Header(list(SmallCaps(list(Str("Hello R!")))))))
    expect_that(x, equals(y))
    x <- pandocfilters:::test(list(Header(list(SmallCaps(Str("Hello R!"))))))
    expect_that(x, equals(y))
    x <- pandocfilters:::test(list(Header(SmallCaps(Str("Hello R!")))))
    expect_that(x, equals(y))

    ## Test SmallCaps with Plain
    if ( get_pandoc_version() < 2 ) {
        y <- "<span style=\"font-variant: small-caps;\">Hello R!</span>"
    } else {
        y <- "<span class=\"smallcaps\">Hello R!</span>"
    }

    x <- pandocfilters:::test(list(Plain(list(SmallCaps(list(Str("Hello R!")))))))
    expect_that(x, equals(y))
    x <- pandocfilters:::test(list(Plain(list(SmallCaps(Str("Hello R!"))))))
    expect_that(x, equals(y))
    x <- pandocfilters:::test(list(Plain(SmallCaps(Str("Hello R!")))))
    expect_that(x, equals(y))
    
} )