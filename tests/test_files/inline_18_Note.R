context("Note")

test_that("Note", {

    block <- Plain(list(Str("x")))
    inline <- Note(block)
    if ( get_pandoc_version() < 2 ) {
        y <- paste("<h1><a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a></h1>",
                   "<div class=\"footnotes\">", "<hr />", "<ol>",
                   "<li id=\"fn1\">x<a href=\"#fnref1\">↩</a></li>", "</ol>", "</div>",
                    sep = "\n")
    } else if ( get_pandoc_version() < 2.1 ) {
        y <- paste("<h1><a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a></h1>",
                   "<section class=\"footnotes\">", "<hr />", "<ol>",
                   "<li id=\"fn1\">x<a href=\"#fnref1\" class=\"footnoteBack\">↩</a></li>", "</ol>", "</section>",
                    sep = "\n")
    } else {
        y <- paste("<h1><a href=\"#fn1\" class=\"footnote-ref\" id=\"fnref1\"><sup>1</sup></a></h1>",
                   "<section class=\"footnotes\">", "<hr />", "<ol>",
                   "<li id=\"fn1\">x<a href=\"#fnref1\" class=\"footnote-back\">↩</a></li>", "</ol>", "</section>",
                    sep = "\n")
    }

    ## Test Str with Header
    expect_that(pandocfilters:::test(list(Header(Note(list(block))))), equals(y))
    expect_that(pandocfilters:::test(list(Header(Note(block)))), equals(y))

    if ( get_pandoc_version() < 2 ) {
        y <- paste("<a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a>", 
                   "<div class=\"footnotes\">", "<hr />", "<ol>", 
                   "<li id=\"fn1\">x<a href=\"#fnref1\">↩</a></li>", "</ol>", "</div>",
                   sep = "\n")
    } else if ( get_pandoc_version() < 2.1 ) {
        y <- paste("<a href=\"#fn1\" class=\"footnoteRef\" id=\"fnref1\"><sup>1</sup></a>", 
                   "<section class=\"footnotes\">", "<hr />", "<ol>", 
                   "<li id=\"fn1\">x<a href=\"#fnref1\" class=\"footnoteBack\">↩</a></li>", "</ol>", "</section>",
                   sep = "\n")
    } else {
        y <- paste("<a href=\"#fn1\" class=\"footnote-ref\" id=\"fnref1\"><sup>1</sup></a>", 
                   "<section class=\"footnotes\">", "<hr />", "<ol>", 
                   "<li id=\"fn1\">x<a href=\"#fnref1\" class=\"footnote-back\">↩</a></li>", "</ol>", "</section>",
                   sep = "\n")
    }

    ## Test Str with Plain
    expect_that(pandocfilters:::test(list(Plain(Note(list(block))))), equals(y))
    expect_that(pandocfilters:::test(list(Plain(Note(block)))), equals(y))

} )

