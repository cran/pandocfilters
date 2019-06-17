context("CodeBlock")

test_that("CodeBlock", {

    attr <- Attr("id", c("Programming Language"), list(c("key", "value")))
    code <- "x <- 3\nprint('Hello R!')"
    block <- CodeBlock(attr, code)

    if ( get_pandoc_version() < 2 ) {
        y <- paste("<pre id=\"id\" class=\"Programming Language\" key=\"value\"><code>x &lt;- 3",
                   "print(&#39;Hello R!&#39;)</code></pre>", sep = "\n")
    } else {
        y <- paste("<pre id=\"id\" class=\"Programming Language\" data-key=\"value\"><code>x &lt;- 3",
           "print(&#39;Hello R!&#39;)</code></pre>", sep = "\n")
    }

    ## Test Str with CodeBlock
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
