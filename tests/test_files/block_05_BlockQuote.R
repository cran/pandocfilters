context("BlockQuote")

test_that("BlockQuote", {

    y <- paste("<blockquote>", "Hello R!", "</blockquote>", sep = "\n")

    block <- BlockQuote(list(Plain(list(Str("Hello R!")))))

    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
