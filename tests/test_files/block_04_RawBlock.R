context("BlockQuote")

test_that("BlockQuote", {

	block <- BlockQuote(list(Plain(list(Str("Hello R!")))))

	y <- paste("<blockquote>", "Hello R!", "</blockquote>", sep = "\n")

    ## Test Str with BlockQuote
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
