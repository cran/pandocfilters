context("Div")

test_that("Div", {

	blocks <- list(Plain(list(Str("Hello R!"))))
	block <- Div(blocks)

    ## Test Div
    x <- pandocfilters:::test(list(block))
    y <- paste("<div>", "Hello R!", "</div>", sep = "\n")
    expect_that(x, equals(y))

} )
