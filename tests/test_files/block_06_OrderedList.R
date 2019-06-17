context("OrderedList")

test_that("OrderedList", {

	y <- paste("<ol>", "<li>A</li>", "<li>B</li>", "<li>C</li>", "</ol>", sep = "\n")

    ordered_1 <- Plain("A")
	ordered_2 <- list(Plain(Str("B")))
	ordered_3 <- list(Plain(list(Str("C"))))
	block <- OrderedList(ListAttributes(), list(ordered_1, ordered_2, ordered_3))
    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )

