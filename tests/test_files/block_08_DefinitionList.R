context("DefinitionList")

test_that("DefinitionList", {

    y <- paste("<dl>", "<dt>key</dt>", "<dd>value", "</dd>", "<dt>key</dt>",  
               "<dd>value", "</dd>", "</dl>", sep = "\n")

    key <- list(Str("key"))
    value <- list(list(Plain(list(Str("value")))))
    block <- DefinitionList(list(list(key, value), list(key, value)))

    x <- pandocfilters:::test(list(block))
    expect_that(x, equals(y))

} )
