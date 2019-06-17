context("Table")

test_that("Table", {

    M <- matrix(1:4, 2)
    T <- Table(M, col_names=c("A", "B"))
     
    x <- pandocfilters:::test(list(T), "markdown")
    if ( get_pandoc_version() < 2 ) {
    	y <- paste("  A   B", "  --- ---", "  1   3", "  2   4", "", "", sep = "\n")
    } else {
    	y <- paste("  A   B", "  --- ---", "  1   3", "  2   4", sep = "\n")
    }
    
    expect_that(x, equals(y))

} )
