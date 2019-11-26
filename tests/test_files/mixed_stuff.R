context("Auxiliary Functions")

test_that("get_pandoc_path", {
    expect_that(is.character(get_pandoc_path()), equals(TRUE))
    expect_that(length(get_pandoc_path()), equals(1L))
} )


test_that("get_pandoc_version", {
    expect_that(is.numeric(get_pandoc_version()), equals(TRUE))
    expect_that(is.character(get_pandoc_version("character")), equals(TRUE))
    expect_that(length(get_pandoc_version()), equals(1L))
    expect_that(length(get_pandoc_version("character")), equals(1L))
} )


test_that("get_pandoc_types_version", {
    expect_that(is.numeric(get_pandoc_types_version()), equals(TRUE))
    expect_that(is.character(get_pandoc_types_version("character")), equals(TRUE))
    expect_that(length(get_pandoc_types_version()), equals(1L))
    expect_that(length(get_pandoc_types_version("character")), equals(1L))
} )
