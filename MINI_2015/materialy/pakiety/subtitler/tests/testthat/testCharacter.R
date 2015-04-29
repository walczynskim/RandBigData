test_that("result is of the character class", {
  expect_equal(class(readSubtitle("http://dl.opensubtitles.org/pl/download/file/1954081967")),
               "character")
})

test_that("result is of the character class", {
  expect_equal(readSubtitle("http://dl.opensubtitles.org/pl/download/file/1954081967")[1], "1")
})

test_that("result is of the character class", {
  expect_equal(class(readSubtitle("http://dl.opensubtitles.org/pl/download/file/1954081967")), "character")
})

