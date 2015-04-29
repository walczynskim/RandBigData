test_that("Function succes", {
  expect_equal(GetMovieInfo("The Matrix"), 0 )
})

test_that("Function creates file", {
  expect_equal({GetMovieInfo("Inception");
                file.exists("Inception.txt")}, TRUE)
})
