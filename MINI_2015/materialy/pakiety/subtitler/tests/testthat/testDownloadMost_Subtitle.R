test_that("successfully downloaded", {
  expect_equal(downloadMost_Subtitle("new"), "Downloaded.")
})

test_that("successfully downloaded", {
  expect_equal(downloadMost_Subtitle("popular"), "Downloaded.")
})

test_that("successfully downloaded", {
  expect_equal(downloadMost_Subtitle("recommended"), "Downloaded.")
})

unlink("downloaded_subtitles", recursive=TRUE)
