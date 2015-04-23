test_that("Result is data.frame",{
expect_true(is.data.frame(NewSubtitleList()))
})
test_that("An invalid parameter",{
  expect_error(NewSubtitleList(3))
  expect_error(NewSubtitleList("polski"))
})

test_that("No subtitles found",{
  expect_equal(NewSubtitleList("abc"),"not found any subtitles")
})
