test_that("checking if return data frame",
  expect_equal(class(OstatnieRomunskieNapisy(2014, 8.8, FALSE, TRUE)), 'data.frame'))

test_that("testing return data frame for data that would give no output",
          expect_equal(class(OstatnieRomunskieNapisy(3000, 10, FALSE, FALSE)), 'data.frame'))


test_that("testing if makes .txt file",
          {
            OstatnieRomunskieNapisy(2013, 9.0, fileSave = TRUE)
            file.exists('OstatnieRomunskieNapisy.txt')
          } == TRUE)
