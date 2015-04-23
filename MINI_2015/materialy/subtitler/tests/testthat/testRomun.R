test_that("checking if character",
  expect_equal(class(OstatnieRomunskieNapisy()), 'character'))

test_that("testing if length equals to 40",
          expect_equal(length(OstatnieRomunskieNapisy()), 40))

