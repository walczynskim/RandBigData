test_that("checking if return list",
          expect_equal(class(pobierz_info_rudas_k(1)), 'list'))
test_that("testing if makes .zip file",
          {
             pobierz_info_rudas_k(31497,"pol")
             file.exists('Jak_rozpetalem_druga_wojne_swiatowa.zip')
          } == TRUE)
