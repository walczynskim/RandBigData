test_that("checking returns a list",
          expect_equal(class(imdb_wikipedia_subtitles_info("http://www.imdb.com/title/tt0095016/")), 'list'))
test_that("testing contain character value at title",
          expect_equal(class(imdb_wikipedia_subtitles_info("http://www.imdb.com/title/tt0095016/")[[1]]), 'character'))
test_that("testing if return data frame for subtitles (last element)",
          expect_equal(class(
            {
              temp1 <- imdb_wikipedia_subtitles_info("http://www.imdb.com/title/tt0095016/")
              temp1[[length(temp1)]]
            }
          ), 'data.frame'))
