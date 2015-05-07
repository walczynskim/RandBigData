test_that("czy tytul ok?", {
      expect_equal(film_info("http://www.imdb.com/title/tt0118799/")$title, "Zycie jest piekne")
})

test_that("successfully downloaded", {
      expect_message(film_info("http://www.imdb.com/title/tt0118799/"), "\nDone\n")
})

test_that("czy powstal niepusty plik .txt?",
{
      film_info("http://www.imdb.com/title/tt0118799/")
      file.info('film_info//info.txt')$size>0
} == TRUE)

test_that("czy pobralo napisy?",
{
      film_info("http://www.imdb.com/title/tt0118799/")
      file.info('film_info//life-is-beautiful-pol-3510207//Life is beautiful.txt')$size>0
} == TRUE)
