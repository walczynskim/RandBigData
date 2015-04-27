test_that("czy tytuł ok", {
   expect_equal(pobierz.info.just.jankowiak(0499549)[[1]]$Title, "Avatar")
})

test_that("czy gatunek ok", {
   expect_equal(pobierz.info.just.jankowiak(0499549)[[2]][1], "dramat")
})

unlink("downloaded_subtitles", recursive=TRUE)