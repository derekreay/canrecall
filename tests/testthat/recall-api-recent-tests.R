library(canrecall)


test_that("recall_date_api length of request", {
  expect_equal(nrow(recentrecall_api(lang = 'en')), 75)
  expect_equal(nrow(recentrecall_api()), 75)
})

test_that("recall_date_api error testing", {
  expect_warning(recentrecall_api(lang = 'es'), 'lang not a supported language input. Defaulting search to en for english')
})
