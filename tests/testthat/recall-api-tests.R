library(canrecall)


test_that("recall_api length of request", {
  expect_equal(nrow(recall_api(search = 'lettuce', cat = '1', lim = 6, lang = 'en')), 6)
  expect_equal(nrow(recall_api(search = 'lettuce', cat = '1', lim = 6, lang = 'es')), 6)
  expect_equal(nrow(recall_api(search = 'lettuce', cat = 'a', lim = 6, lang = 'en')), 6)
  expect_equal(nrow(recall_api(search = 'lettuce', cat = '1', lim = 'a', lang = 'en')), 5)
})

test_that("recall_api error testing", {
  expect_warning(recall_api(search = 'peanuts', cat = '1', lim = 10, lang = 'es'), 'lang not a supported language input. Defaulting search to en for english')
  expect_warning(recall_api(search = 'peanuts', cat = 'a', lim = 10, lang = 'en'), "Only values of 1,2,3 or 4 for cat or category of recall are accepted, searching with cat set to NULL")
  expect_warning(recall_api(search = 'peanuts', cat = '1', lim = 'a', lang = 'en'), "Lim only accepts integers, With incorrect input will only search for 5")
})
