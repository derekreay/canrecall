library(canrecall)


test_that("recall_date_api length of request", {
  expect_equal(nrow(recall_date_api(search = "lettuce", cat = 1, lang = 'en', datestart = '2011', dateend = '2013')), 8)
  expect_equal(nrow(recall_date_api(search = "lettuce", cat = 1, lang = 'es', datestart = '2011', dateend = '2013')), 8)
  expect_equal(nrow(recall_date_api(search = "lettuce", cat = 'a', lang = 'en', datestart = '2011', dateend = '2013')), 9)
  expect_equal(nrow(recall_date_api(search = "lettuce", cat = 1, lang = 'en', datestart = '2012', dateend = '2013')), 6)
})

test_that("recall_date_api error testing", {
  expect_warning(recall_date_api(search = "lettuce", cat = 1, lang = 'es', datestart = '2011', dateend = '2013'), 'lang not a supported language input. Defaulting search to en for english')
  expect_warning(recall_date_api(search = "lettuce", cat = 'a', lang = 'en', datestart = '2011', dateend = '2013'), "Only values of 1,2,3 or 4 for cat or category of recall are accepted, searching with cat set to NULL")
  expect_warning(recall_date_api(search = "lettuce", cat = 1, lang = 'en', datestart = '1911', dateend = '2013'), 'did you enter datestart incorrectly as  1911-01-01 . No data exists before 1974')
})
