
test_that("load config throws error without path to config", {
  expect_error(supressWarnings(load_config()))
})
