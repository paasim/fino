# really just a placeholder for real tests

context("all")
test_that("all works as expected", {
  expect_true(gen_yt(50) %>% valid_yt() %>% all())
  expect_true(gen_vat(50) %>% valid_vat() %>% all())
  expect_true(gen_ovt(50) %>% valid_ovt() %>% all())
  expect_true(gen_ovt(50) %>% ovt_to_yt() %>% valid_yt() %>% all())
  expect_true(gen_vat(50) %>% vat_to_yt() %>% valid_yt() %>% all())
  expect_true(gen_yt(50) %>% yt_to_vat() %>% valid_vat() %>% all())
  expect_true(gen_yt(50) %>% yt_to_ovt() %>% valid_ovt() %>% all())
  expect_true(gen_id(133) %>% valid_id() %>% all())
})
