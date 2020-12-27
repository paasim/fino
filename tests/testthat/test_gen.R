# assumes that valid_x have been tested

context("gen")
test_that("gen_pic works as expected", {
  expect_true(all(valid_pic(gen_pic(133), require_checksum = TRUE)))
})

test_that("gen_yt works as expected", {
  expect_true(all(valid_yt(gen_yt(47), require_checksum = TRUE)))
})

test_that("gen_ovt works as expected", {
  expect_true(all(valid_ovt(gen_ovt(47), require_checksum = TRUE)))
})

test_that("gen_vat works as expected", {
  expect_true(all(valid_vat(gen_vat(47), require_checksum = TRUE)))
})
