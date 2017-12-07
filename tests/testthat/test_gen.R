context("gen")
test_that("gen_id works as expected", {
  expect_true(all(valid_id(gen_id(133),
                           locate = FALSE, require_checksum = TRUE)))
})

test_that("gen_yt works as expected", {
  expect_true(all(valid_yt(gen_yt(47),
                           locate = FALSE, require_checksum = TRUE)))
})

test_that("gen_ovt works as expected", {
  expect_true(all(valid_ovt(gen_ovt(47),
                        locate = FALSE, require_checksum = TRUE)))
})

test_that("gen_vat works as expected", {
  expect_true(all(valid_vat(gen_vat(47),
                            locate = TRUE, require_checksum = TRUE)))
})
