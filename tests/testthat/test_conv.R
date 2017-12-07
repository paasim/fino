context("conv")
test_that("yt_to_ovt works as expected", {
  expect_true(all(valid_ovt(yt_to_ovt(gen_yt(47)),
                            locate = FALSE, require_checksum = TRUE)))
  expect_true(is.na(yt_to_ovt("not_yt")))
})

test_that("yt_to_vat works as expected", {
  expect_true(all(valid_vat(yt_to_vat(gen_yt(47)),
                            locate = TRUE, require_checksum = TRUE)))
  expect_true(is.na(yt_to_vat("not_yt")))
})

test_that("ovt_to_yt works as expected", {
  expect_true(all(valid_yt(ovt_to_yt(gen_ovt(47)),
                           locate = FALSE, require_checksum = TRUE)))
  expect_true(is.na(ovt_to_yt("not_ovt")))
})

test_that("vat_to_yt works as expected", {
  expect_true(all(valid_yt(vat_to_yt(gen_vat(47)),
                           locate = TRUE, require_checksum = TRUE)))
  expect_true(is.na(vat_to_yt("not_vat")))
})
