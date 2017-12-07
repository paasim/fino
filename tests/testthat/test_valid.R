context("valid")
test_that("valid_id works as expected", {
  # some randomly generated, verified valid IDs
  valid_ids <- c("110124-4332", "200953+300H", "200211A2735")
  kinda_valid_id <- "111111-1111"
  invalid_ids <- c(substr(valid_ids, 1, 6), kinda_valid_id, "200211B2735", "13")
  expect_true(all(valid_id(valid_ids, locate = FALSE)))
  expect_false(any(valid_id(invalid_ids)))
  expect_true(valid_id(kinda_valid_id, require_checksum = FALSE))
  expect_true(valid_id("a110124-4332a", locate = TRUE))

})

test_that("valid_yt works as expected", {
  # some randomly generated, verified valid IDs
  valid_yts <- c("3624533-3", "1893817-9", "9885941-0")
  kinda_valid_yt <- "1111111-1"
  invalid_yts <- c(substr(valid_yts, 1, 8), kinda_valid_yt, "9885941+0", "13")
  expect_true(all(valid_yt(valid_yts, locate = FALSE)))
  expect_false(any(valid_yt(invalid_yts)))
  expect_true(valid_yt(kinda_valid_yt, require_checksum = FALSE))
  expect_true(valid_yt("a3624533-3a", locate = TRUE))
})

test_that("valid_ovt works as expected", {
  # some randomly generated, verified valid IDs
  valid_ovts <- c("003736245333", "003718938179", "003798859410")
  kinda_valid_ovt <- "003711111111"
  invalid_ovts <- c(substr(valid_ovts, 1, 8), kinda_valid_ovt, "111111111111", "13")
  expect_true(all(valid_ovt(valid_ovts, locate = FALSE)))
  expect_false(any(valid_ovt(invalid_ovts)))
  expect_true(valid_ovt(kinda_valid_ovt, require_checksum = FALSE))
  expect_true(valid_ovt("a003718938179a", locate = TRUE))
})

test_that("valid_vat works as expected", {
  # some randomly generated, verified valid IDs
  valid_vats <- c("FI36245333", "SE189131281179", "aFI18938179a")
  kinda_valid_vat <- "FI11111111"
  invalid_vats <- c(substr(valid_vats, 1, 8), kinda_valid_vat, "111111111111", "13")
  expect_true(all(valid_vat(valid_vats, locate = TRUE)))
  expect_false(any(valid_vat(invalid_vats)))
  expect_true(valid_vat(kinda_valid_vat, require_checksum = FALSE))
  expect_error(valid_vat("a", locate = FALSE))
})
