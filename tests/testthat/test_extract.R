context("valid")
test_that("extract_pic works as expected", {
  # some randomly generated, verified valid PICs
  valid_pics <- c("A110124-433233", "aa200953+300Hasd", "A200211A273517")
  kinda_valid_pic <- "A111111-1111"
  invalid_pics <- c(substr(valid_pics, 1, 6), kinda_valid_pic, "200211B2735", "13")

  expect_true(all(!valid_pic(valid_pics))) # is not valid without extracting
  expect_true(all(valid_pic(extract_pic(valid_pics))))

  expect_false(any(valid_pic(extract_pic(invalid_pics))))

  expect_true(is.na(extract_pic(kinda_valid_pic, require_checksum = TRUE)))
  expect_true(!is.na(extract_pic(kinda_valid_pic, require_checksum = FALSE)))
})

test_that("extract_yt works as expected", {
  # some randomly generated, verified valid IDs
  valid_yts <- c("ASD3624533-322", "31893817-97", "229885941-03")
  kinda_valid_yt <- "1111111-1"
  invalid_yts <- c(substr(valid_yts, 1, 8), kinda_valid_yt, "9885941+0", "13")

  expect_true(all(!valid_yt(valid_yts))) # is not valid without extracting
  expect_true(all(valid_yt(extract_yt(valid_yts))))

  expect_false(any(valid_yt(extract_yt(invalid_yts))))

  expect_true(is.na(extract_yt(kinda_valid_yt, require_checksum = TRUE)))
  expect_true(!is.na(extract_yt(kinda_valid_yt, require_checksum = FALSE)))
})

test_that("extract_ovt works as expected", {
  # some randomly generated, verified valid IDs
  valid_ovts <- c("aSD00373624533312", "ASD003718938179AAAF", "+_+)(@$003798859410AA")
  kinda_valid_ovt <- "003711111111"
  invalid_ovts <- c(substr(valid_ovts, 1, 8), kinda_valid_ovt, "111111111111", "13")

  expect_true(all(!valid_ovt(valid_ovts))) # is not valid without extracting
  expect_true(all(valid_ovt(extract_ovt(valid_ovts))))

  expect_false(any(valid_ovt(extract_ovt(invalid_ovts))))

  expect_true(is.na(extract_ovt(kinda_valid_ovt, require_checksum = TRUE)))
  expect_true(!is.na(extract_ovt(kinda_valid_ovt, require_checksum = FALSE)))
})

test_that("extract_vat works as expected", {
  # some randomly generated, verified valid IDs
  valid_vats <- c("YESSIRFI36245333212", "12ASE189131281179-0-012")
  kinda_valid_vat <- "FI11111111"
  invalid_vats <- c(substr(valid_vats, 1, 8), kinda_valid_vat, "111111111111", "13")

  expect_true(all(!valid_vat(valid_vats))) # is not valid without extracting
  expect_true(all(valid_vat(extract_vat(valid_vats))))

  expect_false(any(valid_vat(extract_vat(invalid_vats))))

  expect_true(is.na(extract_vat(kinda_valid_vat, require_checksum = TRUE)))
  expect_true(!is.na(extract_vat(kinda_valid_vat, require_checksum = FALSE)))
})
