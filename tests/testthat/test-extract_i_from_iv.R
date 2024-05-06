test_that("extract If from I-V curve works", {
  expect_equal(extract_i_from_iv(c(0, 1, 2), c(3, 4, 5), V0 = 1), 4)
})
