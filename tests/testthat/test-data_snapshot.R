test_that("Test data snapshot", {
  testthat::skip_on_cran()

  eg_dataname <- data(package = "autoslider.core")$results[, "Item"] |>
    (\(x) x[grep("eg_", x)])() |>
    sort()
  for (datai in eg_dataname) {
    expect_snapshot(dim(get(datai)))
    expect_snapshot(head(get(datai)))
  }
})
