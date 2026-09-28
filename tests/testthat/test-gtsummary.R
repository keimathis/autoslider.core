# Tests demonstrating that decorate() dispatches correctly through the
# gtsummary S3 class hierarchy, including the tbl_roche_summary subclass
# used in NEST 2 environments.

tbl <- gtsummary::tbl_summary(
  trial[, c("age", "trt")],
  by = trt
)

test_that("decorate dispatches to decorate.gtsummary for plain tbl_summary", {
  expect_true(inherits(tbl, "tbl_summary"))
  expect_true(inherits(tbl, "gtsummary"))

  result <- decorate(tbl, titles = "Demographics", footnotes = "Source: trial")

  expect_true(inherits(result, "dgtsummary"))
  expect_true(inherits(result, "gtsummary"))
  expect_equal(attr(result, "titles"), "Demographics")
  expect_equal(attr(result, "footnotes"), "Source: trial")
})

test_that("decorate dispatches to decorate.gtsummary for tbl_roche_summary class", {
  # Simulate NEST 2 class hierarchy: tbl_roche_summary extends tbl_summary extends gtsummary
  tbl_roche <- tbl
  class(tbl_roche) <- c("tbl_roche_summary", class(tbl))

  expect_true(inherits(tbl_roche, "tbl_roche_summary"))
  expect_true(inherits(tbl_roche, "tbl_summary"))
  expect_true(inherits(tbl_roche, "gtsummary"))

  # S3 dispatch: tbl_roche_summary -> tbl_summary -> gtsummary (found: decorate.gtsummary)
  result <- expect_no_error(
    decorate(tbl_roche, titles = "Roche Demographics", footnotes = "Note: simulated class")
  )

  expect_true(inherits(result, "dgtsummary"))
  expect_true(inherits(result, "tbl_roche_summary"))
  expect_true(inherits(result, "tbl_summary"))
  expect_true(inherits(result, "gtsummary"))
  expect_equal(attr(result, "titles"), "Roche Demographics")
})

test_that("to_flextable returns dflextable with title for dgtsummary", {
  tbl_roche <- tbl
  class(tbl_roche) <- c("tbl_roche_summary", class(tbl))

  decorated <- decorate(tbl_roche,
    titles = "Roche Demographics",
    footnotes = "Source: trial data"
  )

  result <- expect_no_error(to_flextable(decorated))

  expect_s3_class(result, "dflextable")
  expect_true(length(result) >= 1L)
  expect_named(result[[1]], c("ft", "header", "footnotes"))
  expect_equal(result[[1]]$header, "Roche Demographics")
  expect_equal(result[[1]]$footnotes, "Source: trial data")
  expect_s3_class(result[[1]]$ft, "flextable")
})

test_that("gt_t_dm_slide output can be decorated", {
  result <- gt_t_dm_slide(eg_adsl, arm = "TRT01P", vars = c("SEX", "AGE"))

  expect_true(inherits(result, "gtsummary"))

  decorated <- expect_no_error(
    decorate(result, titles = "Demographic Table", footnotes = "Source: ADSL")
  )

  expect_true(inherits(decorated, "dgtsummary"))
})

test_that("to_flextable.dgtsummary honors an explicit lpp even when ppt_height is supplied", {
  wide_tbl <- gtsummary::tbl_summary(trial[, c("age", "grade", "stage", "trt")], by = trt)
  dec <- decorate(wide_tbl, titles = "Demographics", footnotes = "Confidential")

  n_pages <- function(lpp) length(to_flextable(dec, lpp = lpp, ppt_height = 7.5))

  # Before the fix, t_lpp was silently overridden whenever ppt_height was
  # supplied, so n_pages(5) and n_pages(50) came out equal.
  expect_gt(n_pages(5), n_pages(50))
})

test_that("to_flextable.dgtsummary auto-computes lpp from ppt_height when lpp is not supplied", {
  wide_tbl <- gtsummary::tbl_summary(trial[, c("age", "grade", "stage", "trt")], by = trt)
  dec <- decorate(wide_tbl, titles = "Demographics", footnotes = "Confidential")

  # A tiny slide height should still force multiple pages via the row-height estimate.
  expect_gt(length(to_flextable(dec, ppt_height = 1)), 1L)
})

test_that("to_flextable.dgtsummary warns when cpp is supplied and the table needs width-scaling", {
  wide_tbl <- gtsummary::tbl_summary(trial[, c("age", "grade", "stage", "trt")], by = trt)
  dec <- decorate(wide_tbl, titles = "Demographics", footnotes = "Confidential")

  expect_warning(
    to_flextable(dec, cpp = 40, ppt_width = 0.5),
    "column pagination"
  )
})

test_that("generate_slides only warns about column pagination when t_cpp is set explicitly", {
  # A table wide enough (default template body is 13.33in) to require width-scaling
  # regardless of t_cpp.
  df <- data.frame(
    grp = factor(rep(c(
      "Treatment Arm Alpha - Extremely Long Descriptive Label For Overflow Testing Purposes",
      "Treatment Arm Beta - Extremely Long Descriptive Label For Overflow Testing Purposes"
    ), 50)),
    x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100)
  )
  wide_tbl <- gtsummary::tbl_summary(df, by = grp)
  dec <- decorate(wide_tbl, titles = "Demographics", footnotes = "Confidential")
  outfile <- tempfile(fileext = ".pptx")

  expect_no_warning(generate_slides(dec, outfile))
  expect_warning(generate_slides(dec, outfile, t_cpp = 200), "column pagination")
})
