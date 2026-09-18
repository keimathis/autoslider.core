opts_partial_match_old <- list(
  warnPartialMatchDollar = getOption("warnPartialMatchDollar"),
  warnPartialMatchArgs = getOption("warnPartialMatchArgs"),
  warnPartialMatchAttr = getOption("warnPartialMatchAttr")
)

opts_partial_match_new <- list(
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE
)

# Pin the rtables header/body separator so snapshots don't depend on the
# locale/charset of the R session running the tests.
options(formatters_default_hsep = "-")
