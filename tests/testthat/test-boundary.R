.brf_boundary_source <- function() {
  source_files <- list.files(
    testthat::test_path("..", "..", "R"),
    pattern = "[.]R$", full.names = TRUE
  )
  paste(
    unlist(lapply(source_files, function(path) {
      deparse(parse(file = path, keep.source = FALSE))
    }), use.names = FALSE),
    collapse = "\n"
  )
}

.brf_boundary_dependencies <- function() {
  description <- utils::packageDescription("brfutures")
  fields <- intersect(
    c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances"),
    names(description)
  )
  entries <- unlist(strsplit(
    paste(unlist(description[fields], use.names = FALSE), collapse = ","),
    ","
  ))
  sub("[[:space:]]*[(].*$", "", trimws(entries))
}

test_that("B3 reference data has no trading-runtime dependency", {
  stack <- c(
    "finmaestro", "finrunner", "finstrat", "positionsizer",
    "binancestream", "mt5stream"
  )
  expect_setequal(
    intersect(.brf_boundary_dependencies(), stack),
    "positionsizer"
  )
  forbidden <- setdiff(stack, "positionsizer")
  expect_false(grepl(
    paste(forbidden, collapse = "|"),
    .brf_boundary_source(), ignore.case = TRUE
  ))
})
