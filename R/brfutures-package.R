#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lubridate force_tz interval
#' @importFrom stats runif
#' @importFrom xts xts is.xts xtsAttributes
#' @importFrom zoo index
## usethis namespace: end
NULL

# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables(c(
  "pricing", "service"
))
