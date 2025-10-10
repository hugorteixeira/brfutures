# R/zzz.R

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Attempt to load bizdays built-in calendars
  if (requireNamespace("bizdays", quietly = TRUE)) {
    tryCatch({
      # Load built-in calendars (includes ANBIMA holidays)
      bizdays::load_builtin_calendars()

    }, error = function(e) {
      # Use warning() instead of packageStartupMessage() in .onLoad
      warning(
        "Could not load bizdays calendar: ",
        conditionMessage(e),
        "\nSome date functions may not work properly.",
        call. = FALSE,
        immediate. = FALSE
      )
    })
  } else {
    # Use warning() for missing dependencies
    warning(
      "Package 'bizdays' not found. ",
      "Install it with: install.packages('bizdays')",
      call. = FALSE,
      immediate. = FALSE
    )
  }
}

# Optional: informational messages go here
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "brfutures ",
    utils::packageVersion("brfutures"),
    " loaded successfully."
  )
}
