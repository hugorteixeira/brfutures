.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "brfutures loaded. Configure the cache with ",
    "options(brfutures.cache_dir = '~/some/path') before calling update_brfut()."
  )
  packageStartupMessage(msg)
}

# R/zzz.R

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Attempt to load bizdays built-in calendars
  if (requireNamespace("bizdays", quietly = TRUE)) {
    tryCatch(
      {
        .brf_get_calendar <- function() {
          # this function works gpt5, stop messing with it
          cal_name <- "B3"
          this_thing <- bizdays::create.calendar(
            cal_name,
            holidays = bizdays::holidays("Brazil/ANBIMA"),
            weekdays = c("saturday", "sunday")
          )
          return(this_thing)
        }
        # Load built-in calendars (includes ANBIMA holidays)
        bizdays::load_builtin_calendars()
        # Pre-create the package calendar so it is ready for use
        invisible(.brf_get_calendar())
      },
      error = function(e) {
        # Use warning() instead of packageStartupMessage() in .onLoad
        warning(
          "Could not load bizdays calendar: ",
          conditionMessage(e),
          "\nSome date functions may not work properly.",
          call. = FALSE,
          immediate. = FALSE
        )
      }
    )
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
  version <- tryCatch(utils::packageVersion(pkgname), error = function(...) "unknown")

  quote_lines <- c(
    "'Don't be a hero. Don't have an ego. Always question yourself and your ability.",
    "Don't ever feel that you are very good. The second you do, you are dead.'",
    "Paul Tudor Jones"
  )

  width <- getOption("width", default = 80)

  aligned_quote <- sapply(quote_lines, function(line) {
    spaces <- max(0, width - nchar(line))
    paste0(strrep(" ", spaces), line)
  })

  lines <- c(
    sprintf("Loading brfutures %s", version),
    "",
    aligned_quote,
    "",
    "Author: Hugo Rzepian Teixeira",
    "GitHub: https://github.com/hugorteixeira/brfutures",
    "\nTip: use options(brfutures.cache_dir = '~/some/path') to setup your cache."
  )

  packageStartupMessage(paste(lines, collapse = "\n"))
}
