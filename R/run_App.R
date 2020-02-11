#' Run a basic User Interface to easily run simulation experiments and "download" the simulated data.
#' @examples
#' \dontrun{run_App()}
run_App <- function() {
  appDir <- system.file("shiny-apps", "basic-ui", package = "SimAdmixtR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `SimAdmixtR`.", call. = FALSE)
  }

  runApp(appDir, display.mode = "normal")
}
