progress_callback <- function(update_progress, nrounds) {
  # Return a named list with the callback function
  list(
    name = "progress_update",  # Set a valid name for the callback
    call = function(env) {
      # Update progress bar
      if (!is.null(update_progress)) {
        progress <- env$iteration / nrounds
        update_progress(progress)
      }
    }
  )
}
