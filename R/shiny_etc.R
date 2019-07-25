
shiny_check <- function() {
  if (!interactive()) {
    abort("need an interactive session for this functionality.")
  }

  if (!rlang::is_installed("shiny")) {
    abort("'shiny' package is required for this functionality.")
  } else {
    if (!rlang::is_installed("miniUI")) {
      abort("'miniUI' package is required for this functionality.")
    }
  }

  es <- loadNamespace("shiny")
  try(attachNamespace(es), silent = TRUE)

  em <- loadNamespace("miniUI")
  try(attachNamespace(em), silent = TRUE)
}

shiny_unload <- function() {
  AutoUnloadShiny <- options("AutoUnloadShiny")[[1]]
  if (is.null(AutoUnloadShiny)) AutoUnloadShiny <- TRUE
  if (AutoUnloadShiny) {
    suppressWarnings(try(unloadNamespace("miniUI"), silent = TRUE))
    suppressWarnings(try(unloadNamespace("shiny"), silent = TRUE))
  }
}

global_objects <- function(cls = c("cell_df", "cell_analysis", "rc_df"), check_rc_df = FALSE) {
  cls <- match.arg(cls)

  objs <- ls(envir = globalenv())

  cobjs <- objs %>%
    map_lgl(~ inherits(x = get(.x, envir = globalenv()), cls)) %>%
    objs[.]

  cobjs
}

command_prompt_ask <- function(title, message, default = NULL) {
  cat(paste0(cli_bb(message), "\n"))
  cm <- readline(prompt = paste0(title, ":"))
  if (nchar(cm) == 0) {
    default
  } else {
    cm
  }
}

rstudioapi_ask <- function(title, message, default = NULL) {
  ok <- FALSE
  if (rlang::is_installed("rstudioapi")) {
    if (rstudioapi::hasFun("viewer")) {
      ok <- TRUE
    }
  }

  if (ok) {
    rstudioapi::showPrompt(title = title, message = message, default = default)
  } else {
    command_prompt_ask(title = title, message = message, default = default)
  }
}

global_object_picker <- function(what = c("cell_df", "cell_analysis", "rc_df")) {
  what <- match.arg(what)
  objs <- global_objects(cls = what)
  if (length(objs) == 1) {
    return(invisible(get(objs, envir = globalenv())))
  }
  if (length(objs) < 1) {
    abort(paste0("No ", what, " found in R global environment."))
  }
  if (length(objs) > 1) {
    this_title <- paste0("Select a ", what, " to proceed")
    this_msg <- paste0(
      "Pick from one of these (type the variable name)\n",
      paste0(objs, collapse = ", ")
    )
    this_msg_warned <- FALSE
    repeat({
      sel <- rstudioapi_ask(
        title = this_title,
        message = this_msg
      )
      if (length(sel) == 0) abort("User Cancelled")
      if (sel %in% objs) {
        break()
      } else {
        if (!this_msg_warned) {
          this_msg_warned <- TRUE
          this_msg <- paste0(this_msg, "\n(Wrong input try again)")
        }
      }
    })
    objs <- sel
    return(invisible(get(objs, envir = globalenv())))
  }

  abort("unknown error")
}
