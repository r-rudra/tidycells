pkg_init_if_not_already <- function() {
  if (!exists("na_pkgs", envir = pkg_availability_sim_env)) {
    # Initialization block: this code runs only once to set up the simulation environment.

    assign("na_pkgs", list(), envir = pkg_availability_sim_env)
    assign("is_working_for_this_pkg", list(), envir = pkg_availability_sim_env)

    # ---- Fuse Section ----
    #
    # Here, you can define custom test functions to verify that specific
    # functionality(s) from optional packages are operational.
    #
    # These functions are stored in `is_working_for_this_pkg` and should be
    # keyed by package name. Each function should return:
    #   - FALSE if the required functionality is missing or broken.
    #   - TRUE (or any non-FALSE value) if the functionality is working.
    #
    # This mechanism enables fine-grained control — for example, detecting
    # whether a package is installed but some required feature fails due to a
    # missing system dependency.
    #
    # Structure:
    #
    # pkg_availability_sim_env$is_working_for_this_pkg$<pkg_name> <- function() {
    #   # Custom test logic goes here
    #   # Return FALSE if functionality fails; otherwise return TRUE.
    # }

    # --- Example 1: dplyr ---
    #
    # pkg_availability_sim_env$is_working_for_this_pkg$dplyr <- function() {
    #   d <- dplyr::tibble(x = 1, y = 2)
    #   # Verifies that `mutate()` is operational.
    #   # This may seem redundant for mature packages like dplyr,
    #   # but is useful when packages depend on external systems.
    #   dplyr::mutate(d, z = x + y)$z == 3
    # }

    # --- Example 2: docxtractr ---
    #
    # pkg_availability_sim_env$is_working_for_this_pkg$docxtractr <- function() {
    #   # Attempt to load a .doc file — this will fail if LibreOffice is not installed.
    #   docxtractr::read_docx("some_file.doc")  # Ideally via `system.file()` in real use.
    #
    #   # If the function errors (e.g., LibreOffice is missing), it will return FALSE via tryCatch.
    #   # If successful, pkg_is_available() will ultimately return TRUE.
    #   TRUE
    # }
  }
}
