#' Initiate Python Environment
#'
#'
#' @importFrom reticulate py_discover_config install_python virtualenv_create use_virtualenv py_module_available py_install py_discover_config
init_venv <- function(env_name = "mtar_env", create = TRUE) {
  target_version <- "3.12.9"
  home <- Sys.getenv("USERPROFILE", unset = Sys.getenv("HOME"))
  env_path <- file.path(home, env_name)

  # Detect if Python 3.12.9 already is in reticulate
  py_cfg <- tryCatch(py_discover_config(), error = function(e) NULL)
  if (!is.null(py_cfg)) {
    current_version <- gsub("^(.*?) .*", "\\1", py_cfg$version_string)
  } else {
    current_version <- ""
  }

  if (target_version != current_version) {
    message("Python ", target_version, " is not active. Installing...")
    py_install_path <- install_python(version = target_version, optimized = FALSE)
  } else {
    message("Python ", target_version, " is already available.")
    py_install_path <- py_cfg$python
  }

  # Create virtualenv if it does not exist
  if (!dir.exists(env_path) && create) {
    message("Creating virtual environment at: ", env_path)
    virtualenv_create(envname = env_path, python = py_install_path)
  }

  # Activate virtualenv
  message("Activating virtual environment: ", env_path)
  use_virtualenv(env_path, required = TRUE)

  # `Install required Python packages if necessary
  required <- c("bertopic", "pandas", "numpy", "scikit-learn")
  missing <- required[!vapply(required, py_module_available, logical(1))]

  if (length(missing) > 0) {
    message("Installing missing Python packages: ", paste(missing, collapse = ", "))
    py_install(missing, envname = env_path, method = "virtualenv")

  }

  py_discover_config(use_environment = env_path)
}
