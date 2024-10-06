.onLoad <- function(libname, pkgname) {

  # When installing the package, src/install.libs.R compiles and installs the
  # stan model files. Then when loading an installed version of the package,
  # instantiate::stan_package_model will look in the installation directory to
  # find the executable. But pkgload::load_all does not simulate behaviour of
  # src/install.libs.R so here we compile the stan models and move
  # them into a place where the pkgload system.file shim can find them
  if (pkgload::is_loading()) {

    # User may not have cmdstan installed yet
    if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
      packageStartupMessage("Installing cmdstan")
      cmdstanr::install_cmdstan()
    } else {
      packageStartupMessage(paste("Found cmdstan at path",
                                  cmdstanr::cmdstan_path()))
    }
    # When epikinetics installation is simulated using load_all, the system.file
    # shim looks for files in the local source directory so here we create a
    # temporary 'bin/stan' directory in the source directory so that calls to
    # system.file("bin/stan/model.stan", "epikinetics") will resolve correctly
    if (nchar(libname) == 1) {
      libname <- ""
    }
    bin <- file.path(libname, pkgname, "bin")
    if (!dir.exists(bin)) {
      packageStartupMessage("Creating local bin directory")
      dir.create(bin, recursive = TRUE, showWarnings = TRUE)
    }
    packageStartupMessage("Copying stan files")
    bin_stan <- file.path(libname, pkgname, "bin", "stan")
    source_path <- file.path(libname, pkgname, "src", "stan")

    fs::dir_copy(path = source_path, new_path = bin, overwrite = TRUE)
    instantiate::stan_package_compile(
      models = instantiate::stan_package_model_files(path = bin_stan),
      cpp_options = list(stan_threads = TRUE),
      stanc_options = list("O1")
    )
    packageStartupMessage("Finished compiling models")
  }
}
