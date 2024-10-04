libs <- file.path(R_PACKAGE_DIR, "libs", R_ARCH)
dir.create(libs, recursive = TRUE, showWarnings = FALSE)
for (file in c("symbols.rds", Sys.glob(paste0("*", SHLIB_EXT)))) {
  if (file.exists(file)) {
    file.copy(file, file.path(libs, file))
  }
}
if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
  message("Installing cmdstan")
  cmdstanr::install_cmdstan()
} else {
  message(paste("Found cmdstan at path", cmdstanr::cmdstan_path()))
}
bin <- file.path(R_PACKAGE_DIR, "bin")
if (!file.exists(bin)) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
}
bin_stan <- file.path(bin, "stan")
fs::dir_copy(path = "stan", new_path = bin_stan)
models <- instantiate::stan_package_model_files(path = bin_stan)
message(paste("Compiling models:", paste0(models, collapse = ",")))
instantiate::stan_package_compile(
  models = instantiate::stan_package_model_files(path = bin_stan),
  cpp_options = list(stan_threads = TRUE),
  stanc_options = list("O1")
)
message("Finished compiling models")
