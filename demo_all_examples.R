# Run all complete examples after installing gamlssPosthoc and dependencies.
files <- sort(list.files(system.file("examples", package = "gamlssPosthoc"),
                         pattern = "\\.R$", full.names = TRUE))
for (f in files) {
  message("\n===== Running ", basename(f), " =====")
  sys.source(f, envir = new.env(parent = globalenv()))
}
