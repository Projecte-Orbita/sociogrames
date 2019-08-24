# Serveix per ajudar a "build" el paquet; sobretot per tenir documentació automatitzada
# Tret de: https://stackoverflow.com/questions/33776643/subdirectory-in-r-package


sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    print(nm)
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("SociogramesAtom")