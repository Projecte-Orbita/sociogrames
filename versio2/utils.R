# Útils
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")


netejar_directoris = function(...){
  #options(warn = 2)
  for (element in c(...)){
    unlink(element, recursive = T)
    dir.create(element)
  }
  return(T)
}

ffill <- function(vector, type){
  a = 0
  
  if (is.na(type)){
    for (i in 1:length(vector)){
      if (!is.na(vector[i])){
        a = vector[i]
      }
      else vector[i]=a
    }
  }
  else {
    for (i in 1:length(vector)){
      if (vector[i]!=type){
        a = vector[i]
      }
      else vector[i]=a
    }
  }
  return (vector)
}