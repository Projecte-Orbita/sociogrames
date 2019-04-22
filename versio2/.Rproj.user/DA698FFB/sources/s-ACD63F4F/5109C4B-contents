# Funcions helpers
# 

importar_i_manipular = function(path_fitxer, numero_respostes){
  # importem
  soc = read.csv(path_fitxer, fileEncoding = "UTF-8")
  colnames(soc)[1:2] = c("noms", "num")
  
  ## En cas que sigui necessari, si hi ha missingns en els números i noms de nens (de cada tres només n'han posat un)
  # soc$número =ffill(soc$número, NA)
  # soc$nom = ffill(soc$nom, "")
  
  # matriu que calcula el número de vegades que s'ha estat anomenat en cada categoria
  mat = matrix(data = 0, 
               nrow = nrow(soc)/numero_respostes, 
               ncol = ncol(soc)-2)
  
  for (j in 1:ncol(mat)){
    for (i in 1:nrow(mat)){
      mat[i,j] = sum(soc[,j+2]==i)
    }
  }
  colnames(mat) = colnames(soc[,3:ncol(soc)])
  
  noms = as.character(soc$nom[seq(1, nrow(soc), numero_respostes)]) # faig un vector amb els noms, que necessitaré més endavant
  
  # Estandaritzem tots els resultats:
  mat_est = scale(mat)
  
  return(list(mat, mat_est, noms, soc))
}