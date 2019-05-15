# Útils
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
require(stringr)

importar_i_manipular = function(path_fitxer, numero_respostes){
  # importem
  options(readr.num_columns = 0)  # per treure uns comentaris que emprenyen bastant
  soc = read_csv(path_fitxer, col_names = F)
  soc = as.data.frame(soc)
  colnames(soc)[1:3] = c("noms", "genere", "num")
  
  # Netegem els gèneres per si la gent posa coses rares:
  soc$genere = as.character(soc$genere)
  
  soc$genere[(soc$genere!="nen" & 
                soc$genere!="nena" & 
                soc$genere!="altres" )] = "NS"
  soc$genere[is.na(soc$genere)] = "NS"
  soc$genere = as.factor(soc$genere)
  ## En cas que sigui necessari, si hi ha missingns en els números i noms de nens (de cada tres només n'han posat un)
  # soc$número =ffill(soc$número, NA)
  # soc$nom = ffill(soc$nom, "")
  
  # matriu que calcula el número de vegades que s'ha estat anomenat en cada categoria
  mat = matrix(data = 0, 
               nrow = nrow(soc)/numero_respostes, 
               ncol = ncol(soc)-3)
  
  for (j in 1:ncol(mat)){
    for (i in 1:nrow(mat)){
      mat[i,j] = sum(soc[,j+3]==i, na.rm = T)
    }
  }
  colnames(mat) = colnames(soc[,4:ncol(soc)])
  
  noms = as.character(soc$nom[seq(1, nrow(soc), numero_respostes)]) # faig un vector amb els noms, que necessitaré més endavant
  
  # Estandaritzem tots els resultats:
  mat_est = scale(mat)
  curs = substr(path_fitxer, nchar(path_fitxer)-5,nchar(path_fitxer)-4)
  return(list(mat, mat_est, noms, soc, curs))
}

formatejar_noms = function(columna_noms){
  # Aquí escurcem els noms perquè si no el de l'esquerra no es veu bé:
  # TODO: noms compostos molt llargs poden portar problemes
  
  np = as.data.frame(str_split_fixed(columna_noms, " ", 3))
  
  noms_nens = paste0(np[,1]," ", substr(np[,2],1,1),".")
  quins_duplicats = which(duplicated(noms_nens))
  
  #en cas de que hi hagi nens amb el mateix nom i la mateixa inicial del cognom agafem la inicial del segon cognom:
  noms_nens[quins_duplicats] = paste0(noms_nens[quins_duplicats]," ", substr(np[quins_duplicats,3],1,1),".")
  return(noms_nens)
}

netejar_directoris = function(...){
  #options(warn = 2)
  closeAllConnections()
  closeAllConnections()
  closeAllConnections()
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

calcular_reciproc = function(xarxa, numero_respostes){
  len = round(nrow(xarxa)/numero_respostes)
  gg <- graph.data.frame(xarxa, directed=T)
  gg <- simplify(gg, remove.multiple = F, remove.loops = T) 
  
  adj = as_adjacency_matrix(gg, sparse = TRUE)
  names(xarxa) = c("num", "altre")
  reci = rep(0, nrow(xarxa))
  
  for (i in 1:len){
    for (j in 1:len){
      if (adj[i,j] == 1 & adj[j,i] == 1){
        reci[which(xarxa$num==i & xarxa$altre==j)] = 1
      }
    }
  }
  
  return(reci)
}