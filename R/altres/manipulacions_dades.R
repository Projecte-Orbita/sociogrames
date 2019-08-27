# Manipulacions de dades de sociogrames
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

wd = getwd()
source(file.path(wd, 'R', 'altres', 'utils.R'), encoding = "UTF-8")

require(stringr)
require(readr)
# Aquest fitxer s'haurà de reorganitzar, per ara hi ha coses de manipulacions de noms i com transformar els excels en 
# csv bonics per fer els informes

### Utilitzat ara en els gràfics col·lectius
# Hem d'agafar els noms i la primera lletra del cognom de tots els nens
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
####

# Capitalitzem la primera lletra dels noms perquè quedin sempre ben escrits encara que a l'excel no ho estiguin

# FIXME: cognoms tipus "de la rosa" queden amb tot en majúscules "De La Rosa"

capitalitzar <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) 
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   
  tot = paste0(first, rest)
  return(gsub("-(.)", "-\\U\\1", tot, perl=TRUE)) # Per les lletres de després d'un guió
}

capitalizar_noms = function(columna){
  columna = str_split_fixed(columna, " ", 3)
  cap_noms = apply(columna, 2, capitalitzar)
  if(nrow(columna)==1){
    noms =  trimws(paste(cap_noms[1], cap_noms[2], cap_noms[3], " "), "both")
  }
  else{
    noms = trimws(paste(cap_noms[,1], cap_noms[,2], cap_noms[,3], " "), "both")
  }
  return(noms)
}


# Agafem tots els noms i filtrarem més endavant; només en el gràfic de la classe conjunta

tractar_i_ajuntar_noms = function(tres_columnes_noms){
  noms_nens = paste0(capitalizar_noms(tres_columnes_noms[,1]), # els ifelse són per si hi ha noms buits
                     " ", 
                     ifelse(!is.na(tres_columnes_noms[,2]),capitalizar_noms(tres_columnes_noms[,2]),""), 
                     " ",
                     ifelse(!is.na(tres_columnes_noms[,3]),capitalizar_noms(tres_columnes_noms[,3]),""))
  return(noms_nens)
}


pretractar_excels <-function(path, nom_carpeta,limit=10){
  
  # TODO: capitalitzar els noms quan estan tots en majúscules
  
  require(readxl)
  noms = excel_sheets(path = path)
  classes = lapply(excel_sheets(path), read_excel, path = path)
  cursos = sapply(strsplit(noms, ""), head, 1)
  numeros_classe = sapply(strsplit(noms, ""), tail, 1)
  noms_fitxers = paste0(cursos, numeros_classe)
  # classes = sapply(classes, as.data.frame)  # per alguna raó aquí no funciona; fem loop...
  
  for (class in classes){
    class = as.data.frame(class)
  }
  
  # mirem quins tenen dades:  
  numero_nens_per_classe = sapply(classes, function(x) length(x$Nom[x$Nom!=""]))
  
  # posem un filtre pels valors que no tenen dades o en tenen poques, perquè aniran a part:
  
  classes_bones = which(numero_nens_per_classe>limit+1)
  
  # Formategem els sheets per tenir-los de la nostra manera:
  
  # s'han de triar les columnes, que no són les mateixes per tots els cursos
  
  
  columnes = list(NULL,NULL,c(7:49),c(7:49),NULL,NULL)
  names(columnes) = c(1,2,3,4,5,6)
  
  directori = getwd()
  
  netejar_directoris(file.path(directori, nom_carpeta))
  options(encoding = "UTF-8")
  # s'han de crear els fitxers
  
  for (i in classes_bones){
    fitxer = classes[[i]]
    #fitxer = fitxer[-1,]
    cols = unlist(columnes[cursos[i]], use.names = F)
    #cols = c(cols, 8) # afegim els comentaris, que els posem al final
    df = cbind.data.frame(tractar_i_ajuntar_noms(as.data.frame(fitxer[,4:6], 
                                                               stringsAsFactors =F)), 
                          fitxer[,cols])
    df[,1] = as.character(df[,1])
    #Encoding(df) = "UTF-8"
    con = file(file.path(nom_carpeta, paste0(noms_fitxers[i],".csv")), encoding= "UTF-8")
    write_excel_csv(df, file.path(nom_carpeta, paste0(noms_fitxers[i],".csv")), col_names = F)
  }
  
}


if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  #path_ = 'dades/proves_excel_sociograma.xlsx'
  #carpeta = 'temp/dades'
  #pretractar_excels(path_, carpeta)
}
