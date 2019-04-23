# Informe escola
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
# 
source('informe_collectiu.R')
source('texts_escola.R')
# Aquest fitxer crea els informes per tota l'escola, primer el col·leciu i després els individuals, un fitxer .tex per cada classe

informe_escola = function(path_dades, nom_escola){
  escola = c(nom_escola, gsub(" ", "_", nom_escola))
  
  #####
  # agafem la info de la carpeta d'on treurem les dades (i que abans es passava dins la funció)
  #####
  
  noms_fitxers = as.vector(list.files(paste0('dades/', escola[2])))
  num_curs = substr(noms_fitxers, 1, 1)
  curs_classe = substr(noms_fitxers, 1, 2)
  noms_classes = substr(noms_fitxers, 2, 2)
  escola_curs_classe = paste0(escola[2], "-", curs_classe)
  
  cursos = list()
  for (i in 1:length(noms_fitxers)){
    cursos[[i]] = c(escola_curs_classe[i], num_curs[i])
  }
  
  noms_cursos = vector(mode="list", length=6)
  noms_cursos = c("1r de Primària", "2n de Primària", "3r de Primària", 
                  "4rt de Primària", "5è de Primària", "6è de Primària")
  
  names(noms_cursos) = seq(1, 6)
  
  classes = c()
  for (i in 1:length(noms_fitxers)){
    classes[i] = paste(noms_cursos[num_curs[i]], noms_classes[i])
  }
  wd <- getwd();
  dir.create(paste(getwd(), "/figures/", escola[2], sep ="" ))
  dir.create(paste(getwd(), "/taules/", escola[2], sep ="" ))
  dir.create(paste(getwd(), "/informes/", escola[2], sep ="" ))
  print(curs_classe)
  
  for (cl in 1:length(curs_classe)){
    print(paste0("> Creant els informes per la classe ", classes[cl]))
    sink(file(paste(getwd(), "/informes/", escola[2],"/sociograma_", curs_classe[cl], ".tex", sep =""), 
              open = "wt", encoding = "latin1"));#-", #escola[2], sep = ""));
    
    cat(coses_latex);
    cat("\\begin{document}")
    titol_classes(escola, classes[cl])
    
    # Cridem l'informe col·lectiu:
    nom_fitxer = noms_fitxers[cl]
    
    informe_collectiu(escola[2], nom_fitxer)
    sink()
  }
  
  
  
}

informe_escola('dades/proves_excel_sociograma.xlsx', 'proves')
