# Informe escola
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
options(endoding="UTF-8")
source('informe_collectiu.R', encoding = "UTF-8")
source('informe_individual.R', encoding = "UTF-8")
source('texts_escola.R', encoding = "UTF-8")
source('texts_collectiu.R', encoding = "UTF-8")
source('texts_individual.R', encoding = "UTF-8")
source('utils.R', encoding = "UTF-8")
# Aquest fitxer crea els informes per tota l'escola, primer el col·leciu i després els individuals, un fitxer .tex per cada classe

# Coses per debuguejar més ràpid:
individuals = F
aprofitar = F

informe_escola = function(nom_escola){

  nom_escola_arreglat = make.names(nom_escola)  # és el que farem servir per coses internes i traiem accents i espais
  
  wd <- getwd();
  
  # Creem ara els paths que anirem fent servir:
  path_dades = file.path(getwd(), 'temp/dades')
  path_figures = file.path(getwd(), "temp/figures/")
  path_taules = file.path(getwd(), "temp/taules/")
  path_informes = file.path(getwd(), "temp/informes/")
  
  # Ajuntem tots els paths en un "diccionari" per tenir-los una mica ordenats i poder-hi accedir fàcilment.
  path_llista = list('dades' = path_dades, 
                     'figures' = path_figures, 
                     'taules' = path_taules, 
                     'informes' = path_informes)
  
  
  if (!aprofitar){
    # I creem els directoris sobreescrivint si ja existeixen:
    directoris_ = c(path_figures, path_taules, path_informes)  # Alerta de no netejar el de dades!
    netejar_directoris(c(directoris_, file.path(directoris_, "individuals")))
  }
  
  noms_fitxers = as.vector(list.files(path_llista$dades))
  num_curs = substr(noms_fitxers, 1, 1)
  curs_classe = substr(noms_fitxers, 1, 2)
  noms_classes = substr(noms_fitxers, 2, 2)
  escola_curs_classe = paste0(nom_escola_arreglat, "-", curs_classe)
  
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
  
  for (cl in 1:length(curs_classe)){
    
    print(paste0("> Creant els informes per la classe ", classes[cl]))
    print("> Creant gràfics i taules...")
    
    nom_fitxer = noms_fitxers[cl]
    
    # Informe col·lectiu
    
    if (!aprofitar){
      calculs_collectiu(path_llista = path_llista, nom_fitxer = nom_fitxer, numero_respostes = 3)
    }
    path_ = file.path(path_llista$informes, paste0("sociograma_", curs_classe[cl], ".tex"))
    con = file(path_, open = "wt", encoding = "UTF-8")
    sink(con)
    
    cat(coses_latex);
    
    cat("\\begin{document}\n")
    
    titol_classes(nom_escola, classes[cl])
    
    # Intro
    cat(introduccio_escola)
    
    cat(titol_collectiu)
    #cat(introduccio)
    
    informe_classe(path_llista = path_llista, nom_fitxer = nom_fitxer)
    
    # Informes individuals (en el mateix fitxer)
    
    if (individuals){
      if (!aprofitar){
        noms = calculs_individual(path_llista, nom_fitxer, numero_respostes=3)
      }
      else {
        dades = importar_i_manipular(file.path(path_llista$dades, nom_fitxer), 3)
        noms = dades[[3]]
      }
      cat(titol_individual)
      informe_individual(path_llista = path_llista, nom_fitxer = nom_fitxer, noms)
    }
    
    cat(final_latex)
    sink()
    close(con)
    closeAllConnections()
  }
  
}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  
  nom_escola = 'Escola Súper Important'
  informe_escola(nom_escola)
}

# informe_escola('dades/proves_excel_sociograma.xlsx', 'proves')
