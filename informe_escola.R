# Informe escola

# Aquest és el fitxer principal, que conté la funció informe_escola, que crea els latex pels informes
# Parteix dels csv que estan a la carpeta temp/dades. Crearà un .tex per cada .csv a la carpeta
# TODO: implementar que pugui importar directament d'un excel.

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()  # Importem la configuració
encoding_ = config$encoding
individuals = config$individuals  # Per debuguejar més ràpid
aprofitar = config$aprofitar  # Ídem
numero_respostes = config$numero_respostes
options(endoding=encoding_)

wd = getwd()

source(file.path(wd, 'calculs_previs', 'calculs_collectiu.R'), encoding = encoding_)
source(file.path(wd, 'calculs_previs', 'calculs_individual.R'), encoding = encoding_)
source(file.path(wd, 'informe_collectiu.R'), encoding = encoding_)
source(file.path(wd, 'informe_individual.R'), encoding = encoding_)
source(file.path(wd, 'texts', 'texts_escola.R'), encoding = encoding_)
source(file.path(wd, 'texts', 'texts_collectiu.R'), encoding = encoding_)
source(file.path(wd, 'texts', 'texts_individual.R'), encoding = encoding_)
source(file.path(wd, 'altres', 'utils.R'), encoding = encoding_)
source(file.path(wd, 'altres', 'manipulacions_dades.R'), encoding = encoding_)

informe_escola = function(nom_escola){
  
  # Aquesta funció crea les figures de l'informe i els .tex, una carpeta de figures i un .tex
  # per cada classe
  # Arguments: nom_escola: el nom de l'escola tal com volem que surti a l'informe
  # Importacions: els csv que es trobin a la carpeta temp/dades
  # Retorna: res
  # Exporta: les imatges i els fitxers .tex dels informes.
  
  wd <- getwd();
  
  # Creem ara els paths que anirem fent servir:
  path_dades = file.path(wd, 'temp/dades')
  path_figures = file.path(wd, "temp/figures/")
  path_taules = file.path(wd, "temp/taules/")
  path_informes = file.path(wd, "temp/informes/")
  
  # Ajuntem tots els paths en un "diccionari" per tenir-los una mica ordenats i poder-hi accedir fàcilment.
  path_llista = list('dades' = path_dades, 
                     'figures' = path_figures, 
                     'taules' = path_taules, 
                     'informes' = path_informes)

  # Arreglem el nom de l'escola per fer-lo servir per coses internes i traiem accents i espais
  nom_escola_arreglat = make.names(nom_escola) 
  
  if (!aprofitar){
    # I creem els directoris sobreescrivint si ja existeixen:
    # FIXME: ja els hem netejat abans, no sé si cal una segona vegada
    
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
    
    # Importem les dades que farem servir tant pel col·lectiu com per l'individual:
    
    dades = importar_i_manipular(file.path(path_llista$dades, nom_fitxer), numero_respostes)
    
    # Informe col·lectiu
    
    if (!aprofitar){
      calculs_collectiu(dades, path_llista = path_llista, nom_fitxer = nom_fitxer, numero_respostes = 3)
    }
    
    path_ = file.path(path_llista$informes, paste0("sociograma_", curs_classe[cl], ".tex"))
    con = file(path_, open = "wt", encoding = encoding_)
    sink(con)
    
    cat(coses_latex);
    
    cat("\\begin{document}\n")
    
    titol_classes(nom_escola, classes[cl])
    
    # Intro
    cat(introduccio_escola)
    
    cat(titol_collectiu)
    #cat(introduccio)
    
    informe_classe(path_llista = path_llista)
    
    # Informes individuals (en el mateix fitxer)
    
    if (individuals){
      if (!aprofitar){
        noms = calculs_individual(dades, path_llista, nom_fitxer, numero_respostes=3)
      }
      else {
        noms = dades[[3]]
      }
      cat(titol_individual)
      informe_individual(path_llista = path_llista, noms)
    }
    
    cat(final_latex)
    sink()
    close(con)
    closeAllConnections()
  }
  
}

if (!interactive()) {  
  # equivalent a l'"if __name__==__main__ en python
  
  nom_escola = 'Hogwarts'
  informe_escola(nom_escola)
  
}

