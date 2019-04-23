# Informe col·lectiu
# WIP
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts_collectiu.R', encoding = "UTF-8")
source('calculs_previs_collectiu.R', encoding = "UTF-8")

path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

informe_collectiu = function(escola, nom_fitxer){
  
  #print("> Creant gràfics i taules...")
  
  calculs_collectiu(escola, nom_fitxer)
  
  #print("> Imprimint latex...")
  
  #con <- file(paste0("informes/", escola, "/", nom_fitxer, ".tex"), open = "wt", encoding = "UTF-8")
  #sink(con)
  
  cat(coses_latex)
  pagina_titol(escola[1])
  
  
  cat(introduccio)
  
  # Disrupció
  cat(disrupcio)
  
  afegeix_grafic(paste0(escola, "/disrupcio"))
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(paste0("taules/", escola, "/disrupcio.txt"))
  
  # Prosocialitat
  cat(prosocialitat)
  
  afegeix_grafic(paste0(escola, "/prosocialitat"))
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(paste0("taules/", escola, "/prosocialitat.txt"))
  
  # Víctima
  cat(victimes)
  
  afegeix_grafic(paste0(escola, "/victimes"))
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(paste0("taules/", escola, "/victimes.txt"))
  
  # Acadèmic
  cat(academic)
  
  afegeix_grafic(paste0(escola, "/academic"))
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(paste0("taules/", escola, "/academic.txt"))
  
  # Estat d'ànim
  cat(estat_anim)
  
  afegeix_grafic(paste0(escola, "/estat_anim"))
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(paste0("taules/", escola, "/estat_anim.txt"))
  
  # Caràcter
  cat(caracter)
  
  afegeix_grafic(paste0(escola, "/caracter"))
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(paste0("taules/", escola, "/caracter.txt"))
  
  # Xarxa acadèmica
  
  cat(xarxa_academica)
  
  afegeix_grafic(paste0(escola, "/xarxa_academica"))
  
  # Xarxa relacional
  
  cat(xarxa_relacional)
  
  afegeix_grafic(paste0(escola, "/xarxa_relacional"))
  
  # Final
  
  cat(final_latex)
  
  #sink()
  #close(con)
  
  #print("> Finalitzat correctament.")
}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  informe_collectiu(escola, nom_fitxer)
}