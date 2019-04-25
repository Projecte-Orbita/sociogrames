# Informe col·lectiu
# WIP
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts_collectiu.R', encoding = "UTF-8")
source('calculs_previs_collectiu.R', encoding = "UTF-8")

path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

informe_classe = function(escola, nom_fitxer){
  
  # Disrupció
  cat(disrupcio)
  
  afegeix_grafic("disrupcio")
  
  cat(abans_taula)
  
  importar_i_imprimir_taula("disrupcio")
  
  # Prosocialitat
  cat(prosocialitat)
  
  afegeix_grafic("prosocialitat")
  
  cat(abans_taula)
  
  importar_i_imprimir_taula("prosocialitat")
  
  # Víctima
  cat(victimes)
  
  afegeix_grafic("victimes")
  
  cat(abans_taula)
  
  importar_i_imprimir_taula("victimes")
  
  # Acadèmic
  cat(academic)
  
  afegeix_grafic( "academic")
  
  cat(abans_taula)
  
  importar_i_imprimir_taula("academic")
  
  # Estat d'ànim
  cat(estat_anim)
  
  afegeix_grafic( "estat_anim")
  
  cat(abans_taula)
  
  importar_i_imprimir_taula("estat_anim")
  
  # Caràcter
  cat(caracter)
  
  afegeix_grafic("caracter")
  
  cat(abans_taula)
  
  importar_i_imprimir_taula("caracter")
  
  # Xarxa acadèmica
  
  cat(xarxa_academica)
  
  afegeix_grafic("xarxa_academica")
  
  # Xarxa relacional
  
  cat(xarxa_relacional)
  
  afegeix_grafic("xarxa_relacional")
  
}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  informe_collectiu(escola, nom_fitxer)
}