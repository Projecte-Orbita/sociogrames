# Informe col·lectiu
# WIP
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts_collectiu.R', encoding = "UTF-8")
source('calculs_previs_collectiu.R', encoding = "UTF-8")

path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

informe_classe = function(path_llista, nom_fitxer){
  
  # Disrupció
  cat(disrupcio)
  
  peu_disrupcio = "Grau de de disrupció de cada alumne."
  afegeix_grafic(path_llista, "disrupcio", peu_disrupcio)
  cat(disrupcio_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "disrupcio")
  
  # Prosocialitat
  cat(prosocialitat)
  
  peu_prosocialitat = "Grau de de prosocialitat de cada alumne."
  afegeix_grafic(path_llista, "prosocialitat", peu_prosocialitat)
  cat(prosocialitat_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "prosocialitat")
  
  # Víctima
  cat(victimes)
  
  peu_victimes = "Grau de de victimització de cada alumne."
  afegeix_grafic(path_llista, "victimes", peu_victimes)
  cat(victimes_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "victimes")
  
  # Acadèmic
  cat(academic)
  
  peu_academic = "Grau de valoració acadèmica de cada alumne."
  afegeix_grafic(path_llista, "academic", peu_academic)
  cat(academic_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "academic")
  
  # Estat d'ànim
  cat(estat_anim)
  
  peu_estat_anim = "Grau d’estat d’ànim percebut de cada alumne."
  afegeix_grafic(path_llista, "estat_anim", peu_estat_anim)
  cat(estat_anim_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "estat_anim")
  
  # Caràcter
  cat(caracter)
  
  peu_caracter = "Actitud percebuda de cada alumne."
  afegeix_grafic(path_llista, "caracter", peu_caracter)
  cat(caracter_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "caracter")
  
  # Xarxa acadèmica
  
  cat(xarxa_academica)
  
  peu_xarxa_academica = "Relacions acadèmiques a l’aula."
  afegeix_grafic(path_llista, "xarxa_academica", peu_xarxa_academica)
  
  # Xarxa relacional
  
  cat(xarxa_relacional)
  
  peu_xarxa_relacional = "Relacions socials a l’aula."
  afegeix_grafic(path_llista, "xarxa_relacional", peu_xarxa_relacional)
  
}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  informe_collectiu(path_llista, nom_fitxer)
}