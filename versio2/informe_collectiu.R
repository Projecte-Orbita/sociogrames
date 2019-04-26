# Informe col·lectiu
# WIP
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts_collectiu.R', encoding = "UTF-8")
source('calculs_previs_collectiu.R', encoding = "UTF-8")

path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

informe_classe = function(path_llista, nom_fitxer){
  
  # Disrupció
  cat(disrupcio)
  
  peu_disrupcio = "Grau de de disrupció (física, verbal i relacional) de cada alumne."
  afegeix_grafic(path_llista, "disrupcio", peu_disrupcio)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "disrupcio")
  
  # Prosocialitat
  cat(prosocialitat)
  
  peu_prosocialitat = "Grau de de prosocialitat de cada alumne."
  afegeix_grafic(path_llista, "prosocialitat", peu_prosocialitat)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "prosocialitat")
  
  # Víctima
  cat(victimes)
  
  peu_victimes = "Grau de de victimització (física, verbal i relacional) de cada alumne."
  afegeix_grafic(path_llista, "victimes", peu_victimes)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "victimes")
  
  # Acadèmic
  cat(academic)
  
  peu_academic = "Grau de valoració acadèmica (rendiment acadèmic i participació) de cada alumne."
  afegeix_grafic(path_llista, "academic", peu_academic)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "academic")
  
  # Estat d'ànim
  cat(estat_anim)
  
  peu_estat_anim = "Grau d’estat d’ànim percebut (insatisfacció, enuig, alegria i tristor) de cada alumne."
  afegeix_grafic(path_llista, "estat_anim", peu_estat_anim)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "estat_anim")
  
  # Caràcter
  cat(caracter)
  
  peu_caracter = "Actitud percebuda (lideratge, autonomia i socialització) de cada alumne."
  afegeix_grafic(path_llista, "caracter", peu_caracter)
  
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