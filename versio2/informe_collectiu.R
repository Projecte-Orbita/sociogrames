# Informe col·lectiu


#### Alerta: No funciona tot sol, només com a helper a l'informe escoles ####


Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

gwd = getwd()
source(file.path(gwd, 'texts', 'texts_collectiu.R'), encoding = encoding_)
source(file.path(gwd, 'calculs_previs', 'calculs_previs_collectiu.R'), encoding = encoding_)


informe_classe = function(path_llista, nom_fitxer){
  
  # Comportament
  cat(disrupcio)
  
  peu_disrupcio = "Comportament percebut de cada alumne."
  afegeix_grafic(path_llista, "disrupcio", peu_disrupcio)
  cat(disrupcio_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "disrupcio")
  
  # Víctima
  cat(victimes)
  
  peu_victimes = "Grau de de victimització de cada alumne."
  afegeix_grafic(path_llista, "victimes", peu_victimes)
  cat(victimes_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "victimes")
  
  # Acadèmic
  aca = FALSE
  if (aca){
  
  # Està així perquè la info acadèmica ja és a la xarxa i per tant per no duplicar-la, però encara no és segur, així
  # que ho deixo aquí així.
    
  cat(academic)
  
  peu_academic = "Grau de valoració acadèmica de cada alumne."
  afegeix_grafic(path_llista, "academic", peu_academic)
  cat(academic_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "academic")
  }
  
  
  # Estat d'ànim
  cat(estat_anim)
  
  peu_estat_anim = "Grau d’estat d’ànim percebut de cada alumne."
  afegeix_grafic(path_llista, "estat_anim", peu_estat_anim)
  cat(estat_anim_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "estat_anim")
  
  # Caràcter
  cat(caracter)
  
  peu_caracter = "Caràcter percebut de cada alumne."
  afegeix_grafic(path_llista, "caracter", peu_caracter)
  cat(caracter_post)
  
  cat(abans_taula)
  
  importar_i_imprimir_taula(path_llista, "caracter")
  
  # gràfic 2D relacional
  
  cat("\\section*{Mapa de comportament}")
  
  peu_2D_relacional = "Mapa de posicionament social percebut a la classe"
  afegeix_grafic(path_llista, "disrupcio_2D", peu_2D_relacional)
  
  # Xarxa relacional
  
  #cat(xarxa_relacional)
  cat("\\section*{Xarxa social relacional}")
  
  peu_xarxa_relacional = "Relacions socials a l’aula."
  afegeix_grafic(path_llista, "xarxa_relacional", peu_xarxa_relacional)

  # gràfic 2D acadèmic
  
  cat("\\section*{Mapa de situació acadèmica}")
  
  peu_2D_academic = "Mapa de posicionament acadèmic percebut a la classe"
  afegeix_grafic(path_llista, "academic_2D", peu_2D_academic)
  
  # Xarxa acadèmica
  
  #cat(xarxa_academica)
  cat("\\section*{Xarxa relacional acadèmica}")
  peu_xarxa_academica = "Relacions acadèmiques a l’aula."
  afegeix_grafic(path_llista, "xarxa_academica", peu_xarxa_academica)
  
  # Xarxa amical -> per ara no la posem
  
  # cat("\\section*{Xarxa d'amics}")
  
  # peu_xarxa_amical = "Relacions d'amistat a l’aula."
  # afegeix_grafic(path_llista, "xarxa_amical", peu_xarxa_amical)
  

}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  informe_collectiu(path_llista, nom_fitxer)
}