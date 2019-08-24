# Informe col·lectiu

# Aquest fitxer conté la funció informe_classe, que agafa els resultats dels càlculs previs col·lectius
# i escriu la part col·lectiva del latex de la classe. Per tant, importa figures prèviament creades
# i text the texts_collectius.R i treu una part de text que anirà en un fitxer .tex.
# La funció és cridada per la funció informe_escola a informe_escola.R

#### Alerta: No funciona tot sol, només com a helper a l'informe escoles ####

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")  # FIXME: Només per Windows; per Mac treu un warning

config = config::get()
encoding_ = config$encoding

wd = getwd()
source(file.path(wd, 'texts', 'texts_collectiu.R'), encoding = encoding_)


informe_classe = function(path_llista){
  
  # Funció que escriu la part col·lectiva del latex de la classe
  # Arguments: llista de paths per saber on ha de trobar les imatges
  # Importacions: gràfics col·lectius
  # Retorna: Res; només escriu latex en un fitxer ja obert per un sink previ
               
  # Escrivim les escales:
  
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
  
  
  cat("\\section*{Xarxes}")
  
  # gràfic 2D relacional
  
  cat("\\section*{Mapa social relacional}")
  
  peu_2D_relacional = "Mapa de posicionament social del grup-classe."
  afegeix_grafic(path_llista, "disrupcio_2D", peu_2D_relacional)
  
  cat(mapa_relacional1)
  cat(mapa_relacional2)
  
  # Xarxa relacional
  
  #cat(xarxa_relacional)
  cat("\\subsection*{Xarxa d'estatus social}")
  
  peu_xarxa_relacional = "Xarxa de posicionament social del grup-classe."
  afegeix_grafic(path_llista, "xarxa_relacional", peu_xarxa_relacional)
  
  cat(xarxa_relacional)
  cat(xarxa_relacional2)

  # gràfic 2D acadèmic
  

  cat("\\section*{Mapa social acadèmic}")
  
  peu_2D_academic = "Mapa de posicionament acadèmic del grup-classe."
  afegeix_grafic(path_llista, "academic_2D", peu_2D_academic)
  
  cat(mapa_academic1)
  cat(mapa_academic2)
  
  # Xarxa acadèmica

  cat("\\section*{Xarxa social acadèmica}")
  peu_xarxa_academica = "Xarxa de posicionament acadèmic del grup-classe."
  afegeix_grafic(path_llista, "xarxa_academica", peu_xarxa_academica)
  
  cat(xarxa_academica1)
  cat(xarxa_academica2)
  
  # Xarxa amical -> per ara no la posem
  
  # cat("\\section*{Xarxa d'amics}")
  
  # peu_xarxa_amical = "Relacions d'amistat a l’aula."
  # afegeix_grafic(path_llista, "xarxa_amical", peu_xarxa_amical)

}
