# Informes individuals


#### Alerta: No funciona tot sol, només com a helper a l'informe escoles ####


Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
options(encoding = "UTF-8")

source('texts/texts_individual.R', encoding = "UTF-8")
source('calculs_previs/calculs_previs_individual.R', encoding = "UTF-8")


informe_individual = function(path_llista, nom_fitxer, noms){
  
  rels = read.csv(file.path(path_llista$taules, "relacions.csv"))
  
  for (i in 1:length(noms)){
    
    nom = noms[i]
    
    titol_alumne(nom)
    # cat(introduccio)
    
    #cat("\\subsection*{Escales de relació}")
    
    # Disrupció
    #cat(disrupcio_ind)
    afegeix_grafic_individual(path_llista, "disrupcio", i)
    cat("\\vspace{-2cm}")
    
    # Víctima
    #cat(victimes_ind)
    afegeix_grafic_individual(path_llista, "victimes", i)
    cat("\\vspace{-2cm}")
    
    # Acadèmic
    #cat(academic_ind)
    afegeix_grafic_individual(path_llista, "academic", i)
    cat("\\vspace{-2cm}")
    
    # Estat d'ànim
    #cat(estat_anim_ind)
    afegeix_grafic_individual(path_llista, "estat_anim", i)
    cat("\\vspace{-2cm}")
    
    # Caràcter
    #cat(caracter_ind)
    afegeix_grafic_individual(path_llista, "caracter", i)
    cat("\\vspace{-2cm}")
    
    # Estatus sociomètric
    #cat(estatus_sociometric_ind)
    afegeix_grafic_individual(path_llista, "estatus", i)
    cat("\\newpage")
    
    # Resum
    #cat(resum)
    #afegeix_grafic_resum(path_llista, i)
    
    cat("\\subsection*{Preferències relacionals}")
    escriure_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "academic")
    escriure_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "relacional")
    escriure_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "amical")
  }
}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  informe_individual(path_llista, nom_fitxer, noms)
}