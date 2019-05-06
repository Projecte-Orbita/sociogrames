# Informes individuals

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
options(encoding = "UTF-8")

source('texts_individual.R', encoding = "UTF-8")
source('calculs_previs_individual.R', encoding = "UTF-8")


informe_individual = function(path_llista, nom_fitxer, noms){
  
  rels = read.csv(file.path(path_llista$taules, "relacions.csv"))
  
  for (i in 1:length(noms)){
    
    nom = noms[i]
    
    titol_alumne(nom)
    # cat(introduccio)
    
    cat("\\subsection*{Preferències relacionals}")
    afegeix_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "academic")
    afegeix_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "relacional")
    afegeix_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "amical")
    
    # Disrupció
    cat(disrupcio_ind)
    afegeix_grafic_individual(path_llista, "disrupcio", i)
    
    # Víctima
    cat(victimes_ind)
    afegeix_grafic_individual(path_llista, "victimes", i)
    
    # Acadèmic
    cat(academic_ind)
    afegeix_grafic_individual(path_llista, "academic", i)
    
    # Estat d'ànim
    cat(estat_anim_ind)
    afegeix_grafic_individual(path_llista, "estat_anim", i)
    
    # Caràcter
    cat(caracter_ind)
    afegeix_grafic_individual(path_llista, "caracter", i)
    
    # Estatus sociomètric
    cat(estatus_sociometric_ind)
    afegeix_grafic_individual(path_llista, "estatus", i)
    
    # Resum
    cat(resum)
    afegeix_grafic_resum(path_llista, i)
    
  }
}

if (!interactive()) {  # equivalent a l'"if __name__==__main__ en R
  informe_individual(path_llista, nom_fitxer, noms)
}