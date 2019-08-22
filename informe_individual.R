# Informes individuals

# Aquest fitxer conté la funció informe_individual, que agafa els resultats dels càlculs previs 
# individuals i escriu la part col·lectiva del latex de la classe. Per tant, importa figures 
# prèviament creades i text the texts_collectius.R i treu una part de text que anirà en un fitxer .tex.
# La funció és cridada per la funció informe_escola a informe_escola.R

#### Alerta: No funciona tot sol, només com a helper a l'informe escoles ####

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
options(encoding = "UTF-8")

source('texts/texts_individual.R', encoding = "UTF-8")
source('calculs_previs/calculs_individual.R', encoding = "UTF-8")


informe_individual = function(path_llista, noms){
  
  # Funció que escriu la part individual del latex de la classe
  # Arguments: llista de paths per saber on ha de trobar les imatges i llista de noms dels nens.
  # Importacions: fitxer de relacions: relacions.cvs
  #               fitxers de valoracions de disrupció
  #               gràfics individuals
  # Retorna: Res; només escriu latex en un fitxer ja obert per un sink previ
  
  # Escrivim les escales:
  
  rels = read.csv(file.path(path_llista$taules, "relacions.csv"))
  
  # Importem els fitxers de valoracions, per només haver-ho de fer una vegada
  
  vals_disrupcio = fromJSON(readLines(file.path(path_llista$taules, "vals_disrupcio.json")))
  vals_victimes = fromJSON(readLines(file.path(path_llista$taules, "vals_victimes.json")))
  vals_academic = fromJSON(readLines(file.path(path_llista$taules, "vals_academic.json")))
  vals_ea = fromJSON(readLines(file.path(path_llista$taules, "vals_ea.json")))
  vals_caracter = fromJSON(readLines(file.path(path_llista$taules, "vals_caracter.json")))
  vals_estatus = fromJSON(readLines(file.path(path_llista$taules, "vals_estatus.json")))
  
  for (i in 1:length(noms)){
    
    nom = noms[i]
    
    titol_alumne(nom)
    # cat(introduccio)
    
    cat("\\subsection*{Resultats de les escales}")
    
    # Disrupció
    #cat(disrupcio_ind)
    afegeix_grafic_individual(path_llista, "disrupcio", i)
    cat("\\vspace{-2cm}")
    
    # Víctima
    #cat(victimes_ind)
    afegeix_grafic_individual(path_llista, "victimes", i)
    cat("\\vspace{-2cm}")
    
    # Acadèmic
    # 
    #!!!!!!!!!!!!! Per ara no ho posem !!!!!!!!!!!!!!!!!
    # 
    #cat(academic_ind)
    #afegeix_grafic_individual(path_llista, "academic", i)
    #cat("\\vspace{-2cm}")
    
    # Estat d'ànim
    #cat(estat_anim_ind)
    afegeix_grafic_individual(path_llista, "estat_anim", i)
    cat("\\vspace{-2cm}")
    
    # Caràcter
    #cat(caracter_ind)
    afegeix_grafic_individual(path_llista, "caracter", i)
    cat("\\vspace{-2cm}")
    
    # Estatus sociomètric
    #     # 
    #!!!!!!!!!!!!! Per ara no ho posem !!!!!!!!!!!!!!!!!
    # 
    #cat(estatus_sociometric_ind)
    #afegeix_grafic_individual(path_llista, "estatus", i)
    #cat("\\newpage")
    
    # Resum
    #cat(resum)
    #afegeix_grafic_resum(path_llista, i)
    
    cat("\\subsection*{Resultats de les xarxes}")
    escriure_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "academic")
    escriure_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "relacional")
    escriure_preferencies(rels=rels, noms=noms, i=i, numero_respostes = 3, tipus = "amical")
    
    # Valoracions 
    
    # cat("\\newpage")
    cat("\\subsection*{Interpretació dels resultats}")
    
    nom = noms[i]
    
    valoracions = 6
    
    if (!is.na(vals_disrupcio[nom])){
      cat("\\textbf{Àmbit de comportament}")
      cat("\\begin{itemize}")
      for (element in vals_disrupcio[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(vals_victimes[nom])){
      cat("\\textbf{Àmbit de victimització}")
      cat("\\begin{itemize}")
      for (element in vals_victimes[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(vals_academic[nom])){
      cat("\\textbf{Àmbit acadèmic}")
      cat("\\begin{itemize}")
      for (element in vals_academic[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(vals_ea[nom])){
      cat("\\textbf{Àmbit d'estat ànimic}")
      cat("\\begin{itemize}")
      for (element in vals_ea[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
      
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(vals_caracter[nom])){
      cat("\\textbf{Escala de caràcter}")
      cat("\\begin{itemize}")
      for (element in vals_caracter[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(vals_estatus[nom])){
      cat("\\textbf{Xarxa d'estatus social}")
      cat("\\begin{itemize}")
      for (element in vals_estatus[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    
    if (valoracions == 0){
      cat(paste0("En/na ", nom, " té tots els resultats dins dels valors considerats normals i per tant
                 no hi ha àrees a destacar."))
    }
    
    cat("\\subsection*{Orientacions}")
    cat("FALTA")

  }
}
