# Informes individuals

# Aquest fitxer conté la funció informe_individual, que agafa els resultats dels càlculs previs 
# individuals i escriu la part col·lectiva del latex de la classe. Per tant, importa figures 
# prèviament creades i text the texts_collectius.R i treu una part de text que anirà en un fitxer .tex.
# La funció és cridada per la funció informe_escola a informe_escola.R

#### Alerta: No funciona tot sol, només com a helper a l'informe escoles ####

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

wd = getwd()
source(file.path(wd, 'texts', 'texts_individual.R'), encoding = encoding_)


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
  
  valoracions_disrupcio = fromJSON(readLines(file.path(path_llista$taules, "vals_disrupcio.json")))
  valoracions_victimes = fromJSON(readLines(file.path(path_llista$taules, "vals_victimes.json")))
  #valoracions_academic = fromJSON(readLines(file.path(path_llista$taules, "vals_academic.json")))
  valoracions_ea = fromJSON(readLines(file.path(path_llista$taules, "vals_ea.json")))
  valoracions_caracter = fromJSON(readLines(file.path(path_llista$taules, "vals_caracter.json")))
  #valoracions_estatus = fromJSON(readLines(file.path(path_llista$taules, "vals_estatus.json")))
  valoracions_mapa_social = fromJSON(readLines(file.path(path_llista$taules, "vals_mapa_social.json")))
  valoracions_mapa_academic = fromJSON(readLines(file.path(path_llista$taules, "vals_mapa_academic.json")))
  
  # Importem els fitxers d'orientacions també
  
  orientacions_disrupcio = fromJSON(readLines(file.path(path_llista$taules, "orientacions_disrupcio.json")))
  orientacions_victimes = fromJSON(readLines(file.path(path_llista$taules, "orientacions_victimes.json")))
  #orientacions_academic = fromJSON(readLines(file.path(path_llista$taules, "orientacions_academic.json")))
  orientacions_ea = fromJSON(readLines(file.path(path_llista$taules, "orientacions_ea.json")))
  orientacions_caracter = fromJSON(readLines(file.path(path_llista$taules, "orientacions_caracter.json")))
  #orientacions_estatus = fromJSON(readLines(file.path(path_llista$taules, "orientacions_estatus.json")))
  orientacions_mapa_social = fromJSON(readLines(file.path(path_llista$taules, "orientacions_mapa_social.json")))
  orientacions_mapa_academic = fromJSON(readLines(file.path(path_llista$taules, "orientacions_mapa_academic.json")))
  
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
    
    if (!is.na(valoracions_disrupcio[nom])){
      cat("\\textbf{Àmbit de comportament}")
      cat("\\begin{itemize}")
      for (element in valoracions_disrupcio[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(valoracions_victimes[nom])){
      cat("\\textbf{Àmbit de victimització}")
      cat("\\begin{itemize}")
      for (element in valoracions_victimes[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    # if (!is.na(orientacions_academic[nom])){
    #   cat("\\textbf{Àmbit acadèmic}")
    #   cat("\\begin{itemize}")
    #   for (element in orientacions_academic[nom])
    #     cat(unlist(element))
    #   cat("\\end{itemize}")
    # }
    # else {
    #   valoracions = valoracions - 1
    # }
    if (!is.na(valoracions_ea[nom])){
      cat("\\textbf{Àmbit d'estat ànimic}")
      cat("\\begin{itemize}")
      for (element in valoracions_ea[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
      
    }
    else {
      valoracions = valoracions - 1
    }
    if (!is.na(valoracions_caracter[nom])){
      cat("\\textbf{Escala de caràcter}")
      cat("\\begin{itemize}")
      for (element in valoracions_caracter[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    # if (!is.na(orientacions_estatus[nom])){
    #   cat("\\textbf{Xarxa d'estatus social}")
    #   cat("\\begin{itemize}")
    #   for (element in orientacions_estatus[nom])
    #     cat(unlist(element))
    #   cat("\\end{itemize}")
    # }
    # else {
    #   valoracions = valoracions - 1
    # }
    
    if (!is.na(valoracions_mapa_social[nom])){
      cat("\\textbf{Xarxa social relacional}")
      cat("\\begin{itemize}")
      for (element in valoracions_mapa_social[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      valoracions = valoracions - 1
    }
    
    if (!is.na(valoracions_mapa_academic[nom])){
      cat("\\textbf{Xarxa social acadèmica}")
      cat("\\begin{itemize}")
      for (element in valoracions_mapa_academic[nom])
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
    
    nom = noms[i]
    
    orientacions = 6
    
    if (!is.na(orientacions_disrupcio[nom])){
      cat("\\textbf{Àmbit de comportament}")
      cat("\\begin{itemize}")
      for (element in orientacions_disrupcio[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      orientacions = orientacions - 1
    }
    if (!is.na(orientacions_victimes[nom])){
      cat("\\textbf{Àmbit de victimització}")
      cat("\\begin{itemize}")
      for (element in orientacions_victimes[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      orientacions = orientacions - 1
    }
    # if (!is.na(orientacions_academic[nom])){
    #   cat("\\textbf{Àmbit acadèmic}")
    #   cat("\\begin{itemize}")
    #   for (element in orientacions_academic[nom])
    #     cat(unlist(element))
    #   cat("\\end{itemize}")
    # }
    # else {
    #   orientacions = orientacions - 1
    # }
    if (!is.na(orientacions_ea[nom])){
      cat("\\textbf{Àmbit d'estat ànimic}")
      cat("\\begin{itemize}")
      for (element in orientacions_ea[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
      
    }
    else {
      orientacions = orientacions - 1
    }
    if (!is.na(orientacions_caracter[nom])){
      cat("\\textbf{Escala de caràcter}")
      cat("\\begin{itemize}")
      for (element in orientacions_caracter[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      orientacions = orientacions - 1
    }
    # if (!is.na(orientacions_estatus[nom])){
    #   cat("\\textbf{Xarxa d'estatus social}")
    #   cat("\\begin{itemize}")
    #   for (element in orientacions_estatus[nom])
    #     cat(unlist(element))
    #   cat("\\end{itemize}")
    # }
    # else {
    #   orientacions = orientacions - 1
    # }
    
    if (!is.na(orientacions_mapa_social[nom])){
      cat("\\textbf{Xarxa social relacional}")
      cat("\\begin{itemize}")
      for (element in orientacions_mapa_social[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    else {
      orientacions = orientacions - 1
    }
    
    if (!is.na(orientacions_mapa_academic[nom])){
      cat("\\textbf{Xarxa social acadèmica}")
      cat("\\begin{itemize}")
      for (element in orientacions_mapa_academic[nom])
        cat(unlist(element))
      cat("\\end{itemize}")
    }
    
    # Orientacions globals
    
    if (orientacions == 0){
      cat(paste0("En/na ", nom, " té tots els resultats dins dels valors considerats normals i per tant
                 no hi ha àrees a destacar."))
    }
    
    bones = 0
    bones2 = 0
    arees_bones = c()
    
    
    if ("A_bona" %in% names(valoracions_disrupcio[nom][[1]])) {
      bones = bones + 1
      arees_bones = c(arees_bones, "prosocialitat")
    }
    
    if ("A_bona" %in% names(valoracions_ea[nom][[1]])) {
      bones = bones + 1
      arees_bones = c(arees_bones, "estat d'ànim")
    }
    
    if ("lider" %in% names(valoracions_caracter[nom][[1]])) {
      bones = bones + 1
      arees_bones = c(arees_bones, "lideratge")
    } 
    if ("autonom" %in% names(valoracions_caracter[nom][[1]])) {
      bones = bones + 1
      arees_bones = c(arees_bones, "autonomia") 
        }
     if ("sociable" %in% names(valoracions_caracter[nom][[1]])) {
       bones = bones + 1
       arees_bones = c(arees_bones, "sociabilitat")
    }
    if ("popular" %in% names(valoracions_mapa_social[nom][[1]])) {
      bones2 = bones2 + 1
    }
    if ("popular" %in% names(valoracions_mapa_academic[nom][[1]])) {
      bones2 = bones2 + 1
    }
    # if (exists(valoracions_mapa_social[nom]["popular"])) {
    #   bones = bones + 1
    # }
    # 
    # if (exists(valoracions_mapa_academic[nom]["popular"])) {
    #   bones = bones + 1
    # }
    
    dolentes = 0
    dolentes2 = 0
    arees_dolentes = c()
    if ("B_dolenta" %in% names(valoracions_disrupcio[nom][[1]])) {
      dolentes = dolentes + 1
      arees_dolentes = c(arees_dolentes, "disrupció")
    }
    
    # if (exists(valoracions_victimes[nom]["B_dolenta"])) {
    #   dolentes = dolentes + 1
    #   arees_dolentes = c(arees_dolentes, "victimització")
    # }
    
    if ("B_dolenta" %in% names(valoracions_ea[nom][[1]])) {
      dolentes = dolentes + 1
      arees_dolentes = c(arees_dolentes, "estat d'ànim")
    }
    
    if ("seguidor" %in% names(valoracions_caracter[nom][[1]])) {
      dolentes = dolentes + 1
      arees_dolentes = c(arees_dolentes, "seguidor")
    } 
    if ("dependent" %in% names(valoracions_caracter[nom][[1]])) {
      dolentes = dolentes + 1
      arees_dolentes = c(arees_dolentes, "dependència") 
    }
    if ("aillat" %in% names(valoracions_caracter[nom][[1]])) {
      dolentes = dolentes + 1
      arees_dolentes = c(arees_dolentes, "aïllament")
    }
    
    if ("rebutjat" %in% names(valoracions_mapa_social[nom][[1]])) {
      dolentes2 = dolentes2 + 1
    }
    if ("negligit" %in% names(valoracions_mapa_social[nom][[1]])) {
      dolentes2 = dolentes2 + 1
    }
    if ("controvers" %in% names(valoracions_mapa_social[nom][[1]])) {
      dolentes2 = dolentes2 + 1
    }
    if ("rebutjat" %in% names(valoracions_mapa_academic[nom][[1]])) {
      dolentes2 = dolentes2 + 1
    }
    if ("negligit" %in% names(valoracions_mapa_academic[nom][[1]])) {
      dolentes2 = dolentes2 + 1
    }
    if ("controvers" %in% names(valoracions_mapa_academic[nom][[1]])) {
      dolentes2 = dolentes2 + 1
    }
    
    arees_bones_text = paste(arees_bones, collapse = ", ")
    arees_dolentes_text = paste(arees_dolentes, collapse = ", ")
    
    if (bones > 0 & dolentes == 0){
      cat(paste0("Segons les respostes dels seus companys, el/la ", nom, " mostra trets relacionals i/o acadèmics que afavoreixen la seva adaptació. En concret, valoren com a significativa la seva habilitat en ", arees_bones_text, ". Recomanem doncs tenir en compte aquestes aptituds, facilitant-li oportunitats per desenvolupar-les i utilitzar-les per generar un impacte positiu en el grup."))
    }
    
    if (bones > 0 & dolentes > 0){
      cat(paste0("Segons les respostes dels seus companys, el/la ", nom, " mostra trets relacionals i/o acadèmics que afavoreixen la seva adaptació. En concret, valoren com a significativa la seva habilitat en  ", arees_bones_text, ". Recomanem doncs tenir en compte aquestes aptituds, facilitant-li oportunitats per desenvolupar-les i utilitzar-les per generar un impacte positiu en el grup. 
El fet que el/la ", nom, " mostri aquests trets positius és un factor de protecció sobre la seva autoestima i també li suposa una font de recursos que facilita compensar els aspectes desadaptatius que també s'han constatat."))
    }
    
    if (bones == 0 & dolentes > 0){
      cat(paste0("Segons les respostes dels seus companys, el/la ", nom, " mostra trets relacionals i/o acadèmics que dificulten la seva adaptació. En concret, valoren com a significativament baixa la seva habilitat en ", arees_bones_text, ". Recomanem doncs tenir en compte aquestes limitacions, facilitant-li oportunitats per compensar-les i evitar que li interferixin negativament en el seu dia a dia."))
    }
    if (bones == 0 &  bones2 == 0 & dolentes == 0 & dolentes2 == 0){
    cat(paste0("No hi ha orientacions pel/per la ", nom, "."))
    }
      
      }
}
