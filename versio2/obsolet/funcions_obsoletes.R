# Obsolet


crear_dict_preferencies = function(soc, numero_respostes, path_){
  
  # Obsolet, vegeu "calcular_preferencies"
  
  prefs_dict = list()
  
  # A qui escull
  for (i in 1:round(nrow(soc)/numero_respostes)){
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]] = list("academic"=NULL, "relacional"=NULL)
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["academic"]] = preferencies(soc, i, "academic", 3)
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["relacional"]] = preferencies(soc, i, "relacional", 3)
  }
  
  # Per qui és escollit
  
  
  
  prefs_json = toJSON(prefs_dict, pretty = TRUE, auto_unbox = TRUE)
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0("preferencies.txt")), open = "wt", encoding = "UTF-8")
  sink(con)
  print(prefs_json)
  sink()
  close(con)
}

preferencies = function(soc, nen, area, numero_respostes){
  
  # Obsolet, vegeu "calcular_preferencies"
  
  if (area == "academic"){
    area_col = 4
  }
  else {
    area_col = 6
  }
  prefs_positives = c()
  prefs_negatives = c()
  for (i in 1:numero_respostes){
    prefs_positives = c(prefs_positives, soc$noms[round(numero_respostes * soc[nen + i - 1, area_col])])
    prefs_negatives = c(prefs_negatives, soc$noms[round(numero_respostes * soc[nen + i - 1, area_col + 1])])
  }
  return(list("tries_positives" = prefs_positives, "tries_negatives" = prefs_negatives))
}

preferencies_inverses = function(soc, numero_respostes){
  
  # Obsolet, vegeu "calcular_preferencies"
  
  prefs_dict = list()
  for (i in 1:round(nrow(soc)/numero_respostes)){
    list.append(as.character(soc$noms[1+numero_respostes*(i-1)]))
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["academic"]] = list("triat_positiu" = NULL, "triat_negatiu" = NULL)
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["relacional"]] = list("triat_positiu" = NULL, "triat_negatiu" = NULL)
  }
  
  for (i in soc[, 4]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_positiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_positiu"]],as.character(soc$noms[i]))
  }
  for (i in soc[, 5]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_negatiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_negatiu"]],as.character(soc$noms[i]))
  }
  for (i in soc[, 6]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_positiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_positiu"]],as.character(soc$noms[i]))
  }
  for (i in soc[, 7]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_negatiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_negatiu"]],as.character(soc$noms[i]))
  }
  
  prefs_json = toJSON(prefs_dict, pretty = TRUE, auto_unbox = TRUE)
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0("preferencies_inverses.txt")), open = "wt", encoding = "UTF-8")
  sink(con)
  print(prefs_json)
  sink()
  close(con)
}

taula_classe_ = function(dades, negretes, bones = NULL, path_, titol, titol_peu, peu_taula){
  
  # Obsolet
  
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0(titol, '.txt')), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cols = seq(2,ncol(dades))
  dolentes = cols[!cols %in% (bones+1)]
  
  cat(
    dades %>% 
      mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE), format = "latex")) %>%
      mutate_at((.vars = vars(dolentes)), 
                funs(cell_spec(., "latex", 
                               color = ifelse(. > mean(.) + sd(.), "red", "blue")) )) %>%
      mutate_at(.vars = vars(bones+1), 
                funs(cell_spec(., "latex", 
                               color = ifelse(. > mean(.) + sd(.), "ForestGreen", "blue")) )) %>%
      kable(format = "latex", escape = F, row.names = F, align = "c") %>%
      footnote(general = peu_taula, general_title = titol_peu, 
               footnote_as_chunk = T, title_format = c("bold", "italic")) %>%
      kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),  # no es veuen bé
        full_width =F, 
        position = "center")
    #save_kable(paste0("taules/", titol, ".pdf"), keep_tex = T)  # If we want to store the image file
  )
  
  
  sink()
  close(con)
}

taula_classe_negativa = function(dades, negretes, bones = NULL, path_, titol, titol_peu, peu_taula){
  
  # Obsolet
  
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0(titol, '.txt')), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cols = seq(2,ncol(dades))
  dolentes = cols[!cols %in% (bones+1)]
  
  cat(dades %>% 
        mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE), format = "latex")) %>%
        mutate_at((.vars = vars(dolentes)), 
                  funs(cell_spec(., "latex", 
                                 color = ifelse(. < mean(.) - sd(.), "red", "blue")) )) %>%
        mutate_at(.vars = vars(bones+1), 
                  funs(cell_spec(., "latex", 
                                 color = ifelse(. < mean(.) - sd(.), "ForestGreen", "blue")) )) %>%
        kable(format = "latex", escape = F, row.names = F, align = "c") %>%
        footnote(general = peu_taula, general_title = titol_peu, 
                 footnote_as_chunk = T, title_format = c("bold", "italic")) %>%
        kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),
          full_width =F, position = "center")
      
  )
  sink()
  close(con)
}