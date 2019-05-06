# Creador de taules

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

require(kableExtra)
require(dplyr)

taula_classe = function(dades, negretes, bones = NULL, path_, titol, titol_peu, peu_taula){
  
  # TODO: ajuntar aquesta i les dues següents
  
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
                               color = ifelse(. > mean(.) + sd(.), "green", "blue")) )) %>%
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
                                   color = ifelse(. < mean(.) - sd(.), "green", "blue")) )) %>%
          kable(format = "latex", escape = F, row.names = F, align = "c") %>%
          footnote(general = peu_taula, general_title = titol_peu, 
                   footnote_as_chunk = T, title_format = c("bold", "italic")) %>%
          kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),
            full_width =F, position = "center")
        
  )
  sink()
  close(con)
}

taula_classe_positiva_negativa = function(dades, negretes, bones = NULL, mixtes = NULL, 
                                          path_,titol, titol_peu, peu_taula)
  {
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0(titol, '.txt')), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cols = seq(2,ncol(dades))
  dolentes = c(bones+1,mixtes)
  dolentes = cols[!cols %in% dolentes]
  
  cat(dades %>% 
          mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE), format="latex")) %>%
          mutate_at((.vars = vars(dolentes)), 
                    funs(cell_spec(., "latex", 
                                   color = ifelse(. < mean(.) - sd(.), "red", "blue")) )) %>%
          mutate_at(.vars = vars(bones+1), 
                    funs(cell_spec(., "latex", 
                                   color = ifelse(. > mean(.) + sd(.), "green", "blue")) )) %>%
          mutate_at(.vars = vars(mixtes), 
                    funs(cell_spec(., "latex", 
                                   color = ifelse(. < mean(.) - sd(.), "red",
                                                  ifelse(. > mean(.) + sd(.), "green", "blue"))))) %>%
          kable(format = "latex", escape = F, row.names = F, align = "c") %>%
          footnote(general = peu_taula, general_title = titol_peu, 
                   footnote_as_chunk = T, title_format = c("bold", "italic")) %>%
          kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),
            full_width =F, position = "center")
        # save_kable(paste0("taules/", titol, ".txt"), keep_tex = F)
  )
  sink()
  close(con)
}
