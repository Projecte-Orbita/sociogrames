# Creador de taules

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

require(kableExtra)
require(dplyr)


taula_classe = function(dades, negretes, bones = NULL, mixtes = NULL, path_,titol, titol_peu, peu_taula)
  {
  options(encoding=encoding_)
  con <- file(file.path(path_, paste0(titol, '.txt')), open = "wt", encoding = encoding_)
  sink(con)
  
  cols = seq(2,ncol(dades))
  no_dolentes = c(bones, mixtes)
  dolentes = cols[!cols %in% no_dolentes]
  
  cat(dades %>% 
          mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE), format="latex")) %>%
          mutate_at((.vars = vars(dolentes)), 
                    funs(cell_spec(., "latex", 
                                   color = ifelse(. > mean(.) + sd(.), "red", "blue")) )) %>%
           mutate_at(.vars = vars(bones), 
                     funs(cell_spec(., "latex", 
                                    color = ifelse(. > mean(.) + sd(.), "ForestGreen", "blue")) )) %>%
           mutate_at(.vars = vars(mixtes), 
                     funs(cell_spec(., "latex", 
                                    color = ifelse(. < mean(.) - sd(.), "red",
                                                   ifelse(. > mean(.) + sd(.), "ForestGreen", "blue"))))) %>%
          kable(format = "latex", escape = F, row.names = F, align = "c") %>%
          footnote(general = peu_taula, 
                   general_title = titol_peu, 
                   footnote_as_chunk = T, 
                   title_format = c("bold", "italic")) %>%
          kable_styling(full_width =F, 
                        position = "center")
  )
  sink()
  close(con)
}

taula_preferencies = function(df_taula){
  noms_columnes = c("Ha triat a", "Ha estat triat/da per")
  df_taula = as.data.frame(df_taula)
  cat(df_taula %>% 
        mutate(noms_fora = cell_spec(noms_fora, 
                                     bold = df_taula$noms_fora %in% df_taula$noms_dins, 
                                     format="latex"),
               noms_dins = cell_spec(noms_dins, 
                                     bold = df_taula$noms_dins %in% df_taula$noms_fora, 
                                     format="latex")) %>%
        kable(format = "latex", escape = F, row.names = F, 
              align = "c", col.names = noms_columnes,
              booktabs =T)%>%
        kable_styling(full_width =F, 
                      position = "center")
  )
}