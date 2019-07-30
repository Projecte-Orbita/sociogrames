# Creador de taules

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

require(kableExtra)
require(dplyr)


taula_classe = function(dades, color_A, color_B, bones = NULL, mixtes = NULL, path_,titol, titol_peu, peu_taula)
  {
  options(encoding=encoding_)
  con <- file(file.path(path_, paste0(titol, '.txt')), open = "wt", encoding = encoding_)
  sink(con)
  
  cols = seq(2,ncol(dades))
  no_dolentes = c(bones, mixtes)
  dolentes = cols[!cols %in% no_dolentes]
  color = rep("#000000",  # Si preferim blau com els altres: #0000CD
              nrow(dades))
  color[color_A > 1 | color_B < -1] = "#228B22"  # Verd
  color[color_A < -1 | color_B > 1] = "#CD2626"  # Vermell
  color[color_A > 1 & color_B > 1] = "#68228B"  # Lila fosc
  color[color_A < -1 & color_B < -1] = "#00BFFF"  # Blau cel
  
  cat(dades %>% 
          mutate(Noms = cell_spec(Noms, color = color, format="latex")) %>%
          mutate_at((.vars = vars(dolentes)), 
                    funs(cell_spec(., "latex", 
                                   color = ifelse(. > mean(.) + sd(.), "#CD2626", "#0000CD")) )) %>%
           mutate_at(.vars = vars(bones), 
                     funs(cell_spec(., "latex", 
                                    color = ifelse(. > mean(.) + sd(.), "#228B22", "#0000CD")) )) %>%
           mutate_at(.vars = vars(mixtes), 
                     funs(cell_spec(., "latex", 
                                    color = color))) %>%
                                     # ifelse(. < mean(.) - sd(.), "#CD2626",
                                    #               ifelse(. > mean(.) + sd(.), "#228B22", "#0000CD"))))) %>%
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