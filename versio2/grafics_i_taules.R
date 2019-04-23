Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

require(ggplot2)
require(kableExtra)
require(dplyr)

paleta <- c("#32CD32", "#CD3278", "#1E90FF", "#009E73", "#56B4E9", "#E69F00")

ffill <- function(vector, type){
  a = 0
  
  if (is.na(type)){
    for (i in 1:length(vector)){
      if (!is.na(vector[i])){
        a = vector[i]
      }
      else vector[i]=a
    }
  }
  else {
    for (i in 1:length(vector)){
      if (vector[i]!=type){
        a = vector[i]
      }
      else vector[i]=a
    }
  }
  return (vector)
}

grafic_barres_classe = function(columnes, color, noms = noms, titol){
  agr.m <- melt(columnes, id.vars = "noms")
  agr.m$color = rep(color,nrow(agr.m)/length(noms))
  agr.m$color[agr.m$color==0] = "plain"
  agr.m$color[agr.m$color==1] = "bold"
  agr.m$color = as.factor(agr.m$color)
  agr.m$noms = factor(agr.m$noms, levels = unique(as.character(agr.m$noms)))
  ggplot(agr.m, aes(x = as.factor(noms), y = value, fill=variable)) +
    geom_bar(stat='identity') + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     face = as.character(agr.m$color))) +
    labs(title = titol) + 
    ylab("") + 
    xlab("Alumnes") + 
    ggsave(file = paste("figures/", titol, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

grafic_barres_prosocialitat = function(Prosocialitat, noms){
  ggplot(Prosocialitat, aes(x = as.factor(noms), y = Prosocialitat)) +
    geom_bar(stat='identity', fill = "blue") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     face = Prosocialitat$lletra)) +
    labs(title = "Prosocialitat/Cooperació") + 
    ylab("") + 
    xlab("Alumnes") + 
    ggsave(file = paste("figures/prosocialitat.pdf", sep = ""), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

grafic_formatge = function(dades, tipus, nom_plot, i, paleta = paleta){
  dades$label = paste0(round(dades$value/sum(dades$value)*100),"%")
  formatge = ggplot(dades, aes(x = "", y = value, fill = as.factor(dades$variable))) +
    geom_bar(stat='identity', width = .5) +
    coord_polar("y", start=0, direction = - 1) +
    scale_fill_manual(values = paleta[1:length(dades$variable)]) +
    geom_text(aes(label=label), position = position_stack(vjust = 0.6)) +
    theme_void() + 
    xlab("") +
    theme(legend.position="none") +
    labs(title = paste0(sum(dades$value)," tries\n", tipus)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 6, units = "in") 
  
  return(formatge)
}

grafic_barres_individual = function(dades, numero_maxim, nom_plot, i, paleta = paleta){
  barres = ggplot(dades, aes(x = as.factor(variable), y = value)) +
    geom_bar(stat='identity', 
             fill = paleta[1:length(dades$variable)],
             width = .5) + 
    ylim(c(0,numero_maxim)) +
    theme_bw() + 
    ylab("Número de tries") + 
    xlab("") +
    ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
           dpi = 600, width = 8, height = 6, units = "in") 
  return(barres)
}

taula_classe = function(dades, negretes, bones = NULL, titol = "proves"){
  
  con <- file(paste0("taules/", titol, ".txt"), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cols = seq(2,ncol(dades))
  dolentes = cols[!cols %in% (bones+1)]

  print(
    dades %>% 
    mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE), format = "latex")) %>%
    mutate_at((.vars = vars(dolentes)), 
              funs(cell_spec(., "latex", 
                             color = ifelse(. > mean(.) + sd(.), "red", "blue")) )) %>%
    mutate_at(.vars = vars(bones+1), 
              funs(cell_spec(., "latex", 
                             color = ifelse(. > mean(.) + sd(.), "green", "blue")) )) %>%
    kable(format = "latex", escape = F, row.names = F, align = "c") %>%
    kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),  # no es veuen bé
                  full_width =F, 
                  position = "center")
    #save_kable(paste0("taules/", titol, ".pdf"), keep_tex = T)  # If we want to store the image file
  )
    
  
  sink()
  close(con)
}

taula_classe_negativa = function(dades, negretes, bones = NULL, titol="Proves"){
  
  con <- file(paste0("taules/", titol, ".txt"), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cols = seq(2,ncol(dades))
  dolentes = cols[!cols %in% (bones+1)]
  
  print(dades %>% 
    mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE), format = "latex")) %>%
    mutate_at((.vars = vars(dolentes)), 
              funs(cell_spec(., "latex", 
                             color = ifelse(. < mean(.) - sd(.), "red", "blue")) )) %>%
    mutate_at(.vars = vars(bones+1), 
              funs(cell_spec(., "latex", 
                             color = ifelse(. < mean(.) - sd(.), "green", "blue")) )) %>%
    kable(format = "latex", escape = F, row.names = F, align = "c") %>%
    kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),
                  full_width =F, position = "center")
    # save_kable(paste0("taules/", titol, ".pdf"), keep_tex = F)
  )
  sink()
  close(con)
}

taula_classe_positiva_negativa = function(dades, negretes, bones = NULL, mixtes = NULL, titol="Proves"){
  
  con <- file(paste0("taules/", titol, ".txt"), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cols = seq(2,ncol(dades))
  dolentes = c(bones+1,mixtes)
  dolentes = cols[!cols %in% dolentes]
  
  print(dades %>% 
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
    kable_styling(#latex_options = c("striped", "hover", "condensed", "responsive"),
                  full_width =F, position = "center")
    # save_kable(paste0("taules/", titol, ".txt"), keep_tex = F)
  )
  sink()
  close(con)
}

grafic_xarxa = function(gg, colors, label.color, paraules){
  plot(gg,
       layout=layout_with_lgl, # altres opcions són: layout_with_gem layout_with_fr, layout_with_mds, layout_with_lgl
       frame = T,
       vertex.label.color = label.color, 
       vertex.color = as.character(colors),
       width = 1.5,
       vertex.frame.color = NA,
       vertex.alpha = 0.5,
       #     edge.color = edge.color , !!! no funciona !!!
       edge.curved = .2,
       edge.arrow.size = 0.55, 
       label.cex = 0.5, 
       main = paraules[1]
       )
  
  legend(x=0.7, y=-0.9, c(paraules[2],paraules[3], paraules[4]),
         pch=21, col="#777777", pt.bg=c("chartreuse3", "khaki1", "firebrick"),
         pt.cex=3, cex=1.5, bty="n", ncol=1)
  legend(x=-1.2, y=-0.9, c(paraules[5],paraules[6], paraules[7]),
         pch=21, col="#777777", pt.bg="gray",
         pt.cex=c(4,5,6), cex=1.5, bty="n", ncol=1)
  
  return(TRUE)
}

grafic_resum = function(tot, i){
  options(encoding="UTF-8")
  gp = ggplot(tot, aes(x = ambit, y = tries, fill=dimensio)) +
    geom_bar(stat='identity') + 
    theme_bw() + 
    labs(title = "Gràfic resum") + 
    ylab("") +
    xlab("Àmbit") +
    coord_flip() +
    theme(legend.title=element_blank()) +
    ggsave(file = paste0("figures/individuals/resum-", i, ".pdf"), 
           dpi = 600, width = 8, height = 6, units = "in") 
}

