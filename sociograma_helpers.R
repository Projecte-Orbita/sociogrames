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
    xlab("Alumnes")
}

grafic_formatge = function(dades, tipus, paleta = paleta){
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
    theme(plot.title = element_text(hjust = 0.5))
  
  return(formatge)
}

grafic_barres_individual = function(dades, numero_maxim, paleta = paleta){
  barres = ggplot(dades, aes(x = as.factor(variable), y = value)) +
    geom_bar(stat='identity', 
             fill = paleta[1:length(dades$variable)],
             width = .5) + 
    ylim(c(0,numero_maxim)) +
    theme_bw() + 
    ylab("Número de tries") + 
    xlab("") 
  return(barres)
}

taula_classe = function(dades, negretes, bones = NULL){
  cols = seq(2,ncol(dades))
  
  dolentes = cols[!cols %in% (bones+1)]
  
  dades %>% 
    mutate(Noms = cell_spec(Noms, bold = ifelse(negretes==0,FALSE,TRUE))) %>%
    mutate_at((.vars = vars(dolentes)), 
              funs(cell_spec(., "html", color = ifelse(. > mean(.)+ sd(.), "red", "blue")) )) %>%
    mutate_at(.vars = vars(bones+1), 
              funs(cell_spec(., "html", color = ifelse(. > mean(.)+ sd(.), "green", "blue")) )) %>%
    kable(format = "html", escape = F, row.names = F, align = "c") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width =F, position = "center")
}