Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

require(ggplot2)

paleta <- c("#74ec9c", "#ec445c", "#5bade9", "#ecd044", "#347c7c", "#3c3048", "70f4a8", "94b0a8")
llista_titols = list("disrupcio" = "Disrupció",
                     "victimes" = "Víctimes",
                     "academic" = "Acadèmic",
                     "estat_anim" = "Estat d'ànim",
                     "caracter" = "Caràcter")
##### Gràfics col·lectius

grafic_barres_classe = function(columnes, color, noms = noms, path_, nom_grafic){
  options(encoding="UTF-8")
  titol = unlist(llista_titols[nom_grafic], use.names = F)
  agr.m <- melt(columnes, id.vars = "noms")
  agr.m$color = rep(color,nrow(agr.m)/length(noms))
  agr.m$color[agr.m$color==0] = "plain"
  agr.m$color[agr.m$color==1] = "bold"
  agr.m$color = as.factor(agr.m$color)
  agr.m$noms = factor(agr.m$noms, levels = unique(as.character(agr.m$noms)))
  ggplot(agr.m, aes(x = as.factor(noms), y = value, fill=variable)) +
    geom_bar(stat='identity') + 
    scale_fill_manual(values = paleta) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     face = as.character(agr.m$color)),
          legend.title=element_blank()) +
    labs(title = titol) + 
    ylab("") + 
    xlab("Alumnes") + 
    ggsave(file = file.path(path_, paste0(nom_grafic, '.pdf')), 
           dpi = 600, width = 15, height = 10, units = "cm") 
}

grafic_barres_prosocialitat = function(columnes, noms, path_){
  options(encoding="UTF-8")
  ggplot(columnes, aes(x = as.factor(noms), y = Prosocialitat)) +
    geom_bar(stat='identity', fill = "#5bade9") +  
    scale_fill_manual(values = paleta) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     face = columnes$lletra),
          legend.title=element_blank()) +
    labs(title = "Prosocialitat/Cooperació") + 
    ylab("") + 
    xlab("Alumnes") + 
    ggsave(file = file.path(path_, "prosocialitat.pdf"), 
           dpi = 600, width = 15, height = 10, units = "cm") 
}

grafic_xarxa = function(gg, colors, label.color, vertex.shape, paraules, path_, tipus){

  options(encoding="UTF-8")
  pdf(file.path(path_, paste0(tipus, ".pdf")), width = 10, height = 15)
  plot(gg,
       layout=layout_with_lgl, # altres opcions són: layout_with_gem layout_with_fr, layout_with_mds, layout_with_lgl
       frame = F,
       vertex.label.color = label.color, 
       vertex.color = as.character(colors),
       width = 1.5,
       vertex.frame.color = NA,
       vertex.alpha = 0.75,
       vertex.shape =  vertex.shape,
       #edge.color = edge.color , !!! no funciona !!!
       edge.curved = .2,
       edge.arrow.size = 0.55, 
       label.cex = 0.5,
       #main = ifelse(tipus=="xarxa_academica", "Xarxa Acadèmica", "Xarxa Relacional"),
       main = paraules[1]
  )
  
  legend(x=0.7, y=-0.9, c(paraules[2],paraules[3], paraules[4]),
         pch=21, col="#777777", pt.bg=c("chartreuse3", "khaki1", "firebrick"),
         pt.cex=3, cex=1.5, bty="n", ncol=1)
  legend(x=-1.2, y=-0.9, c(paraules[5],paraules[6], paraules[7]),
         pch=21, col="#777777", pt.bg="gray",
         pt.cex=c(4,5,6), cex=1.5, bty="n", ncol=1)
  dev.off()
  
}

##### Gràfics individuals

grafic_formatge = function(dades, tipus, path_, nom_plot, i, paleta = paleta){
  
  options(encoding="UTF-8")
  dades$label = paste0(round(dades$value/sum(dades$value)*100),"%")
  nom_output = paste0(nom_plot, "-formatges-", i, ".pdf")
    
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
    ggsave(file = file.path(path_, "individuals", nom_output), 
           dpi = 600, width = 8, height = 6, units = "cm") 
  
  #return(formatge)
}

grafic_barres_individual = function(dades, numero_maxim, path_, nom_plot, i, paleta = paleta){
  
  options(encoding="UTF-8")
  nom_output = paste0(nom_plot, "-barres-", i, ".pdf")
  
  barres = ggplot(dades, aes(x = as.factor(variable), y = value)) +
    geom_bar(stat='identity', 
             fill = paleta[1:length(dades$variable)],
             width = .5) + 
    ylim(c(0,numero_maxim)) +
    theme_bw() + 
    ylab("Número de tries") + 
    xlab("") + 
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    ggsave(file = file.path(path_, "individuals", nom_output), 
           dpi = 600, width = 8, height = 6, units = "cm") 
  #return(barres)
}

grafic_resum = function(tot, path_, i){
  
  options(encoding="UTF-8")
  nom_output = paste0("resum-", i, ".pdf")
  
  gp = ggplot(tot, aes(x = ambit, y = tries, fill=dimensio)) +
    geom_bar(stat='identity') + 
    theme_bw() + 
    labs(title = "Gràfic resum") + 
    ylab("") +
    xlab("Àmbit") +
    coord_flip() +
    theme(legend.title=element_blank()) +
    ggsave(file = file.path(path_, "individuals", nom_output), 
           dpi = 600, width = 8, height = 6, units = "in") 
}
