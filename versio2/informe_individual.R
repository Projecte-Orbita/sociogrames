# Informes individuals

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts_collectiu.R', encoding = "UTF-8")
source('calculs_previs_individual.R')

print("> Creant gràfics i taules...")
path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

soc = read.csv(path_fitxer, encoding = "UTF-8")
num_anomenats = 3
noms = as.character(soc$Nom[seq(1, nrow(soc), num_anomenats)])

dades = calculs_individual(path_fitxer)

print("> Imprimint els fitxers .tex...")

for (i in 1:length(noms)){
  
  nom = noms[i]
  con <- file(paste0("informes/individuals/", nom, ".tex"), open = "wt", encoding = "UTF-8")
  sink(con)
  
  cat(coses_latex)
  pagina_titol(nom)
  
  cat(introduccio)
  
  # Disrupció
  cat(disrupcio)
  
  afegeix_grafic_individual("disrupcio", i)
  
  # Prosocialitat
  #cat(prosocialitat)
  
  #afegeix_grafic_individual("prosocialitat", i)
  
  
  # Víctima
  cat(victimes)
  
  afegeix_grafic_individual("victimes", i)
  
  
  # Acadèmic
  cat(academic)
  
  afegeix_grafic_individual("academic", i)
  
  
  # Estat d'ànim
  cat(estat_anim)
  
  afegeix_grafic_individual("estat_anim", i)
  
  # Caràcter
  cat(caracter)
  
  afegeix_grafic_individual("caracter", i)
  
  # Estatus sociomètric
  
  cat(estatus_sociometric)
  
  afegeix_grafic_individual("estatus", i)
  
  # Resum
  
  agr.m = dades[[1]]
  vic.m = dades[[2]]
  aca.m = dades[[3]]
  ea.m = dades[[4]]
  est.m = dades[[5]]
  
  agr.m["Noms"] = rep("Disrupció", nrow(agr.m))
  vic.m["Noms"] = rep("Victimització", nrow(vic.m) )
  aca.m["Noms"] = rep("Acadèmic", nrow(aca.m))
  ea.m["Noms"] = rep("Estat d'ànim", nrow(ea.m))
  est.m["Noms"] = rep("Estatus sociomètric", nrow(est.m))
  
  names(agr.m)[1] = "ambit"
  names(vic.m)[1] = "ambit"
  names(aca.m)[1] = "ambit"
  names(ea.m)[1] = "ambit"
  names(est.m)[1] = "ambit"
  
  vic.m$value = -1*vic.m$value
  aca.m$value[3:4] = -1*aca.m$value[3:4]
  ea.m$value[-1] = -1*ea.m$value[-1]
  est.m$value[c(2,4,6)] = -1*est.m$value[c(2,4,6)] 
  tot = rbind.data.frame(agr.m, vic.m, aca.m, ea.m, est.m)
  tot$ambit = factor(tot$ambit, levels = unique(tot$ambit))
  names(tot) = c("Àmbit", "Dimensió", "Tries")
  
  ggplot(tot, aes(x = Àmbit, y = Tries, fill=Dimensió)) +
    geom_bar(stat='identity') + 
    theme_bw() + 
    labs(title = nom) + 
    ylab("") +
    xlab("Àmbit") +
    coord_flip() +
    theme(legend.title=element_blank())
  
  # Final
  
  cat(final_latex)
  
  sink()
  close(con)
}

print("> Finalitzat correctament.")
