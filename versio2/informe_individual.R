# Informes individuals

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts_collectiu.R', encoding = "UTF-8")
source('calculs_previs_individual.R')

print("> Creant gràfics i taules...")
path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

soc = read.csv(path_fitxer, encoding = "UTF-8")
num_anomenats = 3
noms = as.character(soc$Nom[seq(1, nrow(soc), num_anomenats)])

calculs_individual(path_fitxer)

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
  
  cat(resum)
  
  afegeix_grafic_resum(i)
  
  
  # Final
  
  cat(final_latex)
  
  sink()
  close(con)
}

print("> Finalitzat correctament.")
