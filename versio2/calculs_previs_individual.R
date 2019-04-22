# Càlculs previs individual

#####
#
# Llicència alguna que hem de buscar
#
####


#####
# Aquest és el codi que fa els càlculs i els gràfics que després un altre fitxer farà servir per fer 
# l'informe en latex.

#####
# Imports
source('funcions_ajuda.R', encoding = 'UTF8')
source('grafics_i_taules.R', encoding = 'UTF8')
source('calculs_arees.R', encoding = 'UTF8')


######## Manipulacions inicials ###########

path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

calculs_individual = function(path_fitxer, numero_respostes=3){
  
  dades = importar_i_manipular(path_fitxer, numero_respostes)
  mat = dades[[1]]
  mat_est = dades[[2]]
  noms = dades[[3]]
  
  # Ara anem fent els gràfics i els objectes amb els quals escriurem les taules:
  
  # Prosocialitat:
  disrupcio_ = calcs_disrupcio(mat, noms)
  Disrupcio = disrupcio_[[1]]
  Disrupcio_sino = disrupcio_[[2]]
  Disrupcio = Disrupcio[,c(5,2:4,1)]
  names(Disrupcio)[1] = "Noms"
  
  nom_plot = "disrupcio"
  
  for (i in 1:nrow(Disrupcio)){
    agr.m <- melt(Disrupcio[i,1:4], id.vars = "Noms")
    grafic_barres_individual(agr.m, max(Disrupcio[2:4]), paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
    grafic_formatge(agr.m, "disruptives", paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
  }
  
  # Cooperació
  # Aquest no hi és en l'individual, no sé perquè
  # prosocialitat_ = calcs_prosocialitat(mat, noms)
  # Prosocialitat = prosocialitat_[[1]]
  # Prosocialitat_sino = prosocialitat_[[2]]
  # grafic_barres_prosocialitat(Prosocialitat, noms)
  # # TODO: Potser en aquest apartat podríem fer un diagrama 2D amb notorietat?
  # names(Prosocialitat)[2] = "Noms"
  # Prosocialitat = Prosocialitat[, c(2,1)]
  # taula_classe_negativa(Prosocialitat, Prosocialitat_sino[,1], titol = "prosocialitat")
  
  # Victimisme
  Victimitzacio_ = calcs_victimitzacio(mat, noms)
  Victimitzacio = Victimitzacio_[[1]]
  Vict_sino = Victimitzacio_[[2]]
  Victimitzacio = Victimitzacio[,c(5,2:4,1)]
  names(Victimitzacio)[1] = "Noms"
  
  nom_plot = "victimes"
  
  for (i in 1:nrow(Victimitzacio)){
    vic.m <- melt(Victimitzacio[i,1:4], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Victimitzacio[2:4]), paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
    grafic_formatge(agr.m, "victimitzadores", paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
  }

  
  # Acadèmic
  Academic_ = calcs_academic(mat, noms)
  Academic = Academic_[[1]]
  Academic_sino = Academic_[[2]]
  Academic = Academic[,c(5,1:4)]
  names(Academic)[1] = "Noms"
  Academic[,4:5] = -1*Academic[,4:5]
  
  nom_plot = "academic"
  
  for (i in 1:nrow(Academic)){
    vic.m <- melt(Academic[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Academic[2:5]), paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
    grafic_formatge(agr.m, "acadèmiques", paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
  }
  
  # Estat d'ànim
  Estat_anim_ = calcs_estat_anim(mat, noms)
  Estat_anim = Estat_anim_[[1]]
  Estat_anim_sino = Estat_anim_[[2]]
  Estat_anim = Estat_anim[,c(6, 3,1,2,4)]
  Estat_anim[,3:5] = -1*Estat_anim[,3:5]
  names(Estat_anim)[1] = "Noms"
  
  nom_plot = "estat_anim"
  
  for (i in 1:nrow(Estat_anim)){
    vic.m <- melt(Estat_anim[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Estat_anim[2:5]), paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
    grafic_formatge(agr.m, "d'estat d'ànim", paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
  }

  
  # Caràcter
  Caracter_ = calcs_caracter(mat, noms)
  Caracter = Caracter_[[1]]
  Caracter_sino = Caracter_[[2]]
  Caracter = Caracter[,c(8,1:6)]
  names(Caracter)[1] = "Noms"
  Caracter[,c(3,5,7)] = - Caracter[,c(3,5,7)]
  
  nom_plot = "caracter"
  
  for (i in 1:nrow(Caracter)){
    vic.m <- melt(Caracter[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Caracter[2:7]), paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
    grafic_formatge(agr.m, "de caràcter", paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
  }
  
  # Estatus sociomètric
  
  Estatus_ = calcs_estatus(mat)
  Estatus = Estatus_[[1]]
  Estatus_bo = cbind.data.frame(noms, Estatus[,c(1:6)])
  names(Estatus_bo)[1] = "Noms"
  
  nom_plot = "estatus"
  
  for (i in 1:nrow(Estatus_bo)){
    vic.m <- melt(Estatus_bo[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Estatus_bo[2:7]), paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-barres-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
    grafic_formatge(agr.m, "d'estatus", paleta) +
      ggsave(file = paste("figures/individuals/", nom_plot, "-formatges-", i, ".pdf", sep = ""), 
             dpi = 600, width = 8, height = 6, units = "in") 
  }

  return(list(Disrupcio, Victimitzacio, Academic, Estat_anim, Estatus))
}

#calculs(path_fitxer, 3)
