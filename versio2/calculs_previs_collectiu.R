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

calculs_collectiu = function(path_fitxer, numero_respostes=3){
  
  dades = importar_i_manipular(path_fitxer, numero_respostes)
  mat = dades[[1]]
  mat_est = dades[[2]]
  noms = dades[[3]]
  
  # Ara anem fent els gràfics i els objectes amb els quals escriurem les taules:
  
  # Prosocialitat:
  disrupcio_ = calcs_disrupcio(mat, noms)
  Disrupcio = disrupcio_[[1]]
  Disrupcio_sino = disrupcio_[[2]]
  grafic_barres_classe(Disrupcio[,2:5],Disrupcio_sino[,1], noms, "disrupcio")  # Gràfic
  Disrupcio = Disrupcio[,c(5,2:4,1)]
  names(Disrupcio)[1] = "Noms"
  taula_classe(Disrupcio, Disrupcio_sino[,1], titol = "disrupcio")  # Taula
  
  # Cooperació
  prosocialitat_ = calcs_prosocialitat(mat, noms)
  Prosocialitat = prosocialitat_[[1]]
  Prosocialitat_sino = prosocialitat_[[2]]
  grafic_barres_prosocialitat(Prosocialitat, noms)
  # TODO: Potser en aquest apartat podríem fer un diagrama 2D amb notorietat?
  names(Prosocialitat)[2] = "Noms"
  Prosocialitat = Prosocialitat[, c(2,1)]
  taula_classe_negativa(Prosocialitat, Prosocialitat_sino[,1], titol = "prosocialitat")
  
  # Victimisme
  Victimitzacio_ = calcs_victimitzacio(mat, noms)
  Victimitzacio = Victimitzacio_[[1]]
  Vict_sino = Victimitzacio_[[2]]
  grafic_barres_classe(Victimitzacio[,2:5], Vict_sino[,1], noms, "victimes")
  Victimitzacio = Victimitzacio[,c(5,2:4,1)]
  names(Victimitzacio)[1] = "Noms"
  taula_classe(Victimitzacio, Vict_sino[,1], titol = "victimes")
  
  # Acadèmic
  Academic_ = calcs_academic(mat, noms)
  Academic = Academic_[[1]]
  Academic_sino = Academic_[[2]]
  grafic_barres_classe(Academic[,-5], Academic_sino, noms, "academic")
  Academic[,3] = - Academic[,3]
  Academic[,4] = - Academic[,4]
  Academic = Academic[,c(6,1:5)]
  names(Academic)[1] = "Noms"
  taula_classe_positiva_negativa(Academic, Academic_sino[,1], c(1:2), 5,titol = "academic")
  
  # Estat d'ànim
  Estat_anim_ = calcs_estat_anim(mat, noms)
  Estat_anim = Estat_anim_[[1]]
  Estat_anim_sino = Estat_anim_[[2]]
  grafic_barres_classe(Estat_anim, Estat_anim_sino, noms, "estat_anim")
  Estat_anim = Estat_anim[,c(6,3,1,2,4, 5)]
  Estat_anim[,3:5] = -1*Estat_anim[,3:5]
  names(Estat_anim)[1] = "Noms"
  taula_classe(Estat_anim, Estat_anim_sino[,1],1, titol = "estat_anim")
  
  # Caràcter
  Caracter_ = calcs_caracter(mat, noms)
  Caracter = Caracter_[[1]]
  Caracter_sino = Caracter_[[2]]
  grafic_barres_classe(Caracter, Caracter_sino, noms, "caracter")
  Caracter = Caracter[,c(8,1:7)]
  names(Caracter)[1] = "Noms"
  Caracter[,c(3,5,7)] = - Caracter[,c(3,5,7)]
  taula_classe_positiva_negativa(Caracter, Caracter_sino[,1], c(1,3,5), 8,titol = "caracter")
  
  # Xarxes
  soc = dades[[4]]
  
  # Xarxa acadèmica
  X_Academic_ = calcs_xarxa_academica(soc, mat)
  gg = X_Academic_[[1]]
  colors = X_Academic_[[2]]
  label.color = X_Academic_[[3]]
  paraules = c("Classe X escola Y", "Males notes","Notes mitjanes", "Bones notes",
               "Poc popular","Normal", "Molt popular")
  pdf("figures/xarxa_academica.pdf", width = 12, height = 16)
  grafic_xarxa(gg, colors, label.color, paraules)
  dev.off()
  
  # xarxa relacional
  X_relacional_ = calcs_xarxa_relacional(soc, mat)
  gg = X_Academic_[[1]]
  colors = X_Academic_[[2]]
  label.color = X_Academic_[[3]]
  paraules = c("Classe X escola Y", "Poc disruptiu","Disrupcio mitjana", "Molt disruptiu",
               "Poc popular","Normal", "Molt popular")
  pdf("figures/xarxa_relacional.pdf", width = 12, height = 16)
  grafic_xarxa(gg, colors, label.color, paraules)
  dev.off()
}

#calculs(path_fitxer, 3)