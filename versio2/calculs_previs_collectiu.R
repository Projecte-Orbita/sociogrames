#####
#
# Llicència alguna que hem de buscar
#
####


#####
# Aquest és el codi que fa els càlculs i els gràfics que després un altre fitxer farà servir per fer 
# l'informe en latex.

#####

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")


# Imports
source('utils.R', encoding = 'UTF8')
source('grafics.R', encoding = 'UTF8')
source('taules.R', encoding = 'UTF8')
source('calculs_arees.R', encoding = 'UTF8')


######## Manipulacions inicials ###########

# path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'

calculs_collectiu = function(path_llista, nom_fitxer, numero_respostes=3){
  
  dades = importar_i_manipular(file.path(path_llista$dades, nom_fitxer), numero_respostes)
  mat = dades[[1]]
  mat_est = dades[[2]]
  noms = dades[[3]]
  soc = dades[[4]]
  curs = dades[[5]]
  
  # Escurcem els noms perquè es vegin millor:
  
  noms = formatejar_noms(noms)
  
  # Ara anem fent els gràfics i els objectes amb els quals escriurem les taules:
  
  # Prosocialitat:
  disrupcio_ = calcs_disrupcio(mat, noms)
  Disrupcio = disrupcio_[[1]]
  Disrupcio_sino = disrupcio_[[2]]
  
  grafic_barres_classe(columnes = Disrupcio[,2:5], 
                       color = Disrupcio_sino[,1], 
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic =  "disrupcio")  # Gràfic
  
  Disrupcio = Disrupcio[,c(5,2:4,1)]
  names(Disrupcio)[1] = "Noms"
  
  titol_disrupcio = "Taula 1."
  peu_disrupcio = "Disrupció observada pel grup."
  
  taula_classe(dades = Disrupcio, 
               negretes = Disrupcio_sino[,1],
               bones = NULL,
               path_ = path_llista$taules, 
               titol = "disrupcio",
               titol_peu = titol_disrupcio,
               peu_taula = peu_disrupcio)  # Taula
  
  # Cooperació
  prosocialitat_ = calcs_prosocialitat(mat, noms)
  Prosocialitat = prosocialitat_[[1]]
  Prosocialitat_sino = prosocialitat_[[2]]
  
  grafic_barres_prosocialitat(columnes = Prosocialitat, 
                              noms = noms, 
                              path_ = path_llista$figures)
  
  # TODO: Potser en aquest apartat podríem fer un diagrama 2D amb notorietat?
  names(Prosocialitat)[2] = "Noms"
  Prosocialitat = Prosocialitat[, c(2,1)]
  
  titol_prosocialitat = "Taula 2."
  peu_prosocialitat = "Prosocialitat observada pel grup."
  
  taula_classe_negativa(dades = Prosocialitat, 
                        negretes = Prosocialitat_sino[,1], 
                        bones = NULL,
                        path_ = path_llista$taules, 
                        titol = "prosocialitat",
                        titol_peu = titol_prosocialitat,
                        peu_taula = peu_prosocialitat)
  
  # Victimisme
  Victimitzacio_ = calcs_victimitzacio(mat, noms)
  Victimitzacio = Victimitzacio_[[1]]
  Vict_sino = Victimitzacio_[[2]]
  
  grafic_barres_classe(columnes = Victimitzacio[,2:5], 
                       color = Vict_sino[,1],
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic = "victimes")
  
  Victimitzacio = Victimitzacio[,c(5,2:4,1)]
  names(Victimitzacio)[1] = "Noms"
  
  titol_victimitzacio = "Taula 3."
  peu_vicimitzacio = "Victimització observada pel grup."
  
  taula_classe(dades = Victimitzacio, 
               negretes = Vict_sino[,1],
               bones = NULL,
               path_ = path_llista$taules, 
               titol = "victimes",
               titol_peu = titol_victimitzacio,
               peu_taula = peu_vicimitzacio)
  
  # Acadèmic
  Academic_ = calcs_academic(mat, noms)
  Academic = Academic_[[1]]
  Academic_sino = Academic_[[2]]
  
  grafic_barres_classe( columnes = Academic[,-5], 
                        color = Academic_sino, 
                        noms = noms, 
                        path_ = path_llista$figures, 
                        nom_grafic = "academic")
  
  Academic[,3] = - Academic[,3]
  Academic[,4] = - Academic[,4]
  Academic = Academic[,c(6,1:5)]
  names(Academic)[1] = "Noms"
  
  titol_academic = "Taula 4."
  peu_academic = "Valoració acadèmica observada pel grup."
  
  taula_classe_positiva_negativa(dades= Academic, 
                                 negretes = Academic_sino[,1], 
                                 bones = c(1:2), 
                                 mixtes = 6, 
                                 path_ = path_llista$taules, 
                                 titol = "academic", 
                                 titol_peu = titol_academic,
                                 peu_taula = peu_academic)
  
  # Estat d'ànim
  Estat_anim_ = calcs_estat_anim(mat, noms)
  Estat_anim = Estat_anim_[[1]]
  Estat_anim_sino = Estat_anim_[[2]]
  
  grafic_barres_classe(columnes = Estat_anim[, -5],  # traiem la puntuació global.
                       color = Estat_anim_sino, 
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic = "estat_anim")
  
  Estat_anim = Estat_anim[,c(6,3,1,2,4, 5)]
  Estat_anim[,3:5] = -1*Estat_anim[,3:5]
  names(Estat_anim)[1] = "Noms"
  
  titol_estat_anim = "Taula 5."
  peu_estat_anim = "Estat d’ànim observat pel grup."
  
  taula_classe_positiva_negativa(dades = Estat_anim, 
               negretes = Estat_anim_sino[,1],
               bones = 1,
               mixtes = 5,
               path_ = path_llista$taules, 
               titol = "estat_anim",
               titol_peu = titol_estat_anim,
               peu_taula = peu_estat_anim)
  
  # Caràcter
  Caracter_ = calcs_caracter(mat, noms)
  Caracter = Caracter_[[1]]
  Caracter_sino = Caracter_[[2]]
  
  grafic_barres_classe(columnes = Caracter[,-7], 
                       color = Caracter_sino, 
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic = "caracter")
  
  Caracter = Caracter[,c(8,1:7)]
  names(Caracter)[1] = "Noms"
  
  Caracter[,c(3,5,7)] = - Caracter[,c(3,5,7)]
  
  titol_caracter = "Taula 6."
  peu_caracter = "Actitud observada pel grup."
  
  taula_classe_positiva_negativa(dades = Caracter, 
                                 negretes = Caracter_sino[,1], 
                                 bones = c(1,3,5), 
                                 mixtes = 8, 
                                 path_ = path_llista$taules, 
                                 titol = "caracter",
                                 titol_peu = titol_caracter,
                                 peu_taula = peu_caracter)
  
  # Xarxes
  soc$noms = unlist(lapply(noms, function(x) rep(x, numero_respostes)), use.names = F)
  
  cursos = list("1" = "1r de primària",
                "2" = "2n de primària",
                "3" = "3r de primària",
                "4" = "4rt de primària",
                "5" = "5è de primària",
                "6" = "6è de primària"
                )

  nom_curs = paste(cursos[[substr(curs, 1, 1)]], substr(curs, 2, 2), sep = " ")
  # Xarxa acadèmica
  X_Academic_ = calcs_xarxa_academica(soc, mat, numero_respostes)
  gg = X_Academic_[[1]]
  colors = X_Academic_[[2]]
  label.color = X_Academic_[[3]]
  vertex.shape = X_Academic_[[4]]
  
  paraules = c(nom_curs, "Males notes","Notes mitjanes", "Bones notes",
               "Poc popular","Normal", "Molt popular")
  Encoding(paraules) = "UTF-8-BOM"
  grafic_xarxa(gg = gg, 
               colors = colors, 
               label.color = label.color,
               vertex.shape =  vertex.shape,
               paraules = paraules, 
               path_ = path_llista$figures, 
               tipus = "xarxa_academica")
  
  # xarxa relacional
  X_relacional_ = calcs_xarxa_relacional(soc, mat, numero_respostes)
  gg = X_relacional_[[1]]
  colors = X_relacional_[[2]]
  label.color = X_relacional_[[3]]
  vertex.shape = X_relacional_[[4]]
  
  paraules = c(nom_curs, "Poc disruptiu","Disrupcio mitjana", "Molt disruptiu",
               "Poc popular","Normal", "Molt popular")
  Encoding(paraules) = "UTF-8-BOM"
  grafic_xarxa(gg = gg, 
               colors = colors, 
               label.color = label.color,
               vertex.shape =  vertex.shape,
               paraules = paraules, 
               path_ = path_llista$figures, 
               tipus = "xarxa_relacional")

}

#calculs(path_fitxer, 3)