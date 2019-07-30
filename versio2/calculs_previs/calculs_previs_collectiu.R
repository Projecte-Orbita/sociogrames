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

config = config::get()
encoding_ = config$encoding


# Imports
 
gwd = getwd()
source(file.path(gwd, 'altres', 'utils.R'), encoding = encoding_)
source(file.path(gwd, 'grafics_i_taules', 'grafics.R'), encoding = encoding_)
source(file.path(gwd, 'grafics_i_taules', 'taules.R'), encoding = encoding_)
source(file.path(gwd, 'calculs_previs', 'calculs_arees.R'), encoding = encoding_)


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
  color_A = disrupcio_[[2]]
  color_B = disrupcio_[[3]]
  
  Disrupcio[,2:4] = -1*Disrupcio[,2:4]
  
  grafic_barres_classe(columnes = Disrupcio[,-5], 
                       color_A = color_A,
                       color_B = color_B,
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic =  "disrupcio")  # Gràfic
  
  grafic_2D(Disrupcio[, c(1, 4, 6)],
            tipus = "comportament",
            path_ = path_llista$figures,
            nom_grafic = "disrupcio_2D")
  
  Disrupcio = Disrupcio[,c(6,1:5)]
  names(Disrupcio)[1] = "Noms"
  Disrupcio[,3:5] = -1*Disrupcio[,3:5]
  
  titol_disrupcio = "Taula 1."
  peu_disrupcio = "Comportament observat pel grup."
  
  taula_classe(dades = Disrupcio, 
               color_A = color_A,
               color_B = color_B,
               bones = 2,
               mixtes = 6,
               path_ = path_llista$taules, 
               titol = "disrupcio",
               titol_peu = titol_disrupcio,
               peu_taula = peu_disrupcio)  # Taula
  
  
  # Victimisme
  Victimitzacio_ = calcs_victimitzacio(mat, noms)
  Victimitzacio = Victimitzacio_[[1]]
  color_A = Victimitzacio_[[2]]
  color_B = Victimitzacio_[[3]]
  
  # TODO: victimització té els colors al revés...
  
  grafic_barres_classe(columnes = Victimitzacio[,2:5], 
                       color_A = color_A,
                       color_B = color_B,
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic = "victimes")
  
  Victimitzacio = Victimitzacio[,c(5,2:4,1)]
  names(Victimitzacio)[1] = "Noms"
  
  titol_victimitzacio = "Taula 3."
  peu_vicimitzacio = "Victimització observada pel grup."
  
  taula_classe(dades = Victimitzacio, 
               color_A = color_A,
               color_B = color_B,
               bones = NULL,
               mixtes = 5,
               path_ = path_llista$taules, 
               titol = "victimes",
               titol_peu = titol_victimitzacio,
               peu_taula = peu_vicimitzacio)
  
  # Acadèmic
  Academic_ = calcs_academic(mat, noms)
  Academic = Academic_[[1]]
  color_A = Academic_[[2]]
  color_B = Academic_[[3]]
  
  
  grafic_barres_classe( columnes = Academic[,-5], 
                        color_A = color_A,
                        color_B = color_B,
                        noms = noms, 
                        path_ = path_llista$figures, 
                        nom_grafic = "academic")
  
  temp_df = cbind.data.frame(rowSums(Academic[,1:2]), rowSums(Academic[,3:4]), Academic[,6])
  names(temp_df)[3] = "noms"
  grafic_2D(temp_df,
            tipus = "academic",
            path_ = path_llista$figures,
            nom_grafic = "academic_2D")
  
  Academic[,3] = - Academic[,3]
  Academic[,4] = - Academic[,4]
  Academic = Academic[,c(6,1:5)]
  names(Academic)[1] = "Noms"
  
  titol_academic = "Taula 4."
  peu_academic = "Valoració acadèmica observada pel grup."
  
  taula_classe(dades= Academic, 
               color_A = color_A,
               color_B = color_B,
               bones = c(2:3), 
               mixtes = 6, 
               path_ = path_llista$taules, 
               titol = "academic", 
               titol_peu = titol_academic,
               peu_taula = peu_academic)
  
  # Estat d'ànim
  Estat_anim_ = calcs_estat_anim(mat, noms)
  Estat_anim = Estat_anim_[[1]]
  color_A = Estat_anim_[[2]]
  color_B = Estat_anim_[[3]]

  grafic_barres_classe(columnes = Estat_anim[, -5],  # traiem la puntuació global.
                       color_A = color_A,
                       color_B = color_B,
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic = "estat_anim")
  
  Estat_anim = Estat_anim[,c(6,3,1,2,4, 5)]
  Estat_anim[,3:5] = -1*Estat_anim[,3:5]
  names(Estat_anim)[1] = "Noms"
  
  titol_estat_anim = "Taula 5."
  peu_estat_anim = "Estat d’ànim observat pel grup."
  
  taula_classe(dades = Estat_anim, 
               color_A = color_A,
               color_B = color_B,
               bones = 2,
               mixtes = 6,
               path_ = path_llista$taules, 
               titol = "estat_anim",
               titol_peu = titol_estat_anim,
               peu_taula = peu_estat_anim)
  
  # Caràcter
  Caracter_ = calcs_caracter(mat, noms)
  Caracter = Caracter_[[1]]
  color_A = Caracter_[[2]]
  color_B = Caracter_[[3]]
  grafic_barres_classe(columnes = Caracter[,-7], 
                       color_A = color_A,
                       color_B = color_B,
                       noms = noms, 
                       path_ = path_llista$figures, 
                       nom_grafic = "caracter")
  
  Caracter = Caracter[,c(8,1:7)]
  names(Caracter)[1] = "Noms"
  
  Caracter[,c(3,5,7)] = - Caracter[,c(3,5,7)]
  
  titol_caracter = "Taula 6."
  peu_caracter = "Caràcter observat pel grup."
  
  taula_classe(dades = Caracter, 
               color_A = color_A,
               color_B = color_B,
               bones = c(2,4,6), 
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
  edge.color = X_Academic_[[5]]
  
  paraules = c(nom_curs,  "Bones notes","Notes mitjanes", "Males notes",
               "Poc popular","Normal", "Molt popular")
  Encoding(paraules) = "UTF-8-BOM"
  grafic_xarxa(gg = gg, 
               colors = colors, 
               label.color = label.color,
               vertex.shape =  vertex.shape,
               edge.color = edge.color,
               paraules = paraules, 
               path_ = path_llista$figures, 
               tipus = "xarxa_academica")
  
  # xarxa relacional
  X_relacional_ = calcs_xarxa_relacional(soc, mat, numero_respostes)
  gg = X_relacional_[[1]]
  colors = X_relacional_[[2]]
  label.color = X_relacional_[[3]]
  vertex.shape = X_relacional_[[4]]
  edge.color = X_relacional_[[5]]
  
  paraules = c(nom_curs, "Poc disruptiu","Disrupcio mitjana", "Molt disruptiu",
               "Poc popular","Normal", "Molt popular")
  Encoding(paraules) = "UTF-8-BOM"
  grafic_xarxa(gg = gg, 
               colors = colors, 
               label.color = label.color,
               vertex.shape =  vertex.shape,
               edge.color = edge.color,
               paraules = paraules, 
               path_ = path_llista$figures, 
               tipus = "xarxa_relacional")
  
  # Xarxa Amical
  
  X_amical_ = calcs_xarxa_relacional(soc, mat, numero_respostes)
  gg = X_amical_[[1]]
  colors = X_amical_[[2]]
  label.color = X_amical_[[3]]
  vertex.shape = X_amical_[[4]]
  edge.color = X_amical_[[5]]
  
  paraules = c(nom_curs, "Pocs amics","Mitjana amics", "Molts amics",
               "Poc popular","Normal", "Molt popular")
  Encoding(paraules) = "UTF-8-BOM"
  grafic_xarxa(gg = gg, 
               colors = colors, 
               label.color = label.color,
               vertex.shape =  vertex.shape,
               edge.color = edge.color,
               paraules = paraules, 
               path_ = path_llista$figures, 
               tipus = "xarxa_amical")

}

#calculs(path_fitxer, 3)