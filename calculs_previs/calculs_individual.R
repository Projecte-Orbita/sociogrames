# Càlculs previs individual

# Aquest és el codi que fa els càlculs i els gràfics que després un altre fitxer farà servir per fer 
# l'informe en latex.


Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = as.character(config$encoding)
options(encoding = encoding_)
# Imports

gwd = getwd()
source(file.path(gwd, 'altres', 'utils.R') , encoding = encoding_)
source(file.path(gwd, 'grafics_i_taules', 'grafics.R'), encoding = encoding_)
source(file.path(gwd, 'grafics_i_taules', 'taules.R'), encoding = encoding_)
source(file.path(gwd, 'calculs_previs', 'calculs_arees.R'), encoding = encoding_)
source(file.path(gwd, 'valoracions', 'valoracions.R'), encoding = encoding_)
require(jsonlite)

######## Manipulacions inicials ###########

calculs_individual = function(dades, path_llista, nom_fitxer, numero_respostes=3){
  
  # Funció que analitza la part individual del sociograma i crea els gràfics i les valoracions.
  # Arguments: dades: una llista amb les dades que ens interessen
  #            path_llista: la llista de carpetes
  #            nom_fitxer: el csv amb el que estem treballant en aquests moments
  #            numero_respostes: quin és el màxim número de respostes que pot clicar cada nen
  # Importa: res
  # Retorna: un vector amb els noms dels nens i les nenes
  # Exporta: els gràfics de la part col·lectiva dels informes 
  #          .jsons amb les valoracions per cada nen i nena
  
  # Extraiem les dades que necessitarem:
  
  mat = dades[[1]]
  mat_est = dades[[2]]
  noms = dades[[3]]
  soc = dades[[4]]
  
  # Ara anem fent els gràfics i els objectes amb els quals escriurem les taules:
  
  # Comportament
  disrupcio_ = calcs_disrupcio(mat, noms)
  Disrupcio = disrupcio_[[1]]
  color_A = disrupcio_[[2]]  # Es diu color per mantenir consistència amb el col·lectiu, però és la estandarització A
  color_B = disrupcio_[[3]]  # Ídem B
  Disrupcio = Disrupcio[,c(6,1:4)]
  names(Disrupcio)[1] = "Noms"
  
  nom_plot = "disrupcio"
  
  llista_valoracions = list()
  
  for (i in 1:nrow(Disrupcio)) {
    vic.m <- melt(Disrupcio[i,1:5], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Disrupcio[2:5]), path_llista$figures, nom_plot, i, paleta)
    grafic_formatge(vic.m, "de comportament", path_llista$figures, nom_plot, i, paleta)
    llista_valoracions[noms[i]] = valoracions_disrupcio(noms[i], color_A[i], color_B[i], Disrupcio[i, ])
  }
  
  # Escrivim les valoracions en un fitxer que capturarem més endavant
  con = file(file.path(path_llista$taules, "vals_disrupcio.json"), open = "w+", encoding = encoding_)
  writeLines(toJSON(llista_valoracions), con)
  close(con)
  
  # Victimisme
  Victimitzacio_ = calcs_victimitzacio(mat, noms)
  Victimitzacio = Victimitzacio_[[1]]
  color_A = Victimitzacio_[[2]]
  color_B = Victimitzacio_[[3]]
  Victimitzacio = Victimitzacio[,c(5,2:4,1)]
  names(Victimitzacio)[1] = "Noms"
  
  nom_plot = "victimes"
  
  llista_valoracions = list()
  
  for (i in 1:nrow(Victimitzacio)) {
    vic.m <- melt(Victimitzacio[i,1:4], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Victimitzacio[2:4]), path_llista$figures, nom_plot, i, paleta)
    grafic_formatge(vic.m, "de victimització", path_llista$figures, nom_plot, i, paleta)
    llista_valoracions[noms[i]] = valoracions_victimes(noms[i], color_A[i], color_B[i], Victimitzacio[i, ])
  }
  
  con = file(file.path(path_llista$taules, "vals_victimes.json"), open = "w+", encoding = encoding_)
  writeLines(toJSON(llista_valoracions), con)
  close(con)

  
  # Acadèmic
  Academic_ = calcs_academic(mat, noms)
  Academic = Academic_[[1]]
  color_A = Academic_[[2]]
  color_B = Academic_[[3]]
  Academic = Academic[,c(5,1:4)]
  names(Academic)[1] = "Noms"
  Academic[,4:5] = -1*Academic[,4:5]
  
  nom_plot = "academic"
  
  llista_valoracions = list()
  
  for (i in 1:nrow(Academic)) {
    vic.m <- melt(Academic[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Academic[2:5]), path_llista$figures, nom_plot, i, paleta)
    grafic_formatge(vic.m, "acadèmiques", path_llista$figures, nom_plot, i, paleta)
    llista_valoracions[noms[i]] = valoracions_academic(noms[i], color_A[i], color_B[i])
  }
  con = file(file.path(path_llista$taules, "vals_academic.json"), open = "w+", encoding = encoding_)
  writeLines(toJSON(llista_valoracions), con)
  close(con)
  
  # Estat d'ànim
  Estat_anim_ = calcs_estat_anim(mat, noms)
  Estat_anim = Estat_anim_[[1]]
  color_A = Estat_anim_[[2]]
  color_B = Estat_anim_[[3]]
  Estat_anim = Estat_anim[,c(6, 3,1,2,4)]
  # Estat_anim[,3:5] = -1*Estat_anim[,3:5]
  names(Estat_anim)[1] = "Noms"
  
  nom_plot = "estat_anim"
  
  llista_valoracions = list()
  
  for (i in 1:nrow(Estat_anim)) {
    vic.m <- melt(Estat_anim[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Estat_anim[2:5]), path_llista$figures, nom_plot, i, paleta) 
    grafic_formatge(vic.m, "d'estat d'ànim", path_llista$figures, nom_plot, i, paleta)
    llista_valoracions[noms[i]] = valoracions_estat_anim(noms[i], color_A[i], color_B[i])
  }
  con = file(file.path(path_llista$taules, "vals_ea.json"), open = "w+", encoding = encoding_)
  writeLines(toJSON(llista_valoracions), con)
  close(con)
  
  # Caràcter
  Caracter_ = calcs_caracter(mat, noms)
  Caracter = Caracter_[[1]]
  color_A = Caracter_[[2]]
  color_B = Caracter_[[3]]
  Caracter = Caracter[,c(8,1:6)]
  names(Caracter)[1] = "Noms"
  Caracter[,c(3,5,7)] = -Caracter[,c(3,5,7)]
  
  nom_plot = "caracter"
  
  llista_valoracions = list()
  
  for (i in 1:nrow(Caracter)) {
    vic.m <- melt(Caracter[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Caracter[2:7]), path_llista$figures, nom_plot, i, paleta)
    grafic_formatge(vic.m, "de caràcter", path_llista$figures, nom_plot, i, paleta)
    llista_valoracions[noms[i]] = valoracions_caracter(noms[i], color_A[i], color_B[i])
  }
  con = file(file.path(path_llista$taules, "vals_caracter.json"), open = "w+", encoding = encoding_)
  writeLines(toJSON(llista_valoracions), con)
  close(con)
  
  # Estatus sociomètric
  
  Estatus_ = calcs_estatus(mat)
  Estatus = Estatus_[[1]]
  color_A = Estatus_[[2]]
  color_B = Estatus_[[3]]
  Estatus_bo = cbind.data.frame(noms, Estatus)
  names(Estatus_bo)[1] = "Noms"
  
  nom_plot = "estatus"
  
  llista_valoracions = list()
  
  for (i in 1:nrow(Estatus_bo)) {
    vic.m <- melt(Estatus_bo[i,], id.vars = "Noms")
    grafic_barres_individual(vic.m, max(Estatus_bo[2:10]), path_llista$figures, nom_plot, i, paleta)
    grafic_formatge(vic.m, "d'estatus", path_llista$figures, nom_plot, i, paleta)
    llista_valoracions[noms[i]] = valoracions_estatus(noms[i], color_A[i], color_B[i])
  }
  con = file(file.path(path_llista$taules, "vals_estatus.json"), open = "w+", encoding = encoding_)
  writeLines(toJSON(llista_valoracions), con)
  close(con)
  
  # Resum
  
  for (i in 1:nrow(Estatus_bo)) {
    
    nom = noms[i]
    
    agr.m <- melt(Disrupcio[i,1:4], id.vars = "Noms")
    vic.m <- melt(Victimitzacio[i,1:4], id.vars = "Noms")
    aca.m <- melt(Academic[i,], id.vars = "Noms")
    ea.m <- melt(Estat_anim[i,], id.vars = "Noms")
    est.m <- melt(Estatus_bo[i,], id.vars = "Noms")
   
    agr.m["Noms"] = enc2utf8(rep("Disrupció", nrow(agr.m)))
    vic.m["Noms"] = enc2utf8(rep("Victimització", nrow(vic.m)))
    aca.m["Noms"] = enc2utf8(rep("Acadèmic", nrow(aca.m)))
    ea.m["Noms"] = enc2utf8(rep("Estat d'ànim", nrow(ea.m)))
    est.m["Noms"] = enc2utf8(rep("Estatus sociomètric", nrow(est.m)))
    
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
    names(tot) = c("ambit", "dimensio", "tries")
    
    grafic_resum(tot, path_llista$figures, i)
  }
  
  # Per acabar, fem un dataframe on guardem les preferències de cada nen i nena:
  calcular_preferencies(soc = soc, path_ = path_llista$taules, numero_respostes = 3)
  
  return(noms)

}

#calculs_individual(path_fitxer, 3)

