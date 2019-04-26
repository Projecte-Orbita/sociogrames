# Aquí hi afegeixo els càlculs per cada una de les àrees. Per ara els poso per separat però segurament en un 
# futur proper s'hauran d'ajuntar en una sola funció. A causa de les diferències entre tots els grups no està
# clar com es faria això, i per tant ho començo fent separat
require(reshape2)
require(igraph)
require(rlist)

calcs_disrupcio = function(mat, noms){
  dis_directe = cbind( mat[,10], mat[,12], mat[,14], mat[,16])
  dis_relacional = cbind( mat[,22], mat[,24], mat[,25], mat[,26])
  
  dis_total = rowSums(dis_relacional) + rowSums(dis_directe)
  
  disrupcio = cbind(dis_relacional, dis_directe, dis_total)
  disrupcio_est = scale(disrupcio)
  
  Disrupcio = cbind.data.frame(dis_total, dis_directe[,4], rowSums(dis_directe[,1:3]), rowSums(dis_relacional))
  rownames(Disrupcio) = noms
  colnames(Disrupcio) = c("Disrupció total", 
                          "Disrupció física",
                          "Disrupció verbal",
                          "Disrupció relacional")
  Disrupcio$noms = as.factor(rownames(Disrupcio))
  
  # Ara en fem un posant 1 en els que són significativament liantes
  Disrupcio_sino = as.data.frame(ifelse(scale(Disrupcio[,-ncol(Disrupcio)])>1, 1, 0))
  
  rownames(Disrupcio_sino) = noms
  Disrupcio_sino$noms = rownames(Disrupcio_sino)
  
  return(list(Disrupcio, Disrupcio_sino))
}

calcs_prosocialitat = function(mat, noms){
  # Aquí hi ha un cacau impressionant7
  
  prosocialitat = cbind(mat[,7], mat[,8], mat[,9], mat[,18])
  prosocialitat_est = scale(prosocialitat)
  prosocialitat_total = rowSums(prosocialitat)
  prosocialitat_total_est = scale(rowSums(prosocialitat))
  
  Pro_sino = ifelse(prosocialitat_total_est < 1, 1, 0)
  Prosocialitat = as.data.frame(rowSums(prosocialitat))
  Pro_sino = as.data.frame(Pro_sino)
  rownames(Prosocialitat) = noms
  rownames(Pro_sino) = noms
  colnames(Prosocialitat) = c("Prosocialitat")
  Prosocialitat$noms = factor(rownames(Prosocialitat), 
                              levels = unique(as.character(rownames(Prosocialitat))))
  Prosocialitat$lletra = ifelse(prosocialitat_total_est< -1,"bold", "plain")
  
  return(list(Prosocialitat, Pro_sino))
}

calcs_victimitzacio = function(mat, noms){
  victima_directe = cbind(mat[,11], mat[,13], mat[,15], mat[,17], mat[,19])
  victima_directe_total = rowSums(victima_directe)
  victima_relacional = cbind(mat[,21], mat[,25])
  victima_relacional_total = rowSums(victima_relacional)
  victima = as.data.frame(cbind(victima_directe, victima_relacional))
  
  victima_total = victima_directe_total + victima_relacional_total
  victima_est = scale(victima_total)
  
  Victimitzacio = cbind(victima_total, victima_directe[,4], rowSums(victima_directe[,1:3]), victima_relacional_total)
  colnames(Victimitzacio) = c("Total víctima", 
                              "Víctima física", 
                              "Víctima verbal",
                              "Víctima relacional")
  
  Vict_sino = ifelse(victima_est > 1, 1, 0)
  rownames(Victimitzacio) = noms
  rownames(Vict_sino) = noms
  Victimitzacio = as.data.frame(Victimitzacio)
  Victimitzacio$noms = rownames(Victimitzacio)
  
  return(list(Victimitzacio, Vict_sino))
}

calcs_academic = function(mat, noms){
  academic_total = mat[,38] + mat[,40] - mat[,39] - mat[,41]
  academic = as.data.frame(cbind(mat[,38], mat[,40], mat[,39], mat[,41], academic_total))
  colnames(academic) = c("Bones notes", "Participa", "Males notes", "No participa", "Puntuació global")
  academic_est = scale(academic)
  
  academic_total_est = scale(academic_total)
  academic_sino = ifelse(academic_total_est < -1,1,0)
  
  academic[,3] = - academic[,3]
  academic[,4] = - academic[,4]
  
  academic$noms = factor(noms, levels = as.character(noms))
  
  return(list(academic, academic_sino))
}

calcs_estat_anim = function(mat, noms){
  estat_anim_total = - mat[,28] - mat[,29] + mat[,30] - mat[,31]
  estat_anim = as.data.frame(cbind(mat[,28], mat[,29], mat[,30], mat[,31], estat_anim_total))
  colnames(estat_anim) = c("Dissatisfacció", "Enuig", "Alegria", "Tristor", "Puntuació global")
  estat_anim_est = scale(estat_anim)
  estat_anim_total_est = scale(estat_anim_total)
  estat_anim_sino = ifelse(estat_anim_total_est< -1,1,0)
  estat_anim = -estat_anim
  estat_anim$Alegria = -estat_anim$Alegria
  estat_anim$noms = factor(noms, levels = as.character(noms))
  
  return(list(estat_anim, estat_anim_sino))
}

calcs_caracter = function(mat, noms){
  caracter_total = mat[,32] - mat[,33] + mat[,34] - mat[,35] + mat[,36] - mat[,37]
  caracter = as.data.frame(cbind(mat[,32], mat[,33], mat[,34], mat[,35], mat[,36], mat[,37], caracter_total))
  colnames(caracter) = c("Lideratge", "No lid.", "Autonomia", "No aut.", 
                         "Socialització", "No soc.", "Global")
  caracter_est = scale(caracter)
  
  caracter_total_est = scale(caracter_total)
  caracter_sino = ifelse(caracter_total_est< -1,1,0)
  caracter[,2] = -caracter[,2]
  caracter[,4] = -caracter[,4]
  caracter[,6] = -caracter[,6]
  caracter$noms = factor(noms, levels = as.character(noms))
  
  return(list(caracter, caracter_sino))
}

calcs_estatus = function(mat){
  estatus = cbind.data.frame(mat[,1:6],mat[,19], mat[,26], mat[,27])
  estatus_est = scale(estatus)
  
  return(list(estatus, estatus_est))
}

calcs_xarxa_academica = function(soc, mat){
  xarxa = soc[,c(1, 3, 4)] # estem agafant només els que sí, els que no els obviem per ara
  noms = as.character(xarxa$noms[seq(1,nrow(xarxa),3)])
  gg <- graph.data.frame(xarxa[,c(2,3)], directed=T)
  gg <- simplify(gg, remove.multiple = F, remove.loops = T) 
  deg <- degree(gg, mode="all")
  
  adj = as_adjacency_matrix(gg, sparse = TRUE)
  
  # això és per pintar les relacions bidireccionals de blau (no és molt necessari)
  reci = rep(0, nrow(xarxa))
  xarxa$relacions = reci
  
  for (i in 1:length(noms)){
    for (j in 1:length(noms)){
      if (adj[i,j] == 1 & adj[j,i] == 1){
        xarxa$relacions[which(xarxa$num==i & xarxa$Feina_sí==j)] = 1
      }
    }
  }
  
  V(gg)$label <- as.character(noms)
  V(gg)$label.cex = 1
  V(gg)$label.font = 2
  V(gg)$size <- deg*3 + 1 
  
  # faig una paleta manual perquè l'igraph es lia amb la normal:
  color_academic = as.data.frame(mat[,38] - mat[,39])
  color_academic$noms = noms
  colnames(color_academic) = c("academic", "noms")
  
  color_academic$corregida = color_academic$academic- min(color_academic$academic) + 1
  paleta <- colorRampPalette(c("chartreuse3", 
                               "khaki1", 
                               "firebrick"))(n = max(color_academic$corregida))
  paleta = paste0(paleta, "90")   ## afegeixo transparència
  
  colors = rep("", length(noms))
  
  for (i in 1:length(noms)){
    colors[i] = paleta[color_academic$corregida[i]]
  }
  
  # participació
  
  participa_est = scale(mat[,40] - mat[,41])
  
  label.color = ifelse(participa_est > 1, "chartreuse3", ifelse(participa_est< -1, "firebrick", "black"))
  edge.color = ifelse(xarxa$relacions==1, "darkblue", "black")
  #vertex.shape = as.factor(soc$genere)
  vertex.shape = ifelse(as.character(soc$genere)=="nen", "square",
                        ifelse(as.character(soc$genere)=="nena", "circle", 
                               ifelse(as.character(soc$genere)=="altres", "raster", "crectangle")))
  
  return(list(gg, colors, label.color, vertex.shape))
  
}

calcs_xarxa_relacional = function(soc, mat){
  xarxa = soc[,c(1,3,6)]
  noms = as.character(xarxa$noms[seq(1,nrow(xarxa),3)])
  gg <- graph.data.frame(xarxa[,c(2,3)], directed=T)
  gg <- simplify(gg, remove.multiple = F, remove.loops = T) 
  deg <- degree(gg, mode="all")
  
  adj = as_adjacency_matrix(gg, sparse = TRUE)
  reci = rep(0, nrow(xarxa))
  xarxa$relacions = reci
  
  for (i in 1:length(noms)){
    for (j in 1:length(noms)){
      if (adj[i,j] == 1 & adj[j,i] == 1){
        xarxa$relacions[which(xarxa$num==i & xarxa$Amics==j)] = 1
      }
    }
  }
  
  V(gg)$label <- as.character(noms)
  V(gg)$label.cex = 1
  V(gg)$label.font = 2
  V(gg)$size <- deg*3 + 1
  
  dis_directe = cbind( mat[,10], mat[,12], mat[,14], mat[,16])
  dis_relacional = cbind( mat[,22], mat[,24], mat[,25], mat[,26])
  
  dis_total = rowSums(dis_relacional) + rowSums(dis_directe)
  agressivitat = as.data.frame(dis_total)
  agressivitat$noms = noms
  colnames(agressivitat) = c("disrupcio", "noms")
  
  agressivitat$corregida = agressivitat$disrupcio- min(agressivitat$disrupcio) + 1
  paleta <- colorRampPalette(c("chartreuse3", 
                               "khaki1", 
                               "firebrick"))(n = max(agressivitat$corregida))
  paleta = paste0(paleta, "90")   ## afegeixo transparència
  
  colors = rep("", length(noms))
  
  for (i in 1:length(noms)){
    colors[i] = paleta[agressivitat$corregida[i]]
  }
  
  estat_anim_total = - mat[,28] - mat[,29] + mat[,30] - mat[,31]
  estat_anim_est = scale(estat_anim_total)

  label.color = ifelse(estat_anim_est > 1, "chartreuse3", ifelse(estat_anim_est< -1, "firebrick", "black"))
  edge.color = ifelse(xarxa$relacions==1, "darkblue", "black")
  #vertex.shape = as.factor(soc$genere)
  vertex.shape = ifelse(as.character(soc$genere)=="nen", "square",
                        ifelse(as.character(soc$genere)=="nena", "circle", 
                               ifelse(as.character(soc$genere)=="altres", "raster", "crectangle")))
  
  return(list(gg, colors, label.color, vertex.shape))
}


crear_dict_preferencies = function(soc, numero_respostes, path_){
  prefs_dict = list()
  
  # A qui escull
  for (i in 1:round(nrow(soc)/numero_respostes)){
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]] = list("academic"=NULL, "relacional"=NULL)
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["academic"]] = preferencies(soc, i, "academic", 3)
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["relacional"]] = preferencies(soc, i, "relacional", 3)
  }
  
  # Per qui és escollit
  
  
  
  prefs_json = toJSON(prefs_dict, pretty = TRUE, auto_unbox = TRUE)
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0("preferencies.txt")), open = "wt", encoding = "UTF-8")
  sink(con)
  print(prefs_json)
  sink()
  close(con)
}


preferencies = function(soc, nen, area, numero_respostes){
  if (area == "academic"){
    area_col = 4
  }
  else {
    area_col = 6
  }
  prefs_positives = c()
  prefs_negatives = c()
  for (i in 1:numero_respostes){
    prefs_positives = c(prefs_positives, soc$noms[round(numero_respostes * soc[nen + i - 1, area_col])])
    prefs_negatives = c(prefs_negatives, soc$noms[round(numero_respostes * soc[nen + i - 1, area_col + 1])])
  }
  return(list("tries_positives" = prefs_positives, "tries_negatives" = prefs_negatives))
}

preferencies_inverses = function(soc, numero_respostes){
  
  prefs_dict = list()
  for (i in 1:round(nrow(soc)/numero_respostes)){
    list.append(as.character(soc$noms[1+numero_respostes*(i-1)]))
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["academic"]] = list("triat_positiu" = NULL, "triat_negatiu" = NULL)
    prefs_dict[[as.character(soc$noms[1+numero_respostes*(i-1)])]][["relacional"]] = list("triat_positiu" = NULL, "triat_negatiu" = NULL)
  }
  
  for (i in soc[, 4]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_positiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_positiu"]],as.character(soc$noms[i]))
  }
  for (i in soc[, 5]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_negatiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["academic"]][["triat_negatiu"]],as.character(soc$noms[i]))
  }
  for (i in soc[, 6]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_positiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_positiu"]],as.character(soc$noms[i]))
  }
  for (i in soc[, 7]){
    prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_negatiu"]]=c(prefs_dict[[soc$noms[1+numero_respostes*(i-1)]]][["relacional"]][["triat_negatiu"]],as.character(soc$noms[i]))
  }
  
  prefs_json = toJSON(prefs_dict, pretty = TRUE, auto_unbox = TRUE)
  options(encoding="UTF-8")
  con <- file(file.path(path_, paste0("preferencies_inverses.txt")), open = "wt", encoding = "UTF-8")
  sink(con)
  print(prefs_json)
  sink()
  close(con)
}
