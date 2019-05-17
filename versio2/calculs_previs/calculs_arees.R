Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

# Aquí hi afegeixo els càlculs per cada una de les àrees. Per ara els poso per separat però segurament en un 
# futur proper s'hauran d'ajuntar en una sola funció. A causa de les diferències entre tots els grups no està
# clar com es faria això, i per tant ho començo fent separat
require(reshape2)
require(igraph)
require(rlist)

gwd = getwd()
source(file.path(gwd, 'altres', 'utils.R'), encoding = encoding_)

calcs_disrupcio = function(mat, noms){
  
  prosocialitat = cbind(mat[,7], mat[,8], mat[,9], mat[,18])
  prosocialitat_total = rowSums(prosocialitat)
  
  dis_directe = cbind( mat[,10], mat[,12], mat[,14], mat[,16])
  dis_relacional = cbind( mat[,22], mat[,24], mat[,25], mat[,26])
  
  comportament_total = prosocialitat_total - rowSums(dis_relacional) - rowSums(dis_directe)
  
  Disrupcio = cbind.data.frame(prosocialitat_total, 
                               dis_directe[,4], 
                               rowSums(dis_directe[,1:3]), 
                               rowSums(dis_relacional),
                               comportament_total)
  rownames(Disrupcio) = noms
  colnames(Disrupcio) = c("Prosocialitat", 
                          "Disrupció física",
                          "D. verbal",
                          "D. relacional",
                          "Global")
  Disrupcio$noms = as.factor(rownames(Disrupcio))
  
  # Ara en fem un posant 1 en els que són significativament liantes
  prosocialitat_total_est = as.vector(scale(prosocialitat_total))
  disrupcio_total_est = as.vector(scale(rowSums(dis_directe + dis_relacional)))
  
  Disrupcio_sino = ifelse(disrupcio_total_est > 1 | prosocialitat_total_est > 1, 1, 0)
  
  return(list(Disrupcio, Disrupcio_sino))
}

calcs_prosocialitat = function(mat, noms){
  # Aquí hi ha un cacau impressionant
  # No s'està fent servir perquè s'ha ajuntat amb disrupció -> considerar eliminar
  
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
  
  Victimitzacio = cbind(victima_total, 
                        victima_directe[,4], 
                        rowSums(victima_directe[,c(1:3, 5)]), 
                        victima_relacional_total)
  
  colnames(Victimitzacio) = c("Total víctima", 
                              "Víctima física", 
                              "Víctima verbal",
                              "Víctima relacional")
  
  Vict_sino = ifelse(victima_est > 1 | victima_est < -1, 1, 0)
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
  academic_sino = ifelse(academic_total_est < -1 | academic_total_est > 1,1,0)
  
  academic[,3] = - academic[,3]
  academic[,4] = - academic[,4]
  
  academic$noms = factor(noms, levels = as.character(noms))
  
  return(list(academic, academic_sino))
}

calcs_estat_anim = function(mat, noms){
  estat_anim_total = -1 * (- mat[,28] - mat[,29] + mat[,30] - mat[,31])
  estat_anim = as.data.frame(cbind(mat[,28], mat[,29], mat[,30], mat[,31], estat_anim_total))
  colnames(estat_anim) = c("Dissatisfacció", "Enuig", "Alegria", "Tristor", "Global")
  
  estat_anim_bo_est = scale(mat[,30])
  estat_anim_dolent_est = as.vector(scale(rowSums(estat_anim[, c(1,2,4)])))
  estat_anim_total_est = as.vector(scale(estat_anim_total))
  estat_anim_sino = ifelse(estat_anim_dolent_est > 1 | estat_anim_bo_est > 1,1,0)
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
  
  caracter_bo_est = as.vector(scale(rowSums(caracter[, c(1,3,5)])))
  caracter_dolent_est = as.vector(scale(rowSums(caracter[, c(2,4,6)])))
  caracter_sino = ifelse(caracter_dolent_est > 1 | caracter_bo_est > 1,1,0)
  caracter[,2] = -caracter[,2]
  caracter[,4] = -caracter[,4]
  caracter[,6] = -caracter[,6]
  caracter$noms = factor(noms, levels = as.character(noms))
  
  return(list(caracter, caracter_sino))
}

calcs_estatus = function(mat){
  estatus = cbind.data.frame(mat[,1:6],mat[,19], mat[,26], mat[,27])
  names(estatus) = c("Agrada", "No agrada", 
                     "Agrada deures", "No agrada deures", 
                     "Amic", "Amic recíproc", 
                     "Defensa", "Té amics", "Juga sol")
  estatus_est = scale(estatus)
  estatus_sino = ifelse(estatus_est > 1 | estatus_est < -1, 1, 0)
  
  return(list(estatus, estatus_sino))
}

calcs_xarxa_academica = function(soc, mat, num_respostes){
  
  # TODO: ajuntar les dues o tres xarxes
  
  options(encoding=encoding_)
  xarxa = soc[,c(1, 3, 4)] # estem agafant només els que sí, els que no els obviem per ara
  noms = as.character(xarxa$noms[seq(1,nrow(xarxa),num_respostes)])
  gg <- graph.data.frame(xarxa[,c(2,3)], directed=T)
  gg <- simplify(gg, remove.multiple = F, remove.loops = T) 
  deg <- degree(gg, mode="all")
  
  edge.color = calcular_reciproc(xarxa[,2:3], num_respostes)
  
  V(gg)$label <- as.character(noms)
  V(gg)$label.cex = 1
  V(gg)$label.font = 2
  V(gg)$size = deg*3 + 1
  vertex.shape = ifelse(as.character(soc$genere)=="nen", "square",
                        ifelse(as.character(soc$genere)=="nena", "circle", 
                               ifelse(as.character(soc$genere)=="altres", "vrectangle", "crectangle")))
  vertex.shape = vertex.shape[seq(1, length(vertex.shape), num_respostes)]
  #V(gg)$shape = vertex.shape
  
  # faig una paleta manual perquè l'igraph es lia amb la normal:
  color_academic = as.data.frame(mat[,38] - mat[,39])
  color_academic$noms = noms
  colnames(color_academic) = c("academic", "noms")
  
  color_academic$corregida = color_academic$academic- min(color_academic$academic) + 1
  paleta <- colorRampPalette(c("firebrick", 
                               "khaki1", 
                               "chartreuse3"))(n = max(color_academic$corregida))
  paleta = paste0(paleta, "90")   ## afegeixo transparència
  
  colors = rep("", length(noms))
  
  for (i in 1:length(noms)){
    colors[i] = paleta[color_academic$corregida[i]]
  }
  
  # participació
  
  participa_est = scale(mat[,40] - mat[,41])
  
  label.color = ifelse(participa_est > 1, "chartreuse3", ifelse(participa_est< -1, "firebrick", "black"))
  edge.color = ifelse(edge.color==1, adjustcolor("darkblue", alpha.f = .5), adjustcolor("black", alpha.f = .5))

  #vertex.shape = vertex.shape[seq(1, length(vertex.shape), num_respostes)]
  
  return(list(gg, colors, label.color, vertex.shape, edge.color))
  
}

calcs_xarxa_relacional = function(soc, mat, num_respostes){
  options(encoding=encoding_)
  xarxa = soc[,c(1,3,6)]
  noms = as.character(xarxa$noms[seq(1,nrow(xarxa),num_respostes)])
  gg <- graph.data.frame(xarxa[,c(2,3)], directed=T)
  gg <- simplify(gg, remove.multiple = F, remove.loops = T) 
  deg <- degree(gg, mode="all")
  
  edge.color = calcular_reciproc(xarxa[,2:3], num_respostes)
  
  V(gg)$label <- as.character(noms)
  V(gg)$label.cex = 1
  V(gg)$label.font = 2
  V(gg)$size <- deg*3 + 1
  vertex.shape = ifelse(as.character(soc$genere)=="nen", "square",
                        ifelse(as.character(soc$genere)=="nena", "circle", 
                               ifelse(as.character(soc$genere)=="altres", "vrectangle", "crectangle")))
  vertex.shape = vertex.shape[seq(1, length(vertex.shape), num_respostes)]
  #V(gg)$shape = vertex.shape
  
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
  edge.color = ifelse(edge.color==1, adjustcolor("darkblue", alpha.f = .5), adjustcolor("black", alpha.f = .5))
  
  return(list(gg, colors, label.color, vertex.shape, edge.color))
}

calcs_xarxa_amical = function(soc, mat, num_respostes){
  options(encoding=encoding_)
  xarxa = soc[,c(1,3,8)]
  noms = as.character(xarxa$noms[seq(1,nrow(xarxa),num_respostes)])
  gg <- graph.data.frame(xarxa[,c(2,3)], directed=T)
  gg <- simplify(gg, remove.multiple = F, remove.loops = T) 
  deg <- degree(gg, mode="all")
  
  edge.color = calcular_reciproc(xarxa[,2:3], num_respostes)
  
  V(gg)$label <- as.character(noms)
  V(gg)$label.cex = 1
  V(gg)$label.font = 2
  V(gg)$size <- deg*3 + 1
  vertex.shape = ifelse(as.character(soc$genere)=="nen", "square",
                        ifelse(as.character(soc$genere)=="nena", "circle", 
                               ifelse(as.character(soc$genere)=="altres", "vrectangle", "crectangle")))
  vertex.shape = vertex.shape[seq(1, length(vertex.shape), num_respostes)]
  #V(gg)$shape = vertex.shape
  
  estatus = cbind.data.frame(mat[,1:6],mat[,19], mat[,26], mat[,27])
  est_total = rowSums(estatus)
  est_total$noms = noms
  
  colnames(est_total) = c("estatus", "noms")
  
  est_total$corregida = est_total$estatus- min(est_total$estatus) + 1
  paleta <- colorRampPalette(c("chartreuse3", 
                               "khaki1", 
                               "firebrick"))(n = max(est_total$corregida))
  paleta = paste0(paleta, "90")   ## afegeixo transparència
  
  colors = rep("", length(noms))
  
  for (i in 1:length(noms)){
    colors[i] = paleta[est_total$corregida[i]]
  }
  
  estat_anim_total = - mat[,28] - mat[,29] + mat[,30] - mat[,31]
  estat_anim_est = scale(estat_anim_total)
  
  label.color = ifelse(estat_anim_est > 1, "chartreuse3", 
                       ifelse(estat_anim_est< -1, "firebrick", 
                              "black"))
  
  edge.color = ifelse(edge.color==1, adjustcolor("darkblue", alpha.f = .5), adjustcolor("black", alpha.f = .5))
  
  return(list(gg, colors, label.color, vertex.shape, edge.color))
}

calcular_preferencies = function(soc, path_, numero_respostes){
  
  # Creem un dataframe on guardarem totes les relacions i el guardem perquè sigui utilitzat pels informes
  
  relacions = soc[, 3:9]
  
  # Acadèmic positiu
  relacions$aca_pos = calcular_reciproc(relacions[, 1:2], numero_respostes)
  relacions$aca_neg = calcular_reciproc(relacions[, 1:3], numero_respostes)
  relacions$rel_pos = calcular_reciproc(relacions[, 1:4], numero_respostes)
  relacions$rel_neg = calcular_reciproc(relacions[, 1:5], numero_respostes)
  relacions$amic_pos = calcular_reciproc(relacions[, 1:6], numero_respostes)
  relacions$amic_rec = calcular_reciproc(relacions[, 1:7], numero_respostes)
  
  write.csv(relacions, file.path(path_, "relacions.csv"), row.names = F)
}
