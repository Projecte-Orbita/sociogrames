# Creació de les valoracions

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

options(encoding = encoding_)
# Imports

valoracions_disrupcio = function(nom, color_A, color_B, Disrupcio){
  
  tipus_max = strsplit(names(which.max(Disrupcio[, 3:5])), " ")[[1]][2]
  # tipus_max pot un dels strings "física", "verbal" o "relacional".

  # ratio = Disrupcio[3]/sum(Disrupcio[4:5])  # obsolet
  fisica = Disrupcio[3] > max(Disrupcio[4:5])

  l_temp = list()
  if (color_A > 1){
    l_temp['A_bona'] = "\\item És valorat/da per la major part dels seus companys com a afavoridor/a de relacions socials positives."
  }
  #else if (color_A < -1){
  #  l_temp['A_dolenta'] = "\\item No és valorat/da per part dels seus companys com a facilitador/a de relacions socials positives."
  #}
  # Tret segons modificacions excel
  
  if (color_B > 1){
    l_temp['B_dolenta'] = "\\item És valorat/da per la major part dels seus companys com a disruptiu/va."
    l_temp['tipus_max'] = paste0("\\item La principal disrupció que causa és de tipus ", tipus_max, ".")
  }
  #else if (color_B < -1){
  #  l_temp['B_bona'] = "\\item No és valorat/da per part dels seus companys com a disruptiu/va."
  #}

  #if (ratio > .5){
  #  l_temp['ratio'] = "\\item La major part dels seus companys valoren que exerceix una disrupció explícita (verbal o física)."
  #}

  else if (fisica) {
    l_temp['B_fisica'] = "\\item Tot i que no s'obtenen puntuacions significatives en aquesta escala, cal tenir en compte que és valorat/da per alguns dels seus companys com a disruptiu."
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

valoracions_victimes = function(nom, color_A, color_B, victimes){
  
  ratio = victimes[4]/sum(victimes[2:3])
  fisica = victimes[2] > max(victimes[3:4])
  
  l_temp = list()
  if (color_B > 1) {
    l_temp['B_dolenta'] = "\\item És considerat/da per la major part dels seus companys com a víctima de rebuig i/o exclusió."
  }
  
  #else if (color_B < -1){
  #  l_temp['B_bona'] = "\\item No és considerat/da per la major part dels seus companys com a víctima de rebuig i/o exclusió."
  #}
  # Tret segons excel de modificacions Sociogramav2
  
  else if (fisica) {
    l_temp['B_fisica'] = "\\item Tot i que no s'obtenen puntuacions significatives en aquesta escala, cal tenir en compte que és valorat/da per alguns dels seus companys com a víctima."
  }
  
  #if (ratio > .5){
  #  l_temp['ratio'] = "\\item La major part dels seus companys valoren que rep una victimització explícita (verbal o física)."
  #}
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

# valoracions_academic = function(nom, color_A, color_B) {
# Obsolet

#   l_temp = list()
#   if (color_A > 1) {
#     l_temp['A_bona'] = "\\item És valorat/da per la major part dels seus companys com a competent acadèmicament."
#   }
#   #else if (color_A < -1){
#   #  l_temp['A_dolenta'] = "\\item No és valorat/da per part dels seus companys com a competent acadèmicament."
#   #}
#   
#   if (color_B > 1) {
#     l_temp['B_dolenta'] = "\\item És valorat/da per la major part dels seus companys com a poc competent acadèmicament."
#   }
#   #else if (color_B < -1){
#   #  l_temp['B_bona'] = "\\item No és valorat/da per part dels seus companys com a poc competent acadèmicament."
#   #}
#   
#   return(ifelse(length(l_temp) > 0, list(l_temp), NA))
# }

valoracions_estat_anim = function(nom, color_A, color_B){
  l_temp = list()
  if (color_A > 1) {
    l_temp['A_bona'] = "\\item La major part dels seus companys opina que predominantment mostra un estat d'ànim positiu (d'alegria)."
  }
  #else if (color_A < -1){
  #  l_temp['A_dolenta'] = "\\item La major part dels seus companys opina que predominantment \\emph{no} mostra un estat d'ànim positiu (d'alegria)."
  #}
  
  if (color_B > 1) {
    l_temp['B_dolenta'] = "\\item La major part dels seus companys opina que predominantment mostra un estat d'ànim negatiu (de tristesa, enuig o dissatisfacció)."
  }
  #else if (color_B < -1){
  #  l_temp['B_bona'] = "\\item La major part dels seus companys no opina que predominantment mostri un estat d'ànim negatiu (de tristesa, enuig o dissatisfacció)."
  #}
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

valoracions_caracter = function(nom, color_A, color_B, caracter){
  
  # Aquest és una mica diferent perquè volem cada un dels sub-apartats per separat.
  # caracter = scale(caracter[, -1])
  
  l_temp = list()
  if (caracter[1] > 1) {
    l_temp['lider'] = "\\item La major part dels seus companys opina que té una elevada capacitat de lideratge."
  }
  if (caracter[2] > 1) {
    l_temp['seguidor'] = "\\item La major part dels seus companys opina que tendeix a mostrar conductes de seguidor"
  }
  if (caracter[3] > 1) {
    l_temp['autonom'] = "\\item La major part dels seus companys opina que té una elevada capacitat d'autonomia."
  }
  if (caracter[4] > 1) {
    l_temp['dependent'] = "\\item La major part dels seus companys opina que tendeix a mostrar conductes de dependència."
  }
  if (caracter[5] > 1) {
    l_temp['sociable'] = "\\item La major part dels seus companys opina que té una elevada capacitat de socialització."
  }
  if (caracter[6] > 1) {
    l_temp['aillat'] = "\\item La major part dels seus companys opina que té una elevada capacitat de socialització."
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

# valoracions_estatus = function(nom, color_A, color_B) {
#   
#   # Obsolet
#   
#   l_temp = list()
#   if (color_A > 1) {
#     l_temp['A_bona'] = paste0("\\item El/la ", nom, "  ha rebut un nombre alt de tries favorables per part dels seus companys, fet que ens informa que es tracta d'una persona que exerceix una popularitat positiva dins el grup-classe.")
#   }
#   else if (color_A < -1) {
#     l_temp['A_dolenta'] = paste0("\\item El/la ", nom, " ha rebut un nombre baix de tries favorables per part dels seus companys, fet que ens informa que es tracta d'una persona poc popular dins el grup-classe.")
#   }
#   
#   if (color_B > 1) {
#     l_temp['B_dolenta'] = paste0("\\item El/la ", nom, " ha rebut un nombre alt de tries desfavorables per part dels seus companys, fet que ens informa que es tracta d'una persona que exerceix una popularitat negativa dins el grup-classe.")
#   }
#   else if (color_B < -1) {
#     l_temp['B_bona'] = paste0("\\item El/la ", nom, " ha rebut un nombre baix de tries desfavorables per part dels seus companys, per tant no es tracta d'una persona impopular dins el grup-classe.")
#   }
#   
#   return(ifelse(length(l_temp) > 0, list(l_temp), NA))
# }

valoracions_mapa_social = function(nom, color_A, color_B, disrupcio){
  
  # L'eix Controvers-negligit és una combinació dels anteriors; el calculem:
  
  disrupcio$con_neg = sqrt(disrupcio[1]^2+disrupcio[2]^2)
  
  #disrupcio[, -1] = scale(disrupcio[, -1])
  
  l_temp = list()
  if (disrupcio[1] > 1){
    l_temp['popular'] = "Els companys el/la tenen molt en compte dins el grup i l'escullen per relacionar-s'hi."
  }
  if (disrupcio[2] > 1){
    l_temp['rebutjat'] = "Els companys el/la tenen poc en compte dins el sistema de relacions del grup."
  }
  if (disrupcio$con_neg > 1){
    l_temp['controvers'] = "Els companys el/la tenen molt en compte dins el grup i alguns l'escullen per relacionar-s'hi i altres per excloure'l."
  }
  if (disrupcio$con_neg < -1){
    l_temp['negligit'] = "Els companys el/la tenen molt en compte dins el grup però l'exclouen."
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

valoracions_mapa_academic = function(nom, color_A, color_B, academic){
  
  # L'eix Controvers-negligit és una combinació dels anteriors; el calculem:
  
  academic$con_neg = sqrt(academic[1]^2+academic[2]^2)
  
  #academic[, -1] = scale(academic[, -1] )
  
  l_temp = list()
  if (academic[1] > 1){
    l_temp['popular'] = "Els companys el/la tenen molt en compte dins del grup respecte a la seva alta competència acadèmica."
  }
  if (academic[2] > 1){
    l_temp['rebutjat'] = "Els companys el/la tenen poc en compte dins el grup respecte a la seva competència acadèmica."
  }
  if (academic$con_neg > 1){
    l_temp['controvers'] = "Els companys el/la tenen molt en compte dins el grup i alguns el valoren amb baixa i altres amb alta competència acadèmica."
  }
  if (academic$con_neg < -1){
    l_temp['negligit'] = "Els companys el/la tenen molt en compte dins el grup respecte a la seva baixa competència acadèmica."
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}