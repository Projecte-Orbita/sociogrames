# Orientacions

# Creació de les orientacions

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

config = config::get()
encoding_ = config$encoding

options(encoding = encoding_)
# Imports

orientacions_disrupcio = function(nom, color_A, color_B, Disrupcio){
  
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
    l_temp['B_dolenta'] = paste0("\\item El/la ", nom, " és valorat/da per una part important dels seus companys com a disruptiu/va. Cal que el/la mestre/a estableixi mesures per entendre millor aquesta dinàmica negativa i, posteriorment, intervencions per compensar-la.")
  }
  
  if (fisica){
    l_temp['tipus_max'] = paste0("Alguns dels seus companys valoren el/la ", nom, " com a disruptiu/va. Cal que el/la mestre/a interpreti si aquestes tries poden respondre a factors circumstancials o a una dinàmica negativa real dins el grup-classe.  Cal que el/la mestre/a estableixi mesures per entendre millor aquesta dinàmica negativa i, si es considera necessari, intervencions per compensar-la.")
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

orientacions_victimes = function(nom, color_A, color_B, victimes){
  
  ratio = victimes[4]/sum(victimes[2:3])
  fisica = victimes[2] > max(victimes[3:4])
  
  l_temp = list()
  if (color_B > 1) {
    l_temp['B_dolenta'] = paste0("\\item El/la ", nom, " és valorat/da per una part important dels seus companys com a víctima. Cal que el/la mestre/a estableixi mesures per entendre millor aquesta dinàmica negativa i, posteriorment, intervencions per compensar-la")
  }
  
  #else if (color_B < -1){
  #  l_temp['B_bona'] = "\\item No és considerat/da per la major part dels seus companys com a víctima de rebuig i/o exclusió."
  #}
  # Tret segons excel de modificacions Sociogramav2
  
  else if (fisica) {
    l_temp['B_fisica'] = "\\item Alguns dels seus companys valoren el/la com a víctima. Cal que el/la mestre/a interpreti si aquestes tries poden respondre a factors circumstancials o a una dinàmica negativa real dins el grup-classe. Cal que el/la mestre/a estableixi mesures per entendre millor aquesta dinàmica negativa i, posteriorment i si es considera necessari, intervencions per compensar-la"
  }
  
  #if (ratio > .5){
  #  l_temp['ratio'] = "\\item La major part dels seus companys valoren que rep una victimització explícita (verbal o física)."
  #}
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

# orientacions_academic = function(nom, color_A, color_B) {
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

orientacions_estat_anim = function(nom, color_A, color_B){
  l_temp = list()
  if (color_A > 1) {
    l_temp['A_bona'] = paste0("\\item El/la ", nom, " és valorat/da per una part important dels seus companys com a trist/a, enfadat/da o...  FALTA ")
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

orientacions_caracter = function(nom, color_A, color_B, caracter){
  
  #caracter = scale(caracter[, -1])
  
  l_temp = list()
  if (caracter[2] > 1) {
    l_temp['seguidor'] = paste0("\\item El/la ", nom, " és valorat/da per una part important dels seus companys com a seguidor/a. Cal que el/la mestre/a li doni oportunitats per desenvolupar els recursos suficients (comportament assertiu, autoestima...) i promogui situacions en què es tinguin en compte les seves opinions.")
  }
  if (caracter[4] > 1) {
    l_temp['dependent'] = paste0("\\item El/la ", nom, " és valorat/da per una part important dels seus companys com a dependent. Cal que el/la mestre/a li doni oportunitats per desenvolupar els recursos suficients (hàbits, estratègies de resolució de problemes...) i la confiança per prendre decisions.")
  }
  if (caracter[6] > 1) {
    l_temp['aillat'] = paste0("\\item El/la ", nom, " és valorat/da per una part important dels seus companys com a poc participatiu/va. Cal que el/la mestre/a li doni oportunitats per desenvolupar els recursos suficients (habilitats socials, confiança en sí mateix i/o en el grup, vincle social...) i fomenti situacions que el/la portin a una major implicació i participació dins l'aula.")
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

orientacions_mapa_social = function(nom, color_A, color_B, disrupcio){
  
  l_temp = list()
  if (disrupcio[1] > 1){
    l_temp['popular'] = paste0("\\item El/la ", nom, " té una influència positiva dins el grup que pot ser utilitzada pel mestre per tal d'afavorir una millor dinàmica social en els grups en què participi.")
  }
  if (disrupcio[2] > 1){
    l_temp['rebutjat'] = paste0("\\item El/la ", nom, " està rebent una gran quantitat d'atenció negativa i cal que el mestre observi si té els recursos relacionals suficients i dilueixi la seva importància dins el grup i/o li ofereixi oportunitats per rebre atenció positiva.")
  }
  if (disrupcio[6] > 1){
    l_temp['controvers'] = paste0("\\item El/la ", nom, " té una influència acadèmica positiva dins el grup que pot ser utilitzada pel mestre per d'afavorir una millor dinàmica de treball en els grups en què participi. Cal tenir també en compte que mostri una bona adaptació social i no centri l'atenció exclusivament per la seva competència escolar")
  }
  if (disrupcio[6] < -1){
    l_temp['negligit'] = paste0("\\item El/la ", nom, " genera relacions polars entre els seus companys. Cal aprofitar les habilitats de relació que el porten a establir vincles amb el grup per fomentar relacions positives i reduïr les relacions negatives que estableix amb altres companys.")
  }

  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

orientacions_mapa_academic = function(nom, color_A, color_B, academic){

  l_temp = list()
  if (academic[1] > 1){
    l_temp['popular'] = paste0("El/la ", nom, " té una influència acadèmica positiva dins el grup que pot ser utilitzada pel mestre per d'afavorir una millor dinàmica de treball en els grups en què participi. Cal tenir també en compte que mostri una bona adaptació social i no centri l'atenció exclusivament per la seva competència escolar.")
  }
  if (academic[2] > 1){
    l_temp['rebutjat'] = paste0("El/la ", nom, " està rebent una gran quantitat d'atenció negativa respecte a la seva baixa competència acadèmica i cal que el mestre observi si té els recursos suficients (tècniques d'estudi, planificació i organització, hàbits de treball...) i se li ofereixi reforç positiu en les situacions d'èxit escolar.")
  }
  if (academic[5] > 1){
    l_temp['controvers'] = paste0("El/la ", nom, " genera opinions polars entre els seus companys respecte a la seva competència acadèmica. Cal aprofitar les habilitats de relació que el porten a establir contextos positius de treball per fomentar-los i reduïr les l'impacte que tenen situacions acadèmiques negatives.")
  }
  if (academic[5] < -1){
    l_temp['negligit'] = paste0("El/la ", nom, " està rebent molt poca atenció per part del grup respecte a la seva competència acadèmica i cal que es promoguin situacions que permetin al grup constatar les seves habilitats escolars.")
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}

orientacions_estatus = function(nom, color_A, color_B) {
  l_temp = list()
  if (color_A > 1) {
    l_temp['A_bona'] = paste0("\\item El/la ", nom, "  ha rebut un nombre alt de tries favorables per part dels seus companys, fet que ens informa que es tracta d'una persona que exerceix una popularitat positiva dins el grup-classe.")
  }
  else if (color_A < -1) {
    l_temp['A_dolenta'] = paste0("\\item El/la ", nom, " ha rebut un nombre baix de tries favorables per part dels seus companys, fet que ens informa que es tracta d'una persona poc popular dins el grup-classe.")
  }
  
  if (color_B > 1) {
    l_temp['B_dolenta'] = paste0("\\item El/la ", nom, " ha rebut un nombre alt de tries desfavorables per part dels seus companys, fet que ens informa que es tracta d'una persona que exerceix una popularitat negativa dins el grup-classe.")
  }
  else if (color_B < -1) {
    l_temp['B_bona'] = paste0("\\item El/la ", nom, " ha rebut un nombre baix de tries desfavorables per part dels seus companys, per tant no es tracta d'una persona impopular dins el grup-classe.")
  }
  
  return(ifelse(length(l_temp) > 0, list(l_temp), NA))
}
