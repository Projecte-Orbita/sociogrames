# Texts individual

# per ara són els mateixos que els col·lectius; aquí, per ara, només hi ha el text per agafar els gràfics, que és lleugermanet diferent

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

afegeix_grafic_individual = function(path_llista, nom_grafic, i){
  nom_grafic_barres = file.path(path_llista$figures, "individuals", 
                                paste0(nom_grafic, "-barres-", i, ".pdf"))
  nom_grafic_formatges = file.path(path_llista$figures, "individuals", 
                                  paste0(nom_grafic, "-formatges-", i, ".pdf"))
  cat(
    paste0(
      "
      \\begin{figure}[H]
      \\centering
      \\begin{subfigure}{.65\\textwidth}
      \\centering
      \\includegraphics[width=1\\linewidth]{", nom_grafic_barres, "}
      %\\caption{Diagrama de barres}
      \\label{fig:sub1}
      \\end{subfigure}%
      \\begin{subfigure}{.35\\textwidth}
      \\centering
      \\includegraphics[width=1\\linewidth]{", nom_grafic_formatges, "}
      %\\caption{Diagrama de formatges}
      \\label{fig:sub2}
      \\end{subfigure}
      %\\caption{Diagrama tal i qual}
      \\label{fig:test}
      \\end{figure}
      "
    )
    )
}

afegeix_grafic_resum = function(path_llista, i){
  
  # No s'està fent servir -> considerar moure'l a funcions obsoletes
  
  nom_grafic = file.path(path_llista$figures, "individuals", paste0("resum-", i, ".pdf"))
  cat(
    paste0(
      "
    \\begin{figure}[H]
			\\centering
			\\includegraphics[width=16cm]{", nom_grafic, "}
		\\end{figure}
    "
    )
  )
}


heading_alumnes <- function(nom){ cat("
\\documentclass[a4paper, 12pt, oneside]{article}
\\usepackage{framed}
\\usepackage[left=3cm,right=3cm,top=2cm]{geometry}
\\usepackage[sfdefault]{cabin}
\\usepackage{graphicx,longtable}
%\\usepackage[latin1]{inputenc}
\\usepackage{amsmath}
\\usepackage{color}
\\usepackage{multicol}
\\usepackage{flushend}
\\usepackage{balance}
\\usepackage{float}
%\\usepackage{subfig}
\\usepackage{subcaption}
\\usepackage{enumitem}
\\usepackage{titlesec}
\\usepackage[final]{pdfpages}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}   %paquet que serveix per poder escriure
%els accents de forma normal en Linux
%en Windows canvieu-ho per: \\usepackage[ansinew]{inputenc}
\\usepackage[catalan]{babel}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}
\\usepackage{pdfpages} % per poder posar la tapa en pdf
\\usepackage{tikz} % per pode posar el nom sobre la tapa
\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{../../informe-atom-peu}}
\\rfoot{\\small \\thepage}

%\\setlength\\parindent{0pt}
\\captionsetup[subfigure]{labelformat=empty}
\\fancyfootoffset[LO,LE]{2cm}
\\title{Informe de resultats del Test Àtom}
\\date{}
\\titleformat{\\chapter}[display]
{\\normalfont\\huge\\bfseries}{}{0pt}{\\Huge}
\\titlespacing*{\\chapter}
{0pt}{10pt}{40pt}

\\begin{document}
\\includepdf[pages=-,pagecommand={\\begin{tikzpicture}[remember picture,overlay]\\node [xshift = 0cm, yshift = 4.5cm] at (current page.center)  {\\textbf{\\huge{",nom,"}}};\\end{tikzpicture}}]{../../imatges_informes/Portada-resultats}

"
)
  }

titol_alumne = function(nom){
  cat("
      \\newpage
      
      \\begin{framed}
      \\textbf{", nom, "}
      \\end{framed}
      
      ", sep = "")}

disrupcio_ind = "
\\subsection*{Escala de Disrupció} 

Mesura el grau de disrupció que cada alumne causa a l’aula, segons la percepció dels companys, causat per 
l’\\textbf{agressivitat} física (agressions), verbal (comentaris negatius o insults) i relacional (evitar, ignorar o 
dir rumors sobre altres). 

"
prosocialitat_ind = "
\\subsection*{Escala de Prosocialitat o Cooperació}
Mesura el grau de prosocialitat de cada alumne, és a dir, la percepció dels companys de quin grau l’alumne 
\\textbf{ajuda} als seus companys i crea un bon clima social i escolar.
		
"
victimes_ind = "
\\subsection*{Escala de victimització}
		
Mesura el grau de victimització de cada alumne, és a dir, qui rep o \textbf{pateix conductes agressives} dels 
altres i en quin grau, segons la percepció dels companys. Considera la victimització física (rebre agressions), 
verbal (rebre comentaris negatius o insults) i relacional (ser evitat, ignorat o ser el centre de rumors). 

"
academic_ind = "
\\subsection*{Escala de valoració acadèmica}
		
Mesura la \\textbf{participació i implicació} dels alumnes a l’aula, així com els resultats de rendiment 
acadèmic, percebut pels companys de l’aula. Considera les variables: bones notes (alt rendiment acadèmic), 
males notes (baix rendiment acadèmic), participa (s’implica a la classe) i no participa (no s’implica a la 
classe).
"

estat_anim_ind = "
\\subsection*{Escala d'estat d'ànim percebut}

Mesura l’estat d’ànim dels alumnes segons la percepció dels companys de l’aula. Aquesta escala ens pot 
mostrar indicadors d’un entorn social no satisfactori, tant a nivell personal com escolar. Considera les 
variables: dissatisfacció (queixes), enuig (ràbia), alegria (content) i tristor (desànim).

"

caracter_ind = "
\\subsection*{Escala d’Actitud}
		
Mesura la percepció del grup del \\textbf{caràcter}, forma de ser o capacitat de resoldre els problemes de 
dels alumnes de l’aula. Considera les variables: lideratge (rol de líder o seguidor), autonomia (capacitat de 
gestionar els problemes o demanar ajuda) i socialització (es comunica sovint amb els altres o no).
"

estatus_sociometric_ind = "

\\subsection*{Estatus sociomètric}

En aquest apartat mesurem l'estatus social de cada alumne, tant directe (preguntant directament qui 
són els seus amics) com indirecte. Els resultats d'aquesta àrea s'obtenen a partir de les respostes 
dels alumnes a les següents preguntes:
"

resum = "
\\section*{Resum}

A continuació presentem de forma condensada un gràfic amb totes les dimensions mesurades en el sociograma:
"

afegeix_preferencies = function(rels, noms, i, numero_respostes, tipus){
  
  # Obsolet
  
  if (tipus == "academic"){
    titol = "Acadèmic"
    text11 = " ha triat positivament:"
    text12neg = "I no ha estat triat/da per ningú. \\\\ "
    text12pos = "Ha estat triat/da per:  "
    text21 = "I negativament:"
    text22neg = "I no ha estat triat/da negativament per ningú. \\\\ "
    text22pos = "Ha estat triat/da per:  "
    col_rel = 8
    col = 2
  }
  
  else if (tipus == "relacional"){
    titol = "Relacional"
    text11 = " ha triat positivament:"
    text12neg = "I no ha estat triat/da per ningú. \\\\ "
    text12pos = "Ha estat triat/da per:  "
    text21 = "I negativament:"
    text22neg = "I no ha estat triat/da negativament per ningú. \\\\ "
    text22pos = "Ha estat triat/da per:  "
    col_rel = 10
    col = 4
  }
  
  else if (tipus == "amical") {
    titol = "Amical"
    text11 = " ha triat que són amics o amigues:"
    text12neg = "En realitat, no ha estat triat/da per ningú. \\\\ "
    text12pos = "En realitat, ha estat triat/da per:  "
    text21 = "Ha triat que creu que el triaran a ell/a com a amistat:"
    text22neg = "I ningú creu que l'haurà triat. \\\\ "
    text22pos = "Ha estat triat/da com a amistat recíproca per:  "
    col_rel = 12
    col=6
  }
  
  noms = as.character(noms)
  pos = 1 + numero_respostes*(i-1)
  
  cat(paste0("\\subsubsection*{", titol, "}"))
  cat(paste0("En/na ", noms[i], text11))
  
  cat("\\begin{itemize}")
  k = pos
  for (j in 1:numero_respostes){
    
    nom = noms[rels[k, col]]
    if (is.na(nom)){
      k = k + 1
    }
    
    else {
    cat(ifelse(rels[k, col_rel], 
               paste0("\\item   \\textbf{", nom, "}"),  
               paste0("\\item  ", nom)))
    k = k + 1 
    }
  }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, col]==i)
  
  if (length(quins_triat)==0){
    cat(text12neg)
  }
  
  else {
    cat(text12pos)
    cat("\\begin{itemize}")
    
    for (element in quins_triat){

      cat(ifelse(rels[i, col_rel], 
                 paste0("\\item   \\textbf{", noms[rels[element, 1]], "}"),  
                 paste0("\\item  ", noms[rels[element, 1]])))
      }
    cat("\\end{itemize}")
  }
  
  cat(text21)
  cat("\\begin{itemize}")
  k = pos
  for (j in 1:numero_respostes){
    
    nom = noms[rels[k, col]]
    if (is.na(nom)){
      k = k + 1
    }
    
    else {
      cat(ifelse(rels[k, col_rel], 
                 paste0("\\item   \\textbf{", nom, "}"),  
                 paste0("\\item  ", nom)))
      k = k + 1 
    }
  }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, col + 1]==i)
  if (length(quins_triat)==0){
    cat(text22neg)
  }
  
  else {
    cat(text22pos)
    cat("\\begin{itemize}")
    for (element in quins_triat){

      cat(ifelse(rels[i, col_rel + 1], 
                 paste0("\\item   \\textbf{", noms[rels[element, 1]], "}"),  
                 paste0("\\item  ", noms[rels[element, 1]])))
    }
    cat("\\end{itemize}")
  }
  
}

escriure_preferencies = function(rels, noms, i, numero_respostes, tipus){
  
  if (tipus == "academic"){
    titol = "\\subsubsection*{Acadèmic}\n"
    text11 = "\nEn la pregunta \\emph{\"Amb qui faries un treball?\"}: \\\\"
    text21 = "\nEn la pregunta \\emph{\"Amb qui no faries un treball?\"}: \\\\"
    col_rel = 8
    col = 2
  }
  
  else if (tipus == "relacional"){
    titol = "\\subsubsection*{Relacional}\n"
    text11 = "\nEn la pregunta \\emph{\"Amb qui jugaries al pati?\"}: \\\\"
    text21 = "\nEn la pregunta \\emph{\"Amb qui no jugaries al pati?\"}: \\\\"
    col_rel = 10
    col = 4
  }
  
  else if (tipus == "amical") {
    titol = "\\subsubsection*{Amical}\n"
    text11 = "\nEn la pregunta \\emph{\"Qui són els teus amics?\"}: \\\\"
    text21 = "\nEn la pregunta \\emph{\"Qui creus qeu et triarà com a amic/ga?\"}: \\\\"
    col_rel = 12
    col=6
  }
  
  noms = as.character(noms)
  pos = 1 + numero_respostes*(i-1)
  k = pos
  
  noms_fora = c()
  for (j in 1:numero_respostes){
    
    noms_fora = c(noms_fora, noms[rels[k, col]])
    k = k + 1
  }
  
  quins_triat = which(rels[, col]==i)
  
  noms_dins = c(NA)  # És lleugerament diferent per tenir en compte el cas on no hi ha res.
  for (element in quins_triat){
    i = 1
    noms_dins[i] = noms[rels[element, 1]]
    i = i + 1
  }
  
  n = max(numero_respostes, length(noms_dins))
  length(noms_fora) = n
  length(noms_dins) = n
  
  df_taula = cbind.data.frame(noms_fora, noms_dins)
  df_taula = apply(df_taula, 2, as.character)
  df_taula[is.na(df_taula)] = " "

  cat(titol)
  cat(text11)
  taula_preferencies(df_taula)

  noms_fora = c()
  for (j in 1:numero_respostes){
    
    noms_fora = c(noms_fora, noms[rels[k, col+1]])
    k = k + 1
  }
  
  quins_triat = which(rels[, col+1]==i)
  
  noms_dins = c(NA)
  for (element in quins_triat){
    i = 1
    noms_dins[i] = noms[rels[element, 1]]
    i = i + 1
  }
  
  n = max(numero_respostes, length(noms_dins))
  length(noms_fora) = n
  length(noms_dins) = n
  
  df_taula = cbind.data.frame(noms_fora, noms_dins)
  df_taula = apply(df_taula, 2, as.character)
  df_taula[is.na(df_taula)] = " "
  
  cat(text21)
  taula_preferencies(df_taula)

}