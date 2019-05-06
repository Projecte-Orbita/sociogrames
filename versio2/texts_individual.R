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
\\includepdf[pages=-,pagecommand={\\begin{tikzpicture}[remember picture,overlay]\\node [xshift = 0cm, yshift = 4.5cm] at (current page.center)  {\\textbf{\\huge{",nom,"}}};\\end{tikzpicture}}]{../../Portada-resultats}

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
  
  if (tipus == "academic"){
    titol = "Acadèmic"
    col_rel = 8
    col = 2
  }
  
  else if (tipus == "relacional"){
    titol = "Relacional"
    col_rel = 10
    col = 4
  }
  
  else if (tipus == "amical") {
    titol = "Amical"
    col_rel = 12
    col=6
  }
  
  noms = as.character(noms)
  pos = 1 + numero_respostes*(i-1)
  
  cat(paste0("\\subsubsection*{", titol, "}"))
  cat(paste0("En/na ", noms[i]), " ha triat positivament a:")
  
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
    cat("I no ha estat triat/da per ningú. \\\\ ")
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    
    for (element in quins_triat){

      cat(ifelse(rels[i, col_rel], 
                 paste0("\\item   \\textbf{", noms[rels[element, 1]], "}"),  
                 paste0("\\item  ", noms[rels[element, 1]])))
      }
    cat("\\end{itemize}")
  }
  
  cat(paste0("I negativament a:"))
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
    cat("I no ha estat triat/da negativament per ningú. \\\\ ")
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (element in quins_triat){

      cat(ifelse(rels[i, col_rel + 1], 
                 paste0("\\item   \\textbf{", noms[rels[element, 1]], "}"),  
                 paste0("\\item  ", noms[rels[element, 1]])))
    }
    cat("\\end{itemize}")
  }
  
}


