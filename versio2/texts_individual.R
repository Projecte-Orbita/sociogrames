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

estatus_sociometric = "

\\section*{Estatus sociomètric}

En aquest apartat mesurem l'estatus social de cada alumne, tant directe (preguntant directament qui 
són els seus amics) com indirecte. Els resultats d'aquesta àrea s'obtenen a partir de les respostes 
dels alumnes a les següents preguntes:
"

resum = "
\\section*{Resum}

A continuació presentem de forma condensada un gràfic amb totes les dimensions mesurades en el sociograma:
"


afegeix_preferencies = function(rels, noms, i, numero_respostes){
  
  noms = as.character(noms)
  pos = 1 + numero_respostes*(i-1)
  
  cat("\\subsubsection*{Acadèmic} ")
  cat(paste0("En/na ", noms[i]), " ha triat positivament a:")
  
  cat("\\begin{itemize}")
  for (j in 1:numero_respostes){
    k = pos
    cat(ifelse(rels[k, 8], 
               paste0("\\item   \\textbf{", noms[rels[k, 2]], "}"),  
               paste0("\\item  ", noms[rels[k, 2]])))
    k = k + 1 
    }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, 2]==i)
  
  if (length(quins_triat)==0){
    "I no ha estat triat/da per ningú. \\\\ "
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (j in 1:length(quins_triat)){
      k = pos
      cat(ifelse(rels[k, 8], 
                 paste0("\\item   \\textbf{", noms[rels[k, 1]], "}"),  
                 paste0("\\item  ", noms[rels[k, 1]])))
    k = k + 1
      }
    cat("\\end{itemize}")
  }
  
  cat(paste0(" I negativament a:"))
  cat("\\begin{itemize}")
  
  for (j in 1:numero_respostes){
    k = pos
    cat(ifelse(rels[k, 9], 
               paste0("\\item   \\textbf{", noms[rels[k, 3]], "}"), 
               paste0("\\item   ", noms[rels[k, 3]])))
    k = k + 1
  }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, 3]==i)
  if (length(quins_triat)==0){
    "I no ha estat triat/da per ningú. \\\\ "
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (j in 1:length(quins_triat)){
      k = pos
      cat(ifelse(rels[k, 9], 
                 paste0("\\item   \\textbf{", noms[rels[k, 1]], "}"),  
                 paste0("\\item  ", noms[rels[k, 1]])))
      k = k + 1
    }
    cat("\\end{itemize}")
  }
  
      
  cat("\\subsubsection*{Relacional} ")
  cat(paste0("En/na ", noms[i]), " ha triat positivament a:")
  
  cat("\\begin{itemize}")
  for (j in 1:numero_respostes){
    k = pos
    cat(ifelse(rels[k, 10], 
               paste0("\\item   \\textbf{", noms[rels[k, 3]], "}"),  
               paste0("\\item  ", noms[rels[k, 3]])))
    k = k + 1
    }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, 4]==i)
  
  if (length(quins_triat)==0){
    "I no ha estat triat/da per ningú. \\\\ "
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (j in 1:length(quins_triat)){
      k = pos
      cat(ifelse(rels[k, 10], 
                 paste0("\\item   \\textbf{", noms[rels[k, 1]], "}"),  
                 paste0("\\item  ", noms[rels[k, 1]])))
      k = k + 1
    }
    cat("\\end{itemize}")
  }
  
  cat(paste0("I negativament a:"))
  cat("\\begin{itemize}")
  for (j in 1:numero_respostes){
    k = pos
    cat(ifelse(rels[k, 11], 
               paste0("\\item   \\textbf{", noms[rels[k, 5]], "}"), 
               paste0("\\item   ", noms[rels[k, 5]])))
    k = k + 1
  }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, 5]==i)
  if (length(quins_triat)==0){
    "I no ha estat triat/da per ningú. \\\\ "
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (j in 1:length(quins_triat)){
      k = pos
      cat(ifelse(rels[i, 11], 
                 paste0("\\item   \\textbf{", noms[rels[k, 1]], "}"),  
                 paste0("\\item  ", noms[rels[k, 1]])))
      k = k + 1
    }
    cat("\\end{itemize}")
  }
  
  cat("\\subsubsection*{Amical}")
  cat(paste0("En/na ", noms[i]), " ha triat positivament a:")
  
  cat("\\begin{itemize}")
  for (j in 1:numero_respostes){
    k = pos
    cat(ifelse(rels[i, 12], 
               paste0("\\item   \\textbf{", noms[rels[k, 6]], "}"),  
               paste0("\\item  ", noms[rels[k, 6]])))
    k = k + 1
  }
  cat("\\end{itemize}")
  
  quins_triat = which(rels[, 6]==i)
  
  if (length(quins_triat)==0){
    "I no ha estat triat/da per ningú. \\\\ "
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (j in 1:length(quins_triat)){
      k = pos
      cat(ifelse(rels[k, 12], 
                 paste0("\\item   \\textbf{", noms[rels[k, 1]], "}"),  
                 paste0("\\item  ", noms[rels[k, 1]])))
      k = k + 1
    }
    cat("\\end{itemize}")
  }
  
  cat(paste0("I negativament a:"))
  cat("\\begin{itemize}")
  for (j in 1:numero_respostes){
    k = pos
    cat(ifelse(rels[k, 13], 
               paste0("\\item   \\textbf{", noms[rels[k, 7]], "}"), 
               paste0("\\item   ", noms[rels[k, 7]])))
    k = k + 1
  }    
  cat("\\end{itemize}")
  quins_triat = which(rels[, 7]==i)
  if (length(quins_triat)==0){
    "I no ha estat triat/da per ningú. \\\\ "
  }
  
  else {
    cat("Ha estat triat/da per:  ")
    cat("\\begin{itemize}")
    for (j in 1:length(quins_triat)){
      k = pos
      cat(ifelse(rels[k, 13], 
                 paste0("\\item   \\textbf{", noms[rels[k, 1]], "}"),  
                 paste0("\\item  ", noms[rels[k, 1]])))
      k = k + 1
    }
    cat("\\end{itemize}")
  }   
  
}


