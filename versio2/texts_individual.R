# Texts individual

# per ara són els mateixos que els col·lectius; aquí, per ara, només hi ha el text per agafar els gràfics, que és lleugermanet diferent

Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

afegeix_grafic_individual = function(nom_grafic, i){
  cat(
    paste0(
      "
      \\begin{figure}[H]
      \\centering
      \\begin{subfigure}{.65\\textwidth}
      \\centering
      \\includegraphics[width=1\\linewidth]{../../figures/individuals/", nom_grafic, "-barres-", i, ".pdf}
      %\\caption{Diagrama de barres}
      \\label{fig:sub1}
      \\end{subfigure}%
      \\begin{subfigure}{.35\\textwidth}
      \\centering
      \\includegraphics[width=1\\linewidth]{../../figures/individuals/", nom_grafic, "-formatges-", i, ".pdf}
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

afegeix_grafic_resum = function(i){
  cat(
    paste0(
      "
    \\begin{figure}[H]
			\\centering
			\\includegraphics[width=16cm]{../../figures/individuals/resum-", i, ".pdf}
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

\\maketitle")}


estatus_sociometric = "

\\section*{Estatus sociomètric}

En aquest apartat mesurem l'estatus social de cada alumne, tant directe (preguntant directament qui són els seus amics) com indirecte. Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:

\\textbf{Qui voldries al teu grup per jugar al pati?}

\\textbf{Qui NO voldries al teu grup per jugar al pati?}

\\textbf{Marca/tria/escull els teus millors amics}

\\textbf{Marca/tria/escull els companys que et triarien com a millor amic}

\\textbf{Els altres volen estar al seu costat}

\\textbf{Pocs companys volen estar amb ell}
"

resum = "
\\section*{Resum}

A continuació presentem de forma condensada un gràfic amb totes les dimensions mesurades en el sociograma:
"