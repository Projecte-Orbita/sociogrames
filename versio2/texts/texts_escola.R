# Texts informe per l'escola sencera
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")


coses_latex = "
\\documentclass[a4paper, 12pt, oneside]{book}%{article}
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
\\newcommand{\\sectionbreak}{\\clearpage}  % per començar una pàgina en cada secció
\\usepackage[final]{pdfpages}
\\usepackage[T1]{fontenc}
% \\usepackage[latin1]{inputenc}   %paquet que serveix per poder escriure
%els accents de forma normal en Linux
%\\usepackage[ansinew]{inputenc}  % aquesta és la versio per windows
\\usepackage[utf8]{inputenc}
\\usepackage[catalan]{babel}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}
\\usepackage[table, dvipsnames]{xcolor}
\\usepackage{booktabs}

\\usepackage[printwatermark]{xwatermark}
%\\newwatermark[firstpage,color=gray!50,angle=45,scale=3,xpos=0,ypos=0]{Esborrany v2.3.3}

\\usepackage{afterpage}
\\newcommand\\blankpage{ %  per tenir una pàgina en blanc al final
	\\null
	\\thispagestyle{empty}%
	\\addtocounter{page}{-1}%
	\\newpage}

\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{../../imatges_informes/informe-atom-peu}}
\\rfoot{\\small \\thepage}

\\setlength\\parindent{0pt}
\\captionsetup[subfigure]{labelformat=empty}
\\fancyfootoffset[LO,LE]{2cm}

\\titleformat{\\chapter}[display]
{\\normalfont\\huge\\bfseries}{}{0pt}{\\Huge}
\\titlespacing*{\\chapter}
{0pt}{10pt}{40pt}
"

pagina_titol = function(nom_escola){
  cat(
    paste0(
      "
      \\begin{document}

  \\begin{titlepage}
  \\newcommand{\\HRule}{\\rule{\\linewidth}{0.5mm}} % Defines a new command for the horizontal lines, change thickness here
  \\center % Center everything on the page
  
  \\vspace*{3cm}
  
  \\textsc{\\LARGE Informe  Sociograma Àtom}\\\\ % Name of your university/college
  \\textsc{\\Large ", nom_escola, "}\\\\[0.5cm] % Major heading such as course name
  
  \\HRule \\\\[0.4cm]
  { \\huge \\bfseries Curs A class B}\\\\[0.4cm] % Title of your document
  \\HRule \\\\[1.5cm]

  \\vspace{5cm}
 % \\includegraphics[scale=0.3]{logo_orbita.png} % Include a department/university logo - this will require the graphicx package
  \\vfill % Fill the rest of the page with whitespace
  \\end{titlepage}
      \\vspace{1.1cm}
      "
    )
  )
}

titol_classes <- function(escola, classe){
  cat(paste0(
    "\\begin{titlepage}
  \\newcommand{\\HRule}{\\rule{\\linewidth}{0.5mm}}
  \\center % Center everything on the page
  
  \\vspace*{3cm}
  
  \\textsc{\\LARGE Informe Sociograma Àtom}\\\\[1.5cm] % Name of your university/college
  \\textsc{\\Large ", escola, "}\\\\[0.5cm] % Major heading such as course name
  
  \\HRule \\\\[0.4cm]
  { \\huge \\bfseries ", classe, "}\\\\[0.4cm] % Title of your document
  \\HRule \\\\[1.5cm]
  {\\large \\today}\\\\[2cm]
  \\vspace{5cm}
  \\includegraphics[scale=0.3]{../../imatges_informes/logo_orbita.png} 
  \\vfill % Fill the rest of the page with whitespace
  \\end{titlepage}"));
}

titol_collectiu = "
\\newpage
\\vspace*{\\fill}

\\begin{center}
{\\huge
\\textbf{Informes col·lectius}
}
\\end{center}

\\vspace*{\\fill}
\\newpage
"

titol_individual = "
\\newpage
\\vspace*{\\fill}

\\begin{center}
{\\huge
\\textbf{Informes individuals}
}
\\end{center}

\\vspace*{\\fill}
\\newpage
      "

introduccio_escola = "
\\section*{Introducció}

El sociograma és una eina d'anàlisi de relacions socials entre subjectes pertanyents a un mateix entorn. 
En l'àmbit educatiu, s'utilitza com a eina per comprendre el sistema d’interaccions socials, que poden ser 
d’un grup-classe o a nivell de curs o escolar, aportant informació sobre la intensitat i la qualitat de 
les relacions entre els i les alumnes. Conèixer la posició sociomètrica de cada nen i nena, així com 
comprendre els subgrups dins l’aula ens permet elaborar estratègies per facilitar la convivència escolar 
i influir en el benestar dels alumnes atenent les seves necessitats.

L’anàlisi del present sociograma es basa en 7 àrees mesurades a partir de 7 escales: 
\\begin{enumerate}
\\item Disrupció
\\item Prosocialitat o cooperació
\\item Victimització
\\item Valoració acadèmica
\\item Estat d’ànim percebut
\\item Actitud percebuda
\\item Estatus sociomètric
\\end{enumerate}

A partir dels resultats obtinguts en aquestes escales es desenvolupen dues xarxes:

\\begin{itemize}
\\item Xarxa relacional acadèmica
\\item Xarxa relacional social
\\end{itemize}

Presentem els resultats en dos tipus d'informes, un de grupal, i un d'individual; tots dos presentats en 
aquest document. En l'informe  es presenta cada una de les àrees estudiades per separat. Es mostra un gràfic 
global seguit d’una taula amb les puntuacions desglossades. Al final del document s’inclouen les preguntes 
específiques que s’utilitzen per avaluar cadascuna de les àrees. Tot seguit s’inclou un informe individual, 
on podrem veure totes les àrees de cada alumne.

L’informe ens permet obtenir una fotografia de les relacions de l’aula considerant el punt de vista de cada 
alumne. És important considerar que a l’alumne se li dóna l’opció de respondre cada pregunta indicant tres 
noms de companys que compleixin amb la premissa de la qüestió plantejada. No se li permet anotar el seu propi 
nom. En les qüestions plantejades hi ha preguntes que es formulen de forma directa, indirecta, positiva i 
negativa.

"

final_latex = "
\\afterpage{\\blankpage}

\\end{document}"