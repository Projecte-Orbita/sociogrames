# Texts informe per l'escola sencera
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

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
  
  \\vspace{5cm}
  \\includegraphics[scale=0.3]{../../logo_orbita.png} 
  \\vfill % Fill the rest of the page with whitespace
  \\end{titlepage}"));
}

titol_collectiu = "
      \\title{Informes col·lectius}
      \\date{}
      \\maketitle
      "

titol_individual = "
      \\title{Informes individuals}
      \\date{}
      \\maketitle
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