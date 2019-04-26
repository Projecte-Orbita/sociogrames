# Text
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
require(readr)

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
\\usepackage[final]{pdfpages}
\\usepackage[T1]{fontenc}
% \\usepackage[latin1]{inputenc}   %paquet que serveix per poder escriure
%els accents de forma normal en Linux
%\\usepackage[ansinew]{inputenc}  % aquesta és la versio per windows
\\usepackage[utf8]{inputenc}
\\usepackage[catalan]{babel}

\\definecolor{orbita}{rgb}{0.0235, 0.8275, 0.5921}
\\usepackage[table]{xcolor}

\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\pagestyle{fancy}
\\fancyhf{}  
\\lfoot{\\includegraphics[scale=0.3]{../../informe-atom-peu}}
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

afegeix_grafic = function(path_llista, nom_grafic, peu_grafic){
  path_grafic = file.path(path_llista$figures, paste0(nom_grafic, ".pdf"))
  cat(
    paste0(
    "
    \\begin{figure}[H]
			\\centering
			\\includegraphics[width=15cm]{", path_grafic, "}
      \\caption{", peu_grafic, "}
		\\end{figure}
    "
  )
  )
}

introduccio = "
	\\section*{Introducció}
		
		El sociograma és una tècnica d'anàlisi de dades que permet visualitzar de forma gràfica diferents variables implicades en les relacions entre subjectes. A nivell educatiu, suposa una eina per comprendre el sistema d'interaccions socials d'un grup-classe, aportant informació sobre la intensitat i la qualitat de les relacions entre els alumnes. Conèixer la posició sociomètrica de cada estudiant, així com comprendre els subgrups dins l'aula ens permet elaborar estratègies per facilitar la convivència escolar i influir en el benestar dels alumnes atenent les seves necessitats.
		
		Aquesta és una versió beta de l'informe resultat del Sociograma Àtom als cicles mitjà is superior, tant a nivell estètic com de continguts. Aquest informe està realitzat sobre dades simulades, i per tant els perfils sociomètrics dels nens i nenes poden no tenir sentit. Trobareu el document complet de preguntes en un fitxer que acompanya aquest informe. En totes les preguntes els nens i nenes han de triar els *tres* companys que s'adeqüin millor al que es demana.
		
		Aquesta és una versió beta, si trobeu errors o conceptes o gràfics que no s'entenen si us plau feu-nos-ho saber a info\\@projecteorbita.cat. 
		
		Presentem cada una de les àrees estudiades per separat, presentant primer un gràfic global i després un desglossament de cada puntuació i en forma de taula.
"

disrupcio = "
\\section*{Escala de Disrupció} 

Mesura el grau de disrupció que cada alumne causa a l’aula, segons la percepció dels companys, causat per 
l’\\textbf{agressivitat} física (agressions), verbal (comentaris negatius o insults) i relacional (evitar, ignorar o 
dir rumors sobre altres). 

"

prosocialitat = "
\\section*{Escala de Prosocialitat o Cooperació}
Mesura el grau de prosocialitat de cada alumne, és a dir, la percepció dels companys de quin grau l’alumne 
\\textbf{ajuda} als seus companys i crea un bon clima social i escolar.
		
"

victimes = "
		\\section*{Escala de victimització}
		
Mesura el grau de victimització de cada alumne, és a dir, qui rep o \texbf{pateix conductes agressives} dels 
altres i en quin grau, segons la percepció dels companys. Considera la victimització física (rebre agressions), 
verbal (rebre comentaris negatius o insults) i relacional (ser evitar, ignorat o ser el centre de rumors). 

"

academic = "
		\\section*{Escala de valoració acadèmica}
		
Mesura la \\textbf{participació i implicació} dels alumnes a l’aula, així com els resultats de rendiment 
acadèmic, percebut pels companys de l’aula. Considera les variables: bones notes (alt rendiment acadèmic), 
males notes (baix rendiment acadèmic), participa (s’implica a la classe) i no participa (no s’implica a la 
classe).

"


estat_anim = "
		\\section*{Escala d'estat d'ànim percebut}

Mesura l’estat d’ànim dels alumnes segons la percepció dels companys de l’aula. Aquesta escala ens pot 
mostrar indicadors d’un entorn social no satisfactori, tant a nivell personal com escolar. Considera les 
variables: dissatisfacció (queixes), enuig (ràbia), alegria (content) i tristor (desànim).

"

caracter = "
		\\section*{Escala de d’Actitud}
		
Mesura la percepció del grup del \\textbf{caràcter}, forma de ser o capacitat de resoldre els problemes de 
dels alumnes de l’aula. Considera les variables: lideratge (rol de líder o seguidor), autonomia (capacitat de 
gestionar els problemes o demanar ajuda) i socialització (es comunica sovint amb els altres o no).
"

xarxa_academica = "
		\\section*{Xarxa relacional acadèmica}
		
A continuació presentem la xarxa de relacions acadèmiques entre els nens i nenes, basada en la informació 
de l’escala de valoració acadèmica, l’escala de caràcter i l’escala d’estatus sociomètric.

L’objectiu és recollir les preferències dels alumnes quan han de formar un grup per realitzar una tasca 
acadèmica.

La xarxa consta dels següents elements:

\\begin{enumerate}
\\item \\textbf{Vèrtex}: cada vèrtex és un alumne.
\\begin{itemize} 
\\item \\underline{Forma} (categoria en que es situa l’alumne)
\\begin{itemize}
\\item \\emph{Quadrat}: l’alumne es categoritza com a nen
\\item \\emph{Rodona}: l’alumne es categoritza com a nena
\\item \\emph{Rombe}: l’alumne es categoritza com a altres
\\item \\emph{Triangle}: no respon
\\end{itemize}
\\item \\underline{Mida} (número de tries positives rebudes per fer un treball)
\\begin{itemize}
\\item \\emph{Petita}: poc popular
\\item \\emph{Mitjana}: normal
\\item \\emph{Gran}: molt popular
\\end{itemize}
\\begin{itemize}
\\item \\underline{Color} (resultats de rendiment acadèmic de l’alumne segons la percepció dels companys)
\\item \\emph{Verd}: bones notes
\\item \\emph{Groc}: notes mitjanes
\\item Vermell: males notes
\\end{itemize}
\\end{itemize}
\\item \\textbf{Fletxes:} es refereix a la relació existent entre els alumnes de l’aula.
\\begin{itemize}
\\item \\underline{Normal}: la direcció indica la tria des d’un alumne a l’altre
\\item \\underline{Blava}: indica que els dos alumnes s’han triat de forma recíproca
\\end{itemize}
\\item \\textbf{Noms:} mostra el nom de l’alumne i indica amb el color el grau de participació a l’aula.
\\end{enumerate}
"

xarxa_relacional = "
		\\section*{Xarxa social relacional}
		
A continuació es presenta la xarxa de relacions socials entre els nens i les nenes, partint de les 
respostes a l’escala de disrupció, l’escala estat d’ànim percebut i l’escala d’estatus sociomètric.
L’objectiu és recollir les preferències dels alumnes a l’hora de relacionar-se i observar les dinàmiques 
de grup, tant positives com negatives, considerant les escales avaluades.

La xarxa consta dels següents elements:

\\begin{enumerate}
\\item \\textbf{Vèrtex}: cada vèrtex és un alumne.
\\begin{itemize} 
\\item \\underline{Forma} (categoria en que es situa l’alumne)
\\begin{itemize}
\\item \\emph{Quadrat}: l’alumne es categoritza com a nen
\\item \\emph{Rodona}: l’alumne es categoritza com a nena
\\item \\emph{Rombe}: l’alumne es categoritza com a altres
\\item \\emph{Triangle}: no respon
\\end{itemize}
\\item \\underline{Mida} (número de tries positives rebudes per fer un treball)
\\begin{itemize}
\\item \\emph{Petita}: poc popular
\\item \\emph{Mitjana}: normal
\\item \\emph{Gran}: molt popular
\\end{itemize}
\\begin{itemize}
\\item \\underline{Color} (resultats de rendiment acadèmic de l’alumne segons la percepció dels companys)
\\item \\emph{Verd}: bones notes
\\item \\emph{Groc}: notes mitjanes
\\item Vermell: males notes
\\end{itemize}
\\end{itemize}
\\item \\textbf{Fletxes:} es refereix a la relació existent entre els alumnes de l’aula.
\\begin{itemize}
\\item \\underline{Normal}: la direcció indica la tria des d’un alumne a l’altre
\\item \\underline{Blava}: indica que els dos alumnes s’han triat de forma recíproca
\\end{itemize}
\\item \\textbf{Noms:} mostra el nom de l’alumne i indica amb el color el grau de participació a l’aula.
\\end{enumerate}
"

nota_final = "
		\\emph{Nota}: el color del nom del nen o la nena indica si és percebut com a content pels seus companys. Verd és que sí, vermell és que no, negre és que no hi ha prou respostes sobre ell o ella perquè sigui rellevant. 
"
final_latex = "\\end{document}"

abans_taula = "
I a continuació presentem una taula amb els mateixos resultats:
"

importar_i_imprimir_taula = function(path_llista, nom_fitxer){

  taula = read_file(file.path(path_llista$taules, paste0(nom_fitxer, '.txt')))
  cat(taula)

}