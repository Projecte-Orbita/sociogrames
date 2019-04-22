# Text
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
%\\lfoot{\\includegraphics[scale=0.3]{../../informe-atom-peu}}
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
      \\begin{document}\\begin{titlepage}
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

afegeix_grafic = function(nom_grafic){
  cat(
    paste0(
    "
    \\begin{figure}[H]
			\\centering
			\\includegraphics[width=13cm]{../figures/", nom_grafic, ".pdf}
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
	\\section*{Disrupció} 
		
		Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:
		
		\\textbf{Qui molesta als altres?}
		
		\\textbf{Qui insulta als altres?}
		
		\\textbf{Qui pega als altres?}
		
		\\textbf{Qui diu coses dolentes dels altres?}
		
		\\textbf{Qui no deixa participar?}
		
		Per tant aquí estem mesurant el grau de disrupció que observem que cada alumne causa a l'aula, principalment causat per l'agressivitat, tant física com relacional.
"

prosocialitat = "
		\\section*{Prosocialitat}
		
		Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:
		
		\\textbf{Qui ajuda als altres?}
		
		\\textbf{Qui defensa als altres}
		
		\\textbf{Qui diu coses bones dels altres?}
		
		En aquest apartat mesurem el que anomenem la prosocialitat, que seria el contrari a la disrupció. És a dir, mesurem en quin gran un alumne ajuda als seus companys i crea un bon clima social i escolar. 

"

victimes = "
		\\section*{Víctimes}
		
		Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:
		
		\\textbf{A qui molesten?}
		
		\\textbf{A qui insulten?}
		
		\\textbf{A qui peguen?}
		
		\\textbf{De qui diuen coses dolentes?}
		
		\\textbf{A qui no deixen participar?}
		
		Aquest apartat és complementari a l'anterior, i analitzem les víctimes que la disrupció i l'agressivitat. 
"

academic = "
		\\section*{Acadèmic}
		
		Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:
		
		\\textbf{Treu bones notes}	
		
		\\textbf{No treu bones notes}
		
		\\textbf{Participa a classe}	
		
		\\textbf{No participa a classe}
"


estat_anim = "
		\\section*{Estat d'ànim percebut}
		
		En aquest apartat mesurem factors d'estat d'ànim relacionats que ens poden donar pistes d'un entorn social no satisfactori, tan a nivell personal com escolar. Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:
		
		
		\\textbf{Acostuma a estar content}
		
		\\textbf{Es queixa sovint}
		
		\\textbf{S'enfada amb facilitat}
		
		\\textbf{Sol estar trist}
"

caracter = "
		\\section*{Caràcter}
		
		En aquest apartat mesurem qüestions relacionades amb el caràcter dels i les alumnes, en concret aquells relacionats amb lideratge, autonomia i socialització, ja que pensem que són els que més influècien en les relacions socials dins de l'escola. Els resultats d'aquesta àrea s'obtenen a partir de les respostes dels alumnes a les següents preguntes:
		
		
		\\textbf{Qui lidera els altres}
		
		\\textbf{Qui fa el que li diuen els altres}
		
		\\textbf{Soluciona els problemes sol}
		
		\\textbf{Acostuma a demanar ajuda}
		
		\\textbf{Parla molt amb els altres companys de classe}
		
		\\textbf{No parla gaire amb els companys de classe}
"

xarxa_academica = "
		\\section*{Xarxa social acadèmica}
		
En aquest apartat dibuixem la xarxa de relacions acadèmiques entre els nens i les nenes de la classe partint de la informació de la pregunta \textbf{Qui voldries al teu grup per fer un treball?}.

El gràfic consta dels següents elements: 

Cada vèrtex (rodona) és un alumne i cada fletxa és una relació que indica si l'alumne del qual prové la fletxa indica si faria un treball amb l'alumne cap al qual va. Recordem que cada alumne en pot seleccionar fins a 3. La mida del vèrtex mostra el número de tries positives menys el número de tries negatives que ha rebut aquell alumne. 

A més, mostrem tres magnituds més: el color dels vèrtex indica les bones notes que treu l'alumne tal com és percebut pels altres (informació que també hem presentat en forma de gràfic més amunt). Per altra banda, el color del nom indica si els altres aumnes perceben que aquell nen o nena participa significativament a classe o no. Per acabar, en els casos en que dos alumnes indiquen que treballarien l'un amb l'altre de forma recíproca, la fletxa es blava.
"

xarxa_relacional = "
		\\section*{Xarxa social relacional}
		
		En aquest apartat dibuixem la xarxa de relacions entre els nens i les nenes de la classe partint de la informació de la pregunta \textbf{Tria els teus tres millors amics o amigues}.
		
		El gràfic consta dels següents elements: 
		
		Cada vèrtex (rodona) és un alumne i cada fletxa és una relació que indica si l'alumne del qual prové la fletxa indica que l'alumne cap al qual va és amic o amiga seva. Recordem que cada alumne en pot seleccionar fins a 3. La mida del vèrtex mostra el número de tries que ha rebut aquell alumne. 
		
		A més, mostrem tres magnituds més: el color dels vèrtex indica el grau de disrupció total per aquell alumne, que ja hem vist en el primer apartat de l'informe. Per altra banda, el color del nom indica si els altres aumnes perceben aquell nen o nena com a especialment content o trist (provinent de les preguntes \"Normalment està content\" i \"Normalment està trist\"). Per acabar, en els casos en que dos alumnes indiquen que són amics de forma recíproca, la fletxa es blava.
"

nota_final = "
		\\emph{Nota}: el color del nom del nen o la nena indica si és percebut com a content pels seus companys. Verd és que sí, vermell és que no, negre és que no hi ha prou respostes sobre ell o ella perquè sigui rellevant. 
"
final_latex = "\\end{document}"

abans_taula = "
I a continuació presentem una taula amb els mateixos resultats:
"

importar_i_imprimir_taula = function(path_fitxer){

  taula = read_file(path_fitxer)
  cat(taula)

}