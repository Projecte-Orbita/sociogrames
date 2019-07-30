# Text
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")
require(readr)


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
\\section*{Comportament} 

Aquesta àrea recull les respostes de les escales:

\\begin{enumerate}
\\item Prosocialitat: Mesura el grau de prosocialitat/cooperació, és a dir, la percepció dels companys de quin grau l’alumne \\textbf{ajuda} 
als seus companys i crea un bon clima social i escolar.

\\item Disrupció: mesura el grau d'agressivitat física (agressions), verbal (comentaris negatius o insults) i relacional (evitar, ignorar o dir rumors sobre altres) observada 
pels companys i les companyes.
\\end{enumerate}
"

disrupcio_post = "
Aquestes respostes provenen de les preguntes:

\\begin{itemize}
\\item \\emph{Ajuda els altres quan tenen un problema.}
\\item \\emph{Col·labora amb els altres.}
\\item \\emph{Defensa els altres.}
\\item \\emph{Molesta als altres.}
\\item \\emph{No deixa participar als altres.}
\\item \\emph{Insulta als altres.}
\\item \\emph{Empenta o pega als altres.}
\\end{itemize}
\\newpage
"

prosocialitat = "
\\section*{Escala de Prosocialitat o Cooperació}
Mesura el grau de prosocialitat de cada alumne, és a dir, la percepció dels companys de quin grau l’alumne 
\\textbf{ajuda} als seus companys i crea un bon clima social i escolar.
		
"

prosocialitat_post = "
Aquestes respostes provenen de les preguntes:
\\begin{itemize}
\\item \\emph{Ajuda els altres quan tenen un problema.}
\\item \\emph{Col·labora amb els altres.}
\\item \\emph{Defensa els altres.}
\\end{itemize}
\\newpage
"

victimes = "
		\\section*{Escala de victimització}
		
Mesura el grau de victimització de cada alumne, és a dir, qui rep o \\textbf{pateix conductes agressives} dels 
altres i en quin grau, segons la percepció dels companys. Considera la victimització física (rebre agressions), 
verbal (rebre comentaris negatius o insults) i relacional (ser evitat, ignorat o ser el centre de rumors). 

"

victimes_post = "
Aquestes respostes provenen de les preguntes:
\\begin{itemize}
\\item \\emph{El molesten.}
\\item \\emph{No el deixen participar.}
\\item \\emph{L'insulten.}
\\item \\emph{El peguen o empenten.}
\\item \\emph{No es defensa del que li fan els altres.}
\\end{itemize}
\\newpage
"


academic = "
		\\section*{Escala de valoració acadèmica}
		
Mesura la \\textbf{participació i implicació} dels alumnes a l’aula, així com els resultats de rendiment 
acadèmic, percebut pels companys de l’aula. Considera les variables: bones notes (alt rendiment acadèmic), 
males notes (baix rendiment acadèmic), participa (s’implica a la classe) i no participa (no s’implica a la 
classe).

"
academic_post = "
Aquestes respostes provenen de les preguntes:
\\begin{itemize}
\\item \\emph{Treu bones notes.}
\\item \\emph{No treu bones notes.}
\\item \\emph{Participa a classe.}
\\item \\emph{No participa a classe.}
\\end{itemize}
\\newpage
"
	
estat_anim = "
		\\section*{Escala d'estat d'ànim percebut}

Mesura l’estat d’ànim dels alumnes segons la percepció dels companys de l’aula. Aquesta escala ens pot 
mostrar indicadors d’un entorn social no satisfactori, tant a nivell personal com escolar. Considera les 
variables: dissatisfacció (queixes), enuig (ràbia), alegria (content) i tristor (desànim).

"

estat_anim_post = "
Aquestes respostes provenen de les preguntes:
\\begin{itemize}
\\item \\emph{Acostuma a estar content.}
\\item \\emph{Acostuma a estar trist.}
\\item \\emph{Es queixa sovint.}
\\item \\emph{S'enfada amb facilitat.}
\\end{itemize}
\\newpage
"

caracter = "
		\\section*{Escala de caràcter}
		
Mesura la percepció del grup del \\textbf{caràcter}, forma de ser o capacitat de resoldre els problemes de 
dels alumnes de l’aula. Considera les variables: lideratge (rol de líder o seguidor), autonomia (capacitat de 
gestionar els problemes o demanar ajuda) i socialització (es comunica sovint amb els altres o no).
.

"

caracter_post = "
Aquestes respostes provenen de les preguntes:
\\begin{itemize}
\\item \\emph{Lidera els altres.}
\\item \\emph{Fa el que li diuen els altres.}
\\item \\emph{Soluciona els problemes sol.}
\\item \\emph{Acostuma a demanar ajuda.}
\\item \\emph{Parla molt amb els altres companys de classe.}
\\item \\emph{No parla gaire amb els companys de classe.}
\\end{itemize}
\\newpage
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
\\item \\underline{Forma}: categoria en que es situa l’alumne
\\begin{itemize}
\\item \\emph{Quadrat}: l’alumne es categoritza com a nen
\\item \\emph{Rodona}: l’alumne es categoritza com a nena
\\item \\emph{Rombe}: l’alumne es categoritza com a altres
\\item \\emph{Triangle}: no respon
\\end{itemize}
\\item \\underline{Mida}: número de tries positives rebudes per fer un treball
\\begin{itemize}
\\item \\emph{Petita}: poc popular
\\item \\emph{Mitjana}: normal
\\item \\emph{Gran}: molt popular
\\end{itemize}
\\item \\underline{Color}: resultats de rendiment acadèmic de l’alumne segons la percepció dels companys
\\begin{itemize}
\\item \\emph{Verd}: bones notes
\\item \\emph{Groc}: notes mitjanes
\\item \\emph{Vermell}: males notes
\\end{itemize}
\\end{itemize}
\\item \\textbf{Fletxes}: es refereix a la relació existent entre els alumnes de l’aula.
\\begin{itemize}
\\item \\underline{Normal}: la direcció indica la tria des d’un alumne a l’altre
\\item \\underline{Blava}: indica que els dos alumnes s’han triat de forma recíproca
\\end{itemize}
\\item \\textbf{Noms}: mostra el nom de l’alumne i indica amb el color el grau de participació a l’aula:
\\begin{itemize}
\\item \\underline{Verd}: participa molt
\\item \\underline{negre}: neutre 
\\item \\underline{Vermell}: participa poc
\\end{itemize}
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
\\item \\underline{Forma}: categoria en que es situa l’alumne
\\begin{itemize}
\\item \\emph{Quadrat}: l’alumne es categoritza com a nen
\\item \\emph{Rodona}: l’alumne es categoritza com a nena
\\item \\emph{Rombe}: l’alumne es categoritza com a altres
\\item \\emph{Triangle}: no respon
\\end{itemize}
\\item \\underline{Mida}: número de tries positives rebudes per fer un treball
\\begin{itemize}
\\item \\emph{Petita}: poc popular
\\item \\emph{Mitjana}: normal
\\item \\emph{Gran}: molt popular
\\end{itemize}
\\item \\underline{Color}: resultats de rendiment acadèmic de l’alumne segons la percepció dels companys
\\begin{itemize}
\\item \\emph{Verd}: bones notes
\\item \\emph{Groc}: notes mitjanes
\\item \\emph{Vermell}: males notes
\\end{itemize}
\\end{itemize}
\\item \\textbf{Fletxes:} es refereix a la relació existent entre els alumnes de l’aula.
\\begin{itemize}
\\item \\underline{Normal}: la direcció indica la tria des d’un alumne a l’altre
\\item \\underline{Blava}: indica que els dos alumnes s’han triat de forma recíproca
\\end{itemize}
\\item \\textbf{Noms:} mostra el nom de l’alumne i indica amb el color l'estat dànim percebut pels companys i les companyes:
\\begin{itemize}
\\item \\underline{Verd}: estat d'ànim positiu
\\item \\underline{negre}: neutre
\\item \\underline{Vermell}: estat d'ànim negatiu
\\end{itemize}
\\end{enumerate}
"

nota_final = "
		\\emph{Nota}: el color del nom del nen o la nena indica si és percebut com a content pels seus companys. Verd és que sí, vermell és que no, negre és que no hi ha prou respostes sobre ell o ella perquè sigui rellevant. 
"

abans_taula = "
A continuació presentem els mateixos resultats en format de taula:
"

importar_i_imprimir_taula = function(path_llista, nom_fitxer){

  taula = read_file(file.path(path_llista$taules, paste0(nom_fitxer, '.txt')))
  cat(taula)

}