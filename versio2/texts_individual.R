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
      \\caption{Diagrama de barres}
      \\label{fig:sub1}
      \\end{subfigure}%
      \\begin{subfigure}{.35\\textwidth}
      \\centering
      \\includegraphics[width=1\\linewidth]{../../figures/individuals/", nom_grafic, "-formatges-", i, ".pdf}
      \\caption{Diagrama de formatges}
      \\label{fig:sub2}
      \\end{subfigure}
      \\caption{Diagrama tal i qual}
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
			\\includegraphics[width=13cm]{../../figures/individuals/resum-", i, ".pdf}
		\\end{figure}
    "
    )
  )
}

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