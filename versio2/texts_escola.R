# Texts informe per l'escola sencera

titol_classes <- function(escola, classe){
  cat(paste0(
    "\\begin{titlepage}
  \\newcommand{\\HRule}{\\rule{\\linewidth}{0.5mm}} % Defines a new command for the horizontal lines, change thickness here
  \\center % Center everything on the page
  
  \\vspace*{3cm}
  
  \\textsc{\\LARGE Informe Test Àtom}\\\\[1.5cm] % Name of your university/college
  \\textsc{\\Large ", escola[1], "}\\\\[0.5cm] % Major heading such as course name
  
  \\HRule \\\\[0.4cm]
  { \\huge \\bfseries ", classe, "}\\\\[0.4cm] % Title of your document
  \\HRule \\\\[1.5cm]
  
  \\vspace{5cm}
  \\includegraphics[scale=0.3]{logo_orbita.png} % Include a department/university logo - this will require the graphicx package
  \\vfill % Fill the rest of the page with whitespace
  
  \\end{titlepage}"));
}