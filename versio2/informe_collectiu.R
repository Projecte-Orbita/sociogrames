# Informe col·lectiu
# WIP
Sys.setlocale("LC_ALL", "Catalan_Spain.1252")

source('texts.R', encoding = "UTF-8")
source('calculs_previs.R', encoding = "UTF-8")

print("> Creant gràfics i taules...")
path_fitxer = 'dades/Preguntes sociograma - Sociograma_CMS.csv'
calculs(path_fitxer)

print("> Imprimint latex...")

con <- file(paste0("informes/proves.tex"), open = "wt", encoding = "UTF-8")
sink(con, split = T)

cat(coses_latex)
pagina_titol("Nom escola")

cat(introduccio)

# Disrupció
cat(disrupcio)

afegeix_grafic("disrupcio")

cat(abans_taula)

importar_i_imprimir_taula("taules/disrupcio.txt")

# Prosocialitat
cat(prosocialitat)

afegeix_grafic("prosocialitat")

cat(abans_taula)

importar_i_imprimir_taula("taules/prosocialitat.txt")

# Víctima
cat(victimes)

afegeix_grafic("victimes")

cat(abans_taula)

importar_i_imprimir_taula("taules/victimes.txt")

# Acadèmic
cat(academic)

afegeix_grafic("academic")

cat(abans_taula)

importar_i_imprimir_taula("taules/academic.txt")

# Estat d'ànim
cat(estat_anim)

afegeix_grafic("estat_anim")

cat(abans_taula)

importar_i_imprimir_taula("taules/estat_anim.txt")

# Caràcter
cat(caracter)

afegeix_grafic("caracter")

cat(abans_taula)

importar_i_imprimir_taula("taules/caracter.txt")

# Xarxa acadèmica

cat(xarxa_academica)

afegeix_grafic("xarxa_academica")

# Xarxa relacional

cat(xarxa_relacional)

afegeix_grafic("xarxa_relacional")

# Final

cat(final_latex)

sink()
close(con)

print("Finalitzat correctament.")