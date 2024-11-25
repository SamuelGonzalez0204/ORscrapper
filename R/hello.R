# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(roxygen2, devtools)

LeerFicherosPDF <- function(ruta) {
  ficheros <- list.files(path = ruta, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  return(ficheros)
}


NombreFicherosPDF <- function() {
  ficherosNombre <- list()
  for (i in seq_along(input$upload$name)) {
    file <- input$upload
    ficherosNombre <- c(ficherosNombre, input$upload$name[i])
  }
  return(ficherosNombre)
}


LeerDocumento <- function(nombreFichero) {
  doc <- pdf_text(nombreFichero)
  text <- paste(doc, collapse = "\n")
  return(strsplit(text, "\n")[[1]])
}


BuscarValor <- function(textoBuscar, lines) {
  Encontrados <- character()
  valores <- ifelse(grepl(textoBuscar, lines), 1, 0)
  valores <- which(valores == 1)
  array_buscar <- strsplit(textoBuscar, " ")[[1]]
  for (i in valores) {
    array_line <- strsplit(lines[i], " ")[[1]]
    for (j in 1:(length(array_line)-length(array_buscar))){
      cadena <- character()
      sub_array_line <- array_line[j:(j + length(array_buscar) - 1)]
      if (all(identical(sub_array_line, array_buscar))) {
        añadir <-j+length(array_buscar)
        while (!is.na(array_line[añadir]) && array_line[añadir] != ""){
          cadena <- c(cadena, array_line[añadir])
          añadir<-añadir+1
        }
        cadena <- paste(cadena, collapse = " ")
        Encontrados<- c(Encontrados, cadena)
      }
    }
  }
  return(Encontrados)
}


BuscarVariable <- function(lines, textoBuscar, posicion="1"){
  posicionValor <- grep(textoBuscar, lines)
  if (length(posicionValor) != 0){
    lineaValor <- lines[posicionValor[1]]
      if (textoBuscar == ".*% células tumorales:\\s"){
        lineaValor <- sub("\\s*/.*", "", lineaValor)
      }
      valor <- sub(textoBuscar, "", lineaValor)
      trimws(valor)
  } else {
    "Null"
  }
}


acotarTexto<-function(textoInicio, textoInicio2, linesTotal){
  lines <- character()
  indices_inicio <- grep(textoInicio, linesTotal)
  indices_inicio2 <- grep(textoInicio2, linesTotal)
  indice_limite <- grep(textoLimite, linesTotal)[1]
  indice_limite2 <- grep(textoLimite2, linesTotal)
  if (length(indices_inicio2) != 0 | length(indices_inicio)>1){
    if (length(indices_inicio2) != 0 && length(indices_inicio) != 0){
      lines <- linesTotal[(indices_inicio + 1):(indices_inicio2 - 1)]
    }else if (length(indice_limite2) != 0 && length(indices_inicio)!=0){
      lines <- linesTotal[(indices_inicio + 1):(indice_limite2[1] - 1)]
    }else if (length(indices_inicio2) != 0){
      lines <- linesTotal[(indices_inicio2 + 1):(indice_limite - 1)]
    }else{
      lines <- linesTotal[(indices_inicio + 1):(indice_limite - 1)]
    }
  }else if (length(indices_inicio2) == 0 && length(indices_inicio) == 0){
    lines <- c(lines, "Sin biomarcadores")
  }else {
    lines <- linesTotal[(indices_inicio + 1):(indice_limite - 1)]
  }
  return(lines)
}


ExtraerValorPrincipioFin <-function(lista, lines, patron){
  return(c(lista, BuscarVariable(lines, patron)))
}

ExtraerValorIntermedio <- function(lista, lines, textoBuscar){
  return(unlist(c(lista, unique(BuscarValor(textoBuscar, lines)))))
}

Biopsia <- function(numerosBiopsia){
  listasNB <- lapply(numerosBiopsia, function(x) list(x))
  tipobiopsia <- sapply(listasNB, function(x) substr(x, 5,5))
  tipoBiopsia <- ifelse(tipobiopsia == "B", '1',
                        ifelse(tipobiopsia == "P", '2', '3'))
  return(tipoBiopsia)
}

Chip <- function(ficheros){
  return(gsub(".*?([0-9]+\\.[0-9]+).*", "\\1", ficheros[grep("\\.pdf$", ficheros)]))
}

ExtraerValorDeTablas <- function(lines, inicio, inicio2,
                                 genes_mut_ordenados = list(), patogenicidad_ordenadas = list(), frecuencias = list(), codificaciones = list(), cambios = list(), valores = list()){
  lines <- acotarTexto(inicio, inicio2 ,lines)
  posiciones <- mutaciones_patogenicas <- lista_frec <- mutaciones_pdf <- patogenicidad <- lista_cod <- lista_cambio <- c()
  lines_divididas <- strsplit(lines, "\\s+")
  pos = 0
  for (line in lines_divididas){
    pos = pos+1
    if (line[1] %in% mutaciones){
      posiciones <- c(posiciones, pos)
      mutaciones_pdf <- c(mutaciones_pdf, line[1])
      for (i in strsplit(line, " ")) {
        resultado <- str_match(i, patron_frecuencia)
        resultado2 <- str_match(i, patron_codificacion)
        resultado3 <- str_match(i, "p\\.\\(.*?\\)")
        if (!is.na(resultado)) {
          frec <- resultado[1]
          lista_frec <- c(lista_frec, frec)
        }
        else if (!is.na(resultado2)) {
          cod <- resultado2[1]
          lista_cod <- c(lista_cod, cod)
        }
        if (!is.na(resultado3)) {
          cambio <- resultado3[1]
          lista_cambio <- c(lista_cambio, cambio)
        }
      }
    }
  }
  if (length(posiciones)!= 0){
    for (pos in seq(1, length(posiciones))){
      if (pos == length(posiciones)){
        if (length(grep("pathogenicity", lines[posiciones[pos]:length(lines)])) == 1 || length(grep("Pathogenic", lines[posiciones[pos]:length(lines)])) == 1){
          patogenicidad <- c(patogenicidad, "Pathogenic")
        } else{
          patogenicidad <- c(patogenicidad, "Sin resultados")
        }
      } else if(length(grep("pathogenicity", lines[posiciones[pos]:posiciones[pos+1]-1])) == 1 || length(grep("Pathogenic", lines[posiciones[pos]:posiciones[pos+1]-1])) == 1){
        patogenicidad <- c(patogenicidad, "Pathogenic")
      }else{
        patogenicidad <- c(patogenicidad, "Sin resultados")
      }
    }
  }
  genes_mut_ordenados <- c(genes_mut_ordenados, list(mutaciones_pdf))
  patogenicidad_ordenadas <- c(patogenicidad_ordenadas, list(patogenicidad))
  frecuencias <- c(frecuencias, list(lista_frec))
  codificaciones <- c(codificaciones, list(lista_cod))
  cambios <- append(cambios, list(unlist(lista_cambio)))
  valores <- c(genes_mut_ordenados, patogenicidad_ordenadas, frecuencias, codificaciones, cambios)
  return(valores)
}

BuscarNCBI <- function(patogenicidad_ordenadas){
  for (lista in seq_along(patogenicidad_ordenadas)){
    patogenicidad <- c()
    for (elemento in seq_along(patogenicidad_ordenadas[[lista]])){
      gen = paste(genes_mut_ordenados[[lista]][[elemento]], "[gene]", cod_totales[[lista]][[elemento]])
      res <- entrez_search(db = "clinvar", term = gen)
      if (length(res$ids)!=0){
        esums <- entrez_summary(db = "clinvar", id = res$ids[1])
        resumen <- extract_from_esummary(esums, "germline_classification")
        patogenicidad <- c(patogenicidad, resumen$description)
      }
      else{
        patogenicidad <- c(patogenicidad, "Sin resultados")
      }
    }
    patogenicidad_buscadas <- c(patogenicidad_buscadas, list(patogenicidad))
  }
  return(patogenicidad_buscadas)
}

Fusiones <- function(lines, mutaciones){
  variantes <- character()
  for (linea in lines) {
    for (mutacion in mutaciones) {
      patronGen <-paste0(mutacion, "\\.[A-Za-z0-9]+\\.[A-Za-z0-9]+")
      if (grepl(patronGen, linea)) {
        for (palabra in strsplit(linea, " ")[[1]]){
          if (grepl(patronGen, palabra)){
            variantes <- c(variantes, palabra)
          }
        }
      }
    }
  }
  fusiones <- append(fusiones, list(variantes))
}

SoloPatogenicos <- function(lista_patogenicos, lista2){
  tamaño <- length(lista_patogenicos)
  posiciones <- which(unlist(lista_patogenicos) == "Pathogenic")
  listaReturn <- rep(NA, length(unlist(lista_patogenicos)))
  listaReturn[posiciones] <- unlist(lista2)[posiciones]
  grupos <- gl(tamaño, ceiling(length(listaReturn) / tamaño))
  return(split(listaReturn, grupos))
}

library(tools)
library(pdftools)
library(tidyverse)
library(stringr)
library(readxl)
#library(openxlsx)
library(mongolite)
library(processx)
library(rentrez)


CarpetaEntrada <- "INPUT"
CarpetaDatos <- "DATOS"
CarpetaInformes <- "Informes"
CarpetaSalida <- "OUTPUT"
CarpetaResultados <- "RESULTADOS"
PathBase <- getwd()

ficheroDiagnostico <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Diagnostico.xlsx")
diagnostico <- read_excel(ficheroDiagnostico)
diagnosticos_dic <- setNames(diagnostico$`NÚMERO DIAGNÓSTICO`, diagnostico$DIAGNÓSTICO)

ficheroGenes <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Genes.xlsx")
genes <- read_excel(ficheroGenes)
mutaciones <- unique(genes$GEN)
mutaciones_dic <- setNames(genes$`Número gen`, genes$GEN)

rutaEntrada <- file.path(PathBase, CarpetaEntrada, CarpetaInformes)
ficheros <- LeerFicherosPDF(rutaEntrada)

NHC_Data <- NB_values <-Nbiopsia_Data <- fecha_Data <- texto_Data <- genes_mut2 <- genes_mut_ordenados <- frecuencias_totales <- num_mutaciones <- numero_iden <-
  añadir <- cambiosPato <- frecuenciasPato <- mutaciones_pato <- patogen <- numero_iden_pato <- num_mutacionesPato <-
  diagnostico2 <- sexo <- porcentaje_tumoral <- calidad <- patogenicidad_buscadas <- cod_totales <- valoresTabla <- cambios <-list()
textoDiag <- NHC <- biopsia <- fechas <- chip2 <- fusiones <- character()
numeroDiag <- lista_ensayos <- ensayos_finales <- lista_tratamientos <- tratamientos_finales <- numeric()

benigno <- resultado <- FALSE
max_mut <- 0

patron <- "Ensayos clínicos"
patron2 <- "Tratamientos disponibles"
patron_frecuencia <- "\\d{2}\\.\\d{2}\\%"
patron_cambio <-"\\(.*?\\)"
patron_codificacion <- "c\\.[0-9]+[A-Za-z>_]+"

patron_diagnostico <- ".*Diagnóstico:\\s"
patron_sexo <- ".*Sexo:\\s*"
patron_porcentaje_tumoral <- ".*% células tumorales:\\s"
patron_calidad <- ".*CALIDAD DE LA MUESTRA /LIMITACIONES PARA SU ANÁLISIS:\\s"

textoInicio<- "Variantes de secuencia de ADN"
textoInicio2<-"   Variaciones del número de copias"
textoLimite <- "Genes analizados"
textoLimite2 <-"Comentarios adicionales sobre las variantes"

cod_totales <- frecuencias_totales <-patogenicidad_ordenadas<- genes_mut_ordenados <- patogenicidad_buscadas<- list()


for (ficheroPDF in ficheros){
  lines <- LeerDocumento(ficheroPDF)
  diagnostico2 <- ExtraerValorPrincipioFin(diagnostico2, lines, patron_diagnostico)
  sexo <- ExtraerValorPrincipioFin(sexo, lines, patron_sexo)
  porcentaje_tumoral <- ExtraerValorPrincipioFin(porcentaje_tumoral, lines, patron_porcentaje_tumoral)
  calidad <- ExtraerValorPrincipioFin(calidad, lines, patron_calidad)
  NHC_Data<- ExtraerValorIntermedio(NHC_Data, lines, "NHC:")
  NB_values <- ExtraerValorIntermedio(NB_values, lines, "biopsia:")
  fechas <- ExtraerValorIntermedio(fechas, lines, "Fecha:")
  textoDiag <- ExtraerValorIntermedio(textoDiag, lines, "de la muestra:")
  Biopsia_solida <- Biopsia(NB_values)
  lista_ensayos <- ExtraerValorPrincipioFin(lista_ensayos, lines, patron)
  lista_tratamientos <- ExtraerValorPrincipioFin(lista_tratamientos, lines, patron2)
  valoresTabla <- ExtraerValorDeTablas(lines, textoInicio, textoInicio2)
  genes_mut_ordenados <- c(genes_mut_ordenados, valoresTabla[1])
  patogenicidad_ordenadas <- c(patogenicidad_ordenadas, valoresTabla[2])
  frecuencias_totales <- c(frecuencias_totales, valoresTabla[3])
  cod_totales <- c(cod_totales, valoresTabla[4])
  cambios <- c(cambios, valoresTabla[5])
  fusiones <- Fusiones(lines, mutaciones)
}

ensayos_finales <- ifelse(lista_ensayos %in% 0, 0, 1)
tratamientos_finales <- ifelse(lista_tratamientos %in% 0, 0, 1)

chip2 <- Chip(ficheros)

patogenicidad_buscadas <- BuscarNCBI(patogenicidad_ordenadas)

mutaciones_pato <- c()
mutaciones_pato <- SoloPatogenicos(patogenicidad_ordenadas, genes_mut_ordenados)

cambiosPato <- SoloPatogenicos(patogenicidad_ordenadas, cambios)
frecuenciasPato <- SoloPatogenicos(patogenicidad_ordenadas, frecuencias_totales)


for (lista in genes_mut_ordenados){
  num_mutaciones<- c(num_mutaciones, length(lista))
}

for (i in genes_mut_ordenados){
  for (gen in i){
    añadir <- c(añadir, mutaciones_dic[[gen]])
  }
  numero_iden <- c(numero_iden, list(unlist(añadir)))
  añadir <- list()
}
