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


BuscarVariable <- function(lines, textoBuscar){
  posicionValor <- grep(textoBuscar, lines)
  if (length(posicionValor) != 0){
    lineaValor <- lines[posicionValor[1]]
    if (textoBuscar == patron_porcentaje_tumoral){
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


ExtraerValor <-function(lista, lines, patron){
  return(c(lista, BuscarVariable(lines, patron)))
}

ExtraerValorSinPatron <- function(lista, lines, textoBuscar){
  return(unlist(c(lista, unique(BuscarValor(textoBuscar, lines)))))
}

Biopsia <- function(numerosBiopsia){
  listasNB <- lapply(numerosBiopsia, function(x) list(x))
  tipobiopsia <- sapply(listasNB, function(x) substr(x, 5,5))
  tipoBiopsia <- ifelse(tipobiopsia == "B", '1',
                        ifelse(tipobiopsia == "P", '2', '3'))
  return(tipoBiopsia)
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
  diagnostico2 <- sexo <- porcentaje_tumoral <- calidad <- patogenicidad_buscadas <- cod_totales <-list()
textoDiag <- NHC <- biopsia <- fechas <- chip2 <- fusiones <- character()
numeroDiag <- lista_ensayos <- ensayos_finales <- lista_tratamientos <- tratamientos_finales <- numeric()

benigno <- resultado <- FALSE
max_mut <- 0

patron <- "(\\d+)\\s* Ensayos clínicos"
patron2 <- "(\\d+)\\s* Tratamientos disponibles"
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

for (ficheroPDF in ficheros){
  lines <- LeerDocumento(ficheroPDF)
  diagnostico2 <- ExtraerValor(diagnostico2, lines, patron_diagnostico)
  sexo <- ExtraerValor(sexo, lines, patron_sexo)
  porcentaje_tumoral <- ExtraerValor(porcentaje_tumoral, lines, patron_porcentaje_tumoral)
  calidad <- ExtraerValor(calidad, lines, patron_calidad)
  NHC_Data<- ExtraerValorSinPatron(NHC_Data, lines, "NHC:")
  NB_values <- ExtraerValorSinPatron(NB_values, lines, "biopsia:")
  fechas <- ExtraerValorSinPatron(fechas, lines, "Fecha:")
  textoDiag <- ExtraerValorSinPatron(textoDiag, lines, "de la muestra:")
  Biopsia_solida <- Biopsia(NB_values)
  lista_tratamientos <- ExtraerValor(lista_tratamientos, lines, patron)
}



lista_ensayos <- as.integer(sapply(ficheros, function(ficheroPDF) {
  lines <- LeerDocumento(ficheroPDF)
  ensayos <- sapply(lines, function(line) {
    resultado <- str_match(line, patron)
    if (!is.na(resultado[1])) {
      return(as.integer(resultado[1, 2]))
    } else {
      return(0)
    }
  })
  return(sum(ensayos))
}))

ensayos_finales <- ifelse(lista_ensayos %in% 0, 0, 1)

