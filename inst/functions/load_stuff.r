library(dplyr)
library(gtsummary)
library(ggplot2)
library(kableExtra)
library(lubridate)


knitr::opts_chunk$set(
  # A nyers szöveg kimenetel elé ne tegyen "##"-t
  comment = NA,
  # Ne mutassa a kódokat
  echo = FALSE,
  # Ne cache-eljen
  cached = FALSE,
  # Ne írja ki a warningokat
  warning = FALSE,
  message = FALSE,
  # Ábra középre rendezése
  fig.align = "center",
  #fig.asp = .75,                          # Ábra Hossz/szélesség
  # legyenek 60 karakter szélességűre tördelve
  tidy.opts = list(width.cutoff = 60),
  # legyenek clean codingra megformázva
  tidy = TRUE,#"styler",
  # PNG legyen az alapértelmezett képformátum
  dev = "png",#"tiff",
  compression = "lzw",
  # a PNG képek elég jó minőségűek legyenek
  dpi = 300
  #,fig.path = fig_directory  # Ábra kimenet helye
  )

# Dont screw with these, handles default print function also :(
##options(scipen = 1) # Require 5 instead of 4 for scientific notation (eg. for p-values)
##options(digits = 3) # default no. of digits (!)
options(encoding = "UTF-8")

# Setting up gtsummary themes if gtsummary is *loaded*
if ("gtsummary" %in% loadedNamespaces()) {

  # Apply the NEJM journal theme
  gtsummary::theme_gtsummary_journal(journal = "nejm")

  # Apply the compact theme for a cleaner table appearance
  gtsummary::theme_gtsummary_compact()

} else {

  # Optionally, you can print a message or handle the absence of gtsummary
  message("gtsummary package is not loaded. Themes not applied.")

}

source_all_files <- function(directory) {
  file_paths <- list.files(directory, pattern = "\\.[rR]$", full.names = TRUE)

  for (file_path in file_paths) {
    source(file_path)
  }
}

source_all_files(here::here("R"))

# set.seed(12345) # dont do it if there is stochastic stuff after


