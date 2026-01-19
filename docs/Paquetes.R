# Paquetes básicos del curso (instala y carga)
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  readxl, writexl,
  googlesheets4,
  haven, sjlabelled,
  janitor, skimr,
  httr2, jsonlite, curl,
  inegiR, siebanxicor,
  WDI, wbstats,
  eurostat, imfapi, comtradr,
  rcrossref,
  plotly,
  usethis
)

# wpp2024 (desde GitHub)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("wpp2024", quietly = TRUE)) {
  remotes::install_github("ppgp/wpp2024", upgrade = "never")
}