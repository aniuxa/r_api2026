###############################################################################-
# Curso intersemestral
# Práctica 2: algunos paquetes de consulta
# Autora: Ana Escoto
# Fecha: 2026-01-20
###############################################################################-

# Paquetes ----
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere

pacman::p_load(tidyverse,
               readxl,
               writexl, 
               here,
               magrittr,
               WDI, 
               imfapi,
               geodata, 
               countrycode,
               janitor,
               skimr, 
               gt)

# Paquete countrycode ----
?codelist # pide ayudar

head(countrycode::codelist)

tibble(pais = c("Mexico", "Guatemala", "Uruguay")) %>% 
  dplyr::mutate(iso3 = countrycode(pais, origin = "country.name", destination = "iso3c"))

citation(package = "countrycode")

# Preparando nuestro lugar de trabajo

getwd() # dame wd -> working directory
here::here()

# setwd("~/Library/CloudStorage/GoogleDrive-ana.escoto.1@gmail.com/Mi unidad/2026/curso_api_main/taller_mio")
getwd()

dir.create(here::here("data"), showWarnings = FALSE)

# WDI ----

WDIsearch('gender')

## Búsqueda de un indicador específico ----

WDI(indicator='SG.VAW.REFU.ZS',
    country="all", 
    start=2020, 
    end=2026) %>%
  na.omit()


wdi_1 <-WDI(indicator='SG.VAW.REFU.ZS',
    country="all", 
    start=2020, 
    end=2026) %>%
  na.omit()

## Otra búsqueda

WDIsearch("gdp")


WDI(indicator = "NY.GDP.PCAP.CD", # el que está en el tablero
    country = c("MX", "US"),
    start = 1960)

wdi_2 <-WDI(indicator = "NY.GDP.PCAP.CD", # el que está en el tablero
            country = c("MX", "US"),
            start = 1960)

## Tercera consulta 

data("codelist") # trae al ambiente del paquete countrycode
head(codelist)

wdi_3 <-WDI(indicator = c("NY.GDP.PCAP.CD","SI.POV.GINI" ), # el que está en el tablero
            country = c("MX", "US"),
            start = 1960)

# Paquete geodata ----

tmax_data <- worldclim_global(var = "tmax", 
                              res = 10,
                              path = "data") # aquí donde quiero guardar la información

tmax_data

pop <- geodata::population(2020, res=10, path = "data" )
pop

# imfapi ----- 

# Es preguntar qué datos o flujos de datos en esta fuente

dataflows <- imf_get_dataflows() # aquí obtengo el ID

# Explicame la estructura de los datos 

imf_get_datastructure("WEO") # Aquí veo qué hay adentro

weo <- imf_get(dataflow_id = "WEO") # me baja toda la base

# Para descifrar los códigos

imf_get_codelists(dimension_ids = c("INDICATOR"),
                  dataflow_id = "WEO")

imf_get_codelists(dimension_ids = c("INDICATOR"),
                  dataflow_id = "WEO") %>% 
  dplyr::filter(code == "BCA")


# La consulta de WEO pero de indicadores y países específicos.

imf_get(dataflow_id = "WEO", 
        dimensions = list(INDICATOR = "BCA", 
                          COUNTRY = c("MEX", "CAN")))

# Repaso de manejo de data.frame


skimr::skim(weo)

weo %>% 
  filter(COUNTRY=="MEX") # un solo valor

weo %>% 
  filter(COUNTRY%in%c("MEX", "CAN", "USA")) # varios valores


weo %>% 
  filter(COUNTRY%in%c("MEX", "CAN", "USA")) %>% 
  filter(TIME_PERIOD == "2020")

weo %>% 
  filter(COUNTRY%in%c("MEX", "CAN", "USA")) %>% 
  filter(INDICATOR%in%c("BCA", "GGR"))


weo %>% 
  filter(COUNTRY%in%c("MEX", "CAN", "USA")) %>% 
  filter(INDICATOR=="BCA")

## de largo a ancho 


weo %>% 
  filter(COUNTRY%in%c("MEX", "CAN", "USA")) %>% 
  filter(INDICATOR=="BCA") %>% 
  tidyr::pivot_wider(values_from =  OBS_VALUE, 
                     names_from = COUNTRY )

## Pasarlo a excel


bca <- weo %>% 
  filter(COUNTRY%in%c("MEX", "CAN", "USA")) %>% 
  filter(INDICATOR=="BCA") %>% 
  tidyr::pivot_wider(values_from =  OBS_VALUE, 
                     names_from = COUNTRY )

writexl::write_xlsx(bca, 
                    path = "data/bca.xlsx")

bca %>% 
  gt() # grammar of table
