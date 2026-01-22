################################################################################
# Curso intersemestral
# Práctica 5: datamx.io
# Autora: Ana Escoto 
# Fecha: 2026-01-21
###############################################################################-

# Paquetes----

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  httr2, jsonlite,
  tidyverse,
  janitor, readr, tibble,
  lubridate
)

# Una función -----

mi_funcion <- function(a) {
  b<-a+1
  print(b)
}

mi_funcion(4)

# Función para desc argar
ckan_action <- function(action, query = list(), base = "https://datamx.io/api/3/action") {
  url <- paste0(base, "/", action)
  req <- httr2::request(url)
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
  resp <- httr2::req_perform(req)
  txt  <- httr2::resp_body_string(resp)
  jsonlite::fromJSON(txt, simplifyVector = TRUE)
}

# Una función para buscar
ckan_package_search <- function(q, rows = 10) {
  ckan_action("package_search", query = list(q = q, rows = rows))
}

# Una función para mostrar
ckan_package_show <- function(id) {
  ckan_action("package_show", query = list(id = id))
}

# Acá ya la podemos usar

res_search <- ckan_package_search(q = "Personas desaparecidas y no localizadas", 
                                  rows = 5)

# Resultados (título y "name" del dataset)

hits <- tibble::tibble(
  title = res_search$result$results$title,
  name  = res_search$result$results$name
)

hits

dataset_id <- hits$name[1]
dataset_id

pkg <- ckan_package_show(dataset_id)

recursos <- pkg$result$resources %>%
  as_tibble() %>%
  select(
    name,
    format,
    url,
    last_modified,
    size
  )

recursos

recurso_csv <- recursos %>%
  filter(str_to_upper(format) == "CSV") %>%
  slice(1)

recurso_csv

csv_url <- recurso_csv$url[[1]]
csv_url

# Importación del data.frame 
df_raw <- readr::read_csv(csv_url, show_col_types = FALSE) # con menu

df_raw %>% 
  dplyr::glimpse()

df <- df_raw %>% 
  janitor::clean_names()

names(df)

# Tabulados -----

df %>% 
  janitor::tabyl(entidad) %>% 
  janitor::adorn_totals() %>% 
  janitor::adorn_pct_formatting(digits = 2)
  
tab_entidad <- df %>%
    janitor::tabyl(entidad) %>%
    janitor::adorn_totals("row") %>%
    janitor::adorn_pct_formatting(digits = 1)
  
tab_entidad %>% 
  gt()

tab_entidad %>% 
    head(15)

top10 <- df %>%
    dplyr::count(entidad, sort = TRUE)%>%
    dplyr::slice(1:10)

top10

# tabulados de doble entrada ----

df %>%
  janitor::tabyl(entidad, sexo) %>%
  janitor::adorn_totals(c("row", "col")) # tabulado de frecuencias 

df %>%
  janitor::tabyl(entidad, sexo) %>%
  janitor::adorn_totals(c("row", "col")) %>%
  janitor::adorn_percentages(denominator = "row") %>% # hago porcentajes de fila
  janitor::adorn_pct_formatting(digits = 1)

df %>%
  janitor::tabyl(entidad, sexo) %>%
  janitor::adorn_totals(c("row", "col")) %>%
  janitor::adorn_percentages(denominator = "col") %>% # hago porcentajes de columna
  janitor::adorn_pct_formatting(digits = 1)

df %>%
  janitor::tabyl(entidad, sexo) %>%
  janitor::adorn_totals(c("row", "col")) %>%
  janitor::adorn_percentages(denominator = "all") %>% # hago porcentajes del total
  janitor::adorn_pct_formatting(digits = 1)

df %>% 
  tabyl(fecha_nacimiento)

readr::write_csv(top10, "top10_entidades_desaparecidas.csv")


# Una búsqueda cualquiera con las funciones  ----

# Primero -> hacer la búsqueda 
res_search <- ckan_package_search(q = "pobreza", 
                                  rows = 10)

# Resultados (título y "name" del dataset)

hits <- tibble::tibble(
  title = res_search$result$results$title,
  name  = res_search$result$results$name
)

hits

dataset_id <- hits$name[5]
dataset_id

pkg <- ckan_package_show(dataset_id)

recursos <- pkg$result$resources %>%
  as_tibble() %>%
  select(
    name,
    format,
    url,
    last_modified,
    size
  )

recursos

recurso_csv <- recursos 
recurso_csv

csv_url <- recurso_csv$url[[1]]
csv_url

df_raw2 <- readr::read_csv(csv_url, show_col_types = FALSE) # con menu

## Para otro sitio ----


# Función para desc argar
ckan_action2 <- function(action, query = list(), base = "https://www.datos.gob.mx/api/3/action") {
  url <- paste0(base, "/", action)
  req <- httr2::request(url)
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
  resp <- httr2::req_perform(req)
  txt  <- httr2::resp_body_string(resp)
  jsonlite::fromJSON(txt, simplifyVector = TRUE)
}

# Una función para buscar
ckan_package_search2 <- function(q, rows = 10) {
  ckan_action2("package_search", query = list(q = q, rows = rows))
}


# Primero -> hacer la búsqueda 
res_search2 <- ckan_package_search2(q = "proyecciones", 
                                  rows = 10)


hits2 <- tibble::tibble(
  title = res_search2$result$results$title,
  name  = res_search2$result$results$name
)

hits2


dataset_id2 <- hits2$name[1]
dataset_id2


pkg2 <- ckan_package_show(dataset_id2)

recursos <- pkg2$result$resources %>%
  as_tibble() %>%
  select(
    name,
    format,
    url,
    last_modified,
    size
  )

recursos


recurso_csv <- recursos 
recurso_csv

csv_url <- recurso_csv$url[[2]]
csv_url

df_raw3 <- readr::read_csv(csv_url, show_col_types = FALSE) # con menu
