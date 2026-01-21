###############################################################################-
# Práctica 3
# Consulta de INEGI y Banxico --> paquetes con tokens
# Curso intersemestral
# Autora: Ana Escoto 
# Fecha: 2026 - 01 - 19
###############################################################################-

# Tokens ----

# token_inegi <- "AQUI_VA_TU_TOKEN_INEGI"
# token_bmx   <- "AQUI_VA_TU_TOKEN_BANXICO"
# usethis::edit_r_environ()

token_inegi <- Sys.getenv("INEGI_TOKEN")
token_bmx   <- Sys.getenv("BANXICO_TOKEN")

nchar(token_inegi); nchar(token_bmx)

# Paquetes ----

pacman::p_load(
  tidyverse,
  inegiR,
  siebanxicor,
  httr, jsonlite,
  lubridate,
  plotly
)

# Consultas a INEGI ----


ipc <- inegiR::inegi_series(
  serie = 910392,  # ejemplo de ID
  token = token_inegi,
  database = "BIE-BISE" # ojo distinto a documentación
)

dplyr::glimpse(ipc)

head(ipc, 5)

ipc2 <- ipc %>%
  mutate(
    date = as.Date(date),
    values = as.numeric(values)
  ) %>%
  arrange(date)

ipc2 %>% tail(n=10) # las últimas 6

# Construir la URL ----

paste("a","B", sep = "-")
paste0("a", "B")

# Ejemplo ilustrativo: sustituye con una URL real del constructor
url_ejemplo <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/910392/es/00/false/BIE-BISE/2.0/", token_inegi,"?type=json")

resp <- httr::GET(url_ejemplo)

httr::status_code(resp)

txt <- httr::content(resp, as = "text", encoding = "UTF-8")

jsonlite::fromJSON(txt) |> names()

consulta_a_mano <- jsonlite::fromJSON(txt) %>% 
  pluck("Series") %>% 
  pluck("OBSERVATIONS") %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>% 
  mutate(obs_value=as.numeric(obs_value)) %>% 
  select(time_period, obs_value)

# Consulta BANXICO ---


# Guardar token en la sesión (recomendado; no lo imprimas en pantalla)

siebanxicor::setToken(token_bmx)

serie_id <- "SP1"  # EJEMPLO: cambia por una serie real de tu interés

bmx <- siebanxicor::getSeriesData(serie_id) %>% as.data.frame()

glimpse(bmx)

head(bmx, 5)

bmx2 <- bmx %>%
  mutate(
    date = as.Date(SP1.date),# revisen el valor del indicador
    value = as.numeric(SP1.value)
  ) %>%
  arrange(date) %>% 
  select(-c(starts_with("SP1")))


bmx2 %>% 
  ggplot2::ggplot(aes(x=date, 
                      y= value)) +
  geom_line()

# Grafica ----
ggplot(bmx2, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = paste("Banxico ·", serie_id),
    x = NULL, y = "Valor"
  )

grafica <- ggplot(bmx2, aes(x = date, y = value)) +
  geom_line() +
  labs(
    title = paste("Banxico ·", serie_id),
    x = NULL, y = "Valor"
  )

ggplotly(grafica)