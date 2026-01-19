# Extracted R code from P1.qmd
# Generated automatically

# ---- Chunk 1: {{r}} ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  rcrossref,
  dplyr,
  tidyr,
  stringr,
  purrr,
  lubridate,
  ggplot2
)

# ---- Chunk 2: {{r}} ----
# Reemplaza por tu correo institucional
options(crr_email = "ana.escoto@politicas.unam.mx")

# ---- Chunk 3: {{r}} ----
res <- cr_works(query = "economía del cuidado", limit = 5)

# ---- Chunk 4: {{r}} ----
res$data %>% 
  names()

# ---- Chunk 5: {{r warning=FALSE}} ----
`%||%` <- function(x, y) if (is.null(x)) y else x

res$data %>% 
  transmute(
    titulo = title,
    anio = issued %||% published.online,
    doi = doi,
    revista = container.title
  )

# ---- Chunk 6: {{r warning=FALSE}} ----
limpiar_works <- function(df) {
  df %>%
    transmute(
      title = map_chr(title, ~ .x[1] %||% NA_character_),
      journal = map_chr(`container.title`, ~ .x[1] %||% NA_character_),
      doi = doi,
      url = url,
      date = issued %||% published.online,
      type = type,
      publisher = publisher
    ) %>% 
    mutate(year= year(as_date(date)))
}

df <- limpiar_works(res$data)
df

# ---- Chunk 7: {{r warning=FALSE}} ----
seg <- cr_works(
  query = "labor market segmentation",
  filter = c(from_pub_date = "2010-01-01"),
  limit = 10
)

limpiar_works(seg$data) %>% 
  select(year, title, journal, doi) %>%
  arrange(desc(year))

# ---- Chunk 8: {{r warning=FALSE}} ----
fec <- cr_works(
  query = "(fertility preferences OR fertility intentions) Mexico",
  filter = c(from_pub_date = "2014-01-01"),
  limit = 10
)

limpiar_works(fec$data) %>% 
  select(year, title, journal, doi) %>%
  arrange(desc(year))

# ---- Chunk 9: {{r warning=FALSE}} ----
ut <- cr_works(
  query = "(unpaid work) OR (time use)",
  filter = c(from_pub_date = "2015-01-01", type = "journal-article"),
  limit = 10
)

limpiar_works(ut$data) %>% 
  select(year, title, journal, doi) %>%
  arrange(desc(year))

# ---- Chunk 10: {{r}} ----
aut <- cr_works(query = NULL, query_author = "Nancy Folbre", limit = 10)
limpiar_works(aut$data) %>% 
  select(year, title, journal, doi) %>%
  arrange(desc(year))

# ---- Chunk 11: {{r}} ----
rev <- cr_works(
  query = "care economy",
  filter = c(container_title = "Feminist Economics", from_pub_date = "2010-01-01"),
  limit = 10
)

limpiar_works(rev$data) %>%
  select(year, title, journal, doi) %>%
  arrange(desc(year))

# ---- Chunk 12: {{r}} ----
rev2 <- cr_works(filter = c(issn = "1354-5701", from_pub_date = "2018-01-01"), limit = 10)
limpiar_works(rev2$data) %>% 
  select(year, title, journal, doi) %>%
  arrange(desc(year))

# ---- Chunk 13: {{r}} ----
one <- cr_works(dois = "10.1038/nphys1170")  # ejemplo (cambia por uno real de tu tema)
one$data %>% limpiar_works()

# ---- Chunk 14: {{r}} ----
# Ejemplo: traer 50 registros en bloques
tema <- "gender wage gap"
cursor <- "*"
acum <- list()

for (i in 1:5) {
  out <- cr_works(query = tema, cursor = cursor, cursor_max = 100, limit = 10)
  acum[[i]] <- out$data
  cursor <- out$meta$`next-cursor`
}

big <- bind_rows(acum) %>% limpiar_works()
big %>% count(year, sort = TRUE)

# ---- Chunk 15: {{r}} ----
big %>%
  filter(!is.na(year)) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  labs(
    title = "Resultados en Crossref por año (ejemplo)",
    x = NULL,
    y = "Número de registros"
  )

# ---- Chunk 16: {{r}} ----
salida <- big %>%
  select(year, title, journal, doi, url) %>%
  arrange(desc(year))

readr::write_csv(salida, "crossref_resultados.csv")
