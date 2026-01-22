################################################################################
# Curso intersemestral
# Práctica 4: tablas "bonitas"
# Autora: Ana Escoto 
# Fecha: 2026-01-21
###############################################################################-

# Paquetes ----

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  scales,
  janitor,
  WDI,
  imfapi,
  countrycode,
  gt, # grammar of tables
  knitr,
  kableExtra # kableExtra
)

# Repasito de WDI

wdi_raw <- WDI(
  indicator = "SP.POP.TOTL",
  country = c("MEX", "BRA", "ARG", "COL", "CHL"),
  star = 2022,
  end = 2022
)

wdi_raw

wdi_raw %>%
  dplyr::select(country, SP.POP.TOTL) %>%
  dplyr::rename(
    País = country,
    Población = SP.POP.TOTL # ponemos el nombre el indicador
  ) %>%
  kableExtra::kable()

# Acá vamos poniendo cosas ya dentro de kable
wdi_raw %>%
  select(country, SP.POP.TOTL) %>%
  rename(
    País = country,
    Población = SP.POP.TOTL
  ) %>%
  mutate(Población = scales::comma(Población))%>%
  kable(
    caption = "Población total (2022)",
    align = "rc" # left -> l - center -> c right ->
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE 
  )

# gt() grammar of tables ----

wdi_raw %>%
  select(country, SP.POP.TOTL) %>%
  rename(
    country_code = country,
    Población = SP.POP.TOTL
  ) %>%
  gt()

tabla_gt <- wdi_raw %>%
  select(country,  SP.POP.TOTL) %>%
  rename(
    country_code = country,
    Población = SP.POP.TOTL
  ) %>%
  gt() %>%
  tab_header(
    title = "Población total en países seleccionados",
    subtitle = "Banco Mundial, 2022"
  )%>%
  fmt_number(
    columns = Población,
    decimals = 0,
    use_seps = TRUE
  )

tabla_gt

tabla_gt_flags <- wdi_raw %>%
  select(country, iso2c , SP.POP.TOTL) %>%
  rename(
    country_code = country,
    Población = SP.POP.TOTL
  ) %>%
  gt() %>% 
  gt::fmt_flag(columns = iso2c) %>%
  fmt_number(
    columns = Población,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  tab_header(
    title = "Población total con banderas",
    subtitle = "Banco Mundial (WDI)"
  )

tabla_gt_flags

# Repaso de IMF ----

weo <- imfapi::imf_get(
  dataflow_id = "WEO",
  dimensions  = list(
    INDICATOR = "NGDP_RPCH", # solo un indicador
    COUNTRY = c("MEX", "BRA", "ARG", "COL", "CHL")
  )
)

weo %>% 
  janitor::clean_names(case="title")

# Cómo hacemos para quedarnos con el último valor disponible para cada país

weo_simple <- weo %>%
  janitor::clean_names() %>%
  group_by(country) %>%
  filter(time_period == max(time_period, na.rm = TRUE)) %>%
  ungroup() %>%
  select(country, time_period, obs_value) 

weo_simple %>%
  gt(rowname_col = "country") %>%
  fmt_number(
    columns = obs_value,
    decimals = 2
  ) %>%
  tab_header(
    title = "Crecimiento del PIB real",
    subtitle = "Último año disponible (FMI – WEO)"
  ) %>%
  cols_label(
    time_period = "Año",
    obs_value = "Variación %"
  )

# Fusión de tablas ----


paises_iso3 <- c("MEX", "GTM", "URY", "BRA", "USA")

wdi_1980 <- WDI::WDI(
  country   = paises_iso3,
  indicator = c(pop_total = "SP.POP.TOTL",
                pib_pc_usd = "NY.GDP.PCAP.CD"), 
  start = 1980) %>%
  as_tibble() %>%
  mutate(
    iso2c = countrycode::countrycode(iso3c, "iso3c", "iso2c")
  ) %>%
  select(iso2c, iso3c, country, year, pop_total, pib_pc_usd)

wdi_1980

weo_gdp <- imfapi::imf_get(
  dataflow_id = "WEO",
  dimensions  = list(
    COUNTRY   = paises_iso3,
    INDICATOR = "NGDP_RPCH",
    FREQUENCY = "A"
  ) ) %>%
  as_tibble() %>%
  select(COUNTRY, TIME_PERIOD, OBS_VALUE) %>%
  rename(iso3c = COUNTRY,
         year = TIME_PERIOD,
         gdp_real_growth = OBS_VALUE) %>%
  mutate(year = as.integer(year))

weo_gdp %>% head(10)

tabla <- wdi_1980 %>%
  dplyr::left_join(weo_gdp, by = c("iso3c", "year")) # ojo

tabla

tabla %>%
  select(country, year, pop_total, pib_pc_usd, gdp_real_growth) %>%
  knitr::kable(
    format = "html",
    digits = 1,
    col.names = c(
      "País",
      "Año",
      "Población total",
      "PIB per cápita (USD)",
      "Crecimiento PIB real (%)")
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "left"
  ) %>%
  kableExtra::column_spec(1, bold = TRUE) %>%
  kableExtra::add_header_above(c(" " = 2, "WDI" = 2, "FMI (WEO)" = 1))

tabla %>%
  filter(year%in%c(2000, 2010)) %>% 
  gt::gt() %>%
  gt::fmt_flag(columns = iso2c) %>%
  gt::cols_label(
    iso2c = "",
    country = "País",
    year = "Año (WDI)",
    pop_total = "Población total",
    pib_pc_usd = "PIB per cápita (USD)",
    gdp_real_growth = "Crecimiento PIB real (%)"
  ) %>%
  gt::fmt_number(columns = pop_total, decimals = 0, use_seps = TRUE) %>%
  gt::fmt_currency(columns = pib_pc_usd, currency = "USD", decimals = 0) %>%
  gt::fmt_number(columns = gdp_real_growth, decimals = 1) %>%
  gt::tab_header(
    title = "Indicadores socioeconómicos (WDI) y macro (FMI-WEO)",
    subtitle = "Ejemplo de tabla con banderas usando {gt::fmt_flag()}"
  ) %>%
  gt::tab_spanner(label = "WDI", columns = c(year, pop_total, pib_pc_usd)) %>%
  gt::tab_spanner(label = "FMI (WEO)", columns = c(gdp_real_growth)) %>%
  gt::cols_align(align = "left", columns = c(iso2c, country)) %>%
  gt::cols_align(align = "right", columns = c(pop_total, pib_pc_usd, gdp_real_growth)) %>%
  gt::opt_table_outline() %>%
  gt::opt_row_striping() -> tabla_final

tabla_final

rm(tabla_gt)
ls() #lista todos los objetos del ambiente
rm(list = ls())

gtsave(tabla_final, "tabla_poblacion.docx")

