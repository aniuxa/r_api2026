###############################################################################-
# Práctica 6: gráficos con ggplot2
# Curso intersemestral 
# Autora: Ana Escoto 
# Fecha: 2026-01-22
#############################################################################-

# Paquetes ----

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, # donde está también ggplot2
  WDI,
  scales, # escalar
  ggthemes, # extensión de temas compatibles con ggplot2
  RColorBrewer, # Genera colores y escalas
  viridis, # colores --> colorblind
  ggrepel, # para las etiquetas no se encimen
  plotly # para gráficos interactivos. 
)

citation(package = "WDI")

# Volver a hacer una consulta con WDI

data("WDI_data")
head(WDI_data)
WDI_data$country

lac <- WDI_data$country %>%
  as_tibble() %>%
  filter(region == "Latin America & Caribbean") %>%
  filter(iso2c != "") %>%
  filter(!region %in% c("Aggregates")) %>%
  distinct(iso2c, country)

lac %>%
  slice_tail(n = 10) # aleatorio

anio_ref <- 2022

indicadores_5 <- c(
  pob_total   = "SP.POP.TOTL",        # población total
  pib_pc      = "NY.GDP.PCAP.CD",     # PIB per cápita (US$ corrientes)
  esperanza   = "SP.DYN.LE00.IN",     # esperanza de vida (años)
  mort_inf    = "SP.DYN.IMRT.IN",     # mortalidad infantil (por 1000 nacidos vivos)
  educ_gdp    = "SE.XPD.TOTL.GD.ZS"   # gasto público en educación (% del PIB)
)
lac$country

# Hacemos una nueva consulta a WDI

wdi_5_2022 <- WDI(
  country = lac$iso2c, #los codigos de los países
  indicator = indicadores_5, # listado de indicadores
  start = anio_ref, # el año de inicio
  end = anio_ref # año de finalización
) %>%
  as_tibble() %>%
  select(iso2c, country, year, everything())

wdi_5_2022 %>%
  slice_max(educ_gdp, n = 10) # top10 de países de LAC en gasto en educación % PIB

top15_pibpc <- wdi_5_2022 %>%
  select(country, year, pib_pc) %>%
  filter(!is.na(pib_pc)) %>%
  slice_max(pib_pc, n = 15)

!is.na(c(NA, 9, 9))

class(top15_pibpc)

top15_pibpc %>% # dataframe
  ggplot() + # símbolos de +
  aes(x = country, 
      y = pib_pc) +
  geom_col() +
  coord_flip()


top15_pibpc %>% # dataframe
  mutate(country = forcats::fct_reorder(country, pib_pc)) %>% # se ordenan los países según el desempeño del pib pc
  ggplot() + # símbolos de +
  aes(x = country, 
      y = pib_pc) +
  geom_col() +
  coord_flip()

top15_pibpc %>% # dataframe
  mutate(country = forcats::fct_reorder(country, pib_pc)) %>% # se ordenan los países según el desempeño del pib pc
  ggplot() + # símbolos de +
  aes(x = country, 
      y = pib_pc) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Mi título", 
    subtitle = "Subtítulo",
    x = "País", 
    y = "PIB pc", 
    caption = "Fuente:  Arel-Bundock V (2025): _WDI: World Development Indicators and Other World Bank Data_."
  )

top15_pibpc %>%
  mutate(country = fct_reorder(country, pib_pc)) %>%
  ggplot() +
  aes(x = country, y = pib_pc) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "PIB per cápita en América Latina y el Caribe",
    subtitle = paste0("Top 15 países, año ", anio_ref),
    x = NULL,
    y = "PIB per cápita (US$ corrientes)"
  ) +
  theme_minimal()

top15_pibpc %>%
  mutate(country = fct_reorder(country, pib_pc)) %>%
  ggplot() +
  aes(x = country, y = pib_pc) +
  geom_col() +
  geom_text(aes(label = pib_pc)) +
  coord_flip() 

top15_pibpc %>%
  mutate(country = fct_reorder(country, pib_pc)) %>%
  ggplot(aes(x = country, y = pib_pc)) +
  geom_col() +
  geom_text(
    aes(label = scales::dollar(pib_pc,
    hjust = -0.1,
    size = 3)
  )) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "PIB per cápita en América Latina y el Caribe",
    subtitle = paste0("Top 15 países, año ", anio_ref),
    x = NULL,
    y = "PIB per cápita (US$ corrientes)"
  ) +
  theme_minimal()

# Parte práctica 

# Elijan otro indicador y hacen un top10 de los países LAC


## Vamos a comparar un solo país en el tiempo ---

mex_gdp_growth <- WDI(
  country = "MX",
  indicator = c(gdp_growth = "NY.GDP.MKTP.KD.ZG"),
  start = 2000, 
  end = 2024
) %>%
  as_tibble() %>%
  select(country, iso2c, year, gdp_growth) %>%
  arrange(year)

mex_gdp_growth %>% slice_head(n = 10)

mex_gdp_growth %>% 
  ggplot() +
  aes(x = year,
      y = gdp_growth) +
  geom_line() +
  geom_hline(yintercept = 0, color = "#FAAC78") +
  geom_vline(xintercept = 2017,
             color = "cyan4",
             linetype = "longdash", 
             linewidth = 3)

periodos <- tibble(
  inicio = c(2019, 2025),
  fin    = c(2024, 2024),
  etiqueta = c("Periodo AMLO (años completos)",
               "Periodo Sheinbaum (años completos)")
) %>%
  mutate(fin = pmin(fin, max(mex_gdp_growth$year)))

mex_gdp_growth %>%
  ggplot() +
  aes(x = year, y = gdp_growth) +
  geom_line() +
  geom_rect(
    data = periodos, 
    aes(xmin=inicio, 
        xmax = fin, 
        ymin = -Inf,
        ymax = Inf), 
    alpha = 0.2, 
    inherit.aes = FALSE, 
    fill = "#660C2B" ) + 
  geom_hline(yintercept = 0, linewidth = 0.4, color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", linewidth = 0.6) +
  annotate("text",
           x = 2020.2,
           y = max(mex_gdp_growth$gdp_growth, na.rm = TRUE) + 0.4,
           label = "COVID-19 (2020)",
           hjust = 0, vjust = 1, size = 2.5) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title = "México: crecimiento anual del PIB",
    subtitle = "WDI (NY.GDP.MKTP.KD.ZG) con anotación de COVID-19 y periodos (aprox.)",
    x = NULL,
    y = "Crecimiento del PIB (% anual)"
  ) +
  theme_minimal()

# Comparar países y sus tendencias ----

paises_comp <- c("MEX", "BRA", "ARG", "CHL", "COL", "PER", "ECU", "URY")

pibpc_ts <- WDI(
  country = paises_comp,
  indicator = c(pibpc_const = "NY.GDP.PCAP.KD"),
  start = 2010, 
  end = 2024
) %>%
  as_tibble() %>%
  select(country, iso2c, year, pibpc_const) %>%
  arrange(country, year)

pibpc_ts[pibpc_ts$year == 2010,]$pibpc_const

pibpc_indice <- pibpc_ts %>%
  group_by(country) %>%
  mutate(base_2010 = pibpc_const[year == 2010][1])%>%
  ungroup() %>%
  mutate(indice_2010 = 100 * pibpc_const / base_2010)

pibpc_indice %>% slice_head(n = 10)

pibpc_indice %>%
  ggplot() +
  aes(x = year, y = indice_2010, color = country) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(
    title = "PIB per cápita (constante): comparación de tendencias",
    subtitle = "Índice 2010 = 100 (valores relativos para comparar trayectorias)",
    x = NULL,
    y = "Índice (2010 = 100)",
    color = "País"
  ) +
  theme_minimal()

# Cambiando la paleta de colores

RColorBrewer::display.brewer.all()

pibpc_indice %>%
  ggplot() +
  aes(x = year, y = indice_2010, color = country) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "PIB per cápita (constante): comparación de tendencias",
    subtitle = "Índice 2010 = 100 (valores relativos para comparar trayectorias)",
    x = NULL,
    y = "Índice (2010 = 100)",
    color = "País"
  ) +
  theme_minimal()

pibpc_indice %>%
  ggplot() +
  aes(x = year, y = indice_2010, color = country) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  scale_color_viridis_d(option = "D") +
  labs(
    title = "PIB per cápita (constante): comparación de tendencias",
    subtitle = "Índice 2010 = 100 (valores relativos para comparar trayectorias)",
    x = NULL,
    y = "Índice (2010 = 100)",
    color = "País"
  ) +
  ggthemes::theme_excel_new() -> g1

# Paletas manuales ----

mi_paleta <-c("#CFC6B5", "#242424", "#6A1235", "#656565", "#9C8546", "#98647A") # 6 categorías

# Me quedo con 6 países

unique(pibpc_indice$country)

pibpc_indice %>%
  filter(country%in%unique(pibpc_indice$country)[1:6]) %>% 
  ggplot() +
  aes(x = year, y = indice_2010, color = country) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  scale_color_manual(values = mi_paleta) +
  labs(
    title = "PIB per cápita (constante): comparación de tendencias",
    subtitle = "Índice 2010 = 100 (valores relativos para comparar trayectorias)",
    x = NULL,
    y = "Índice (2010 = 100)",
    color = "País"
  )

# Más sobre viridis
# https://scales.r-lib.org/reference/pal_viridis.html


ggsave(plot = g1, 
       filename = "mi_primer_grafico.png", 
       height = 5,
       width = 9, 
       dpi = 300)


ggsave(plot = ggplot2::last_plot(), #visible en la pantalla de PLOTS
       filename = "mi_primer_grafico2.png", 
       height = 5,
       width = 9, 
       dpi = 300)

## Opcional

paises_10 <- c("MEX", "BRA", "ARG", "CHL", "COL", "PER", "ECU", "URY", "GTM", "DOM")

ev_2013_2023 <- WDI(
  country = paises_10,
  indicator = c(ev = "SP.DYN.LE00.IN"),
  start = 2013, end = 2023
) %>%
  as_tibble() %>%
  select(country, iso2c, year, ev) %>%
  filter(year %in% c(2013, 2023)) %>%
  pivot_wider(names_from = year, 
              values_from = ev) %>%
  rename(ev_2013 = `2013`, ev_2023 = `2023`) %>%
  mutate(cambio = ev_2023 - ev_2013)

ev_2013_2023

lims <- range(c(ev_2013_2023$ev_2013, ev_2013_2023$ev_2023), na.rm = TRUE)

ev_2013_2023 %>%
  ggplot(aes(x = ev_2013, y = ev_2023, label = country, color = cambio)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.6) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(show.legend = FALSE, max.overlaps = 20) +
  scale_x_continuous(limits = lims) +
  scale_y_continuous(limits = lims) +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Esperanza de vida: comparación 2013 vs 2023",
    subtitle = "La diagonal representa ‘sin cambio’ (y = x). El color muestra el cambio (2023 - 2013).",
    x = "Esperanza de vida (años) en 2013",
    y = "Esperanza de vida (años) en 2023",
    color = "Cambio"
  ) +
  theme_minimal() -> g_ev
g_ev

g_ev +
  labs(
    title = "Ejemplo con ggthemes: theme_economist()",
    x = "2013",
    y = "2023"
  ) +
  ggthemes::theme_economist()

pibpc_indice %>%
  filter(year %in% c(2013, 2024)) %>%
  ggplot(aes(x = factor(year), y = indice_2010, fill = country)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Paleta discreta (RColorBrewer)",
    x = NULL,
    y = "Índice (2010=100)",
    fill = "País"
  ) +
  theme_minimal()

ev_2013_2023 %>%
  ggplot(aes(x = ev_2013, y = ev_2023, color = cambio)) +
  geom_point(size = 3) +
  scale_color_viridis_c(option = "D") +
  labs(
    title = "Paleta continua (viridis)",
    x = "2013",
    y = "2023",
    color = "Cambio"
  ) +
  theme_minimal()

g1 <- top15_pibpc %>%
  mutate(country = fct_reorder(country, pib_pc)) %>%
  ggplot(aes(x = country, y = pib_pc)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "PIB per cápita en LAC (Top 15)",
    subtitle = paste0("Año ", anio_ref),
    x = NULL,
    y = "US$ corrientes"
  ) +
  theme_minimal()

g1

ggsave("fig_pibpc_top15.png", 
       plot = g1, 
       width = 9, height = 5,
       dpi = 300)

ggsave("fig_pibpc_top15.pdf",
       plot = g1,
       width = 9,
       height = 5)

interactivo<- ggplotly(g1)
interactivo
