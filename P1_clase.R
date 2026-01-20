###############################################################################-
# Curso Intersemestral 
# Uso de API y paquetes 
# Autora: Ana Escoto 
# Fecha: 2026-01-19
###############################################################################-

# Paquetes ----

pacman::p_load(rcrossref, 
               tidyverse, 
               lubridate)


# Establecer el correo  ----

options(crr_email = "ana.escoto@politicas.unam.mx")

citation(package = "rcrossref")

# Hacer búsquedas por "trabajos"

rcrossref::cr_works(query = "economía del cuidado", limit = 5)

ref <- rcrossref::cr_works(query = "economía del cuidado", limit = 5)

# Nos da como resultado una lista
# Una lista es un conjunto de varios objetos

ref$meta # metadatos

ref$data # una tabla con los resultados

ref <- rcrossref::cr_works(query = "economía del cuidado", limit = 10, sort="relevance")

ref$data

head(ref$data) # las primeras 6 líenas
names(ref$data) # me da los nombres de las columnas

# con formato de pipe 

# Ctrl + Shift + M 

# %>% # dplyr
# |> # pipe nativo

# slice(decorate(bake(mix(ingredientes))))

ref$data %>% 
  head()

ref$data %>% 
  names()

ref$data %>% 
  dplyr::count(language)

# Trabajar con fechas

ref$data %>% 
  dplyr::select(created, deposited, indexed, issued, published.online, published.print)

# Otras búsquedas ----

cr_works(query = "labor market segmentation", 
        filter = c(from_pub_date = "2015-01-01" ), 
        limit = 30)


cr_works(query = "(fertility preferences) OR (fertility intentions) Mexico", 
         filter = c(from_pub_date = "2015-01-01", type = "journal-article"), 
         limit = 30) 

# Búsquedas por autor(a) ----

cr_works(query = NULL, 
         query_author = "Nancy Folbre", limit = 15)


aut <- cr_works(query = NULL, 
                query_author = "Nancy Folbre", limit = 15)

aut$data$author

# Según journals ----

journals <- cr_journals(query = "The Journal of Peasant Studies", limit = 10) 

journals$data$issn

head(journals$data)

# Los últimos 10 artículos publicados de una revista
cr_journals("0306-6150",  works = T, limit = 10, 
            sort = "issued", order = "desc")


cr_journals(query = "Peasant", limit = 10) 

# Ejemplo: 50 registros por bloques ----

tema <- "gender wage gap"
cursor <- "*"
acum <- list()


for (i in 1:5) {
 busqueda <- cr_works(query = tema, 
                      cursor = cursor, 
                      cursor_max = 100, 
                      limit = 10)
 acum[[i]] <- busqueda$data
 cursor  <- busqueda$meta$`next-cursor`
 }

busqueda$meta
consolidado <- bind_rows(acum)

acum


# Ejercicio 


# Buscar los trabajos de un búsqueda de palabras. 
# Incluir un filtro de los trabajos que sean publicados después de la pandemia
# Incluir 40 registros al menos en 4 bloques

