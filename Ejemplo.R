## EJEMPLO DE Datos de estaciones meteorológicas Conagua
# Reproducción del ejercicio https://2025-ciencia-reproducible.netlify.app/posts/2025-04-11-datos-abiertos/

# descarga el archivo "kmz" de interés
url_agua <- "http://smn.conagua.gob.mx/"
url_kmz <- "tools/RESOURCES/Normales_Climatologicas/EstacionesClimatologicas.kmz"
archivo_kmz <- "./estaciones.kmz"

httr2::request(paste0(url_agua, url_kmz)) |>
  httr2::req_perform() |>
  httr2::resp_body_raw() |>
  writeBin(archivo_kmz)

# desempaca el "kmz" y guarda el resultado en "temp-kmz"
unzip(zipfile = archivo_kmz, exdir = "temp-kmz")

# carga la biblioteca para procesar el "xml"
pacman::p_load(xml2)

data <- read_html("temp-kmz//doc.kml")
xml_data <- as_list(data)

# los datoa de interés están en "folders"
# dentro de la rama html/body/kml/document/
xml_data_1 <- xml_data[["html"]][["body"]][["kml"]][["document"]]

# Estaciones operativas
datos_ext_opr <- xml_data_1[["folder"]][[3]]

# Estaciones suspendidas
datos_ext_ssp <- xml_data_1[["folder"]][[4]]

datos_todo <- c(datos_ext_opr, datos_ext_ssp)

library(tidyr)

for (r in datos_todo)
{
  if(!identical(r[[1]],"Estaciones operativas") &
     (!identical(r[[1]],"Estaciones suspendidas")))
  {
    e <- r$extendeddata$schemadata
    # recupera datos de la estación
    nombres <- character(length(e)-1)
    valores <- character(length(e)-1)
    for (i in 1:(length(e)-1))
    {
      nombres[i] <- attr(e[[i]], "name")
      if (!is_empty(e[[i]])) valores[i] <- e[[i]][[1]]
    }

    datos_temp <- tibble(nombres, valores) %>%
      pivot_wider(names_from = nombres, values_from = valores)

    if (identical(r, datos_ext_opr[[2]]))
    {
      datos_est <- tibble(datos_temp)
    } else {
      datos_est <- rows_append(datos_est, datos_temp)
    }
  }
}

#Error in is_empty(e[[i]]) : no se pudo encontrar la función "is_empty"

library(DT)

datos_est |>
  select(CLAVE, ESTADO, NOMBRE, SITUACION) |>
  datatable()

#############################

library(tidyr)
library(dplyr)
library(DT)

# Datos diarios Xalapa
dat_csv <- "ver/dia30135.txt"
url_meteo <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios"

url_xalapa <- paste(url_meteo, dat_csv, sep = "/")

dat_meteo_meta <- readLines(url_xalapa, n = 25)

# datos de la estación
estacion <- tibble(est = unlist(dat_meteo_meta[11:19])) |>
  mutate(var = str_extract(est, "(.*)(?=\\s:)"),
         val = str_extract(est, "(?<=\\s:)(.*)"),
         var = str_trim(var),
         val = str_trim(val), .keep = "unused" ) |>
  pivot_wider(names_from = var, values_from = val)

# variables meteorologicas
variables <- str_split(readLines(url_xalapa, n = 24)[24], "\t")[[1]][c(1,3:6)]

# datos meteorológicos
dat_meteo <- read.csv(url_xalapa, skip = 25, sep = "\t", header = F)

names(dat_meteo) <- variables

dat_meteo |>
  mutate(idEstacion = estacion$ESTACIÓN) |>
  datatable()
####################################################################
####################################################################

library(tidyr)
library(dplyr)
library(DT)
install.packages("textreadr")
install.packages("ricomisc")
library(rvest)
library(ggplot2)
library(scales)

#Ejemplo limón persa
url_limon <- "http://www.economia-sniim.gob.mx/nuevo/Consultas/MercadosNacionales/PreciosDeMercado/Agricolas/ResultadosConsultaFechaFrutasYHortalizas.aspx?fechaInicio=01/01/2025&fechaFinal=01/04/2025&ProductoId=426&OrigenId=30&Origen=Veracruz&DestinoId=301&Destino=Veracruz:%20Central%20de%20Abasto%20de%20Jalapa&PreciosPorId=2&RegistrosPorPagina=500"

page <- read_html(url_limon)

tabla_precios <- page %>%
  html_node("table#tblResultados") %>%
  html_table(header = TRUE) %>%
  slice(-1) %>%
  mutate(precio_num = as.numeric(`Precio Frec`),
         feha_d = as.Date(Fecha, format = "%d/%m/%Y")) %>%
  rename_with(., ~ str_replace_all(iconv(.x, to='latin1//TRANSLIT'), "'", ""))

# print result to console
tabla_precios |>
  select(Fecha:Obs.) |>
  datatable()


tabla_precios |>
  ggplot(aes(x = feha_d, y = precio_num)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.3) +
  labs(title = "Precio del limón sin semilla",
       subtitle = "Xalapa, Ver. entre 1/ene/2025 y 1/abr/2025") +
  xlab(label = "Fecha") +
  ylab(label = "Precio ($ mn/kg)") +
  scale_x_date(labels = date_format("%b-%Y", locale = "es"))
