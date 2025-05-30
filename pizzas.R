##PIZZAS
#Tomado de https://2025-ciencia-reproducible.netlify.app/posts/2025-04-24-paper-typst/

library(stringr)
url_datos <- "URL de los datos"
dat_datos_id <- str_extract(url_datos, "(?<=d/)(.*)(?=/view)")

url_drive <- "https://docs.google.com/uc?id=%s&export=download"
masa <- read.csv(sprintf(url_drive, dat_datos_id))


##Tengo mis datos en sharepoint o en OneDrive
library(Microsoft365R)

archivo <- "M3-estadística/Datos/masa-para-pizza.csv"

# Autoriza el acceso a mi cuenta de OneDrive
od <- get_personal_onedrive()

# Accedo al archivo de interés
datos_od <- od$load_dataframe(archivo, show_col_type = FALSE)

flextable::flextable(datos_od, cwidth = 1)

#La tarea consiste en que preparas un reporte con el formato de un artículo y que sea parte de tu blog.
" En este caso usarás la extensión academic-typst que está en la página de extensiones de Quarto. Para incorporar esta extensión a tu proyecto usa el siguiente comando en la pestaña de terminal.
#quarto use template kazuyanagimoto/quarto-academic-typst
