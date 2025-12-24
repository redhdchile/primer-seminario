library(googlesheets4)
library(dplyr)
library(glue)
library(knitr)

# Desactivar autenticación para spreadsheets públicos
gs4_deauth()

# Leer datos del Google Spreadsheet
url_hoja <- "https://docs.google.com/spreadsheets/d/10CI16PtJPUO5M_sz1bMJeDuIj4QN9phG-DRlhqD51C0/edit?usp=sharing"
datos_participantes <- read_sheet(url_hoja, sheet = "info-ponentes-sitio-web")

# Procesar los datos
datos_participantes <- datos_participantes |>
  mutate(
    # Si foto está vacía, usar default-avatar.svg, si no, agregar la ruta
    foto_path = ifelse(
      is.na(foto) | foto == "", 
      "img/participantes/default-avatar.svg",
      paste0("img/participantes/", foto)
    ),
    # Reemplazar NA en bio con string vacío
    bio = ifelse(is.na(bio), "", bio),
    # Reemplazar NA en nombre con string vacío
    nombre = ifelse(is.na(nombre), "", nombre)
  )

# Generar HTML para cada presentador
generar_div <- function(foto, nombre, bio) {
  glue('
::: {{.presentador-item}}
::: {{.presentador-foto-wrapper}}
![]({foto}){{.presentador-foto}}
:::

::: {{.presentador-info}}
[{nombre}]{{.presentador-nombre}}

{bio}
:::
:::
  ')
}