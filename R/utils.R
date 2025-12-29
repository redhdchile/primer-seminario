library(googlesheets4)
library(dplyr)
library(glue)
library(knitr)
library(stringr)



# Generar HTML para cada presentador
generar_div_presentador <- function(foto, nombre, bio) {
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


# Formatear datos presentadores 
formatear_presentadores <- function(presentadores) {
  if (is.na(presentadores) || presentadores == "") {
    return("")
  }
  # Cambiar comas por <br>
  nombres <- str_split(presentadores, ",")[[1]] |>
    str_trim() |>
    (\(x) paste0("> ", x))() |>    
    paste(collapse = "<br>\n          ")
  return(nombres)
}

# Generar HTML para cada actividad
generar_div_actividad <- function(titulo, descripcion, presentadores, moderador) {
  
  # NAs por cadenas vacías
  titulo <- ifelse(is.na(titulo) || titulo == "", "", titulo)
  descripcion <- ifelse(is.na(descripcion) || descripcion == "", "", descripcion)
  moderador <- ifelse(is.na(moderador) || moderador == "", "", moderador)
  
  html <- '<div class="actividad">\n'
  
  # Título
  if (titulo != "") {
    html <- paste0(html, '        <div class="titulo-actividad">', titulo, '</div>\n')
  }
  
  # Descripción
  if (descripcion != "") {
    html <- paste0(html, '        <div class="descripcion">', descripcion, '</div>\n')
  }
  
  # Presentadores
  presentadores_html <- formatear_presentadores(presentadores)
  if (presentadores_html != "") {
    html <- paste0(html, '        <div class="presentadores">\n          ', presentadores_html, '\n        </div>\n')
  }
  
  # Moderadores
  if (moderador != "") {
    html <- paste0(html, '        <div class="moderador"><strong>Modera:</strong> ', moderador, '</div>\n')
  }
  
  html <- paste0(html, '      </div>')
  return(html)
}

# Generar HTML del cronograma
generar_html_cronograma <- function(datos) {
  html_total <- '<div class="cronograma-container">\n'
  
  for (i in 1:nrow(datos)) {
    fila <- datos[i, ]
    
    # Inicio de fila
    html_total <- paste0(html_total, '  <div class="cronograma-row">\n')
    html_total <- paste0(html_total, '    <div class="horario">', fila$horario, '</div>\n')
    
    # Verificar si hay actividades en paralelo
    tiene_paralelas <- !is.na(fila$actividad_2) && fila$actividad_2 != ""
    
    if (tiene_paralelas) {

      html_total <- paste0(html_total, '    <div class="actividades actividades-paralelas">\n')
      
      # Actividad 1
      html_total <- paste0(html_total, '      ', 
                          generar_div_actividad(fila$actividad_1, fila$descripcion_1, 
                                          fila$presentadores_1, fila$moderador_1), '\n')
      
      # Actividad 2
      html_total <- paste0(html_total, '      ', 
                          generar_div_actividad(fila$actividad_2, fila$descripcion_2, 
                                          fila$presentadores_2, fila$moderador_2), '\n')
      
      html_total <- paste0(html_total, '    </div>\n')
    } else {
      # Actividad única
      html_total <- paste0(html_total, '    <div class="actividades">\n')
      html_total <- paste0(html_total, '      <div class="actividad-unica">\n')
      
      # Solo actividad 1
      if (!is.na(fila$actividad_1) && fila$actividad_1 != "") {
        html_total <- paste0(html_total, '        <div class="titulo-actividad">', fila$actividad_1, '</div>\n')
      }
      
      if (!is.na(fila$descripcion_1) && fila$descripcion_1 != "") {
        html_total <- paste0(html_total, '        <div class="descripcion">', fila$descripcion_1, '</div>\n')
      }
      
      html_total <- paste0(html_total, '      </div>\n')
      html_total <- paste0(html_total, '    </div>\n')
    }
    
    # Fin de fila
    html_total <- paste0(html_total, '  </div>\n\n')
  }
  
  html_total <- paste0(html_total, '</div>')
  return(html_total)
}