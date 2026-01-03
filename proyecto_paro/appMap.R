# Título: Visualizador de Municipios de España con Coordenadas CSV
# Descripción: Esta aplicación carga los datos y coordenadas (LONGITUD_ETRS89 y LATITUD_ETRS89)
# de tu archivo CSV 'MUNICIPIOS.csv' y los visualiza en un mapa Leaflet interactivo.

# --- 1. Carga de Librerías Necesarias ---
library(shiny)
library(leaflet) # Para mapas interactivos
library(readr)   # Para leer archivos CSV
library(dplyr)   # Para manipulación de datos
library(sf)      # Para trabajar con datos espaciales
library(ggplot2)
library(tidyr)
library(httr)
library(scales)
library(shinyjs)
library(DT)
library(plotly)

# --- 2. Preparación de Datos ---
# RUTA DEL ARCHIVO CSV DE MUNICIPIOS
ruta_csv_municipios <- "mapData/MUNICIPIOS.csv"
# RUTA DEL ARCHIVO CSV DE PROVINCIAS PARA MAPEO DE CCAA
ruta_csv_provincias <- "mapData/PROVINCIAS.csv"

# Cargar y limpiar el CSV de PROVINCIAS
tryCatch({
    # Cargar la tabla de mapeo Provincia -> CCAA
    mapeo_provincias <- read_delim(
        ruta_csv_provincias,
        delim = ";",
        col_types = cols(.default = "c"),
        locale = locale(encoding = "WINDOWS-1252")
    ) %>%
        # Seleccionar y renombrar las columnas clave para la unión
        select(COD_PROV, PROVINCIA_REF = PROVINCIA, CCAA = COMUNIDAD_AUTONOMA)
        
}, error = function(e) {
    message("Error al cargar o procesar el CSV de provincias: ", e$message)
    stop("Deteniendo la app. Asegúrate de que 'PROVINCIAS.csv' existe.")
})


# Cargar y limpiar el CSV de MUNICIPIOS
tryCatch({
    # Usamos read_delim del paquete readr para ser más robustos con el delimitador ";"
    datos_municipios_crudo <- read_delim(
        ruta_csv_municipios, 
        delim = ";", 
        col_types = cols(.default = "c"), 
        locale = locale(encoding = "WINDOWS-1252")
    )
    
    # Preparación de datos (Renombrado, limpieza y unión)
    datos_mapa <- datos_municipios_crudo %>%
        
        # Renombrar columnas y preparar datos
        rename(
            nombre = NOMBRE_ACTUAL,
            longitud = LONGITUD_ETRS89,
            latitud = LATITUD_ETRS89,
            cod_geo = COD_GEO,
            COD_PROV = COD_PROV # Usamos esta columna como clave de unión
        ) %>%
        
        # Convertir coordenadas y población
        mutate(
            # Reemplazar comas por puntos para la conversión numérica (estándar en R)
            longitud = as.numeric(gsub(",", ".", longitud)),
            latitud = as.numeric(gsub(",", ".", latitud)),
            Poblacion = as.numeric(gsub(",", ".", POBLACION_MUNI))
        ) %>%
        
        # --- ASIGNACIÓN DE CCAA MEDIANTE UNIÓN (JOIN) ---
        # Unir con la tabla de mapeo de provincias usando COD_PROV
        left_join(mapeo_provincias, by = "COD_PROV") %>%
        
        # Crear variable de Categoría para visualización
        mutate(
            Categoria = factor(
                case_when(
                    Poblacion > 50000 ~ "Grande (>50k hab)",
                    Poblacion > 10000 ~ "Mediana (10k-50k hab)",
                    TRUE ~ "Pequeña (<10k hab)"
                )
            )
        ) %>%
        
        # Seleccionar solo las columnas finales que nos interesan
        # Usamos la columna PROVINCIA del CSV de municipios y la CCAA del archivo de mapeo
        select(cod_geo, nombre, longitud, latitud, Poblacion, Categoria, PROVINCIA, CCAA) %>%
        
        # Eliminar filas con coordenadas o nombre faltantes
        filter(!is.na(longitud), !is.na(latitud), !is.na(nombre))
    
    # Convertir el data frame a objeto SF (Spatial Feature)
    datos_mapa_sf <- datos_mapa %>%
        st_as_sf(coords = c("longitud", "latitud"), crs = 4326) 
    
    # Datos globales para el selector
    opciones_ccaa <- sort(unique(na.omit(trimws(datos_mapa$CCAA))))
    opciones_provincia <- sort(unique(na.omit(trimws(datos_mapa$PROVINCIA))))
    
}, error = function(e) {
    message("Error al cargar o procesar el CSV de municipios: ", e$message)
    stop("Deteniendo la app. Asegúrate de que 'MUNICIPIOS.csv' existe y que el formato de coordenadas es correcto.")
})

anios <- 2010:2024
anios_titulos <- paste(min(anios), max(anios), sep = "–")

# Función para leer ficheros SEPE 
leer_sepe_csv <- function(ruta) {
  if (!file.exists(ruta)) {
    warning(sprintf("Archivo no encontrado: %s", ruta))
    return(NULL)
  }
  
  df <- tryCatch(
    read.csv(
      ruta,
      sep = ",",
      fileEncoding = "latin1",
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      message(sprintf("Error leyendo %s : %s", basename(ruta), e$message))
      return(NULL)
    }
  )
  
  if (is.null(df)) return(NULL)
  
  es_col_numerica <- sapply(df, function(x) {
    any(grepl("[0-9]", x, perl = TRUE), na.rm = TRUE)
  })
  cols_num <- names(df)[es_col_numerica]
  
  df[cols_num] <- lapply(df[cols_num], function(col) {
    col_chr <- as.character(col)
    col_chr[col_chr == "<5"] <- "0"
    col_chr
  })
  
  df[cols_num] <- lapply(df[cols_num], function(col) {
    col_corrected <- gsub(",", ".", as.character(col), fixed = TRUE)
    suppressWarnings(as.numeric(col_corrected))
  })
  
  df
}

# normalizamos los nombres de CCAA
normalizar_ccaa <- function(x) {
  x <- trimws(x)
  x <- gsub("–", "-", x)
  x <- gsub("—", "-", x)
  x <- gsub("−", "-", x)
  x <- gsub("  +", " ", x)
  x <- gsub("Castilla *- *La Mancha", "Castilla-La Mancha", x)
  x <- gsub("Castilla *La Mancha", "Castilla-La Mancha", x)
  x
}

dir_data <- "data"
dir_contratos   <- file.path(dir_data, "contratos")
dir_paro        <- file.path(dir_data, "paro")
dir_dtes_empleo <- file.path(dir_data, "dtes_empleo")

encontrar_archivo_anio <- function(dir, anio, patron_base) {
  if (!dir.exists(dir)) {
    warning(sprintf("Directorio no existe: %s", dir))
    return(NA_character_)
  }
  
  patron <- paste0("^", patron_base, "_", anio, ".*\\.csv$")
  
  candidatos <- list.files(dir, pattern = patron, full.names = TRUE, ignore.case = TRUE)
  
  if (length(candidatos) == 0) {
    # por si no lleva .csv (a veces guardan sin extensión) o nombres raros
    patron2 <- paste0("^", patron_base, "_", anio)
    candidatos <- list.files(dir, pattern = patron2, full.names = TRUE, ignore.case = TRUE)
  }
  
  if (length(candidatos) == 0) {
    warning(sprintf("No encontré archivo para %s %s en %s", patron_base, anio, dir))
    return(NA_character_)
  }
  
  # si hay varios, cogemos el primero
  candidatos[[1]]
}

leer_datos_anio <- function(anio) {
  f_paro <- encontrar_archivo_anio(dir_paro, anio, "Paro_por_municipios")
  f_contratos <- encontrar_archivo_anio(dir_contratos, anio, "Contratos_por_municipios")
  f_dtes <- encontrar_archivo_anio(dir_dtes_empleo, anio, "DTES_empleo_por_municipios")
  
  paro <- if (!is.na(f_paro) && file.exists(f_paro)) leer_sepe_csv(f_paro) else NULL
  contratos <- if (!is.na(f_contratos) && file.exists(f_contratos)) leer_sepe_csv(f_contratos) else NULL
  dtes <- if (!is.na(f_dtes) && file.exists(f_dtes)) leer_sepe_csv(f_dtes) else NULL
  
  list(
    anio = anio,
    paro = paro,
    contratos = contratos,
    dtes = dtes
  )
}

# --- 3. Definición de la Interfaz de Usuario (UI) ---
filtros_generales_ui <- function(id, anios) {
  ns <- NS(id)
  
  tagList(
    h4("Filtros generales"),
    selectInput(ns("ccaa_sel"), "Comunidad Autónoma:",
                choices = c("Todas" = "", sort(opciones_ccaa)), selected = NULL, multiple = FALSE),
    selectInput(ns("metrica_sel"), "Seleccione una métrica:",
                choices = c("Paro Registrado" = "paro",
                            "Contratos Registrados" = "contratos",
                            "Demandantes de Empleo" = "dtes"),
                selected = "paro"),
    sliderInput(ns("year_range"), "Rango de años:",
                min = min(anios), max = max(anios),
                value = c(min(anios), max(anios)),
                sep = "")
  )
}

filtros_comparativa_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(ns("ccaa_multi"), "Seleccione las CCAA que desee visualizar:",
                       choices = NULL, selected = NULL, inline = FALSE),
    radioButtons(ns("modo_valor"), "Modo:",
                 choices = c("Valor absoluto" = "abs",
                             "Diferencia vs media nacional" = "diff",
                             "% vs media nacional" = "pct"),
                 selected = "abs")
  )
}

filtros_sectores_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    selectInput(ns("vista_sector"), "Modo de vista:",
                choices = c("Porcentaje (100%)" = "pct",
                            "Volumen" = "abs"),
                selected = "pct")
  )
}

ui <- navbarPage(
  title = paste("SEPE — Datos por CCAA (", anios_titulos, ")"),
  id = "main_nav",
  header = tagList(useShinyjs()),
  
  tabPanel("INICIO",
           sidebarLayout(
             sidebarPanel(
               h4("Filtros generales"),
               selectInput("ccaa_sel", "Comunidad Autónoma:", 
                           choices  = c("Todas" = "", sort(opciones_ccaa)),
                           selected = NULL,
                           multiple = FALSE
               ),
               selectInput("metrica_sel", "Seleccione una métrica:",
                           choices = c("Paro Registrado" = "paro",
                                       "Contratos Registrados" = "contratos",
                                       "Demandantes de Empleo" = "dtes"),
                           selected = "paro"),
               sliderInput("year_range", "Rango de años:",
                           min = min(anios), max = max(anios),
                           value = c(min(anios), max(anios)), sep = ""),
               radioButtons("modo_valor", "Modo de visualización:",
                            choices = c("Valor absoluto" = "abs",
                                        "Diferencia vs media nacional" = "diff",
                                        "% vs media nacional" = "pct"),
                            selected = "abs"),
               width = 5
             ),
             mainPanel(
               h4("Instrucciones"),
               p("Selecciona Comunidad y métrica en el panel izquierdo. Usa la pestaña 'Tabla de datos' para ver datos en bruto."),
               verbatimTextOutput("availability_info"),
               width = 7
             )
           )
  ),
  
  tabPanel("Esta grafica ponerla en Controles", value = "tab_bar",
           fluidRow(
             column(12, h3(textOutput("titulo_grafico1")), p("Evolución anual para la CCAA seleccionada.")),
             column(12, plotOutput("plot_bar_ccaa", height = "420px"))
           )
  ),
  
  tabPanel("VISTA GENERAL", value = "tab_heatmap",
           sidebarLayout(
             sidebarPanel(filtros_generales_ui("vistagen_filtros", anios), width = 4),
             mainPanel(plotOutput("plot_heatmap", height = "420px"), width = 8)
           )
  ),
  
  tabPanel("RANKING DE CCAA", value = "tab_topbottom",
           sidebarLayout(
             sidebarPanel(filtros_generales_ui("ranking_filtros", anios), width = 4),
             mainPanel(plotOutput("plot_bottom5", height = "420px"), width = 8)
           )
  ),
  
  tabPanel("COMPARATIVA", value = "tab_div",
           # fluidRow(
           #   column(12, h3("CCAA vs media nacional (desviación)")),
           #   column(12, plotOutput("plot_comparativa", height = "520px"))
           # )
           sidebarLayout(
             sidebarPanel(filtros_generales_ui("comp_filtros", anios),
                          hr(),
                          filtros_comparativa_ui("comp2"),
                          width = 4),
             mainPanel(plotOutput("plot_comparativa", height = "420px"), width = 8)
           )
  ),
  
  tabPanel("DISTRIBUCIÓN POR SECTORES", value="tab_sector",
           # fluidRow(
           #   column(12, h3("Distribución sectorial (composición)")),
           #   column(12, plotOutput("plot_sector", height="520px"))
           # )
           sidebarLayout(
             sidebarPanel(filtros_generales_ui("sectores_filtros", anios), 
                          hr(),
                          filtros_sectores_ui("sectores_filtros"),
                          width = 4),
             mainPanel(plotOutput("plot_sector", height = "420px"), width = 8)
           )
  ),
  
  tabPanel("PARO VS CONTRATACIÓN", value="tab_bubble",
           # fluidRow(
           #   column(12, h3("Paro vs Contratos por sector (periodo seleccionado)")),
           #   column(12, plotlyOutput("plot_bubble", height="520px"))
           # )
           sidebarLayout(
             sidebarPanel(filtros_generales_ui("bubble_filtros", anios),
                          hr(),
                          # filtro extra
                          checkboxInput("bubble_log", "Escala logarítmica", FALSE),
                          width = 4
             ),
             mainPanel(plotOutput("plot_bubble", height = "420px"), width = 8)
           )
  ),
  
  tabPanel("MAPA INTERACTIVO", value = "mapa",
           fluidPage(
             tags$head(
               tags$style(HTML("
               .leaflet-container {
                height: 80vh;
                border-radius: 8px;
                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
              }
              .header-title {
                text-align: center;
                color: #004d99;
                padding-bottom: 10px;
              }
            "))
          ),
         
          div(class = "header-title",
             h1("Mapa Interactivo de Municipios de España por Agrupación Administrativa")
          ),
             
          fluidRow(
           column(4,
                  selectInput(
                    "ccaa_select", "Filtrar por Comunidad Autónoma:",
                    choices = c("Todas" = "", sort(opciones_ccaa)),
                    selected = ""
                  )
           ),
           column(4,
                  selectInput(
                    "provincia_select", "Filtrar por Provincia:",
                    choices = c("Todas" = ""),
                    selected = ""
                  )
           ),
           column(4,
                  actionButton(
                    "reset_map", "Mostrar Mapa de España Completo",
                    class = "btn-primary", style = "margin-top: 25px;"
                  )
           )
          ),
          
          leafletOutput("mapa_municipios")
          )
  ),
  
  tabPanel("TABLA DE DATOS", value = "tab_table",
           fluidRow(
             column(12, h3("Datos agregados por CCAA y año")),
             column(12, DTOutput("dt_data"))
           )
  )
  
)

# --- 4. Definición del Servidor (Server Logic) ---
server <- function(input, output, session) {

    # 4.1. Filtro de provincias reactivo (depende de la CCAA seleccionada)
    provincias_filtradas <- reactive({
        if (input$ccaa_select == "") {
            # Si no hay CCAA seleccionada, devuelve todas las provincias
            return(datos_mapa$PROVINCIA)
        } else {
            # Si hay CCAA seleccionada, devuelve solo las provincias de esa CCAA
            datos_mapa %>% 
                filter(CCAA == input$ccaa_select) %>% 
                pull(PROVINCIA) %>% 
                unique()
        }
    })
    
    # 4.2. Observador para actualizar el selector de provincias
    observeEvent(provincias_filtradas(), {
        # Obtiene las opciones de provincia actualizadas
        opciones_prov <- sort(provincias_filtradas())
        
        # Actualiza el selector de provincia
        updateSelectInput(
            session, 
            "provincia_select", 
            choices = c("Todas" = "", opciones_prov),
            # Asegura que la selección actual siga siendo válida (o se resetee)
            selected = "" )
    })
    
    # 4.3. Filtrado de datos reactivo para el mapa
    datos_filtrados <- reactive({
        datos <- datos_mapa_sf
        
        # Filtrar por CCAA si está seleccionada
        if (input$ccaa_select != "") {
            datos <- datos %>% filter(CCAA == input$ccaa_select)
        }
        
        # Filtrar por Provincia si está seleccionada
        if (input$provincia_select != "") {
            datos <- datos %>% filter(PROVINCIA == input$provincia_select)
        }
        
        return(datos)
    })
    
    # 4.4. Lógica para resetear el mapa
    observeEvent(input$reset_map, {
        updateSelectInput(session, "ccaa_select", selected = "")
        updateSelectInput(session, "provincia_select", selected = "")
        # Recentra el mapa a la vista general de España al resetear
        leafletProxy("mapa_municipios") %>%
            setView(lng = -3.70, lat = 40.41, zoom = 6)
    })


    # Paleta de colores (calculada fuera del renderLeaflet para ser eficiente)
    pal <- colorFactor(
        palette = c("#cc0000", "#ffcc00", "#00b300"), 
        domain = datos_mapa_sf$Categoria
    )
    
    # 4.5. Renderizado del mapa Leaflet (Mapa base estático)
    output$mapa_municipios <- renderLeaflet({
        leaflet() %>%
            addTiles(
                options = providerTileOptions(minZoom = 5)
            ) %>%
            # Centrar inicialmente en el centro de España
            setView(lng = -3.70, lat = 40.41, zoom = 6) %>%
            # Añadir la leyenda al mapa base estático
            addLegend(pal = pal, values = datos_mapa_sf$Categoria, opacity = 0.9, title = "Población Municipal",
                      position = "bottomright")
    })
    
    # 4.6. Proxy para actualizar los marcadores dinámicamente (AHORA SIEMPRE CON CLUSTERING GEOGRÁFICO)
    observe({
        
        # Ya no se comprueba la vista de "full Spain", siempre se usan los datos filtrados
        datos_actuales <- datos_filtrados()
        
        proxy <- leafletProxy("mapa_municipios") %>%
            # Limpiar todos los marcadores y agrupadores anteriores
            clearMarkers() %>%
            clearMarkerClusters()
            
        # Recalcular las etiquetas de popup para los municipios actuales
        labels_actuales <- paste0(
            "<strong>", datos_actuales$nombre, " (", datos_actuales$PROVINCIA, ")", "</strong><br/>",
            "Cód. GEO: ", datos_actuales$cod_geo, "<br/>",
            "Población: ", format(datos_actuales$Poblacion, big.mark = ".", decimal.mark = ","), "<br/>",
            "Categoría: ", datos_actuales$Categoria
        ) %>% lapply(htmltools::HTML)

        proxy %>%
            # Añadir los marcadores con CLUSTERING GEOGRÁFICO estándar
            addCircleMarkers(
                data = datos_actuales,
                radius = ~log(Poblacion + 1) * 2,
                color = ~pal(Categoria),
                stroke = FALSE, 
                fillOpacity = 0.8,
                popup = labels_actuales,
                clusterOptions = markerClusterOptions(), # Clustering geográfico SIEMPRE activo
                layerId = ~cod_geo 
            )
            
        # Ajustar el zoom del mapa automáticamente al área filtrada si hay datos
        if (nrow(datos_actuales) > 0) {
            # Calcula los límites para el ajuste de la vista
            bounds <- st_bbox(datos_actuales)
            proxy %>%
                fitBounds(
                    lng1 = bounds["xmin"], lat1 = bounds["ymin"],
                    lng2 = bounds["xmax"], lat2 = bounds["ymax"]
                )
        }
    })
    
    # --- SERVER SEPE ---
    
    cache <- reactiveValues(
      base = NULL,
      sector = NULL
    )
    
    agregar_ccaa_anio <- function(df_raw, metric = c("paro", "contratos", "dtes")) {
      metric <- match.arg(metric)
      if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)
      
      names(df_raw) <- trimws(names(df_raw))

      col_ccaa <- if ("Comunidad Aut" %in% names(df_raw)) "Comunidad Aut" else
        names(df_raw)[grepl("comunidad", names(df_raw), ignore.case = TRUE)][1]
      if (is.na(col_ccaa)) return(NULL)
      
      if (!("anio" %in% names(df_raw))) return(NULL)
      df_raw$anio <- suppressWarnings(as.integer(df_raw$anio))
      df_raw <- df_raw[!is.na(df_raw$anio), , drop = FALSE]
      if (nrow(df_raw) == 0) return(NULL)
      
      col_val <- switch(
        metric,
        paro = "total Paro Registrado",
        contratos = "Total Contratos",
        dtes = "total Dtes Empleo"
      )
      
      # si en contratos/dtes el nombre exacto cambia un poco, lo buscamos por patrón
      if (!(col_val %in% names(df_raw))) {
        patron <- switch(
          metric,
          paro = "total.*paro",
          contratos = "total.*contrat",
          dtes = "total.*dtes|total.*demand"
        )
        col_val <- names(df_raw)[grepl(patron, names(df_raw), ignore.case = TRUE)][1]
      }
      if (is.na(col_val)) return(NULL)
      
      df_raw[[col_ccaa]] <- normalizar_ccaa(as.character(df_raw[[col_ccaa]]))
      df_raw[[col_val]] <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df_raw[[col_val]]))))
      df_raw[[col_val]][is.na(df_raw[[col_val]])] <- 0
      
      df_raw %>%
        group_by(anio, comunidad = .data[[col_ccaa]]) %>%
        summarise(valor = sum(.data[[col_val]], na.rm = TRUE), .groups = "drop")
    }
    
    # ---- Datos agregados base (CCAA x año) ----
    datos_agregados <- reactive({
      if (!is.null(cache$base)) return(cache$base)
      
      contratos_list <- list()
      paro_list <- list()
      dtes_list <- list()
      
      withProgress(message = "Cargando y agregando datos SEPE", value = 0, {
        for (i in seq_along(anios)) {
          ano <- anios[i]
          setProgress(i / length(anios), detail = paste("Procesando año", ano))
          
          pack <- leer_datos_anio(ano)
          
          df_c <- pack$contratos
          df_p <- pack$paro
          df_d <- pack$dtes
          
          if (!is.null(df_c)) {
            agg <- agregar_ccaa_anio(df_c, "contratos")
            if (!is.null(agg)) contratos_list[[as.character(ano)]] <- agg %>%
                rename(contratos_total = valor)
          }
          
          if (!is.null(df_p)) {
            agg <- agregar_ccaa_anio(df_p, "paro")
            if (!is.null(agg)) paro_list[[as.character(ano)]] <- agg %>%
                rename(paro_total = valor)
          }
          
          if (!is.null(df_d)) {
            agg <- agregar_ccaa_anio(df_d, "dtes")
            if (!is.null(agg)) dtes_list[[as.character(ano)]] <- agg %>%
                rename(dtes_total = valor)
          }
        }
      })
      
      contratos_ccaa <- if (length(contratos_list) > 0) bind_rows(contratos_list) else
        tibble::tibble(anio = integer(), comunidad = character(), contratos_total = numeric())
      paro_ccaa <- if (length(paro_list) > 0) bind_rows(paro_list) else
        tibble::tibble(anio = integer(), comunidad = character(), paro_total = numeric())
      dtes_ccaa <- if (length(dtes_list) > 0) bind_rows(dtes_list) else
        tibble::tibble(anio = integer(), comunidad = character(), dtes_total = numeric())
      
      df <- paro_ccaa %>%
        full_join(contratos_ccaa, by = c("anio", "comunidad")) %>%
        full_join(dtes_ccaa, by = c("anio", "comunidad")) %>%
        mutate(comunidad = as.character(comunidad), anio = as.integer(anio))
      
      cache$base <- df
      df
    })
    
    datos_sector <- function(metrica_sel, year_range) {
      sector_cols <- switch(
        metrica_sel,
        "contratos" = c("Contratos  Agricultura","Contratos  Industria","Contratos Construcción","Contratos  Servicios"),
        "paro"      = c("Paro Agricultura","Paro Industria","Paro Construcción","Paro Servicios","Paro Sin empleo Anterior"),
        "dtes"      = c("Dtes EmpleoAgricultura","Dtes Empleo Industria","Dtes Empleo Construcción","Dtes Empleo Servicios","Dtes Empleo Sin empleo Anterior")
      )
      sector_cols <- trimws(gsub("  +", " ", sector_cols))
      
      anios_sel <- seq(year_range[1], year_range[2])
      out <- list()
      
      for (ano in anios_sel) {
        pack <- leer_datos_anio(ano)
        df <- switch(
          metrica_sel,
          "paro" = pack$paro,
          "contratos" = pack$contratos,
          "dtes" = pack$dtes
        )
        if (is.null(df)) next
        
        # normalizamos nombres por si hay dobles espacios u otros
        names(df) <- trimws(gsub("  +", " ", names(df)))
        if (!all(sector_cols %in% names(df))) next
        
        # agregamos por CCAA+Año pero sumando columnas sectoriales
        df[[ "Comunidad Aut" ]] <- normalizar_ccaa(as.character(df[[ "Comunidad Aut" ]]))
        df$anio <- suppressWarnings(as.integer(df$anio))
        
        agg <- df %>%
          group_by(anio, comunidad = .data[["Comunidad Aut"]]) %>%
          summarise(across(all_of(sector_cols), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = "drop")
        
        out[[as.character(ano)]] <- agg
      }
      
      if (length(out) == 0) return(NULL)
      
      bind_rows(out) %>%
        pivot_longer(cols = all_of(sector_cols), names_to = "sector", values_to = "valor") %>%
        mutate(comunidad = as.character(comunidad), anio = as.integer(anio))
    }

    observeEvent(datos_agregados(), {
      df <- datos_agregados()
      req(df, nrow(df) > 0, "comunidad" %in% names(df))
      
      ccaa_choices2 <- sort(unique(na.omit(as.character(df$comunidad))))
      req(length(ccaa_choices2) > 0)
      
      updateSelectInput(session, "ccaa_sel", choices = ccaa_choices2, selected = ccaa_choices2[1])
      
      updateCheckboxGroupInput(session, "comp2-ccaa_multi",
                               choices = ccaa_choices2,
                               selected = ccaa_choices2[1:min(6, length(ccaa_choices2))])
    })
    
    # --- SERVER SEPE ---

    col_metrica <- function(metrica) {
      switch(metrica,
             paro = "paro_total",
             contratos = "contratos_total",
             dtes = "dtes_total",
             "paro_total")
    }
    
    # aplica modo (abs / diff / pct) contra media nacional del periodo
    aplicar_modo <- function(df, modo, col_val) {
      if (modo == "abs") return(df)
      
      media_nacional <- mean(df[[col_val]], na.rm = TRUE)
      if (is.na(media_nacional) || media_nacional == 0) media_nacional <- 1
      
      if (modo == "diff") {
        df[[col_val]] <- df[[col_val]] - media_nacional
      } else if (modo == "pct") {
        df[[col_val]] <- 100 * (df[[col_val]] - media_nacional) / media_nacional
      }
      df
    }
    
    # ---- TAB INICIO: texto informativo ----
    output$availability_info <- renderText({
      df <- datos_agregados()
      req(df)
      
      if (nrow(df) == 0 || all(is.na(df$anio))) {
        return("Cargando datos... (aún no hay registros agregados)")
      }
      
      rango <- input$year_range
      req(length(rango) == 2)
      
      paste0(
        "Datos cargados: ", length(unique(df$comunidad)), " CCAA · ",
        min(df$anio, na.rm = TRUE), "–", max(df$anio, na.rm = TRUE),
        " | Rango seleccionado: ", rango[1], "–", rango[2]
      )
    })
    
    output$titulo_grafico1 <- renderText({
      met <- input$metrica_sel %||% "paro"
      ccaa <- input$ccaa_sel %||% ""
      paste0("Evolución anual — ", ccaa, " (", met, ")")
    })
    
    output$plot_bar_ccaa <- renderPlot({
      df <- datos_agregados()
      req(df)
      
      ccaa_ui <- normalizar_ccaa(input$ccaa_sel)
      rango <- input$year_range
      metrica <- input$metrica_sel
      modo <- input$modo_valor
      req(rango, metrica, modo)
      
      col <- col_metrica(metrica)
      
      d <- df %>%
        mutate(comunidad_norm = normalizar_ccaa(comunidad)) %>%
        filter(comunidad_norm == ccaa_ui, anio >= rango[1], anio <= rango[2]) %>%
        transmute(anio, comunidad = comunidad_norm, valor = .data[[col]])
      
      req(nrow(d) > 0)
      d <- aplicar_modo(d, modo, "valor")
      
      ggplot(d, aes(x = anio, y = valor)) +
        geom_col() +
        labs(
          x = "Año",
          y = ifelse(modo == "pct", "% vs media nacional",
                     ifelse(modo == "diff", "Diferencia vs media nacional", "Valor")),
          title = paste0("Evolución anual — ", input$ccaa_sel)
        ) +
        theme_minimal()
    })
    
    output$plot_heatmap <- renderPlot({
      df <- datos_agregados()
      req(df)
      
      metrica <- input[["vistagen_filtros-metrica_sel"]] %||% "paro"
      rango <- input[["vistagen_filtros-year_range"]] %||% c(min(anios), max(anios))
      modo <- input$modo_valor %||% "abs"
      
      col <- col_metrica(metrica)
      
      d <- df %>%
        filter(anio >= rango[1], anio <= rango[2]) %>%
        select(anio, comunidad, all_of(col)) %>%
        mutate(valor = .data[[col]]) %>%
        select(anio, comunidad, valor)
      
      req(nrow(d) > 0)
      
      # modo vs media nacional POR AÑO (más lógico en heatmap)
      if (modo != "abs") {
        d <- d %>%
          group_by(anio) %>%
          mutate(media_nac = mean(valor, na.rm = TRUE)) %>%
          ungroup()
        
        d$media_nac[d$media_nac == 0 | is.na(d$media_nac)] <- 1
        
        if (modo == "diff") d$valor <- d$valor - d$media_nac
        if (modo == "pct")  d$valor <- 100 * (d$valor - d$media_nac) / d$media_nac
      }
      
      ggplot(d, aes(x = anio, y = comunidad, fill = valor)) +
        geom_tile() +
        labs(
          x = "Año",
          y = "CCAA",
          title = "Heatmap año × CCAA",
          fill = ifelse(modo == "pct", "% vs media nac",
                        ifelse(modo == "diff", "Dif vs media nac", "Valor"))
        ) +
        theme_minimal()
    })
    
    output$plot_bottom5 <- renderPlot({
      df <- datos_agregados()
      req(df)
      
      metrica <- input[["ranking_filtros-metrica_sel"]] %||% "paro"
      rango <- input[["ranking_filtros-year_range"]] %||% c(min(anios), max(anios))
      col <- col_metrica(metrica)
      
      d <- df %>%
        filter(anio >= rango[1], anio <= rango[2]) %>%
        group_by(comunidad) %>%
        summarise(media = mean(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
        arrange(media) %>%
        slice_head(n = 5)
      
      req(nrow(d) > 0)
      
      ggplot(d, aes(x = reorder(comunidad, media), y = media)) +
        geom_col() +
        coord_flip() +
        labs(
          x = NULL, y = "Media periodo",
          title = paste0("Bottom 5 CCAA (", rango[1], "–", rango[2], ")")
        ) +
        theme_minimal()
    })
    
    output$plot_comparativa <- renderPlot({
      df <- datos_agregados()
      req(df, nrow(df) > 0)
      
      rango <- input$`comp_filtros-year_range`
      metrica <- input$`comp_filtros-metrica_sel`
      modo <- input$`comp2-modo_valor`
      ccaa_multi <- input$`comp2-ccaa_multi`
      req(rango, metrica, modo, ccaa_multi)
      req(length(ccaa_multi) >= 1)
      
      col <- col_metrica(metrica)
      
      d <- df %>%
        mutate(comunidad_norm = normalizar_ccaa(comunidad)) %>%
        filter(anio >= rango[1], anio <= rango[2]) %>%
        transmute(anio, comunidad = comunidad_norm, valor = .data[[col]])
      
      req(nrow(d) > 0)
      
      # Media nacional por año (media entre CCAA ese año)
      ref <- d %>%
        group_by(anio) %>%
        summarise(media_nacional = mean(valor, na.rm = TRUE), .groups = "drop")
      
      d2 <- d %>%
        filter(comunidad %in% normalizar_ccaa(ccaa_multi)) %>%
        left_join(ref, by = "anio") %>%
        mutate(valor_plot = case_when(
          modo == "abs"  ~ valor,
          modo == "diff" ~ valor - media_nacional,
          modo == "pct"  ~ 100 * (valor - media_nacional) / media_nacional,
          TRUE ~ valor
        ))
      
      req(nrow(d2) > 0)
      
      ggplot(d2, aes(x = anio, y = valor_plot, color = comunidad, group = comunidad)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(
          x = "Año",
          y = ifelse(modo == "pct", "% vs media nacional",
                     ifelse(modo == "diff", "Diferencia vs media nacional", "Valor")),
          title = "Comparativa CCAA vs media nacional"
        ) +
        theme_minimal()
    })
    
    output$plot_sector <- renderPlot({
      ccaa <- input$`sectores_filtros-ccaa_sel`
      rango <- input$`sectores_filtros-year_range`
      metrica_sel <- input$`sectores_filtros-metrica_sel`
      vista <- input$`sectores_filtros-vista_sector`
      
      req(rango, metrica_sel, vista)
      req(metrica_sel %in% c("paro", "contratos", "dtes"))
      
      ds <- datos_sector(metrica_sel, rango)
      req(ds, nrow(ds) > 0)
      
      # si elige CCAA concreta
      if (!is.null(ccaa) && ccaa != "") {
        ccaa_ui <- normalizar_ccaa(ccaa)
        d <- ds %>% filter(comunidad == ccaa_ui)
        titulo_ccaa <- ccaa_ui
      } else {
        d <- ds
        titulo_ccaa <- "España (todas las CCAA)"
      }
      
      d <- d %>%
        group_by(sector) %>%
        summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(valor))
      
      req(nrow(d) > 0)
      
      if (vista == "pct") {
        total <- sum(d$valor, na.rm = TRUE)
        if (is.na(total) || total == 0) total <- 1
        d <- d %>% mutate(valor = 100 * valor / total)
        ylab <- "Porcentaje (%)"
      } else {
        ylab <- "Volumen"
      }
      
      ggplot(d, aes(x = reorder(sector, valor), y = valor)) +
        geom_col() +
        coord_flip() +
        labs(
          x = "Sector",
          y = ylab,
          title = paste0("Distribución por sectores — ", titulo_ccaa, " (", metrica_sel, ")")
        ) +
        theme_minimal()
    })
    
    output$plot_bubble <- renderPlot({
      ccaa <- input$`bubble_filtros-ccaa_sel`
      rango <- input$`bubble_filtros-year_range`
      log_scale <- isTRUE(input$bubble_log)
      req(rango)
      
      dp <- datos_sector("paro", rango)
      dc <- datos_sector("contratos", rango)
      req(dp, dc)
      
      # Filtrar por CCAA o usar todas
      if (!is.null(ccaa) && ccaa != "") {
        ccaa_ui <- normalizar_ccaa(ccaa)
        dp <- dp %>% filter(comunidad == ccaa_ui)
        dc <- dc %>% filter(comunidad == ccaa_ui)
        titulo_ccaa <- ccaa_ui
      } else {
        titulo_ccaa <- "España (todas las CCAA)"
      }
      
      norm_sector <- function(x) {
        x <- trimws(gsub("  +", " ", x))
        x <- gsub("Construcción", "Construccion", x)
        x <- gsub("Sin empleo Anterior", "Sin empleo", x)
        x
      }
      
      dp2 <- dp %>%
        mutate(sector_norm = norm_sector(sector)) %>%
        group_by(sector_norm) %>%
        summarise(paro = sum(valor, na.rm = TRUE), .groups = "drop")
      
      dc2 <- dc %>%
        mutate(sector_norm = norm_sector(sector)) %>%
        group_by(sector_norm) %>%
        summarise(contratos = sum(valor, na.rm = TRUE), .groups = "drop")
      
      # FULL JOIN para no perder sectores por discrepancias
      d <- full_join(dp2, dc2, by = "sector_norm") %>%
        mutate(
          paro = ifelse(is.na(paro), 0, paro),
          contratos = ifelse(is.na(contratos), 0, contratos)
        ) %>%
        # quita sectores que sean 0 en ambos
        filter(paro > 0 | contratos > 0)
      
      req(nrow(d) > 0)
      
      p <- ggplot(d, aes(x = paro, y = contratos, size = paro + contratos, label = sector_norm)) +
        geom_point(alpha = 0.7) +
        labs(
          x = "Paro (suma periodo)",
          y = "Contratos (suma periodo)",
          title = paste0("Paro vs Contratos por sector — ", titulo_ccaa)
        ) +
        theme_minimal()
      
      if (log_scale) p <- p + scale_x_log10() + scale_y_log10()
      p
    })

    # ---- TABLA DE DATOS ----
    output$dt_data <- renderDT({
      df <- datos_agregados()
      req(df)
      
      datatable(
        df %>% arrange(anio, comunidad),
        options = list(pageLength = 12, scrollX = TRUE)
      )
    })
    
}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)