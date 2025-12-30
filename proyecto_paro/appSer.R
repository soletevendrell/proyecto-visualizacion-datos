library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(scales)
library(leaflet)   # NUEVO: Para el mapa interactivo
library(sf)        # NUEVO: Para manejar geometrías
library(mapSpain)  # NUEVO: Para obtener las formas de las provincias de España

# --- Configuración Inicial ---
anios <- 2010:2025
anios_titulos <- paste(min(anios), max(anios), sep = "–")

# Asegúrate de que estos scripts existen en tu carpeta
source("preprocessing.R")

# Carga de datos inicial (igual que tu código original)
res <- descargar_datasets_sepe(anios = anios, dir_data = "data")
res <- descargar_y_procesar_poblacion(codigos_ine = 2854:2908, dir_data = "data", anio_min = 2010, anio_max = 2025)

# Función de lectura ROBUSTA (Latin1)
leer_sepe_csv <- function(ruta) {
  if(!file.exists(ruta)) return(NULL)
  tryCatch({
    df <- read.csv(ruta, sep = ";", stringsAsFactors = FALSE, fileEncoding = "Latin1", check.names = FALSE)
    if(ncol(df) <= 1) {
      df <- read.csv(ruta, sep = ",", stringsAsFactors = FALSE, fileEncoding = "Latin1", check.names = FALSE)
    }
    return(df)
  }, error = function(e) {
    read.csv(ruta, stringsAsFactors = FALSE)
  })
}

# --------------------------------------------------------------------------------------
# ---------- UI ----------
# --------------------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel(paste("SEPE — Análisis Provincial y Geográfico (", anios_titulos, ")")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Configuración Global"),
      
      # Selector de Métrica (Común para ambas pestañas)
      selectInput("metrica_sel", "Indicador:",
                  choices = c("Paro Registrado" = "paro",
                              "Contratos Registrados" = "contratos",
                              "Demandantes de Empleo" = "dtes"),
                  selected = "paro"),
      
      # Filtros Específicos para la pestaña GRÁFICO
      conditionalPanel(
        condition = "input.tabs_main == 'tab_grafico'",
        h4("Filtros Evolución"),
        selectInput("prov1", "Provincia Principal:", choices = NULL), 
        selectInput("prov2", "Comparar con (Opcional):", choices = NULL),
        
        sliderInput("rango_anios", "Rango de Años:",
                    min = min(anios), max = max(anios),
                    value = c(min(anios), max(anios)),
                    step = 1, sep = ""),
        
        radioButtons("granularidad", "Escala Temporal:",
                     choices = c("Anual (Media)" = "anual", 
                                 "Mensual (Detalle)" = "mensual"),
                     selected = "mensual"),
        
        checkboxInput("relativo", "Ver distribución % (Áreas Apiladas)", value = FALSE)
      ),
      
      # Filtros Específicos para la pestaña MAPA
      conditionalPanel(
        condition = "input.tabs_main == 'tab_mapa'",
        h4("Filtros Mapa"),
        helpText("El mapa muestra la media anual (o suma total) del año seleccionado."),
        
        # CAMBIO: Slider simple de años en lugar de fechas
        sliderInput("anio_mapa_sel", "Selecciona Año:",
                    min = min(anios), max = max(anios),
                    value = max(anios), 
                    step = 1, sep = ""),
                    
        selectInput("sector_mapa", "Sector a visualizar:", 
                    choices = c("Agricultura", "Industria", "Construcción", "Servicios", "Sin empleo Anterior"))
      ),
      
      width = 3
    ),
    
    mainPanel(
      # CAMBIO PRINCIPAL: TabsetPanel para separar Gráfico y Mapa
      tabsetPanel(id = "tabs_main",
                  
        # Pestaña 1: Tu gráfico original
        tabPanel("Evolución Temporal", value = "tab_grafico",
                 br(),
                 h3(textOutput("titulo_grafico")),
                 plotOutput("plot_sectores", height = "600px")
        ),
        
        # Pestaña 2: El nuevo mapa Leaflet
        tabPanel("Mapa Geográfico", value = "tab_mapa",
                 br(),
                 h3(textOutput("titulo_mapa")),
                 leafletOutput("mapa_leaflet", height = "650px"),
                 helpText("Nota: Colores más oscuros indican mayor valor absoluto.")
        )
      ),
      br(),
      verbatimTextOutput("debug_info"),
      width = 9
    )
  )
)

# --------------------------------------------------------------------------------------
# ---------- SERVER ----------
# --------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- 1. Carga de Geometrías ---
  mapa_spain_sf <- reactive({
    tryCatch({
      # moveCAN = TRUE mueve Canarias para que salga cerca de la península
      geo <- esp_get_prov(moveCAN = TRUE)
      
      # ESTA LÍNEA ES LA CLAVE PARA QUITAR LOS WARNINGS:
      # Transformamos explícitamente a WGS84 (EPSG: 4326)
      geo <- st_transform(geo, crs = 4326) 
      
      geo %>% 
        mutate(cod_prov_int = as.integer(cpro))
        
    }, error = function(e) return(NULL))
  })
  
  # --- 2. Lógica de Extracción SEPE (Tu código original) ---
  agregador_sectores_provincial <- function(df_raw, tipo_metrica) {
    if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)
    names(df_raw) <- gsub("\\s+", " ", names(df_raw)) 
    
    if ("Cod mes" %in% names(df_raw)) {
      df_raw$mes_cod <- as.character(df_raw$`Cod mes`)
      df_raw$anio <- suppressWarnings(as.integer(substr(df_raw$mes_cod, 1, 4)))
      df_raw$mes  <- suppressWarnings(as.integer(substr(df_raw$mes_cod, 5, 6)))
    } else { return(NULL) }
    
    if (!("Provincia" %in% names(df_raw))) return(NULL)
    if ("Cod provincia" %in% names(df_raw)) {
       df_raw$cod_prov <- as.integer(df_raw$`Cod provincia`)
    } else { df_raw$cod_prov <- NA }

    prefix <- switch(tipo_metrica,
      "paro" = "Paro", "contratos" = "Contratos", "dtes" = "Dtes Empleo"
    )
    
    sectores_sufijos <- c("Agricultura", "Industria", "Construcción", "Servicios")
    if (tipo_metrica != "contratos") sectores_sufijos <- c(sectores_sufijos, "Sin empleo Anterior")
    
    df_grouped <- df_raw %>% 
      filter(!is.na(anio)) %>%
      group_by(anio, mes, cod_prov, Provincia) 
    
    lista_sectores <- list()
    for (sufijo in sectores_sufijos) {
      col_name <- paste(prefix, sufijo)
      if (!(col_name %in% names(df_raw))) {
         col_name_pegado <- paste0(prefix, sufijo)
         if (col_name_pegado %in% names(df_raw)) col_name <- col_name_pegado else next
      }
      temp <- df_grouped %>% summarise(valor = sum(.data[[col_name]], na.rm = TRUE), .groups = "drop")
      temp$sector <- sufijo
      lista_sectores[[sufijo]] <- temp
    }
    if (length(lista_sectores) > 0) bind_rows(lista_sectores) else NULL
  }
  
  # --- 3. Carga de Datos Base ---
  datos_base <- reactive({
    lista_final <- list()
    withProgress(message = 'Cargando datos...', value = 0, {
      n_anios <- length(anios)
      for (i in seq_along(anios)) {
        ano <- anios[i]
        setProgress(i/n_anios, detail = paste("Año", ano))
        
        r_paro  <- list.files("data", pattern = paste0("Paro.*", ano, ".*csv"), full.names = TRUE, recursive = TRUE)
        r_contr <- list.files("data", pattern = paste0("Contratos.*", ano, ".*csv"), full.names = TRUE, recursive = TRUE)
        r_dtes  <- list.files("data", pattern = paste0("Dtes.*", ano, ".*csv"), full.names = TRUE, recursive = TRUE)
        
        if(length(r_paro) > 0) {
          d <- leer_sepe_csv(r_paro[1])
          r <- agregador_sectores_provincial(d, "paro")
          if(!is.null(r)) { r$metrica <- "paro"; lista_final[[paste("p", ano)]] <- r }
        }
        if(length(r_contr) > 0) {
          d <- leer_sepe_csv(r_contr[1])
          r <- agregador_sectores_provincial(d, "contrat")
          if(!is.null(r)) { r$metrica <- "contratos"; lista_final[[paste("c", ano)]] <- r }
        }
        if(length(r_dtes) > 0) {
          d <- leer_sepe_csv(r_dtes[1])
          r <- agregador_sectores_provincial(d, "dtes")
          if(!is.null(r)) { r$metrica <- "dtes"; lista_final[[paste("d", ano)]] <- r }
        }
      }
    })
    res <- bind_rows(lista_final)
    # Crear columna fecha para facilitar filtrado
    if(!is.null(res)) {
      res$fecha <- as.Date(paste(res$anio, res$mes, "01", sep="-"))
    }
    res
  })

  # --- Updates de UI ---
  observe({
    df <- datos_base()
    req(df)
    provs <- sort(unique(df$Provincia))
    sel_def <- if("Madrid" %in% provs) "Madrid" else provs[1]
    updateSelectInput(session, "prov1", choices = provs, selected = sel_def)
    updateSelectInput(session, "prov2", choices = c("Ninguna" = "", provs), selected = "")
  })
  
  # UI Dinámico: Slider de Fecha para el mapa (basado en fechas disponibles)
  output$ui_fecha_mapa <- renderUI({
    df <- datos_base()
    req(df)
    fechas_disp <- sort(unique(df$fecha))
    
    sliderInput("fecha_mapa_sel", "Selecciona Mes y Año:",
                min = min(fechas_disp),
                max = max(fechas_disp),
                value = max(fechas_disp), # Por defecto el último disponible
                timeFormat = "%Y-%m",
                step = 30, # Paso aproximado de un mes
                animate = animationOptions(interval = 1000, loop = FALSE))
  })

  # --- 4. Transformación Gráfico Lineal (Tu lógica original) ---
  datos_grafico <- reactive({
    req(input$prov1, input$metrica_sel)
    df <- datos_base()
    req(df)
    
    provs_sel <- c(input$prov1)
    if (input$prov2 != "") provs_sel <- c(provs_sel, input$prov2)
    
    df_filt <- df %>% 
      filter(metrica == input$metrica_sel,
             Provincia %in% provs_sel,
             anio >= input$rango_anios[1],
             anio <= input$rango_anios[2])
    
    if (input$granularidad == "anual") {
      df_agrupado <- df_filt %>%
        group_by(anio, Provincia, sector, metrica) %>%
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), .groups = "drop")
      df_agrupado$fecha <- as.Date(paste0(df_agrupado$anio, "-01-01"))
    } else {
      df_agrupado <- df_filt # Ya tiene fecha
    }
    
    if (input$relativo) {
      df_agrupado <- df_agrupado %>%
        group_by(fecha, Provincia) %>%
        mutate(total_momento = sum(valor, na.rm = TRUE), valor_final = (valor / total_momento) * 100) %>%
        ungroup()
    } else {
      df_agrupado$valor_final <- df_agrupado$valor
    }
    df_agrupado
  })
  
  # --- 5. Transformación para MAPA ---
  datos_para_mapa <- reactive({
    req(input$anio_mapa_sel, input$sector_mapa, input$metrica_sel)
    df <- datos_base()
    req(df)
    
    anio_target <- input$anio_mapa_sel
    
    # Paso 1: Filtrar solo el año seleccionado y la métrica
    df_anio <- df %>% 
      filter(metrica == input$metrica_sel, anio == anio_target)
    
    # Paso 2: Calcular el valor ABSOLUTO del sector seleccionado (Numerador)
    # Nota: Si son contratos sumamos, si es paro hacemos la media anual.
    df_sector <- df_anio %>%
      filter(sector == input$sector_mapa) %>%
      group_by(cod_prov, Provincia) %>%
      summarise(
        valor_abs = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE),
        .groups = "drop"
      )
    
    # Paso 3: Calcular el valor TOTAL de todos los sectores (Denominador)
    df_total <- df_anio %>%
      group_by(cod_prov) %>%
      summarise(
        total_prov = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE),
        .groups = "drop"
      )
    
    # Paso 4: Unir y calcular % RELATIVO
    df_final <- left_join(df_sector, df_total, by = "cod_prov") %>%
      mutate(
        valor_rel = (valor_abs / total_prov) * 100
      )
    
    return(df_final)
  })


  # --- 6. Render Plots ---
  
  output$titulo_grafico <- renderText({
    txt_metrica <- switch(input$metrica_sel, "paro" = "Paro", "contratos" = "Contratos", "dtes" = "Demandantes")
    txt_tipo <- if (input$relativo) "(Distribución %)" else "(Valores Absolutos)"
    paste0("Evolución ", txt_metrica, " por Sector ", txt_tipo)
  })
  
  output$titulo_mapa <- renderText({
    req(input$fecha_mapa_sel)
    fecha_txt <- format(as.Date(input$fecha_mapa_sel), "%B %Y")
    paste0("Mapa Provincial: ", input$sector_mapa, " - ", fecha_txt)
  })

  output$plot_sectores <- renderPlot({
    df_plot <- datos_grafico()
    req(df_plot)
    validate(need(nrow(df_plot) > 0, "Sin datos para visualizar."))
    
    df_plot$sector <- tools::toTitleCase(df_plot$sector)
    p <- ggplot(df_plot, aes(x = fecha, y = valor_final, fill = sector, color = sector)) +
      scale_fill_viridis_d(option = "D", end = 0.9) +
      scale_color_viridis_d(option = "D", end = 0.9) +
      labs(x = NULL, y = if(input$relativo) "% del Total" else "Total Registros", fill = "Sector", color = "Sector") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", strip.text = element_text(face="bold"))
    
    if (input$relativo) {
      p <- p + geom_area(alpha = 0.85, color = "white", size = 0.2) + coord_cartesian(expand = FALSE)
    } else {
      p <- p + geom_line(size = 1.1)
    }
    
    p <- p + scale_x_date(date_labels = "%Y", date_breaks = "1 year")
    if (input$granularidad == "mensual") p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (length(unique(df_plot$Provincia)) > 1) {
      p <- p + facet_wrap(~Provincia, scales = "free_y", ncol = 1)
    } else {
      p <- p + ggtitle(unique(df_plot$Provincia))
    }
    p
  })
  
  # --- 7. Render Leaflet (MODIFICADO) ---
  output$mapa_leaflet <- renderLeaflet({
    datos <- datos_para_mapa()
    mapa_sf <- mapa_spain_sf()
    
    validate(need(!is.null(mapa_sf), "Cargando geometrías..."),
             need(nrow(datos) > 0, "No hay datos para este año."))
    
    # Cruzamos datos espaciales con nuestros datos calculados
    mapa_completo <- left_join(mapa_sf, datos, by = c("cod_prov_int" = "cod_prov"))
    
    # CAMBIO: La paleta se crea basada en el valor RELATIVO (%)
    pal <- colorNumeric(palette = "viridis", domain = mapa_completo$valor_rel, na.color = "#e0e0e0")
    
    # CAMBIO: El Popup muestra ambos datos, pero destacamos el ABSOLUTO
    popup_txt <- paste0(
      "<strong>", mapa_completo$Provincia.x, "</strong><br>",
      "Sector: ", input$sector_mapa, "<br>",
      "-------------------------<br>",
      "<strong>Total (Abs): ", format(round(mapa_completo$valor_abs, 0), big.mark = ".", decimal.mark = ","), "</strong><br>",
      "Peso en Prov (%): ", format(round(mapa_completo$valor_rel, 2), decimal.mark = ","), "%"
    )
    
    leaflet(mapa_completo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -3.7, lat = 40.4, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(valor_rel), # Pintamos según % relativo
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        popup = popup_txt
      ) %>%
      # La leyenda muestra el porcentaje
      addLegend(pal = pal, values = ~valor_rel, opacity = 0.7, 
                title = paste0("% ", input$sector_mapa),
                labFormat = labelFormat(suffix = "%"),
                position = "bottomright")
  })
  
  output$debug_info <- renderText({
    if (input$tabs_main == "tab_grafico") {
       df <- datos_grafico()
       return(paste("Registros Gráfico:", nrow(df)))
    } else {
       df <- datos_para_mapa()
       return(paste0("Mapa Provincial (", input$anio_mapa_sel, "): Peso del sector ", input$sector_mapa))
    }
  })
}

shinyApp(ui = ui, server = server)