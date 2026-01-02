library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(scales)
library(leaflet)
library(sf)
library(mapSpain)

# --- Configuración Inicial ---
anios <- 2010:2025
anios_titulos <- paste(min(anios), max(anios), sep = "–")
Pmin = 0.02
Pmax = 0.98

# Asegúrate de que estos scripts existen en tu carpeta
source("preprocessing.R")

# Carga de datos inicial
res <- descargar_datasets_sepe(anios = anios, dir_data = "data")
# Descargamos población (fundamental para el nuevo cálculo del mapa)
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
      
      selectInput("metrica_sel", "Indicador:",
                  choices = c("Paro Registrado" = "paro",
                              "Contratos Registrados" = "contratos",
                              "Demandantes de Empleo" = "dtes"),
                  selected = "contratos"),
      
      # --- FILTROS PESTAÑA GRÁFICO ---
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
      
      # --- FILTROS PESTAÑA MAPA ---
      conditionalPanel(
        condition = "input.tabs_main == 'tab_mapa'",
        h4("Filtros Mapa"),
        
        # 1. Granularidad propia del mapa
        radioButtons("granularidad_mapa", "Escala Temporal:",
                     choices = c("Anual" = "anual", 
                                 "Mensual" = "mensual"),
                     selected = "anual"),
        
        # 2. Slider Dinámico (Anual o Mensual con Play)
        uiOutput("ui_slider_mapa_dinamico"),
        
        selectInput("sector_mapa", "Sector a visualizar:", 
                    choices = c("Agricultura", "Industria", "Construcción", "Servicios", "Sin empleo Anterior"),
                    selected = "Servicios"),
        
        helpText("Nota: El valor relativo (%) se calcula sobre la POBLACIÓN TOTAL de la provincia.")
      ),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs_main",
                  
        tabPanel("Evolución Temporal", value = "tab_grafico",
                 br(),
                 h3(textOutput("titulo_grafico")),
                 plotOutput("plot_sectores", height = "600px")
        ),
        
        tabPanel("Mapa Geográfico", value = "tab_mapa",
                 br(),
                 h3(textOutput("titulo_mapa")),
                 leafletOutput("mapa_leaflet", height = "650px")
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
  
  # --- 0. Carga de Población ---
  poblacion_data <- reactive({
    archivos <- list.files("data", pattern = "poblacion|padron", full.names = TRUE, recursive = TRUE)
    if (length(archivos) == 0) return(NULL)
    
    df_pob <- leer_sepe_csv(archivos[1])
    if (is.null(df_pob)) return(NULL)
    if (!all(c("anio", "Cod provincia", "poblacion_total") %in% names(df_pob))) return(NULL)
    
    df_pob %>%
      select(anio, cod_prov = `Cod provincia`, poblacion_total) %>%
      mutate(cod_prov = as.integer(cod_prov), anio = as.integer(anio))
  })

  # --- 1. Carga de Geometrías ---
  mapa_spain_sf <- reactive({
    tryCatch({
      geo <- esp_get_prov(moveCAN = TRUE)
      geo <- st_transform(geo, crs = 4326) 
      geo %>% mutate(cod_prov_int = as.integer(cpro))
    }, error = function(e) return(NULL))
  })
  
  # --- 2. Lógica SEPE ---
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
          d_list <- lapply(r_paro, leer_sepe_csv) 
          d <- bind_rows(d_list) 
          r <- agregador_sectores_provincial(d, "paro")
          if(!is.null(r)) { r$metrica <- "paro"; lista_final[[paste("p", ano)]] <- r }
        }
        if(length(r_contr) > 0) {
          d_list <- lapply(r_contr, leer_sepe_csv)
          d <- bind_rows(d_list)
          r <- agregador_sectores_provincial(d, "contratos")
          if(!is.null(r)) { r$metrica <- "contratos"; lista_final[[paste("c", ano)]] <- r }
        }
        if(length(r_dtes) > 0) {
          d_list <- lapply(r_dtes, leer_sepe_csv)
          d <- bind_rows(d_list)
          r <- agregador_sectores_provincial(d, "dtes")
          if(!is.null(r)) { r$metrica <- "dtes"; lista_final[[paste("d", ano)]] <- r }
        }
      }
    })
    res <- bind_rows(lista_final)
    if(!is.null(res)) {
      res$anio <- as.integer(res$anio)
      res$mes <- as.integer(res$mes)
      res$fecha <- as.Date(paste(res$anio, res$mes, "01", sep="-"))
    }
    res %>% arrange(fecha)
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
  
  # --- UI DINÁMICA: SLIDER MAPA ---
  output$ui_slider_mapa_dinamico <- renderUI({
    req(input$granularidad_mapa)
    df <- datos_base()
    req(df) 
    if (nrow(df) == 0) return(NULL)
    
    if (input$granularidad_mapa == "anual") {
      anios_disp <- sort(unique(df$anio))
      if(length(anios_disp) == 0) return(NULL)
      sliderInput("tiempo_mapa_sel", "Año:",
                  min = min(anios_disp), max = max(anios_disp),
                  value = max(anios_disp), step = 1, sep = "",
                  animate = animationOptions(interval = 2000, loop = FALSE))
    } else {
      fechas_disp <- sort(unique(df$fecha))
      if(length(fechas_disp) == 0) return(NULL)
      sliderInput("tiempo_mapa_sel", "Mes y Año:",
                  min = min(fechas_disp), max = max(fechas_disp),
                  value = max(fechas_disp), timeFormat = "%m/%Y", step = 30, 
                  animate = animationOptions(interval = 1000, loop = FALSE))
    }
  })

  # --- 4. Transformación Gráfico Lineal ---
  datos_grafico <- reactive({
    req(input$prov1, input$metrica_sel)
    df <- datos_base()
    req(df)
    provs_sel <- c(input$prov1)
    if (input$prov2 != "") provs_sel <- c(provs_sel, input$prov2)
    
    df_filt <- df %>% 
      filter(metrica == input$metrica_sel, Provincia %in% provs_sel,
             anio >= input$rango_anios[1], anio <= input$rango_anios[2])
    
    if (input$granularidad == "anual") {
      df_agrupado <- df_filt %>%
        group_by(anio, Provincia, sector, metrica) %>%
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), .groups = "drop")
      df_agrupado$fecha <- as.Date(paste0(df_agrupado$anio, "-01-01"))
    } else {
      df_agrupado <- df_filt 
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
  
  # --- 5. Transformación para MAPA (Con limpieza de NAs) ---
  datos_para_mapa <- reactive({
    req(input$tiempo_mapa_sel, input$sector_mapa, input$metrica_sel, input$granularidad_mapa)
    df <- datos_base()
    df_pob <- poblacion_data() 
    req(df, df_pob)
    
    # 1. Filtro Temporal y Agregación
    if (input$granularidad_mapa == "anual") {
      anio_target <- as.integer(input$tiempo_mapa_sel)
      
      df_filt <- df %>% 
        filter(metrica == input$metrica_sel, anio == anio_target) %>%
        group_by(cod_prov, Provincia, sector) %>%
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), .groups = "drop")
      
      anio_join <- anio_target 
      
    } else {
      fecha_target <- as.Date(input$tiempo_mapa_sel)
      anio_join <- as.integer(format(fecha_target, "%Y")) 
      
      fechas_disponibles <- unique(df$fecha)
      fecha_cercana <- fechas_disponibles[which.min(abs(fechas_disponibles - fecha_target))]
      
      df_filt <- df %>% filter(metrica == input$metrica_sel, fecha == fecha_cercana)
    }

    # 2. Filtro Sector
    df_sector <- df_filt %>%
      filter(sector == input$sector_mapa) %>%
      group_by(cod_prov, Provincia) %>%
      summarise(valor_abs = sum(valor, na.rm=TRUE), .groups="drop")
    
    # 3. Cruce Población y Limpieza NA
    df_pob_anio <- df_pob %>% filter(anio == anio_join)
    
    df_final <- left_join(df_sector, df_pob_anio, by = "cod_prov") %>%
      mutate(
        # SI VALOR_ABS es NA (no hay contratos), lo ponemos a 0
        valor_abs = tidyr::replace_na(valor_abs, 0),
        # Calculamos relativo. Si pob es NA o 0, el resultado será NA o Inf, lo trataremos después
        valor_rel = (valor_abs / poblacion_total) * 100
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
    req(input$tiempo_mapa_sel)
    txt_fecha <- if(input$granularidad_mapa == "mensual") format(as.Date(input$tiempo_mapa_sel), "%B %Y") else paste("Año", input$tiempo_mapa_sel)
    paste0("Mapa: ", input$sector_mapa, " (% sobre Población) - ", txt_fecha)
  })

  output$plot_sectores <- renderPlot({
    df_plot <- datos_grafico()
    req(df_plot)
    validate(need(nrow(df_plot) > 0, "Sin datos."))
    
    df_plot$sector <- tools::toTitleCase(df_plot$sector)
    p <- ggplot(df_plot, aes(x = fecha, y = valor_final)) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom", strip.text = element_text(face="bold", size=20),
            axis.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
            legend.title=element_text(size=18, face="bold"), legend.text=element_text(size=16))

    if (input$relativo) {
      p <- p + geom_area(aes(fill = sector), alpha=0.85, color="white", size=0.2) +
        coord_cartesian(expand=FALSE) + scale_fill_brewer(palette="Set1") + 
        labs(x=NULL, y="% del Total", fill="Sector")
    } else {
      p <- p + geom_line(aes(color=sector, group=sector), size=1.2) +
        scale_color_brewer(palette="Set1") + 
        labs(x=NULL, y="Total Registros", color="Sector")
    }
    
    num_anios <- input$rango_anios[2] - input$rango_anios[1]
    if (input$granularidad == "mensual") {
      if (num_anios <= 3) { b<-"1 month"; l<-"%b %Y" } else if (num_anios <= 5) { b<-"3 months"; l<-"%b %Y" } else { b<-"6 months"; l<-"%m/%Y" }
      p <- p + scale_x_date(date_labels=l, date_breaks=b, expand=c(0.01,0)) + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
    } else {
      p <- p + scale_x_date(date_labels="%Y", date_breaks="1 year") + theme(axis.text.x=element_text(angle=0, hjust=0.5))
    }
    if (length(unique(df_plot$Provincia)) > 1) p <- p + facet_wrap(~Provincia, scales="free_y", ncol=1)
    else p <- p + ggtitle(unique(df_plot$Provincia)) + theme(plot.title=element_text(size=24, face="bold", hjust=0.5))
    p
  })
  
  # --- 7. Render Leaflet (CORREGIDO: ESCALAS DISTINTAS Y NaNs) ---
  output$mapa_leaflet <- renderLeaflet({
    # 1. Datos del momento actual
    datos <- datos_para_mapa()
    mapa_sf <- mapa_spain_sf()
    
    # 2. Datos globales para la escala
    df_global <- datos_base()
    df_pob <- poblacion_data()
    
    validate(need(!is.null(mapa_sf), "Cargando geometrías..."),
             need(nrow(datos) > 0, "No hay datos para esta fecha/año."))
    
    # --- CÁLCULO DE LA ESCALA GLOBAL FIJA ---
    
    # Filtramos la historia completa de este Indicador y Sector
    df_base_hist <- df_global %>%
      filter(metrica == input$metrica_sel, sector == input$sector_mapa)
    
    # Agrupamos según la granularidad seleccionada para que la escala tenga sentido
    if (input$granularidad_mapa == "anual") {
      # MODO ANUAL: Sumas/Medias anuales
      df_rango_global <- df_base_hist %>%
        group_by(anio, cod_prov) %>%
        summarise(
          valor_abs = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE),
          .groups = "drop"
        )
    } else {
      # MODO MENSUAL: Sumas mensuales
      df_rango_global <- df_base_hist %>%
        group_by(anio, mes, cod_prov) %>%
        summarise(
          valor_abs = sum(valor, na.rm=TRUE),
          .groups = "drop"
        )
    }
    
    # Calculamos % histórico sobre población
    df_rango_global <- df_rango_global %>%
      left_join(df_pob, by = c("cod_prov", "anio")) %>%
      mutate(valor_rel = (valor_abs / poblacion_total) * 100)
    
    # === CAMBIO: CALCULO DE PERCENTILES Pmin% (Min) y Pmax% (Max) ===
    quantiles <- quantile(df_rango_global$valor_rel, probs = c(Pmin, Pmax), na.rm = TRUE)
    min_global <- quantiles[1]
    max_global <- quantiles[2]
    
    # Seguridad anti-errores (por si todo es 0 o hay NAs)
    if(is.na(min_global)) min_global <- 0
    if(is.na(max_global) || max_global <= min_global) max_global <- min_global + 0.001
    
    # -------------------------------------------------------------------------
    
    mapa_completo <- left_join(mapa_sf, datos, by = c("cod_prov_int" = "cod_prov")) %>%
      mutate(
        # 1. Valores Reales para mostrar en texto (limpiando NAs)
        valor_abs_show = ifelse(is.na(valor_abs), 0, valor_abs),
        valor_rel_real = ifelse(is.na(valor_rel) | is.infinite(valor_rel), 0, valor_rel),
        
        # 2. CLAMPING / TOPADO (Para el color):
        # Todo lo que esté por debajo del Pmin se iguala al Pmin.
        # Todo lo que esté por encima del Pmax se iguala al Pmax.
        # Esto evita el warning de "values outside color scale".
        valor_para_color = pmin(pmax(valor_rel_real, min_global), max_global)
      )
    
    # La paleta va del Pmin al Pmax
    pal <- colorNumeric(palette = "YlOrRd", domain = c(min_global, max_global), na.color = "#e0e0e0")
    
    popup_txt <- paste0(
      "<strong>", mapa_completo$Provincia.x, "</strong><br>",
      "Sector: ", input$sector_mapa, "<br>",
      "-------------------------<br>",
      "Total Absoluto: ", format(round(mapa_completo$valor_abs_show, 0), big.mark = ".", decimal.mark = ","), "<br>",
      "Población Prov: ", format(round(mapa_completo$poblacion_total, 0), big.mark = ".", decimal.mark = ","), "<br>",
      # Mostramos el valor REAL, no el topado
      "<strong>% sobre Población: ", format(round(mapa_completo$valor_rel_real, 3), decimal.mark = ","), "%</strong>"
    )
    
    leaflet(mapa_completo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -3.7, lat = 40.4, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(valor_para_color), # Usamos el valor topado para pintar
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        popup = popup_txt
      ) %>%
      addLegend(pal = pal, values = ~valor_para_color, opacity = 0.7, 
                title = paste0("% Pob. (Sat. 5-95%)"),
                labFormat = labelFormat(suffix = "%"),
                position = "bottomright")
  })
  
  output$debug_info <- renderText({
    if (input$tabs_main == "tab_grafico") {
       df <- datos_grafico()
       return(paste("Registros Gráfico:", nrow(df)))
    } else {
       df <- datos_para_mapa()
       return(paste("Registros Mapa:", nrow(df), "| Temporal:", input$tiempo_mapa_sel))
    }
  })
}

shinyApp(ui = ui, server = server)