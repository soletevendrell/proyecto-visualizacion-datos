# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(httr)
library(scales)
library(shinyjs)
library(DT)
library(plotly)

# Definir la lista de años al inicio
anios <- 2010:2024
anios_titulos <- paste(min(anios), max(anios), sep = "–")

# --------------------------------------------------------------------------------------
# ---------- 1. FUNCIÓN PARA LEER UN FICHERO DEL SEPE ----------
# --------------------------------------------------------------------------------------
leer_sepe_csv <- function(ruta) {
  if (!file.exists(ruta)) {
    warning(sprintf("Archivo no encontrado: %s", ruta))
    return(NULL)
  }

  df <- tryCatch(
    read.csv(
      ruta,
      sep = ";",
      fileEncoding = "latin1",
      header = TRUE,
      skip = 1,
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

# --------------------------------------------------------------------------------------
# ---------- 1b. CORREGIR LOS NOMBRES DE LAS CCAA ----------
# --------------------------------------------------------------------------------------
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

# --------------------------------------------------------------------------------------
# ---------- PRE 2. DESCARGA DEL DATASET (Se ejecuta al cargar el script) ----------
# --------------------------------------------------------------------------------------
dir_data <- file.path("data")
dir_contratos <- file.path(dir_data, "contratos")
dir_paro      <- file.path(dir_data, "paro")
dir_dtes_empleo <- file.path(dir_data, "dtes_empleo")

dir.create(dir_contratos, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_paro, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_dtes_empleo, recursive = TRUE, showWarnings = FALSE)

descargar_si_existe <- function(url, destfile) {
  res_head <- tryCatch(httr::HEAD(url, timeout(10)), error = function(e) NULL)
  status <- NULL
  if (!is.null(res_head)) status <- httr::status_code(res_head)

  if (is.null(status) || status >= 400) {
    res_get <- tryCatch(httr::GET(url, httr::progress(), httr::write_disk(destfile, overwrite = TRUE), timeout(60)),
                        error = function(e) e)
    if (inherits(res_get, "error")) {
      message(sprintf("No se pudo descargar %s : %s", basename(destfile), res_get$message))
      if (file.exists(destfile)) file.remove(destfile)
      return(FALSE)
    } else {
      status_get <- httr::status_code(res_get)
      if (status_get >= 400) {
        message(sprintf("No disponible (HTTP %s): %s", status_get, basename(destfile)))
        if (file.exists(destfile)) file.remove(destfile)
        return(FALSE)
      } else {
        message(sprintf("Descargado: %s", basename(destfile)))
        return(TRUE)
      }
    }
  } else {
    res <- tryCatch(httr::GET(url, httr::progress(), httr::write_disk(destfile, overwrite = TRUE), timeout(60)),
                    error = function(e) e)
    if (inherits(res, "error")) {
      message(sprintf("Error descargando %s : %s", basename(destfile), res$message))
      if (file.exists(destfile)) file.remove(destfile)
      return(FALSE)
    } else {
      st <- httr::status_code(res)
      if (st >= 400) {
        message(sprintf("No disponible (HTTP %s): %s", st, basename(destfile)))
        if (file.exists(destfile)) file.remove(destfile)
        return(FALSE)
      } else {
        message(sprintf("Descargado: %s", basename(destfile)))
        return(TRUE)
      }
    }
  }
}

base_contratos <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Contratos_por_municipios_%s_csv.csv"
base_paro      <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_%s_csv.csv"
base_dtes_empleo <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Dtes_empleo_por_municipios_%s_csv.csv"

for (ano in anios) {
  url_c <- sprintf(base_contratos, ano)
  dest_c <- file.path(dir_contratos, sprintf("Contratos_por_municipios_%s_csv.csv", ano))
  if (!file.exists(dest_c)) {
    message(sprintf("Comprobando contratos %s ...", ano))
    descargar_si_existe(url_c, dest_c)
  } else {
    message(sprintf("Ya existe: %s", dest_c))
  }

  url_p <- sprintf(base_paro, ano)
  dest_p <- file.path(dir_paro, sprintf("Paro_por_municipios_%s_csv.csv", ano))
  if (!file.exists(dest_p)) {
    message(sprintf("Comprobando paro %s ...", ano))
    descargar_si_existe(url_p, dest_p)
  } else {
    message(sprintf("Ya existe: %s", dest_p))
  }

  url_d <- sprintf(base_dtes_empleo, ano)
  dest_d <- file.path(dir_dtes_empleo, sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano))
  if (!file.exists(dest_d)) {
    message(sprintf("Comprobando demandantes de empleo %s ...", ano))
    descargar_si_existe(url_d, dest_d)
  } else {
    message(sprintf("Ya existe: %s", dest_d))
  }
}

# --------------------------------------------------------------------------------------
# ---------- 3. UI (NAVBAR con páginas por gráfico) ----------
# --------------------------------------------------------------------------------------
filtros_generales_ui <- function(id, anios) {
  ns <- NS(id)
  
  tagList(
    h4("Filtros generales"),
    selectInput(ns("ccaa_sel"), "Comunidad Autónoma:",
                choices = NULL, selected = NULL, multiple = FALSE),
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
                           choices  = NULL,
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
           # fluidRow(
           #   column(12, h3(textOutput("titulo_grafico2")), p("Heatmap año × Comunidad.")),
           #   column(12, plotOutput("plot_heatmap", height = "600px"))
           # )
           sidebarLayout(
             sidebarPanel(filtros_generales_ui("vistagen_filtros", anios), width = 4),
             mainPanel(plotOutput("plot_heatmap", height = "420px"), width = 8)
           )
  ),

#  tabPanel("Comparativa (líneas)", value = "tab_line",
#           fluidRow(
#             column(12, h3("Comparativa temporal entre CCAA")),
#             column(12, plotOutput("plot_line_ccaa_all", height = "520px"))
#           )
#  ),

  tabPanel("RANKING DE CCAA", value = "tab_topbottom",
           # fluidRow(
           #   column(12, h3("Top 5 y Bottom 5 por media del periodo")),
           #   column(6, plotOutput("plot_top5", height = "420px")),
           #   column(6, plotOutput("plot_bottom5", height = "420px"))
           # )
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

  tabPanel("TABLA DE DATOS", value = "tab_table",
           fluidRow(
             column(12, h3("Datos agregados por CCAA y año")),
             column(12, DTOutput("dt_data"))
           )
  )

)

# --------------------------------------------------------------------------------------
# ---------- 4. SERVER ----------
# --------------------------------------------------------------------------------------
server <- function(input, output, session) {

  cache <- reactiveValues(
    base = NULL,
    sector = NULL
  )
  
  agregar_ccaa_anio <- function(df_raw, value_cols) {
    if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)
    
    names(df_raw) <- trimws(names(df_raw))
    
    # Columnas clave (con nombres raros en SEPE)
    col_ccaa <- "Comunidad Autónoma"
    col_mes  <- "Código mes"
    if (!(col_ccaa %in% names(df_raw))) return(NULL)
    if (!(col_mes  %in% names(df_raw)))  return(NULL)
    
    df_raw[[col_ccaa]] <- normalizar_ccaa(df_raw[[col_ccaa]])
    
    # Asegura numérico en columnas de valores
    for (c in value_cols) {
      if (c %in% names(df_raw)) {
        df_raw[[c]] <- suppressWarnings(as.numeric(gsub(",", ".", df_raw[[c]])))
        df_raw[[c]][is.na(df_raw[[c]])] <- 0
      }
    }
    
    df_raw %>%
      mutate(anio = suppressWarnings(as.integer(substr(.data[[col_mes]], 1, 4)))) %>%
      filter(!is.na(anio)) %>%
      group_by(anio, comunidad = .data[[col_ccaa]]) %>%
      summarise(across(all_of(value_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop")
  }

  safe_read <- function(ruta) {
    if (!file.exists(ruta)) {
      message(sprintf("Archivo no encontrado: %s", basename(ruta)))
      return(NULL)
    }
    leer_sepe_csv(ruta)
  }

  safe_bind <- function(lst, empty_df) {
    if (length(lst) == 0) return(empty_df)
    bind_rows(lst)
  }
  
  datos_agregados <- reactive({
    if (!is.null(cache$base)) return(cache$base)
    
    contratos_list <- list()
    paro_list <- list()
    dtes_list <- list()
    
    withProgress(message = "Cargando y agregando datos...", value = 0, {
      for (i in seq_along(anios)) {
        ano <- anios[i]
        setProgress(i/length(anios), detail = paste("Procesando año", ano))
        
        df_c <- safe_read(file.path("data","contratos", sprintf("Contratos_por_municipios_%s_csv.csv", ano)))
        df_p <- safe_read(file.path("data","paro",      sprintf("Paro_por_municipios_%s_csv.csv", ano)))
        df_d <- safe_read(file.path("data","dtes_empleo",sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano)))
        
        if (!is.null(df_c)) {
          agg <- agregar_ccaa_anio(df_c, c("Total Contratos"))
          if (!is.null(agg)) contratos_list[[as.character(ano)]] <- agg %>% rename(contratos_total = `Total Contratos`)
        }
        
        if (!is.null(df_p)) {
          agg <- agregar_ccaa_anio(df_p, c("total Paro Registrado"))
          if (!is.null(agg)) paro_list[[as.character(ano)]] <- agg %>% rename(paro_total = `total Paro Registrado`)
        }
        
        if (!is.null(df_d)) {
          agg <- agregar_ccaa_anio(df_d, c("total Dtes Empleo"))
          if (!is.null(agg)) dtes_list[[as.character(ano)]] <- agg %>% rename(dtes_total = `total Dtes Empleo`)
        }
      }
    })
    
    contratos_ccaa <- if (length(contratos_list) > 0) bind_rows(contratos_list) else tibble::tibble(anio=integer(), comunidad=character(), contratos_total=numeric())
    paro_ccaa      <- if (length(paro_list)      > 0) bind_rows(paro_list)      else tibble::tibble(anio=integer(), comunidad=character(), paro_total=numeric())
    dtes_ccaa      <- if (length(dtes_list)      > 0) bind_rows(dtes_list)      else tibble::tibble(anio=integer(), comunidad=character(), dtes_total=numeric())
    
    df <- paro_ccaa %>%
      full_join(contratos_ccaa, by = c("anio","comunidad")) %>%
      full_join(dtes_ccaa,      by = c("anio","comunidad")) %>%
      mutate(
        comunidad = as.character(comunidad),
        anio = as.integer(anio)
      )
    
    cache$base <- df
    df
  })
  
  observeEvent(datos_agregados(), {
    df <- datos_agregados()
    req(df)
    req(nrow(df) > 0)
    req("comunidad" %in% names(df))
    
    ccaa_choices <- sort(unique(na.omit(as.character(df$comunidad))))
    req(length(ccaa_choices) > 0)
    ids <- c("vistagen_filtros", "ranking_filtros", "comp_filtros", "sectores_filtros", "bubble_filtros")
    
    for (id in ids) {
      updateSelectInput(session, paste0(id, "-ccaa_sel"),
                        choices = ccaa_choices,
                        selected = ccaa_choices[1])
    }
    
    updateCheckboxGroupInput(session, "comp2-ccaa_multi",
                             choices = ccaa_choices,
                             selected = ccaa_choices[1:min(6, length(ccaa_choices))])
  })
  
  datos_sector <- function(metrica_sel, year_range) {
    sector_cols <- switch(metrica_sel,
                          "contratos" = c("Contratos  Agricultura","Contratos  Industria","Contratos Construcción","Contratos  Servicios"),
                          "paro"      = c("Paro Agricultura","Paro Industria","Paro Construcción","Paro Servicios","Paro Sin empleo Anterior"),
                          "dtes"      = c("Dtes EmpleoAgricultura","Dtes Empleo Industria","Dtes Empleo Construcción","Dtes Empleo Servicios","Dtes Empleo Sin empleo Anterior"))
    
    anios_sel <- seq(year_range[1], year_range[2])
    out <- list()
    
    for (ano in anios_sel) {
      ruta <- switch(metrica_sel,
                     "contratos" = file.path("data","contratos", sprintf("Contratos_por_municipios_%s_csv.csv", ano)),
                     "paro"      = file.path("data","paro", sprintf("Paro_por_municipios_%s_csv.csv", ano)),
                     "dtes"      = file.path("data","dtes_empleo", sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano)))
      
      df <- safe_read(ruta)
      if (is.null(df)) next
      
      agg <- agregar_ccaa_anio(df, sector_cols)
      if (is.null(agg)) next
      out[[as.character(ano)]] <- agg
    }
    
    if (length(out) == 0) return(NULL)
    
    bind_rows(out) %>%
      pivot_longer(cols = all_of(sector_cols), names_to = "sector", values_to = "valor") %>%
      mutate(comunidad = as.character(comunidad), anio = as.integer(anio))
  }

  datos_plot <- function(metrica_sel, year_range) {
    df_merged <- datos_agregados()
    req(df_merged, nrow(df_merged) > 0)
    
    metrica_col <- paste0(metrica_sel, "_total")
    req(metrica_col %in% names(df_merged))
    
    df_merged %>%
      filter(anio >= year_range[1], anio <= year_range[2]) %>%
      select(anio, comunidad, valor = .data[[metrica_col]]) %>%
      tidyr::drop_na(valor)
  }
  
  datos_bubble <- reactive({
    req(input$ccaa_sel)
    anios_sel <- seq(input$year_range[1], input$year_range[2])
    
    cargar_sector_metrica <- function(metrica, anios_sel) {
      cols <- switch(metrica,
                     "contratos" = c("Contratos  Agricultura","Contratos  Industria","Contratos Construcción","Contratos  Servicios"),
                     "paro"      = c("Paro Agricultura","Paro Industria","Paro Construcción","Paro Servicios","Paro Sin empleo Anterior")
      )
      
      out <- list()
      for (ano in anios_sel) {
        ruta <- if (metrica == "contratos") {
          file.path("data","contratos", sprintf("Contratos_por_municipios_%s_csv.csv", ano))
        } else {
          file.path("data","paro", sprintf("Paro_por_municipios_%s_csv.csv", ano))
        }
        
        df <- safe_read(ruta)
        if (is.null(df)) next
        
        agg <- agregar_ccaa_anio(df, cols)
        if (is.null(agg) || nrow(agg) == 0) next
        
        out[[as.character(ano)]] <- agg
      }
      
      if (length(out) == 0) return(NULL)
      
      df_wide <- bind_rows(out) %>% filter(comunidad == input$ccaa_sel)
      if (nrow(df_wide) == 0) return(NULL)
      
      df_wide %>%
        pivot_longer(cols = all_of(cols), names_to = "sector", values_to = "valor") %>%
        group_by(sector) %>%
        summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    }
    
    df_c0 <- cargar_sector_metrica("contratos", anios_sel)
    df_p0 <- cargar_sector_metrica("paro", anios_sel)
    
    if (is.null(df_c0) || is.null(df_p0)) return(NULL)
    
    df_c <- df_c0 %>% rename(contratos = valor)
    df_p <- df_p0 %>% rename(paro = valor)
    
    dplyr::full_join(df_p, df_c, by = "sector") %>%
      tidyr::drop_na(paro, contratos)
  })

  titulo_metrica <- reactive({
      switch(input$metrica_sel,
             "paro" = "Paro Registrado",
             "contratos" = "Contratos Registrados",
             "dtes" = "Demandantes de Empleo",
             "Métrica Desconocida")
  })
  
  datos_plot_comp <- function(metrica_sel, year_range) {
    df <- datos_plot(metrica_sel, year_range)
    req(df)
    
    esp <- df %>%
      group_by(anio) %>%
      summarise(total_espana = sum(valor, na.rm = TRUE), .groups = "drop")
    
    df %>%
      left_join(esp, by = "anio") %>%
      mutate(
        media_nac = total_espana / n_distinct(comunidad),
        diff = valor - media_nac,
        pct  = 100 * (valor / media_nac - 1)
      )
  }
  
  get_filtros <- function(id) {
    list(
      ccaa_sel   = input[[paste0(id, "-ccaa_sel")]],
      metrica    = input[[paste0(id, "-metrica_sel")]],
      ccaa_multi = input[[paste0(id, "-ccaa_multi")]],
      years      = input[[paste0(id, "-year_range")]],
      vista_sec  = input[[paste0(id, "-vista_sector")]]
    )
  }
  

  # ---- Outputs existentes ----
  output$titulo_grafico1 <- renderText({ paste("Gráfico 1: Evolución de", titulo_metrica(), "en una CCAA") })
  output$titulo_grafico2 <- renderText({ paste("Gráfico 2:", titulo_metrica(), "por año y CCAA (vista conjunta)") })

  output$plot_bar_ccaa <- renderPlot({
    df_long <- datos_plot(input$metrica_sel, input$year_range)
    req(df_long)
    req(nrow(df_long) > 0)
    req(input$ccaa_sel)

    df_ccaa <- df_long %>%
      filter(comunidad == input$ccaa_sel) %>%
      arrange(anio)

    validate(need(nrow(df_ccaa) > 0, paste("No hay datos de", titulo_metrica(), "para la comunidad seleccionada.")))

    df_ccaa$anio_f <- factor(df_ccaa$anio, levels = sort(unique(df_ccaa$anio)))

    ggplot(df_ccaa, aes(x = anio_f, y = valor)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = format(round(valor, 0), big.mark = ".", decimal.mark = ",")),
                position = position_dodge(width = 0.7),
                vjust = -0.5, size = 3) +
      labs(x = "Año",
           y = paste(titulo_metrica(), "(media anual)"),
           title = paste("Evolución de", titulo_metrica(), "en", input$ccaa_sel)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5), plot.title = element_text(face = "bold"))
  })

  output$plot_heatmap <- renderPlot({
    f <- get_filtros("vistagen_filtros")
    req(f$metrica, f$years)
    
    df_long <- datos_plot(f$metrica, f$years)
    req(nrow(df_long) > 0)

    years_levels <- sort(unique(df_long$anio), decreasing = TRUE)
    df_long$anio_f <- factor(df_long$anio, levels = years_levels)
    df_long$comunidad_f <- factor(df_long$comunidad, levels = sort(unique(df_long$comunidad)))

    low_col <- ifelse(input$metrica_sel == "paro", "white", "steelblue1")
    high_col <- ifelse(input$metrica_sel == "paro", "firebrick", "darkblue")

    ggplot(df_long, aes(x = comunidad_f, y = anio_f, fill = valor)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = low_col, high = high_col, name = paste(titulo_metrica(), "(media anual)")) +
      labs(x = "Comunidad Autónoma", y = "Año", title = paste("Distribución de", titulo_metrica(), "por Comunidad Autónoma y año")) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(size = 9), plot.title = element_text(face = "bold"), panel.grid = element_blank())
  })

  # ---- Nueva: comparativa líneas (varias CCAA + media nacional) ----
  # output$plot_line_ccaa_all <- renderPlot({
  #   df_long <- datos_plot()
  #   req(df_long)
  #   req(nrow(df_long) > 0)
  # 
  #   sel_ccaa <- input$ccaa_multi
  #   if (is.null(sel_ccaa) || length(sel_ccaa) == 0) {
  #     sel_ccaa <- unique(df_long$comunidad)[1:min(6, length(unique(df_long$comunidad)))]
  #   }
  # 
  #   df_sel <- df_long %>% filter(comunidad %in% sel_ccaa)
  # 
  #   # media nacional total españa
  #   df_media <- df_long %>%
  #     group_by(anio) %>%
  #     summarise(total_espana = sum(valor, na.rm = TRUE), .groups = "drop")
  #   
  #   ggplot() +
  #     geom_line(data = df_sel, aes(x = anio, y = valor, color = comunidad, group = comunidad), size = 0.9) +
  #     geom_point(data = df_sel, aes(x = anio, y = valor, color = comunidad), size = 1.8) +
  #     geom_line(data = df_media, aes(x = anio, y = total_espana), size = 1.05, linetype = "dashed") +
  #     geom_point(data = df_media, aes(x = anio, y = total_espana), size = 2, shape = 21, fill = "white") +
  #     labs(x = "Año", y = paste(titulo_metrica(), "(media anual)"), color = "Comunidad", title = paste("Comparativa:", titulo_metrica(), "— CCAA seleccionadas + media nacional")) +
  #     scale_x_continuous(breaks = sort(unique(df_long$anio))) +
  #     scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  #     theme_minimal(base_size = 12) +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold"))
  # })

  # ---- Nueva: Top / Bottom CCAA por media del periodo ----
  output$plot_top5 <- renderPlot({
    f <- get_filtros("ranking_filtros")
    req(f$metrica, f$years)
    
    df_long <- datos_plot(f$metrica, f$years)
    req(nrow(df_long) > 0)

    resumen <- df_long %>%
      group_by(comunidad) %>%
      summarise(media_periodo = mean(valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(media_periodo))

    top5 <- head(resumen, 5)
    validate(need(nrow(top5) > 0, "No hay datos para Top 5."))

    ggplot(top5, aes(x = reorder(comunidad, media_periodo), y = media_periodo)) +
      geom_col() +
      coord_flip() +
      labs(x = "", y = paste("Media de", titulo_metrica()), title = "Top 5 CCAA (mayor media en el periodo)") +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$plot_bottom5 <- renderPlot({
    f <- get_filtros("ranking_filtros")
    req(f$metrica, f$years)
    
    df_long <- datos_plot(f$metrica, f$years)
    req(nrow(df_long) > 0)

    resumen <- df_long %>%
      group_by(comunidad) %>%
      summarise(media_periodo = mean(valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(media_periodo)

    bottom5 <- head(resumen, 5)
    validate(need(nrow(bottom5) > 0, "No hay datos para Bottom 5."))

    ggplot(bottom5, aes(x = reorder(comunidad, -media_periodo), y = media_periodo)) +
      geom_col() +
      coord_flip() +
      labs(x = "", y = paste("Media de", titulo_metrica()), title = "Bottom 5 CCAA (menor media en el periodo)") +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  
  output$plot_comparativa <- renderPlot({
    # filtros de la pestaña comparativa
    sel <- input[["comp2-ccaa_multi"]]
    modo <- input[["comp2-modo_valor"]]
    req(modo)
    
    fg <- get_filtros("comp_filtros")
    req(fg$metrica, fg$years)
    
    df <- datos_plot_comp(fg$metrica, fg$years)
    req(df, nrow(df) > 0)
    
    if (is.null(sel) || length(sel) == 0) {
      sel <- unique(df$comunidad)[1:min(6, length(unique(df$comunidad)))]
    }
    df <- df %>% filter(comunidad %in% sel)
    
    ycol <- switch(modo,
                   "abs" = "valor",
                   "diff" = "diff",
                   "pct" = "pct")
    
    ggplot(df, aes(x = anio, y = .data[[ycol]], color = comunidad)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(x = "Año", y = modo,
           title = "Evolución y divergencia respecto a la media nacional") +
      theme_minimal()
  })
  
  output$plot_sector <- renderPlot({
    f <- get_filtros("sectores_filtros")
    req(f$metrica, f$years)
    
    df <- datos_sector(f$metrica, f$years)
    validate(need(!is.null(df) && nrow(df) > 0, "No hay datos sectoriales para los filtros seleccionados."))
    
    sel <- f$ccaa_multi
    if (is.null(sel) || length(sel) == 0) sel <- f$ccaa_sel
    req(sel)
    
    df <- df %>% filter(comunidad %in% sel)
    validate(need(nrow(df) > 0, "No hay datos para las CCAA seleccionadas."))
    
    if (!is.null(f$vista_sec) && f$vista_sec == "pct") {
      df <- df %>%
        group_by(anio, comunidad) %>%
        mutate(valor = valor / sum(valor, na.rm = TRUE)) %>%
        ungroup()
    }
    
    ggplot(df, aes(x = anio, y = valor, fill = sector)) +
      geom_area(alpha = 0.9, position = "stack") +
      facet_wrap(~comunidad, scales = "free_y") +
      labs(
        x = "Año",
        y = ifelse(!is.null(f$vista_sec) && f$vista_sec == "pct", "Proporción", "Volumen"),
        title = "Evolución de la composición sectorial"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot_bubble <- renderPlotly({
    
    ccaa <- input[["bubble_filtros-ccaa_sel"]]
    years <- input[["bubble_filtros-year_range"]]
    req(ccaa, years)
    
    df <- datos_bubble()
    req(df)
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(
      x = paro,
      y = contratos,
      size = contratos,
      color = sector,
      text = paste(
        "Sector:", sector,
        "<br>Paro:", format(paro, big.mark=".", decimal.mark=","),
        "<br>Contratos:", format(contratos, big.mark=".", decimal.mark=",")
      )
    )) +
      geom_point(alpha = 0.8) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })


  # ---- Tabla interactiva ----
  output$dt_data <- renderDT({
    df <- datos_agregados()
    req(df)
    dt <- df %>% arrange(desc(anio))
    datatable(dt, options = list(pageLength = 25, searchHighlight = TRUE), rownames = FALSE)
  })

  # --- Info de Carga de Datos (Debug) ---
  output$availability_info <- renderText({
    df <- datos_agregados()
    if (is.null(df) || nrow(df) == 0) {
      return("Error: No se han cargado datos de SEPE. Revisa las rutas de los archivos.")
    }

    n_ccaa <- length(unique(df$comunidad))
    sum_paro <- if ("paro_total" %in% names(df)) sum(!is.na(df$paro_total)) else 0
    sum_contratos <- if ("contratos_total" %in% names(df)) sum(!is.na(df$contratos_total)) else 0
    sum_dtes <- if ("dtes_total" %in% names(df)) sum(!is.na(df$dtes_total)) else 0

    avg_paro <- if (n_ccaa > 0) sum_paro / n_ccaa else 0
    avg_contratos <- if (n_ccaa > 0) sum_contratos / n_ccaa else 0
    avg_dtes <- if (n_ccaa > 0) sum_dtes / n_ccaa else 0

    anios_presentes <- if (nrow(df) > 0) paste(sort(unique(df$anio)), collapse = ", ") else "Ninguno"

    lines <- c()
    lines <- c(lines, sprintf("Años disponibles: %s", anios_presentes))
    lines <- c(lines, sprintf("Comunidades autónomas únicas: %d", n_ccaa))
    lines <- c(lines, sprintf("Paro (registros): %d (≈ %.0f años por CCAA)", sum_paro, avg_paro))
    lines <- c(lines, sprintf("Contratos (registros): %d (≈ %.0f años por CCAA)", sum_contratos, avg_contratos))
    lines <- c(lines, sprintf("Demandantes (registros): %d (≈ %.0f años por CCAA)", sum_dtes, avg_dtes))
    paste(lines, collapse = "\n")
  })

}

shinyApp(ui = ui, server = server)
