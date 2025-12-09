# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(httr)
library(scales)
library(shinyjs)
library(DT)


# Definir la lista de años al inicio
anios <- 2010:2024
anios_titulos <- paste(min(anios), max(anios), sep = "–")

source("preprocessing.R")
res <- descargar_datasets_sepe(anios = anios, dir_data = "data")
res <- descargar_y_procesar_poblacion(codigos_ine = 2855:2907, dir_data = "data", anio_min = 2010, anio_max = 2025)


# --------------------------------------------------------------------------------------
# ---------- 2. UI (NAVBAR con páginas por gráfico) ----------
# --------------------------------------------------------------------------------------
ui <- navbarPage(
  title = paste("SEPE — Datos por CCAA (", anios_titulos, ")"),
  id = "main_nav",
  header = tagList(useShinyjs()),

  tabPanel("Controles",
           sidebarLayout(
             sidebarPanel(
               h4("Filtros generales"),
               selectInput("ccaa_sel", "Comunidad Autónoma:", choices = NULL, selected = NULL),
               selectInput("metrica_sel", "Métrica a visualizar:",
                           choices = c("Paro Registrado" = "paro",
                                       "Contratos Registrados" = "contratos",
                                       "Demandantes de Empleo" = "dtes"),
                           selected = "paro"),
               checkboxGroupInput("ccaa_multi", "CCAA para comparativa (líneas):",
                                  choices = NULL, selected = NULL, inline = FALSE),
               width = 3
             ),
             mainPanel(
               h4("Instrucciones"),
               p("Selecciona Comunidad y métrica en el panel izquierdo. Usa la pestaña 'Tabla de datos' para ver datos en bruto."),
               verbatimTextOutput("availability_info"),
               width = 9
             )
           )
  ),

  tabPanel("Por CCAA (barras)", value = "tab_bar",
           fluidRow(
             column(12, h3(textOutput("titulo_grafico1")), p("Evolución anual para la CCAA seleccionada.")),
             column(12, plotOutput("plot_bar_ccaa", height = "420px"))
           )
  ),

  tabPanel("Vista conjunta (Heatmap)", value = "tab_heatmap",
           fluidRow(
             column(12, h3(textOutput("titulo_grafico2")), p("Heatmap año × Comunidad.")),
             column(12, plotOutput("plot_heatmap", height = "600px"))
           )
  ),

  tabPanel("Comparativa (líneas)", value = "tab_line",
           fluidRow(
             column(12, h3("Comparativa temporal entre CCAA")),
             column(12, plotOutput("plot_line_ccaa_all", height = "520px"))
           )
  ),

  tabPanel("Top / Bottom CCAA", value = "tab_topbottom",
           fluidRow(
             column(12, h3("Top 5 y Bottom 5 por media del periodo")),
             column(6, plotOutput("plot_top5", height = "420px")),
             column(6, plotOutput("plot_bottom5", height = "420px"))
           )
  ),

  tabPanel("Tabla de datos", value = "tab_table",
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
  source("read_data.R")

  # Exponer la tabla de población como reactive dentro del server
  poblacion_df_server <- reactive({
    get_poblacion_municipio()
  })

  agregador_ccaa <- function(df_raw, patrones_busqueda = c("paro", "contrat", "demand", "dtes", "total")) {
    if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) return(NULL)

    if ("Comunidad Aut" %in% names(df_raw)) {    
      df_raw$`Comunidad Aut` <- normalizar_ccaa(df_raw$`Comunidad Aut`)
    } else {
      warning("No encontrada la columna 'Comunidad Autónoma' en un fichero.")
      return(NULL)
    }

    numeric_cols <- names(df_raw)[sapply(df_raw, is.numeric)]
    candidate_cols <- names(df_raw)[sapply(names(df_raw), function(n) {
      any(vapply(patrones_busqueda, function(p) grepl(p, n, ignore.case = TRUE), logical(1)))
    }) & names(df_raw) %in% numeric_cols]

    chosen <- NULL
    if ("total Paro Registrado" %in% candidate_cols) {
      chosen <- "total Paro Registrado"
    } else if ("Total" %in% candidate_cols) {
      chosen <- "Total"
    } else if (length(candidate_cols) > 0) {
      chosen <- candidate_cols[1]
    } else if (length(numeric_cols) > 0) {
      chosen <- numeric_cols[1]
    }

    if (is.null(chosen)) {
       warning("No se ha podido identificar la columna de valor para agregación.")
       return(NULL)
    }

    if (!("anio" %in% names(df_raw))) {
      warning("No hay columna 'Código mes' para extraer el año; se omite este fichero.")
      return(NULL)
    }

    df_raw %>%
      mutate(anio = suppressWarnings(as.integer(substr(`anio`, 1, 4)))) %>%
      filter(!is.na(anio)) %>%
      group_by(anio, comunidad = `Comunidad Aut`) %>%
      summarise(valor = mean(.data[[chosen]], na.rm = TRUE), .groups = "drop")
  }

  safe_read <- function(ruta) {
    if (!file.exists(ruta)) {
      message(sprintf("Archivo no encontrado: %s", basename(ruta)))
      return(NULL)
    }
    leer_sepe_csv(ruta)
  }

  datos_agregados <- reactive({
    contratos_list <- list()
    paro_list <- list()
    dtes_list <- list()

    withProgress(message = 'Cargando y agregando datos...', value = 0, {
      n_anios <- length(anios)
      for (i in seq_along(anios)) {
        ano <- anios[i]
        setProgress(i/n_anios, detail = paste("Procesando año", ano))

        ruta_contratos <- file.path("data", "contratos", sprintf("Contratos_por_municipios_%s_csv_processed.csv", ano))
        ruta_paro      <- file.path("data", "paro", sprintf("Paro_por_municipios_%s_csv_processed.csv", ano))
        ruta_dtes      <- file.path("data", "dtes_empleo", sprintf("Dtes_empleo_por_municipios_%s_csv_processed.csv", ano))

        df_c <- safe_read(ruta_contratos)
        
        if (!is.null(df_c)) {
          agg_c <- agregador_ccaa(df_c, patrones_busqueda = c("contrat", "contrato", "total"))
          if (!is.null(agg_c)) contratos_list[[as.character(ano)]] <- agg_c %>% rename(contratos_total = valor)
        }

        df_p <- safe_read(ruta_paro)
        if (!is.null(df_p)) {
          agg_p <- agregador_ccaa(df_p, patrones_busqueda = c("paro", "total"))
          if (!is.null(agg_p)) paro_list[[as.character(ano)]] <- agg_p %>% rename(paro_total = valor)
        }

        df_d <- safe_read(ruta_dtes)
        if (!is.null(df_d)) {
          agg_d <- agregador_ccaa(df_d, patrones_busqueda = c("demand", "dtes", "demandant", "total"))
          if (!is.null(agg_d)) dtes_list[[as.character(ano)]] <- agg_d %>% rename(dtes_total = valor)
        }
      }
    })

    contratos_ccaa <- if (length(contratos_list) > 0) bind_rows(contratos_list) else tibble::tibble(anio = integer(), comunidad = factor(), contratos_total = numeric())
    paro_ccaa_local <- if (length(paro_list) > 0) bind_rows(paro_list) else tibble::tibble(anio = integer(), comunidad = factor(), paro_total = numeric())
    dtes_ccaa <- if (length(dtes_list) > 0) bind_rows(dtes_list) else tibble::tibble(anio = integer(), comunidad = factor(), dtes_total = numeric())

    df_merged <- paro_ccaa_local %>%
      full_join(contratos_ccaa, by = c("anio", "comunidad")) %>%
      full_join(dtes_ccaa, by = c("anio", "comunidad"))

    if (nrow(df_merged) > 0) {
        df_merged$comunidad <- as.factor(df_merged$comunidad)
        df_merged$anio <- as.integer(df_merged$anio)
    }

    df_merged
  })

  observeEvent(datos_agregados(), {
      df <- datos_agregados()
      ccaa_choices <- sort(unique(df$comunidad))

      updateSelectInput(
          session,
          "ccaa_sel",
          choices = ccaa_choices,
          selected = if (length(ccaa_choices) > 0) ccaa_choices[1] else NULL
      )

      updateCheckboxGroupInput(
        session,
        "ccaa_multi",
        choices = ccaa_choices,
        selected = if (length(ccaa_choices) > 0) ccaa_choices[1:min(6, length(ccaa_choices))] else NULL
      )

      if (nrow(df) > 0) {
          anios_presentes <- unique(df$anio)
          if (length(anios_presentes) > 0) {
              new_title <- paste("Datos de Empleo SEPE por Comunidad Autónoma (", min(anios_presentes), "–", max(anios_presentes), ")")
              shinyjs::runjs(paste0("document.title = '", new_title, "';"))
          }
      }
  })

  datos_plot <- reactive({
      df_merged <- datos_agregados()
      req(!is.null(df_merged))
      req(nrow(df_merged) > 0)

      metrica_col <- paste0(input$metrica_sel, "_total")

      if (!(metrica_col %in% names(df_merged))) {
          return(NULL)
      }

      df_long <- df_merged %>%
          select(anio, comunidad, valor = !!metrica_col) %>%
          drop_na(valor)

      df_long
  })

  titulo_metrica <- reactive({
      switch(input$metrica_sel,
             "paro" = "Paro Registrado",
             "contratos" = "Contratos Registrados",
             "dtes" = "Demandantes de Empleo",
             "Métrica Desconocida")
  })

  # ---- Outputs existentes ----
  output$titulo_grafico1 <- renderText({ paste("Gráfico 1: Evolución de", titulo_metrica(), "en una CCAA") })
  output$titulo_grafico2 <- renderText({ paste("Gráfico 2:", titulo_metrica(), "por año y CCAA (vista conjunta)") })

  output$plot_bar_ccaa <- renderPlot({
    df_long <- datos_plot()
    req(df_long)
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
    df_long <- datos_plot()
    req(df_long)

    years_levels <- sort(unique(df_long$anio), decreasing = TRUE)
    df_long$anio_f <- factor(df_long$anio, levels = years_levels)
    df_long$comunidad_f <- factor(df_long$comunidad, levels = sort(unique(df_long$comunidad)))

    low_col <- ifelse(input$metrica_sel == "paro", "white", "steelblue1")
    high_col <- ifelse(input$metrica_sel == "paro", "firebrick", "darkblue")

    ggplot(df_long, aes(x = comunidad_f, y = anio_f, fill = valor)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = low_col, high = high_col, name = paste(titulo_metrica(), "(media anual)")) +
      labs(x = "Comunidad Aut", y = "Año", title = paste("Distribución de", titulo_metrica(), "por Comunidad Autónoma y año")) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(size = 9), plot.title = element_text(face = "bold"), panel.grid = element_blank())
  })

  # ---- Nueva: comparativa líneas (varias CCAA + media nacional) ----
  output$plot_line_ccaa_all <- renderPlot({
    df_long <- datos_plot()
    req(df_long)

    sel_ccaa <- input$ccaa_multi
    if (is.null(sel_ccaa) || length(sel_ccaa) == 0) {
      sel_ccaa <- unique(df_long$comunidad)[1:min(6, length(unique(df_long$comunidad)))]
    }

    df_sel <- df_long %>% filter(comunidad %in% sel_ccaa)

    # media nacional por año
    df_media <- df_long %>% group_by(anio) %>% summarise(media_nacional = mean(valor, na.rm = TRUE), .groups = "drop")

    ggplot() +
      geom_line(data = df_sel, aes(x = anio, y = valor, color = comunidad, group = comunidad), size = 0.9) +
      geom_point(data = df_sel, aes(x = anio, y = valor, color = comunidad), size = 1.8) +
      geom_line(data = df_media, aes(x = anio, y = media_nacional), size = 1.05, linetype = "dashed") +
      geom_point(data = df_media, aes(x = anio, y = media_nacional), size = 2, shape = 21, fill = "white") +
      labs(x = "Año", y = paste(titulo_metrica(), "(media anual)"), color = "Comunidad", title = paste("Comparativa:", titulo_metrica(), "— CCAA seleccionadas + media nacional")) +
      scale_x_continuous(breaks = sort(unique(df_long$anio))) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold"))
  })

  # ---- Nueva: Top / Bottom CCAA por media del periodo ----
  output$plot_top5 <- renderPlot({
    df_long <- datos_plot()
    req(df_long)

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
    df_long <- datos_plot()
    req(df_long)

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
      base_msg <- "Error: No se han cargado datos de SEPE. Revisa las rutas de los archivos."
    } else {
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

      base_msg <- paste(lines, collapse = "\n")
    }

    # Añadir resumen de población (si está disponible)
    pob_df <- poblacion_df_server()
    if (!is.null(pob_df) && nrow(pob_df) > 0) {
      años_pob <- sort(unique(pob_df$anio))
      if (length(años_pob) > 0) {
        año_reciente <- max(años_pob, na.rm = TRUE)
        pob_reciente <- pob_df %>% filter(anio == año_reciente)
        n_municipios <- length(unique(pob_reciente$cod_mun))
        total_pob <- sum(pob_reciente$poblacion, na.rm = TRUE)
        pobl_lines <- c()
        pobl_lines <- c(pobl_lines, sprintf("Población: años disponibles: %s", paste(años_pob, collapse = ", ")))
        pobl_lines <- c(pobl_lines, sprintf("Municipios (año %d): %d", año_reciente, n_municipios))
        pobl_lines <- c(pobl_lines, sprintf("Población total (año %d): %s", año_reciente, format(total_pob, big.mark = ".", decimal.mark = ",")))
        base_msg <- paste(base_msg, paste(pobl_lines, collapse = "\n"), sep = "\n\n")
      } else {
        base_msg <- paste(base_msg, "Población: sin datos disponibles.", sep = "\n\n")
      }
    } else {
      base_msg <- paste(base_msg, "Población: sin datos disponibles.", sep = "\n\n")
    }

    base_msg
  })

}

shinyApp(ui = ui, server = server)
