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
# ---------- AÑADIDO: DESCARGA Y LECTURA DE POBLACIÓN (INE 2855..2907) ----------
# --------------------------------------------------------------------------------------
# Este bloque utiliza la función descargar_si_existe() ya definida más arriba.
dir_poblacion <- file.path(dir_data, "poblacion")
dir.create(dir_poblacion, recursive = TRUE, showWarnings = FALSE)

base_ine_pob <- "https://www.ine.es/jaxiT3/files/t/csv_bdsc/%s.csv"
codigos_ine <- 2855:2907

# Descarga de cada CSV si no existe
poblacion_files <- character(0)
for (cod in codigos_ine) {
  url <- sprintf(base_ine_pob, cod)
  destfile <- file.path(dir_poblacion, sprintf("ine_poblacion_%s.csv", cod))
  if (!file.exists(destfile)) {
    message(sprintf("Comprobando/descargando INE %s ...", cod))
    ok <- descargar_si_existe(url, destfile)   # <-- usa la función existente
    if (isTRUE(ok) && file.exists(destfile)) {
      poblacion_files <- c(poblacion_files, destfile)
    } else {
      message(sprintf("No se descargó %s (omitido).", cod))
    }
  } else {
    message(sprintf("Ya existe: %s", basename(destfile)))
    poblacion_files <- c(poblacion_files, destfile)
  }
}

# Lector específico para el formato:
# "Municipios;Sexo;Periodo;Total" (ej. "02001 Abengibre;Total;2024;759")
leer_ine_csv_poblacion <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)

  df <- tryCatch(
    read.csv(ruta, sep = ";", fileEncoding = "latin1", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      message(sprintf("Error leyendo INE %s : %s", basename(ruta), e$message))
      return(NULL)
    }
  )

  if (is.null(df) || ncol(df) == 0) return(NULL)
  names(df) <- trimws(names(df))

  # localizar columnas relevantes
  col_mun <- names(df)[grepl("^Municipios$|^Municipio$|municipios|municipio", names(df), ignore.case = TRUE)]
  col_sex <- names(df)[grepl("^Sexo$|sexo", names(df), ignore.case = TRUE)]
  col_per <- names(df)[grepl("^Periodo$|Periodo|periodo|Año|Anio|anio", names(df), ignore.case = TRUE)]
  col_tot <- names(df)[grepl("^Total$|total|TOTAL|Poblaci|habitantes", names(df), ignore.case = TRUE)]

  if (length(col_mun) == 0 || length(col_sex) == 0 || length(col_per) == 0 || length(col_tot) == 0) {
    message(sprintf("Estructura inesperada en %s. No contiene columnas mínimas.", basename(ruta)))
    return(NULL)
  }

  col_mun <- col_mun[1]; col_sex <- col_sex[1]; col_per <- col_per[1]; col_tot <- col_tot[1]

  # Filtrar sólo Sexo == "Total"
  filas_total <- tolower(trimws(as.character(df[[col_sex]]))) == "total"
  df <- df[filas_total, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  # Extraer código municipal (primeros 5 dígitos)
  municipios_raw <- as.character(df[[col_mun]])
  cod_mun <- sub("^\\s*([0-9]{5}).*$", "\\1", municipios_raw)
  cod_mun[!grepl("^[0-9]{5}$", cod_mun)] <- NA_character_

  # Periodo → año
  anio <- suppressWarnings(as.integer(as.character(df[[col_per]])))

  # Limpiar Total → numérico
  tot_raw <- as.character(df[[col_tot]])
  tot_clean <- gsub("\\.", "", tot_raw)
  tot_clean <- gsub(",", ".", tot_clean, fixed = TRUE)
  poblacion_num <- suppressWarnings(as.numeric(tot_clean))

  out <- data.frame(
    cod_mun = cod_mun,
    anio = anio,
    poblacion = poblacion_num,
    fuente_file = basename(ruta),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$cod_mun) & !is.na(out$anio) & !is.na(out$poblacion), , drop = FALSE]
  if (nrow(out) == 0) return(NULL)

  out <- out %>% select(cod_mun, anio, poblacion)
  out
}

# Leer y concatenar todos los ficheros descargados
poblacion_list <- lapply(poblacion_files, leer_ine_csv_poblacion)
poblacion_list <- Filter(Negate(is.null), poblacion_list)

if (length(poblacion_list) > 0) {
  poblacion_municipio_df <- bind_rows(poblacion_list) %>%
    mutate(cod_mun = as.character(cod_mun),
           anio = as.integer(anio),
           poblacion = as.numeric(poblacion)) %>%
    arrange(cod_mun, desc(anio))
} else {
  poblacion_municipio_df <- tibble::tibble(cod_mun = character(), anio = integer(), poblacion = numeric())
}

get_poblacion_municipio <- function() {
  poblacion_municipio_df
}

message(sprintf("Población: ficheros procesados: %d; registros municipales: %d",
                length(poblacion_files), nrow(poblacion_municipio_df)))
# --------------------------------------------------------------------------------------
# ---------- FIN BLOQUE POBLACIÓN ----------
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# ---------- 3. UI (NAVBAR con páginas por gráfico) ----------
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

  # Exponer la tabla de población como reactive dentro del server
  poblacion_df_server <- reactive({
    get_poblacion_municipio()
  })

  agregador_ccaa <- function(df_raw, patrones_busqueda = c("paro", "contrat", "demand", "dtes", "total")) {
    if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) return(NULL)

    if ("Comunidad Autónoma" %in% names(df_raw)) {
      df_raw$`Comunidad Autónoma` <- normalizar_ccaa(df_raw$`Comunidad Autónoma`)
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

    if (!("Código mes" %in% names(df_raw))) {
      warning("No hay columna 'Código mes' para extraer el año; se omite este fichero.")
      return(NULL)
    }

    df_raw %>%
      mutate(anio = suppressWarnings(as.integer(substr(`Código mes`, 1, 4)))) %>%
      filter(!is.na(anio)) %>%
      group_by(anio, comunidad = `Comunidad Autónoma`) %>%
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

        ruta_contratos <- file.path("data", "contratos", sprintf("Contratos_por_municipios_%s_csv.csv", ano))
        ruta_paro      <- file.path("data", "paro", sprintf("Paro_por_municipios_%s_csv.csv", ano))
        ruta_dtes      <- file.path("data", "dtes_empleo", sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano))

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
      labs(x = "Comunidad Autónoma", y = "Año", title = paste("Distribución de", titulo_metrica(), "por Comunidad Autónoma y año")) +
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
