# -----------------------------
# FUNCIÓN COMÚN: descargar_si_existe
# -----------------------------
descargar_si_existe <- function(
  url,
  destfile,
  overwrite = FALSE,
  timeout_head = 10,
  timeout_get = 60,
  quiet = FALSE,
  procesar = TRUE,
  procesar_fun = NULL   # función (o nombre) para procesar tras descargar; si NULL no procesa
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("El paquete 'httr' es necesario. Instálalo con install.packages('httr').")
  }

  if (!quiet) message(sprintf("Comprobando: %s -> %s", url, basename(destfile)))
  proc_destfile <- sub("\\.csv$", "_processed.csv", destfile)

  # If processed exists and not overwrite -> OK
  if (file.exists(proc_destfile) && !isTRUE(overwrite)) {
    if (!quiet) message(sprintf("Ya existe y está procesado: %s", proc_destfile))
    return(TRUE)
  }

  # If raw CSV exists and not overwrite -> optionally process if processed missing
  if (file.exists(destfile) && !isTRUE(overwrite)) {
    if (!quiet) message(sprintf("Ya existe el CSV: %s", destfile))
    if (isTRUE(procesar) && !file.exists(proc_destfile)) {
      if (!is.null(procesar_fun)) {
        if (!quiet) message(sprintf("Procesando (existente): %s", destfile))
        # soportar procesar_fun como función o nombre
        tryCatch({
          if (is.character(procesar_fun)) do.call(get(procesar_fun), list(destfile))
          else if (is.function(procesar_fun)) procesar_fun(destfile)
          else stop("procesar_fun debe ser NULL, nombre de función o una función.")
          if (!quiet) message("Procesado OK: ", basename(proc_destfile))
        }, error = function(e) {
          message("Error al procesar (existente): ", e$message)
        })
      } else {
        if (!quiet) message("Procesado solicitado pero no hay 'procesar_fun' (omitido).")
      }
    }
    return(TRUE)
  }

  # Intentar HEAD (si el servidor lo permite)
  res_head <- tryCatch(httr::HEAD(url, httr::timeout(timeout_head)), error = function(e) NULL)
  status <- if (!is.null(res_head)) httr::status_code(res_head) else NULL

  do_get <- function() {
    res_get <- tryCatch(
      httr::GET(url, httr::progress(), httr::write_disk(destfile, overwrite = TRUE), httr::timeout(timeout_get)),
      error = function(e) e
    )
    if (inherits(res_get, "error")) {
      if (!quiet) message(sprintf("No se pudo descargar %s : %s", basename(destfile), res_get$message))
      if (file.exists(destfile)) file.remove(destfile)
      return(FALSE)
    }
    st <- httr::status_code(res_get)
    if (st >= 400) {
      if (!quiet) message(sprintf("No disponible (HTTP %s): %s", st, basename(destfile)))
      if (file.exists(destfile)) file.remove(destfile)
      return(FALSE)
    }

    if (!quiet) message(sprintf("Descargado: %s", basename(destfile)))

    # Si se pidió procesar y se suministró función, llamarla
    if (isTRUE(procesar)) {
      if (!is.null(procesar_fun)) {
        if (!quiet) message(sprintf("Procesando (descargado): %s", destfile))
        tryCatch({
          if (is.character(procesar_fun)) do.call(get(procesar_fun), list(destfile))
          else if (is.function(procesar_fun)) procesar_fun(destfile)
          else stop("procesar_fun debe ser NULL, nombre de función o una función.")
          if (!quiet) message("Procesado OK: ", basename(proc_destfile))
        }, error = function(e) {
          # no eliminamos el CSV; dejamos para reintentos posteriores
          message("Error al procesar (descargado): ", e$message)
        })
      } else {
        if (!quiet) message("Procesado solicitado pero no hay 'procesar_fun' (omitido).")
      }
    }

    return(TRUE)
  }

  # Si HEAD no respondió o devolvió >=400 hacemos GET directo
  if (is.null(status) || status >= 400) {
    return(do_get())
  } else {
    return(do_get())
  }
}

# -----------------------------
# descargar_datasets_sepe: usa la función superior descargar_si_existe
# -----------------------------
descargar_datasets_sepe <- function(
  anios = 2010:2024,
  dir_data = "data",
  overwrite = FALSE,
  timeout_head = 10,
  timeout_get = 60,
  quiet = FALSE,
  procesar = TRUE
) {
  # URLs base
  base_contratos <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Contratos_por_municipios_%s_csv.csv"
  base_paro      <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_%s_csv.csv"
  base_dtes_empleo <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Dtes_empleo_por_municipios_%s_csv.csv"

  # crear estructura de directorios
  dir_contratos <- file.path(dir_data, "contratos")
  dir_paro      <- file.path(dir_data, "paro")
  dir_dtes_empleo <- file.path(dir_data, "dtes_empleo")
  dir.create(dir_contratos, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_paro, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_dtes_empleo, recursive = TRUE, showWarnings = FALSE)

  # intentar source del pipeline de procesado (si existe)
  if (file.exists("processing_sepe.R")) {
    tryCatch(source("processing_sepe.R"), error = function(e) {
      if (!quiet) message("No se pudo source('processing_sepe.R'): ", e$message)
    })
  } else {
    if (!quiet) message("processing_sepe.R no encontrado en el directorio de trabajo; se omitirá el procesado automático si procesar = TRUE.")
  }

  resultados <- setNames(vector("list", length(anios)), as.character(anios))

  for (ano in anios) {
    ano_chr <- as.character(ano)
    resultados[[ano_chr]] <- list(contratos = FALSE, paro = FALSE, dtes_empleo = FALSE)

    # contratos
    url_c <- sprintf(base_contratos, ano)
    dest_c <- file.path(dir_contratos, sprintf("Contratos_por_municipios_%s_csv.csv", ano))
    ok_c <- tryCatch(
      descargar_si_existe(url_c, dest_c, overwrite = overwrite, timeout_head = timeout_head, timeout_get = timeout_get, quiet = quiet, procesar = procesar, procesar_fun = if (exists("procesar_csv_sepe")) "procesar_csv_sepe" else NULL),
      error = function(e) { if (!quiet) message(e$message); FALSE }
    )
    resultados[[ano_chr]]$contratos <- isTRUE(ok_c) && file.exists(dest_c)

    # paro
    url_p <- sprintf(base_paro, ano)
    dest_p <- file.path(dir_paro, sprintf("Paro_por_municipios_%s_csv.csv", ano))
    ok_p <- tryCatch(
      descargar_si_existe(url_p, dest_p, overwrite = overwrite, timeout_head = timeout_head, timeout_get = timeout_get, quiet = quiet, procesar = procesar, procesar_fun = if (exists("procesar_csv_sepe")) "procesar_csv_sepe" else NULL),
      error = function(e) { if (!quiet) message(e$message); FALSE }
    )
    resultados[[ano_chr]]$paro <- isTRUE(ok_p) && file.exists(dest_p)

    # dtes_empleo
    url_d <- sprintf(base_dtes_empleo, ano)
    dest_d <- file.path(dir_dtes_empleo, sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano))
    ok_d <- tryCatch(
      descargar_si_existe(url_d, dest_d, overwrite = overwrite, timeout_head = timeout_head, timeout_get = timeout_get, quiet = quiet, procesar = procesar, procesar_fun = if (exists("procesar_csv_sepe")) "procesar_csv_sepe" else NULL),
      error = function(e) { if (!quiet) message(e$message); FALSE }
    )
    resultados[[ano_chr]]$dtes_empleo <- isTRUE(ok_d) && file.exists(dest_d)
  }

  if (!quiet) {
    message("Resumen de descargas:")
    for (ano in names(resultados)) {
      res <- resultados[[ano]]
      message(sprintf(" %s: contratos=%s, paro=%s, dtes_empleo=%s",
                      ano,
                      ifelse(res$contratos, "OK", "NO"),
                      ifelse(res$paro, "OK", "NO"),
                      ifelse(res$dtes_empleo, "OK", "NO")))
    }
  }

  invisible(resultados)
}

# -----------------------------
# descargar_y_procesar_poblacion: usa la función superior descargar_si_existe
# -----------------------------

descargar_y_procesar_poblacion <- function(
  codigos_ine = 2854:2908, # AMPLIADO: 2854 es Álava (01) hasta 2908 (Melilla)
  dir_data = "data",
  dir_poblacion_sub = "poblacion",
  overwrite = FALSE,
  anio_min = 2010,
  anio_max = 2025,
  quiet = FALSE
) {
  # dependencias mínimas
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Instala el paquete 'dplyr'.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Instala el paquete 'tidyr'.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Instala el paquete 'tibble'.")

  # comprobar que descargar_si_existe está disponible
  if (!exists("descargar_si_existe", mode = "function")) {
    stop("No se encontró la función 'descargar_si_existe' en el entorno.")
  }

  dir_poblacion <- file.path(dir_data, dir_poblacion_sub)
  dir.create(dir_poblacion, recursive = TRUE, showWarnings = FALSE)
   
  # Cambiamos el nombre del fichero de salida
  ruta_poblacion_salida <- file.path(dir_poblacion, "poblacion_provincia_processed.csv")

  # Si el fichero procesado ya existe y no overwrite -> salir
  if (file.exists(ruta_poblacion_salida) && !isTRUE(overwrite)) {
    if (!quiet) message("El fichero procesado ya existe y overwrite = FALSE. Omitiendo: ", ruta_poblacion_salida)
    poblacion_existente <- tryCatch({
      read.csv(ruta_poblacion_salida, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE, check.names = FALSE)
    }, error = function(e) NULL)
    
    return(invisible(list(files = list.files(dir_poblacion, pattern = "^ine_poblacion_.*\\.csv$", full.names = TRUE),
                          poblacion = poblacion_existente,
                          ruta_salida = ruta_poblacion_salida)))
  }

  base_ine_pob <- "https://www.ine.es/jaxiT3/files/t/csv_bdsc/%s.csv"
  poblacion_files <- character(0)

  # Descarga
  for (cod in codigos_ine) {
    url <- sprintf(base_ine_pob, cod)
    destfile <- file.path(dir_poblacion, sprintf("ine_poblacion_%s.csv", cod))

    ok <- tryCatch(
      descargar_si_existe(url, destfile, overwrite = overwrite, timeout_head = 10, timeout_get = 60, quiet = quiet, procesar = FALSE),
      error = function(e) { if (!quiet) message("Error en descarga: ", e$message); FALSE }
    )
    if (isTRUE(ok) && file.exists(destfile)) {
      poblacion_files <- c(poblacion_files, destfile)
    }
  }

  # Función de lectura interna
  leer_ine_csv_poblacion <- function(ruta) {
    if (!file.exists(ruta)) return(NULL)
    df <- tryCatch(
      read.csv(ruta, sep = ";", fileEncoding = "latin1", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) { return(NULL) }
    )
    if (is.null(df) || ncol(df) == 0) return(NULL)
    names(df) <- trimws(names(df))

    col_mun <- names(df)[grepl("^Municipios$|^Municipio$|municipios|municipio", names(df), ignore.case = TRUE)][1]
    col_sex <- names(df)[grepl("^Sexo$|sexo", names(df), ignore.case = TRUE)][1]
    col_per <- names(df)[grepl("^Periodo$|Periodo|periodo|Año|Anio|anio|Year", names(df), ignore.case = TRUE)][1]
    col_tot <- names(df)[grepl("^Total$|total|TOTAL|Poblaci|habitantes|Population", names(df), ignore.case = TRUE)][1]

    if (is.na(col_mun) || is.na(col_sex) || is.na(col_per) || is.na(col_tot)) return(NULL)

    municipios_raw <- as.character(df[[col_mun]])
    # Extraer CP (5 dígitos)
    cod_mun <- sub("^\\s*([0-9]{5}).*$", "\\1", municipios_raw)
    cod_mun[!grepl("^[0-9]{5}$", cod_mun)] <- NA_character_

    anio <- suppressWarnings(as.integer(as.character(df[[col_per]])))
    
    tot_clean <- gsub("\\.", "", as.character(df[[col_tot]]))
    tot_clean <- gsub(",", ".", tot_clean, fixed = TRUE)
    poblacion_num <- suppressWarnings(as.numeric(tot_clean))

    sexo_raw <- tolower(trimws(as.character(df[[col_sex]])))
    sexo <- dplyr::recode(sexo_raw,
                          "hombres" = "hombres", "hombre" = "hombres",
                          "mujeres" = "mujeres", "mujer" = "mujeres",
                          "total" = "total", "ambos" = "total",
                          .default = NA_character_)

    out <- data.frame(cod_mun = cod_mun, anio = anio, sexo = sexo, poblacion = poblacion_num, stringsAsFactors = FALSE)
    out[!is.na(out$cod_mun) & !is.na(out$anio) & !is.na(out$poblacion), , drop = FALSE]
  }

  # Procesamiento y Agregación
  poblacion_list <- lapply(poblacion_files, leer_ine_csv_poblacion)
  poblacion_list <- Filter(Negate(is.null), poblacion_list)

  if (length(poblacion_list) > 0) {
    # 1. Unir y Pivotar
    df_raw <- dplyr::bind_rows(poblacion_list) %>%
      dplyr::mutate(
        cod_mun = as.character(cod_mun),
        anio = as.integer(anio),
        poblacion = as.numeric(poblacion)
      ) %>%
      tidyr::pivot_wider(
        names_from = sexo,
        values_from = poblacion,
        names_prefix = "poblacion_"
      ) %>%
      dplyr::filter(anio >= anio_min, anio <= anio_max)

    # 2. DEFINIR MAPEO PROVINCIA -> CA
    # Creamos un tibble auxiliar para cruzar los datos
    mapa_ca_prov <- tibble::tibble(
      cod_provincia = c(
        # Andalucía (01)
        "04","11","14","18","21","23","29","41",
        # Aragón (02)
        "22","44","50",
        # Asturias (03)
        "33",
        # Baleares (04)
        "07",
        # Canarias (05)
        "35","38",
        # Cantabria (06)
        "39",
        # Castilla y León (07)
        "05","09","24","34","37","40","42","47","49",
        # Castilla-La Mancha (08)
        "02","13","16","19","45",
        # Cataluña (09)
        "08","17","25","43",
        # C. Valenciana (10)
        "03","12","46",
        # Extremadura (11)
        "06","10",
        # Galicia (12)
        "15","27","32","36",
        # Madrid (13)
        "28",
        # Murcia (14)
        "30",
        # Navarra (15)
        "31",
        # País Vasco (16)
        "01","20","48",
        # La Rioja (17)
        "26",
        # Ceuta (18)
        "51",
        # Melilla (19)
        "52"
      ),
      `Cod CA` = c(
        rep("01", 8), # Andalucía
        rep("02", 3), # Aragón
        "03",         # Asturias
        "04",         # Baleares
        rep("05", 2), # Canarias
        "06",         # Cantabria
        rep("07", 9), # CyL
        rep("08", 5), # CLM
        rep("09", 4), # Cataluña
        rep("10", 3), # Valencia
        rep("11", 2), # Extremadura
        rep("12", 4), # Galicia
        "13",         # Madrid
        "14",         # Murcia
        "15",         # Navarra
        rep("16", 3), # País Vasco
        "17",         # La Rioja
        "18",         # Ceuta
        "19"          # Melilla
      )
    )

    # 3. AGRUPAR POR PROVINCIA Y AÑADIR CA
    poblacion_final_df <- df_raw %>%
      dplyr::mutate(cod_provincia = substr(cod_mun, 1, 2)) %>%
      # Agrupación por provincia y año
      dplyr::group_by(cod_provincia, anio) %>%
      dplyr::summarise(
        poblacion_hombres = sum(poblacion_hombres, na.rm = TRUE),
        poblacion_mujeres = sum(poblacion_mujeres, na.rm = TRUE),
        poblacion_total   = sum(poblacion_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Unir con el mapa de Comunidades Autónomas
      dplyr::left_join(mapa_ca_prov, by = "cod_provincia") %>%
      # Renombrar para output final
      dplyr::rename(`Cod provincia` = cod_provincia) %>%
      # Reordenar columnas: Anio, Cod CA, Cod Provincia, resto...
      dplyr::select(anio, `Cod CA`, `Cod provincia`, dplyr::everything()) %>%
      # Ordenar filas: Anio -> CA -> Provincia
      dplyr::arrange(anio, `Cod CA`, `Cod provincia`)
      
  } else {
    poblacion_final_df <- tibble::tibble(
      anio = integer(),
      `Cod CA` = character(),
      `Cod provincia` = character(),
      poblacion_hombres = numeric(),
      poblacion_mujeres = numeric(),
      poblacion_total = numeric()
    )
  }

  # Guardar CSV procesado
  tryCatch({
    write.csv(poblacion_final_df, file = ruta_poblacion_salida, row.names = FALSE, fileEncoding = "ISO-8859-1")
    if (!quiet) message("Fichero guardado (con Cod CA y ordenado): ", ruta_poblacion_salida)

    # Limpieza de ficheros temporales
    if (length(poblacion_files) > 0) {
      sapply(poblacion_files, function(f) if(file.exists(f)) try(file.remove(f), silent=TRUE))
      if (!quiet) message("Eliminados ficheros fuente temporales.")
    }

  }, error = function(e) {
    warning("No se pudo escribir el fichero: ", e$message)
    ruta_poblacion_salida <<- NA_character_
  })

  invisible(list(files = poblacion_files, poblacion = poblacion_final_df, ruta_salida = ruta_poblacion_salida))
}