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
  codigos_ine = 2855:2907,
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
    stop("No se encontró la función 'descargar_si_existe' en el entorno. Defínela antes de llamar a esta función.")
  }

  dir_poblacion <- file.path(dir_data, dir_poblacion_sub)
  dir.create(dir_poblacion, recursive = TRUE, showWarnings = FALSE)
  ruta_poblacion_salida <- file.path(dir_poblacion, "poblacion_municipio_processed.csv")

  # Si el fichero procesado ya existe y no estamos forzando overwrite -> salir sin hacer nada
  if (file.exists(ruta_poblacion_salida) && !isTRUE(overwrite)) {
    if (!quiet) message("El fichero procesado ya existe y overwrite = FALSE. No se hace nada: ", ruta_poblacion_salida)
    # intentamos leer el fichero existente para devolver el data.frame (si es posible)
    poblacion_existente <- tryCatch({
      read.csv(ruta_poblacion_salida, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE, check.names = FALSE)
    }, error = function(e) {
      if (!quiet) message("No se pudo leer el fichero procesado existente: ", e$message)
      NULL
    })
    return(invisible(list(files = list.files(dir_poblacion, pattern = "^ine_poblacion_.*\\.csv$", full.names = TRUE),
                          poblacion = poblacion_existente,
                          ruta_salida = ruta_poblacion_salida)))
  }

  base_ine_pob <- "https://www.ine.es/jaxiT3/files/t/csv_bdsc/%s.csv"
  poblacion_files <- character(0)

  for (cod in codigos_ine) {
    url <- sprintf(base_ine_pob, cod)
    destfile <- file.path(dir_poblacion, sprintf("ine_poblacion_%s.csv", cod))

    ok <- tryCatch(
      descargar_si_existe(url, destfile, overwrite = overwrite, timeout_head = 10, timeout_get = 60, quiet = quiet, procesar = FALSE),
      error = function(e) { if (!quiet) message("Error en descarga: ", e$message); FALSE }
    )
    if (isTRUE(ok) && file.exists(destfile)) {
      poblacion_files <- c(poblacion_files, destfile)
    } else {
      if (!quiet) message(sprintf("No se descargó %s (omitido).", cod))
    }
  }

  # lector específico (encapsulado)
  leer_ine_csv_poblacion <- function(ruta) {
    if (!file.exists(ruta)) return(NULL)
    df <- tryCatch(
      read.csv(ruta, sep = ";", fileEncoding = "latin1", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) {
        if (!quiet) message(sprintf("Error leyendo INE %s : %s", basename(ruta), e$message))
        return(NULL)
      }
    )
    if (is.null(df) || ncol(df) == 0) return(NULL)
    names(df) <- trimws(names(df))

    # localizar columnas relevantes (flexible)
    col_mun <- names(df)[grepl("^Municipios$|^Municipio$|municipios|municipio", names(df), ignore.case = TRUE)]
    col_sex <- names(df)[grepl("^Sexo$|sexo", names(df), ignore.case = TRUE)]
    col_per <- names(df)[grepl("^Periodo$|Periodo|periodo|Año|Anio|anio|Year", names(df), ignore.case = TRUE)]
    col_tot <- names(df)[grepl("^Total$|total|TOTAL|Poblaci|habitantes|Population", names(df), ignore.case = TRUE)]

    if (length(col_mun) == 0 || length(col_sex) == 0 || length(col_per) == 0 || length(col_tot) == 0) {
      if (!quiet) message(sprintf("Estructura inesperada en %s. No contiene columnas mínimas.", basename(ruta)))
      return(NULL)
    }

    col_mun <- col_mun[1]; col_sex <- col_sex[1]; col_per <- col_per[1]; col_tot <- col_tot[1]

    municipios_raw <- as.character(df[[col_mun]])
    cod_mun <- sub("^\\s*([0-9]{5}).*$", "\\1", municipios_raw)
    cod_mun[!grepl("^[0-9]{5}$", cod_mun)] <- NA_character_

    anio <- suppressWarnings(as.integer(as.character(df[[col_per]])))

    tot_raw <- as.character(df[[col_tot]])
    tot_clean <- gsub("\\.", "", tot_raw)
    tot_clean <- gsub(",", ".", tot_clean, fixed = TRUE)
    poblacion_num <- suppressWarnings(as.numeric(tot_clean))

    sexo_raw <- tolower(trimws(as.character(df[[col_sex]])))
    sexo <- dplyr::recode(sexo_raw,
                          "hombres" = "hombres",
                          "hombre" = "hombres",
                          "mujeres" = "mujeres",
                          "mujer" = "mujeres",
                          "total"   = "total",
                          "ambos"   = "total",
                          .default = NA_character_)

    out <- data.frame(
      cod_mun = cod_mun,
      anio = anio,
      sexo = sexo,
      poblacion = poblacion_num,
      fuente_file = basename(ruta),
      stringsAsFactors = FALSE
    )

    out <- out[!is.na(out$cod_mun) & !is.na(out$anio) & !is.na(out$poblacion), , drop = FALSE]
    if (nrow(out) == 0) return(NULL)
    # devolver columnas básicas
    out <- out[, c("cod_mun", "anio", "sexo", "poblacion"), drop = FALSE]
    out
  }

  # Leer y concatenar
  poblacion_list <- lapply(poblacion_files, leer_ine_csv_poblacion)
  poblacion_list <- Filter(Negate(is.null), poblacion_list)

  if (length(poblacion_list) > 0) {
    poblacion_municipio_df <- dplyr::bind_rows(poblacion_list) %>%
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
      dplyr::filter(anio >= anio_min, anio <= anio_max) %>%
      dplyr::arrange(cod_mun, dplyr::desc(anio))
  } else {
    poblacion_municipio_df <- tibble::tibble(
      cod_mun = character(),
      anio = integer(),
      poblacion_hombres = numeric(),
      poblacion_mujeres = numeric(),
      poblacion_total = numeric()
    )
  }

  # renombrar cod_mun -> 'Cod municipio' para mantener consistencia
  poblacion_municipio_df <- dplyr::rename(poblacion_municipio_df, `Cod municipio` = cod_mun)

  # Guardar CSV procesado (sobrescribir)
  tryCatch({
    write.csv(poblacion_municipio_df, file = ruta_poblacion_salida, row.names = FALSE, fileEncoding = "ISO-8859-1")
    if (!quiet) message("Fichero de población guardado en: ", ruta_poblacion_salida)

    # BORRAR los CSV origen descargados (solo los que figuran en poblacion_files)
    if (length(poblacion_files) > 0) {
      removed <- sapply(poblacion_files, function(f) {
        if (file.exists(f)) {
          tryCatch({ file.remove(f); TRUE }, error = function(e) { if (!quiet) message("No se pudo borrar ", f, ": ", e$message); FALSE })
        } else FALSE
      })
      if (!quiet) message("Eliminados ", sum(removed), " ficheros fuente de población.")
    }

  }, error = function(e) {
    warning("No se pudo escribir el fichero de población: ", e$message)
    ruta_poblacion_salida <<- NA_character_
  })

  if (!quiet) {
    message(sprintf("Población: ficheros procesados: %d; registros municipales: %d",
                    length(poblacion_files), nrow(poblacion_municipio_df)))
  }

  invisible(list(files = poblacion_files, poblacion = poblacion_municipio_df, ruta_salida = ruta_poblacion_salida))
}
