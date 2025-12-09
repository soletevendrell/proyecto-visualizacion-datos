procesar_csv_sepe <- function(ruta_csv) {
  # --- 0. Leer CSV ----------------------------------------------------------
  df <- read.csv(
    file = ruta_csv,
    sep = ";",
    header = TRUE,
    fileEncoding = "ISO-8859-1",
    stringsAsFactors = FALSE,
    skip = 1,
    check.names = FALSE
  )
  
  # Limpiar nombres de columnas
  names(df) <- trimws(names(df))
  
  # Renombrar columnas principales si existen
  if (length(names(df)) >= 1) names(df)[1] <- "Cod mes"
  if (length(names(df)) >= 3) names(df)[3] <- "Cod comunidad"
  if (length(names(df)) >= 4) names(df)[4] <- "Comunidad Aut"
  if (length(names(df)) >= 5) names(df)[5] <- "Cod provincia"
  
  primera_col_name <- names(df)[1]

  # --- Sustituir "<5" por 0 en columnas numéricas (detectadas por contenido) ---
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
  
  
  # --- 1. Crear anio ---------------------------------------------------
  if (!is.null(df[[1]])) {
    df[["anio"]] <- substr(as.character(df[[1]]), 1, 4)
  } else {
    stop("La primera columna no existe o está vacía.")
  }
  
  # --- 2. Normalizar Codigo Municipio -> Cod municipio ---------------------
  idx_mun <- which(names(df) == "Codigo Municipio")
  if (length(idx_mun) != 1L) {
    stop("No se encontró la columna 'Codigo Municipio' en el CSV.")
  }
  pad5 <- function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- ""
    x_chr <- trimws(x_chr)
    sapply(x_chr, function(s) {
      if (nchar(s) >= 5) return(s)
      paste0(strrep("0", 5 - nchar(s)), s)
    }, USE.NAMES = FALSE)
  }
  df[[idx_mun]] <- pad5(df[[idx_mun]])
  names(df)[idx_mun] <- "Cod municipio"
  
  # --- 3. Eliminar columna 'mes' si existe ---------------------------------
  idx_mes <- which(names(df) == "mes")
  if (length(idx_mes) == 1L) df <- df[, -idx_mes, drop = FALSE]
  
  # --- 4. Reordenar: dejar anio como segunda columna --------------------
  idx_cod_anio <- which(names(df) == "anio")
  if (length(idx_cod_anio) == 1L) {
    otras <- setdiff(seq_along(df), c(1, idx_cod_anio))
    df <- df[, c(1, idx_cod_anio, otras), drop = FALSE]
  }
  
  # --- 5. Determinar la columna "TOTAL" por índice fijo (9) ----------------
  idx_total <- 9
  if (idx_total > ncol(df)) stop("El data.frame no tiene una columna 9. No se puede usar como columna TOTAL.")
  nombre_total <- names(df)[idx_total]
  
  # Columna 9 - nombre y vector numérico (por si usa comas decimales)
  nombre_col9 <- names(df)[9]
  col9_raw <- df[[9]]
  col9_num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col9_raw))))
  
  # --- 6. Preparar claves y tmp_ext para min/max ---------------------------
  key_cols <- c("anio", "Cod municipio")
  if (!all(key_cols %in% names(df))) stop("Faltan claves de agrupación en el data.frame.")
  
  tmp_ext <- data.frame(
    df[key_cols],
    col9 = col9_num,
    Cod_mes = as.character(df[["Cod mes"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Asegurar tipos de ordenación coherentes
  tmp_ext[["anio"]] <- as.character(tmp_ext[["anio"]])
  tmp_ext[["Cod municipio"]] <- as.character(tmp_ext[["Cod municipio"]])
  
  # Orden y extracción de mínimos por grupo
  ord_min <- with(tmp_ext, order(tmp_ext[["anio"]], tmp_ext[["Cod municipio"]], col9, na.last = TRUE))
  tmp_min <- tmp_ext[ord_min, , drop = FALSE]
  min_df <- tmp_min[!duplicated(tmp_min[c("anio", "Cod municipio")]), , drop = FALSE]
  
  # Orden y extracción de máximos por grupo
  ord_max <- with(tmp_ext, order(tmp_ext[["anio"]], tmp_ext[["Cod municipio"]], -col9, na.last = TRUE))
  tmp_max <- tmp_ext[ord_max, , drop = FALSE]
  max_df <- tmp_max[!duplicated(tmp_max[c("anio", "Cod municipio")]), , drop = FALSE]
  
  # Renombrar columnas de min/max y las columnas de mes como solicitado
  nombre_col_min <- paste0(nombre_col9, "_min")
  nombre_col_max <- paste0(nombre_col9, "_max")
  
  names(min_df)[names(min_df) == "col9"]    <- nombre_col_min
  names(min_df)[names(min_df) == "Cod_mes"] <- "Cod min_mes"
  
  names(max_df)[names(max_df) == "col9"]    <- nombre_col_max
  names(max_df)[names(max_df) == "Cod_mes"] <- "Cod max_mes"
  
  # --- 7. AGRUPACIÓN por anio y Cod municipio --------------------------
  df <- df[order(as.character(df[["anio"]]), as.character(df[["Cod municipio"]])), , drop = FALSE]
  
  # Columnas a la izquierda/derecha de la columna total (índice 9)
  if (idx_total > 1) left_cols <- names(df)[1:(idx_total - 1)] else left_cols <- character(0)
  right_cols <- names(df)[idx_total:ncol(df)]
  
  left_non_key_cols <- setdiff(left_cols, key_cols)
  
  # 7.1 Agregación de columnas no-numéricas (primera fila del grupo) o generar df con claves si no hay left cols
  if (length(left_non_key_cols) > 0) {
    df_left_agg <- aggregate(df[left_non_key_cols], by = df[key_cols], FUN = function(x) x[1])
  } else {
    df_left_agg <- unique(df[key_cols])
    rownames(df_left_agg) <- NULL
  }
  
  # 7.2 Agregación de columnas numéricas (media, 2 decimales)
  # --- CORRECCIÓN: seleccionar columnas (no filas) con df[, right_cols, drop = FALSE]
  if (length(right_cols) == 0L) stop("No se han detectado columnas a la derecha de la columna TOTAL (índice 9).")
  df_right <- df[, right_cols, drop = FALSE]
  df_right[] <- lapply(df_right, function(x) {
    x_char <- as.character(x)
    # Eliminar separador de miles si existe y normalizar coma decimal
    x_char <- gsub("\\.", "", x_char) # quita puntos de miles (si corresponde)
    x_char <- gsub(",", ".", x_char)
    suppressWarnings(as.numeric(x_char))
  })
  
  df_right_agg <- aggregate(df_right, by = df[key_cols], FUN = function(x) round(mean(x, na.rm = TRUE), 2))
  
  # 7.3 Merge de ambas tablas
  df_agg <- merge(df_left_agg, df_right_agg, by = key_cols, all = TRUE)
  
  # 7.4 Añadir min/max (usando los nuevos nombres de columna de mes)
  last2 <- function(x) substr(trimws(x), pmax(nchar(trimws(x)) - 1, 1), nchar(trimws(x)))

  if ("Cod min_mes" %in% names(min_df))  min_df[["Cod min_mes"]] <- last2(min_df[["Cod min_mes"]])
  if ("Cod max_mes" %in% names(max_df))  max_df[["Cod max_mes"]] <- last2(max_df[["Cod max_mes"]])

  # --- Merge como antes ---
  if (nrow(min_df) > 0) {
    df_agg <- merge(df_agg, min_df[, c(key_cols, nombre_col_min, "Cod min_mes")], by = key_cols, all.x = TRUE)
  } else {
    df_agg[[nombre_col_min]] <- NA
    df_agg[["Cod min_mes"]] <- NA
  }

  if (nrow(max_df) > 0) {
    df_agg <- merge(df_agg, max_df[, c(key_cols, nombre_col_max, "Cod max_mes")], by = key_cols, all.x = TRUE)
  } else {
    df_agg[[nombre_col_max]] <- NA
    df_agg[["Cod max_mes"]] <- NA
  }

  
  # --- 8. Reordenar columnas para salida ----------------------------------
  cols_current <- names(df_agg)
  col_order <- c(
    intersect(primera_col_name, cols_current),
    "anio",
    "Cod municipio",
    setdiff(cols_current, c(primera_col_name, "anio", "Cod municipio"))
  )
  col_order <- unique(col_order[col_order %in% cols_current])
  df_agg <- df_agg[, col_order, drop = FALSE]
  
  # --- Eliminar la primera columna del resultado si existe -----------
  if (ncol(df_agg) >= 1) {
    df_agg <- df_agg[, -1, drop = FALSE]   # Eliminar la primera columna
  } else {
    warning("El data.frame resultado no tiene columnas; no se puede eliminar la primera columna.")
  }

  
  # --- Reordenar filas por Cod municipio y luego por anio -------
  if (all(c("Cod municipio", "anio") %in% names(df_agg))) {
    # forzamos a character para evitar problemas con factores/números
    df_agg[["Cod municipio"]] <- as.character(df_agg[["Cod municipio"]])
    df_agg[["anio"]] <- as.character(df_agg[["anio"]])
    df_agg <- df_agg[order(df_agg[["Cod municipio"]], df_agg[["anio"]]), , drop = FALSE]
  } else {
    warning("No se pudo reordenar por 'Cod municipio' y 'anio' porque no existen ambas columnas en el resultado.")
  }
  
  # --- 9. Escribir CSV de salida ------------------------------------------
  if (grepl("\\.csv$", ruta_csv, ignore.case = TRUE)) {
    ruta_salida <- sub("\\.csv$", "_processed.csv", ruta_csv, ignore.case = TRUE)
  } else {
    ruta_salida <- paste0(ruta_csv, "_processed.csv")
  }
  
  write.csv(
    df_agg,
    file = ruta_salida,
    row.names = FALSE,
    fileEncoding = "ISO-8859-1"
  )
  
  if (file.exists(ruta_csv)) file.remove(ruta_csv)

  message("Fichero procesado escrito en: ", ruta_salida)
  invisible(ruta_salida)
}
