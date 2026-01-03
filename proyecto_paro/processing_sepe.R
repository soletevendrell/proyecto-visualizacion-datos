procesar_csv_sepe <- function(ruta_csv) {
  
  # --- 0. Leer CSV ----------------------------------------------------------
  # Se salta la primera fila que es un título descriptivo largo
  df <- read.csv(
    file = ruta_csv,
    sep = ";",
    header = TRUE,
    fileEncoding = "ISO-8859-1",
    stringsAsFactors = FALSE,
    skip = 1,
    check.names = FALSE
  )
  
  # Limpiar nombres de columnas (quitar espacios extra)
  names(df) <- trimws(names(df))
  
  # --- 1. RENOMBRADO INICIAL DE COLUMNAS BASADO EN POSICIÓN ---
  # 1=Mes, 3=Cod CA, 5=Cod Prov
  if (length(names(df)) >= 1) names(df)[1] <- "Cod mes"
  if (length(names(df)) >= 3) names(df)[3] <- "Cod CA"      
  if (length(names(df)) >= 4) names(df)[4] <- "Comunidad Aut"
  if (length(names(df)) >= 5) names(df)[5] <- "Cod provincia"
  # Asumimos que la col 6 es el Nombre de la Provincia (útil para no perderlo)
  if (length(names(df)) >= 6) names(df)[6] <- "Provincia"
  
  # --- 2. LIMPIEZA DE DATOS NUMÉRICOS ("<5" y comas) ---
  es_col_numerica <- sapply(df, function(x) {
    any(grepl("[0-9]", x, perl = TRUE), na.rm = TRUE)
  })
  
  # Identificamos columnas numéricas (excluyendo los códigos iniciales que pueden parecer números)
  # Normalmente desde la columna "Total" (aprox col 9) hacia adelante son los datos
  # Pero usaremos tu lógica de detección, forzando a que las de identificación NO se conviertan si no queremos
  cols_num <- names(df)[es_col_numerica]
  
  # Excluir explícitamente columnas de códigos que no queremos sumar numéricamente ahora
  cols_excluir <- c("Cod mes", "Cod CA", "Cod provincia", "anio")
  cols_num <- setdiff(cols_num, cols_excluir)
  
  # Primero quitamos <5
  df[cols_num] <- lapply(df[cols_num], function(col) {
    col_chr <- as.character(col)
    col_chr[col_chr == "<5"] <- "0"
    col_chr
  })
  
  # Convertimos a numérico gestionando decimales
  df[cols_num] <- lapply(df[cols_num], function(col) {
    col_corrected <- gsub(",", ".", as.character(col), fixed = TRUE)
    suppressWarnings(as.numeric(col_corrected))
  })
  
  # --- 3. CREACIÓN DE VARIABLES AUXILIARES (AÑO, FORMATOS) ---
  
  # Anio (primeros 4 caracteres de la columna 1 "Cod mes")
  if (!is.null(df[["Cod mes"]])) {
    df[["anio"]] <- substr(as.character(df[["Cod mes"]]), 1, 4)
  } else {
    stop("La columna 'Cod mes' no existe.")
  }
  
  # Función auxiliar para rellenar ceros (padding)
  pad_zeros <- function(x, width) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- ""
    x_chr <- trimws(x_chr)
    sapply(x_chr, function(s) {
      if (nchar(s) >= width || nchar(s) == 0) return(s)
      paste0(strrep("0", width - nchar(s)), s)
    }, USE.NAMES = FALSE)
  }
  
  # Normalizar Codigo Provincia y CA para que sean claves fiables
  if ("Cod CA" %in% names(df)) {
    df[["Cod CA"]] <- pad_zeros(df[["Cod CA"]], 2)
  }
  if ("Cod provincia" %in% names(df)) {
    df[["Cod provincia"]] <- pad_zeros(df[["Cod provincia"]], 2)
  }

  # --- 4. PREPARACIÓN PARA AGREGACIÓN (SUMA POR PROVINCIA Y MES) ---
  
  # Identificar columna Total (suele ser la 9) para separar descriptivos de datos
  idx_total <- 9
  if (idx_total > ncol(df)) {
      # Fallback: buscar columna llamada "Total"
      idx_total <- which(names(df) == "Total")
      if(length(idx_total) == 0) idx_total <- 9 # Asumimos 9 si falla todo
  }
  
  # Columnas que formarán la "Clave" de agrupación (Group By)
  # Queremos mantener: Año, Mes, Provincia (Cod y Nombre), CA (Cod y Nombre)
  cols_agrupacion <- c("anio", "Cod mes", "Cod provincia", "Provincia", "Cod CA", "Comunidad Aut")
  
  # Asegurarnos de que existen en el df antes de agrupar
  cols_agrupacion <- intersect(cols_agrupacion, names(df))
  
  # Columnas a sumar (las numéricas de datos)
  # Son las columnas numéricas detectadas antes, excluyendo las claves
  cols_datos <- setdiff(cols_num, cols_agrupacion)
  
  # Si por algún motivo metió el Cod Municipio en numéricos, lo quitamos
  cols_datos <- cols_datos[!grepl("Municipio", cols_datos, ignore.case = TRUE)]
  
  # --- 5. AGREGACIÓN ---
  
  # Mensaje de depuración
  message("Agrupando datos por: ", paste(cols_agrupacion, collapse=", "))
  
  # aggregate(datos ~ claves, FUN = sum)
  # Usamos la sintaxis de fórmula o lista. Usaremos lista para manejar nombres dinámicos
  df_agg <- aggregate(
    x = df[cols_datos],
    by = df[cols_agrupacion],
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  
  # --- 6. ORDENAR Y LIMPIEZA FINAL ---
  
  # Ordenar por Cod Provincia y luego por Cod Mes (cronológico)
  df_agg <- df_agg[order(df_agg[["Cod provincia"]], df_agg[["Cod mes"]]), ]
  
  # Reorganizar columnas para que quede bonito (Claves primero, luego datos)
  # Ponemos 'Cod mes' primero o 'anio' según preferencia. Dejaremos 'Cod mes' primero como original.
  col_order_pref <- c("Cod mes", "anio", "Cod CA", "Comunidad Aut", "Cod provincia", "Provincia")
  cols_finales <- c(
    intersect(col_order_pref, names(df_agg)),
    setdiff(names(df_agg), col_order_pref)
  )
  
  df_agg <- df_agg[, cols_finales]
  
  # --- 7. GUARDAR ---
  ruta_salida <- sub("\\.csv$", "_processed.csv", ruta_csv, ignore.case = TRUE)
  if (ruta_salida == ruta_csv) ruta_salida <- paste0(ruta_csv, "_processed.csv")

  write.csv(df_agg, file = ruta_salida, row.names = FALSE, fileEncoding = "ISO-8859-1")
  
  # Eliminar original si se desea (descomentar con precaución)
  if (file.exists(ruta_csv)) file.remove(ruta_csv)
  
  message("Procesado OK (Suma mensual por provincia): ", ruta_salida)
  invisible(ruta_salida)
}