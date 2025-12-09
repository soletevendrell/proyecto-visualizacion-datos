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
      sep = ",",
      fileEncoding = "ISO-8859-1",
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

  df
}

# --------------------------------------------------------------------------------------
# ---------- 2. CORREGIR LOS NOMBRES DE LAS CCAA ----------
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
