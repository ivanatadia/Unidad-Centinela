library(data.table)
library(dplyr)
library(tidyr)


crear_dicotomicas_rapida2 <- function(data, col_signos, col_comorbilidades, col_determinacion, col_resultado) {
  # Convertir el dataset a data.table para mejorar el rendimiento
  setDT(data)
  
  # Identificar los valores únicos en cada columna de comorbilidades y signos/síntomas
  unique_comorbilidades <- unique(na.omit(data[[col_comorbilidades]]))
  unique_signos <- unique(na.omit(data[[col_signos]]))
  
  # Crear columnas dicotómicas para cada comorbilidad
  for (comorbilidad in unique_comorbilidades) {
    nueva_col <- paste0("COMORB_", comorbilidad)
    data[, (nueva_col) := as.integer(get(col_comorbilidades) == comorbilidad)]
  }
  
  # Crear columnas dicotómicas para cada signo/síntoma
  for (signo in unique_signos) {
    nueva_col <- paste0("SINTOMA_", signo)
    data[, (nueva_col) := as.integer(get(col_signos) == signo)]
  }
  
  # Crear columnas para determinación y resultados
  unique_determinaciones <- unique(na.omit(data[[col_determinacion]]))
  
  for (determinacion in unique_determinaciones) {
    nueva_col <- paste0("DETERMINACION_", determinacion)
    data[, (nueva_col) := ifelse(get(col_determinacion) == determinacion, get(col_resultado), NA_character_)]
  }
  
  # Verificar si 'IDEVENTOCASO' existe para agrupar y consolidar duplicados
  if ("IDEVENTOCASO" %in% names(data)) {
    # Obtener nombres de las columnas dicotómicas y de determinación
    dicotomic_cols <- grep("COMORB_|SINTOMA_", names(data), value = TRUE)
    determinacion_cols <- grep("DETERMINACION_", names(data), value = TRUE)
    
    # Consolidar dicotómicas sumando valores
    data <- data[, c(
      lapply(.SD[, ..dicotomic_cols, with = FALSE], sum, na.rm = TRUE),
      lapply(.SD[, ..determinacion_cols, with = FALSE], function(x) paste(na.omit(unique(x)), collapse = ";")),
      lapply(.SD[, !names(.SD) %in% c(dicotomic_cols, determinacion_cols), with = FALSE], function(x) x[1])
    ), by = IDEVENTOCASO]
    
    # Reemplazar valores mayores a 1 por 1 en las columnas dicotómicas
    data[, (dicotomic_cols) := lapply(.SD, function(x) pmin(x, 1, na.rm = TRUE)), .SDcols = dicotomic_cols]
  } else {
    warning("La columna 'IDEVENTOCASO' no está en los datos. Los duplicados no se consolidarán.")
  }
  
  return(data)
}



####Analizo los sintomas###

crear_tabla_signos_sintomas <- function(data, grupo_edad_var) {
  # Convertir a data.table
  setDT(data)
  
  # Seleccionar las columnas que comienzan con "SINTOMA_"
  sintoma_cols <- grep("^SINTOMA_", names(data), value = TRUE)
  
  # Asegurarse de que la columna de grupo de edad esté presente
  if (!grupo_edad_var %in% names(data)) {
    stop("La columna especificada para grupo de edad no existe en el dataset.")
  }
  
  # Crear la tabla resumen
  tabla_resumen <- data[, lapply(.SD, sum, na.rm = TRUE), 
                        by = c(grupo_edad_var), 
                        .SDcols = sintoma_cols]
  
  # Renombrar las columnas para mayor claridad
  setnames(tabla_resumen, old = sintoma_cols, new = sub("^SINTOMA_", "", sintoma_cols))
  
  # Convertir de formato largo a ancho (signos y síntomas como filas y grupo etario como columnas)
  tabla_final <- melt(tabla_resumen, id.vars = grupo_edad_var, 
                      variable.name = "Signo_Sintoma", value.name = "Conteo")
  
  tabla_final <- dcast(tabla_final, Signo_Sintoma ~ get(grupo_edad_var), 
                       value.var = "Conteo", fun.aggregate = sum, fill = 0)
  
  # Agregar una columna de totales por signo/síntoma
  tabla_final[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = !"Signo_Sintoma"]
  
  return(tabla_final)
}


###Analizo comorbilidades###

crear_tabla_comorbilidades <- function(data, grupo_edad_var) {
  # Convertir a data.table
  setDT(data)
  
  # Seleccionar las columnas que comienzan con "COMORB_"
  comorb_cols <- grep("^COMORB_", names(data), value = TRUE)
  
  # Asegurarse de que la columna de grupo de edad esté presente
  if (!grupo_edad_var %in% names(data)) {
    stop("La columna especificada para grupo de edad no existe en el dataset.")
  }
  
  # Crear la tabla resumen
  tabla_resumen <- data[, lapply(.SD, sum, na.rm = TRUE), 
                        by = c(grupo_edad_var), 
                        .SDcols = comorb_cols]
  
  # Renombrar las columnas para mayor claridad
  setnames(tabla_resumen, old = comorb_cols, new = sub("^COMORB_", "", comorb_cols))
  
  # Convertir de formato largo a ancho (comorbilidades como filas y grupo etario como columnas)
  tabla_final <- melt(tabla_resumen, id.vars = grupo_edad_var, 
                      variable.name = "Comorbilidad", value.name = "Conteo")
  
  tabla_final <- dcast(tabla_final, Comorbilidad ~ get(grupo_edad_var), 
                       value.var = "Conteo", fun.aggregate = sum, fill = 0)
  
  # Agregar una columna de totales por comorbilidad
  tabla_final[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = !"Comorbilidad"]
  
  return(tabla_final)
}

####ANALIZO DETERMINACIONES(ivi)###

crear_tabla_determinaciones<- function(data, grupo_edad_var) {
  # Convertir a data.table
  setDT(data)
  
  # Seleccionar las columnas que comienzan con "SINTOMA_"
  determ_cols <- grep("^DETERMINACION_", names(data), value = TRUE)
  
  # Asegurarse de que la columna de grupo de edad esté presente
  if (!grupo_edad_var %in% names(data)) {
    stop("La columna especificada para grupo de edad no existe en el dataset.")
  }
  
  # Crear la tabla resumen
  tabla_resumen <- data[, lapply(.SD, sum, na.rm = TRUE), 
                        by = c(grupo_edad_var), 
                        .SDcols = determ_cols]
  
  # Renombrar las columnas para mayor claridad
  setnames(tabla_resumen, old = determ_cols, new = sub("DETERMINACION_", "", determ_cols))
  
  # Convertir de formato largo a ancho (signos y síntomas como filas y grupo etario como columnas)
  tabla_final <- melt(tabla_resumen, id.vars = grupo_edad_var, 
                      variable.name = "Determinaciones", value.name = "Conteo")
  
  tabla_final <- dcast(tabla_final, Determinaciones ~ get(grupo_edad_var), 
                       value.var = "Conteo", fun.aggregate = sum, fill = 0)
  
  # Agregar una columna de totales por signo/síntoma
  tabla_final[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = !"Determinaciones"]
  
  return(tabla_final)
}








