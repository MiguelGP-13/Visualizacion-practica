# modules/utils.R
# (Ejemplo de una función que puedes usar)
procesar_datos <- function(data, filter_column, filter_value) {
  data %>% filter(!!sym(filter_column) %in% filter_value)
}
