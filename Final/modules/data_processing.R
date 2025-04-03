# modules/data_processing.R
library(dplyr)
library(readxl)

cargar_datos <- function() {
  # Cargar datos desde los archivos de Excel
  pacientes <- read_excel("data/pacientes.xlsx", sheet = 1)
  eventos_sangrado <- read_excel("data/eventos.xlsx", sheet = "sangrado")
  eventos_tromboticos <- read_excel("data/eventos.xlsx", sheet = "trombotico")
  
  # Agregar edad y sexo a las tablas de eventos
  eventos_sangrado <- eventos_sangrado %>%
    inner_join(pacientes, by = "Paciente") %>%
    select(Paciente, Edad, Sexo, `Tipo de sangrado`, 
           `Gravedad de la hemorragia (TIMI)`, `Gravedad de la hemorragia (GUSTO)`, 
           `Gravedad de la hemorragia (BARC)`, `Procedimientos terapéuticos`, 
           `Descenso de hemoglobina`, `¿El paciente ha subido una trasfusión?`)
  
  eventos_tromboticos <- eventos_tromboticos %>%
    inner_join(pacientes, by = "Paciente") %>%
    select(Paciente, Edad, Sexo, `¿El paciente ha sufrido un evento trombótico previo a la inclusión?`, 
           `Tipo de evento trombótico`, `Tipo de invervención`, `TYPE_THROMBOTIC_PRE`, 
           `Numero  anticoagulantes`, `Numero  antiagregantes`, `Otro medicamentos`)
  
  # Unir ambas tablas en una sola
  eventos_totales <- bind_rows(
    eventos_sangrado %>% mutate(Evento = "Sangrado"),
    eventos_tromboticos %>% mutate(Evento = "Trombotico")
  )
  
  # Devolver los datos preparados
  list(
    pacientes = pacientes,
    eventos_sangrado = eventos_sangrado,
    eventos_tromboticos = eventos_tromboticos,
    eventos_totales = eventos_totales
  )
}
