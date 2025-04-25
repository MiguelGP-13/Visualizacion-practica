# modules/data_processing.R

cargar_datos <- function() {
  # Cargar datos desde los archivos de Excel
  pacientes <- read_excel("../data/pacientes.xlsx", sheet = 1)
  eventos_sangrado <- read.csv("../data/sangrado_limpio.csv", header = TRUE, sep = ",", check.names = FALSE)
  eventos_tromboticos <- read.csv("../data/trombotico_limpio.csv", header = TRUE, sep = ",", check.names = FALSE)
  cardiovascular <- read.csv('../data/historia_cardiovascular_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)
  habitos <- read_excel("../data/pacientes.xlsx", sheet = 6)
  factores_riesgo <- read.csv('../data/factores_riesgo_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)

  
  # Agregar edad y sexo a las tablas de eventos
  eventos_sangrado <- eventos_sangrado %>%
    inner_join(pacientes, by = "Paciente") %>%
    select(Paciente, Edad, Sexo, `Tipo de sangrado`, 
           `Gravedad de la hemorragia (TIMI)`, `Gravedad de la hemorragia (GUSTO)`, 
           `Gravedad de la hemorragia (BARC)`, `Procedimientos terapéuticos`, 
           `Descenso de hemoglobina`, `¿El paciente ha subido una trasfusión?`,
           `ANTICOAGULANT_STRING`, `ANTIPLATELET_STRING`, `OTHER_STRING`)
  
  eventos_tromboticos <- eventos_tromboticos %>%
    inner_join(pacientes, by = "Paciente") %>%
    select(Paciente, Edad, Sexo, 
           `Tipo de evento trombótico`, `Tipo de invervención`, `TYPE_THROMBOTIC_PRE`, 
           `ANTICOAGULANT_STRING`, `ANTIPLATELET_STRING`, `OTHER_STRING`)
  
  # Unir ambas tablas en una sola
  eventos_totales <- bind_rows(
    eventos_sangrado %>% mutate(Evento = "Sangrado"),
    eventos_tromboticos %>% mutate(Evento = "Trombotico")
  )
  
  # Unir y procesar datos
  full_pacientes <- pacientes %>%
    inner_join(habitos, by = c("Paciente" = "Codígo")) %>%
    inner_join(factores_riesgo, by = c("Paciente" = "Codígo")) %>%
    inner_join(cardiovascular, by = "Paciente") %>%
    mutate(
      Tabaquismo_num = case_when(
        Tabaquismo == "NEVER_SMOKER" ~ 0,
        Tabaquismo == "EX_SMOKER" ~ 1,
        Tabaquismo == "SMOKER" ~ 2
      ),
      Consumo_diario_de_alcohol_num = case_when(
        `Consumo diario de alcohol` == "NO" ~ 0,
        `Consumo diario de alcohol` == "YES" ~ 1
      ),
      Minutos_intervalo = cut(`Minutos semanales de actividad aeróbica`, 
                              breaks = seq(0, max(`Minutos semanales de actividad aeróbica`, na.rm = TRUE), by = 60),
                              include.lowest = TRUE, right = FALSE, labels = FALSE)
    )


  # Devolver los datos preparados
  list(
    pacientes = pacientes,
    eventos_sangrado = eventos_sangrado,
    eventos_tromboticos = eventos_tromboticos,
    eventos_totales = eventos_totales,
    full_pacientes = full_pacientes
  )
}
