# Crear tabla dinámica o detalle individual
create_individual_profile <- function(data, paciente_id) {
  profile_data <- data[data$Paciente == paciente_id, ]
  profile_data
}
