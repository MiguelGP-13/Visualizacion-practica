create_sankey <- function(data) {
  ggplot(data, aes(axis1 = Guía_clínica, axis2 = Tipo_de_anticoagulante, fill = Tipo_de_anticoagulante)) +
    geom_alluvium(aes(weight = Numero_de_meses), alpha = 0.7) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    labs(title = "Diagrama Sankey de Terapias", x = "Etapas", y = "Pacientes")
}
