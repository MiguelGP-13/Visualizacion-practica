create_bubble_chart <- function(data) {
  ggplot(data, aes(x = Edad, y = Diabetes, size = Dislipdemia, color = Enfermedad_renal)) +
    geom_point(alpha = 0.6) +
    theme_minimal() +
    labs(title = "Diagnósticos en Bubble Chart", x = "Edad", y = "Diabetes", size = "Dislipidemia", color = "Enfermedad Renal")
}
