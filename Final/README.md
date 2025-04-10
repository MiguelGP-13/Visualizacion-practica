# Instrucciones Detalladas para Añadir una Nueva Página y Visualización a la App

project/
├── app.R                                  # Archivo principal para ejecutar la app
├── modules/
│   ├── ui_module.R                        # Controlador principal de UI
│   ├── server_module.R                    # Controlador principal del servidor
│   ├── data_processing.R                  # Procesamiento y carga de datos
│   ├── utils.R                            # Funciones reutilizables
│   ├── visualizations/                    # Carpeta de visualizaciones (servidor)
│   │   ├── grafico_eventos.R              # Lógica del gráfico principal
│   │   ├── detalle_tramo.R                # Lógica para los detalles del tramo
│   │   ├── nueva_visualizacion.R          # Lógica para nueva visualización
│   ├── views/                             # Carpeta de vistas (UI)
│   │   ├── main.R                         # Página principal de navegación
│   │   ├── grafico_eventos_ui.R           # UI del gráfico de eventos
│   │   ├── detalle_tramo_ui.R             # UI de los detalles del tramo
│   │   ├── nueva_visualizacion_ui.R       # UI de la nueva visualización
├── data/                                  # Carpeta para los datos de entrada
│   ├── pacientes.xlsx                     # Datos de pacientes
│   ├── eventos.xlsx                       # Datos de eventos


Este documento explica los pasos necesarios para añadir **una nueva página** y **una nueva visualización** a tu aplicación modularizada. Seguiremos la estructura propuesta, donde las secciones de la aplicación están organizadas en módulos.

## **1. Crear el Archivo de la Nueva Página**
### Ubicación: `modules/views/`
1. **Archivo:** Crea un archivo `.R` para la nueva página.
   - Por ejemplo, si la página se llama "Visualización Adicional", crea un archivo `nueva_visualizacion_ui.R`.
   - Este archivo debe contener la definición de la UI específica para esta página.

2. **Contenido:** Define el diseño de la página. Aquí tienes un ejemplo base:
   ```R
   # modules/views/nueva_visualizacion_ui.R

   nueva_visualizacion_ui <- function() {
       fluidPage(
           titlePanel("Nueva Visualización"),
           sidebarLayout(
               sidebarPanel(
                   h4("Opciones de configuración:"),
                   checkboxInput("opcion1", "Opción 1", value = TRUE),
                   sliderInput("parametro", "Parámetro:", min = 1, max = 100, value = 50)
               ),
               mainPanel(
                   h3("Gráfica:"),
                   plotOutput("nueva_visualizacion_plot")
               )
           )
       )
   }
    ```
---
## **2. Crear el Archivo del Servidor para la Nueva Visualización**
### Ubicación: `modules/visualizations/`
1. **Archivo:** Crea un archivo `.R` para la lógica del servidor de la nueva visualización.
   - Por ejemplo, crea un archivo `nueva_visualizacion.R`.
   - Este archivo debe contener la definición de la UI específica para esta página.
2. **Contenido:** Define la función para manejar la lógica de esta visualización. Aquí tienes un ejemplo:
   ```R
   # modules/visualizations/nueva_visualizacion.R

    crear_nueva_visualizacion <- function(input, output, session, datos) {
        output$nueva_visualizacion_plot <- renderPlot({
            # Lógica para generar la visualización
            datos_filtrados <- datos$eventos_totales %>% 
                filter(Evento == "Sangrado" & Edad <= input$parametro)
            
            ggplot(datos_filtrados, aes(x = Edad, fill = Sexo)) +
                geom_histogram(color = "black", bins = 10) +
                labs(
                    title = "Nueva Visualización basada en Parámetros",
                    x = "Edad",
                    y = "Frecuencia"
                ) +
                theme_minimal()
        })
    }
    ```
---
## **3. Modificar el Controlador Principal del Servidor**
## Archivo: `modules/server_module.R`
1. Abre el archivo del servidor principal.

2. Añadir el módulo de la nueva visualización:
    - Añade `source("modules/visualizations/nueva_visualizacion.R")` para cargar la nueva visualización.

    - Llama a la función `crear_nueva_visualizacion()` dentro del servidor.
    ```R
    # Cargar nuevas visualizaciones
    source("modules/visualizations/nueva_visualizacion.R")

    server <- function(datos) {
        function(input, output, session) {
            # Variables compartidas
            ranges <- reactiveValues(x = NULL)
            click_info <- reactiveValues(tipo = NULL, tramo = NULL)
            
            # Visualizaciones existentes
            crear_grafico_eventos(input, output, session, datos, ranges, click_info)
            crear_detalle_tramo(input, output, session, datos, click_info)

            # Nueva visualización
            crear_nueva_visualizacion(input, output, session, datos)
        }
    }
    ```
---
## **4. Modificar el Controlador Principal de la UI**
## Archivo: `modules/ui_module.R`
1. Abre el archivo del módulo de UI.
2. Cargar el archivo de la nueva página: Añade `source("modules/views/nueva_visualizacion_ui.R")` al inicio del archivo.
3. Añadir la nueva página al `navbarPage`:
   - Añade una nueva pestaña que llame a la función de la UI de la nueva página.
    ```R
    # Cargar vistas
    source("modules/views/nueva_visualizacion_ui.R")

    ui <- function(pacientes) {
        navbarPage(
            title = "Aplicación Interactiva",
            tabPanel("Inicio", main_ui()),
            tabPanel("Gráfico Eventos", grafico_eventos_ui(pacientes)),
            tabPanel("Detalle Tramo", detalle_tramo_ui()),
            tabPanel("Nueva Visualización", nueva_visualizacion_ui())  # Nueva pestaña
        )
    }

    ```
---
## **5. Probar la Nueva Página y Visualización**

1. **Ejecutar la aplicación:**
   - Abre el archivo principal `app.R` y ejecuta la app con el siguiente comando en RStudio:
     ```R
     shiny::runApp()
     ```
   - Esto levantará la aplicación y deberías ver la nueva pestaña en el `navbarPage`.

2. **Probar la funcionalidad:**
   - Haz clic en la pestaña "Nueva Visualización" y verifica que el diseño y los controles de la página aparezcan correctamente.
   - Ajusta los controles para probar que interactúan correctamente con la lógica definida en el archivo del servidor (`nueva_visualizacion.R`).

3. **Depurar:**
   - Si encuentras errores, revisa estos puntos:
     - ¿Están correctamente cargados los archivos de UI y lógica del servidor?
     - ¿Las variables usadas en los controles tienen coincidencia en la lógica del servidor?
     - ¿Los datos están siendo referenciados correctamente en `datos`?

---

## **6. (Opcional) Añadir Funciones Comunes en `utils.R`**

Si tienes lógica o cálculos que puedan ser reutilizados en varias visualizaciones o páginas, es una buena idea agregar estas funciones al archivo `utils.R`.

### 1. **Abrir o Crear el Archivo `utils.R`:**
   - Si no existe, crea un archivo llamado `utils.R` en la carpeta `modules/`.

### 2. **Añadir Funciones Reutilizables:**
   - Por ejemplo, si necesitas filtrar datos con base en parámetros específicos, puedes crear una función como esta:
     ```R
     # modules/utils.R
     procesar_datos <- function(data, evento, limite_edad) {
         data %>%
             filter(Evento == evento & Edad <= limite_edad)
     }
     ```

### 3. **Usar las Funciones en tu Lógica:**
   - En cualquier archivo del servidor, llama a las funciones de `utils.R` para evitar duplicar lógica.
   - Asegúrate de cargar este archivo en los módulos donde lo necesites:
     ```R
     source("modules/utils.R")
     ```

---

## Resumen de los Archivos Modificados

1. **`modules/views/nueva_visualizacion_ui.R`**:
   - Contiene el diseño de la nueva página.
2. **`modules/visualizations/nueva_visualizacion.R`**:
   - Contiene la lógica del servidor para la nueva visualización.
3. **`modules/server_module.R`**:
   - Se cargó la nueva visualización y su lógica en el servidor.
4. **`modules/ui_module.R`**:
   - Se añadió la pestaña correspondiente al `navbarPage`.
5. **`modules/utils.R`** (opcional):
   - Contiene funciones reutilizables para cálculos y transformaciones compartidas.


