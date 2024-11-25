library(readxl)

CAP <- read_excel("CAP.xlsx")

library(shiny)
library(ggplot2)
library(dplyr)
library(openxlsx) # Para guardar datos en Excel


# Diccionario para mapear nombres visibles a nombres internos
diccionario_nombres <- list(
  "Edad" = "edad",
  "Puntaje de Conocimientos" = "Puntaje_C",
  "Puntaje de Actitudes" = "Puntaje_A",
  "Puntaje de Prácticas" = "Puntaje_P"
)

# Etiquetas personalizadas para las variables
etiquetas_eje_x <- diccionario_nombres

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Visualización Dinámica: Histogramas, Boxplots y Resúmenes"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Selecciona una variable:",
        choices = names(diccionario_nombres), # Mostrar nombres descriptivos
        selected = "Edad"
      ),
      sliderInput(
        inputId = "bins",
        label = "Número de bins (para el histograma):",
        min = 5,
        max = 50,
        value = 10
      ),
      checkboxGroupInput(
        inputId = "graficos",
        label = "Selecciona los gráficos a mostrar:",
        choices = c("Histograma", "Boxplot", "Resumen"),
        selected = c("Histograma") # Por defecto solo el histograma
      ),
      downloadButton(outputId = "descargar_datos", label = "Descargar Datos"),
      downloadButton(outputId = "descargar_histograma", label = "Descargar Histograma"),
      downloadButton(outputId = "descargar_boxplot", label = "Descargar Boxplot")
    ),
    
    mainPanel(
      plotOutput(outputId = "histograma"),
      plotOutput(outputId = "boxplot", height = "100px"), # Reducir altura del boxplot
      tableOutput(outputId = "tabla_resumen") # Espacio para la tabla
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  # Renderizar el histograma
  output$histograma <- renderPlot({
    if ("Histograma" %in% input$graficos) {
      variable_real <- diccionario_nombres[[input$variable]]
      
      ggplot(data = CAP, aes_string(x = variable_real)) +
        geom_histogram(
          bins = input$bins,
          fill = "skyblue",
          color = "black"
        ) +
        labs(
          title = paste("Histograma de", input$variable),
          x = input$variable,
          y = "Frecuencia"
        ) +
        theme_minimal()
    }
  })
  
  # Renderizar el boxplot horizontal
  output$boxplot <- renderPlot({
    if ("Boxplot" %in% input$graficos) {
      variable_real <- diccionario_nombres[[input$variable]]
      
      ggplot(data = CAP, aes_string(x = variable_real, y = "1")) +
        geom_boxplot(
          fill = "lightgreen",
          color = "black",
          outlier.colour = "red", # Color para los valores atípicos
          outlier.size = 2, # Tamaño de los valores atípicos
          width = 0.5 # Controlar el ancho del boxplot
        ) +
        labs(
          title = paste("Boxplot de", input$variable),
          x = input$variable,
          y = ""
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank() # Eliminar líneas de fondo en y
        )
    }
  })
  
  # Generar el resumen estadístico de la variable seleccionada
  output$tabla_resumen <- renderTable({
    if ("Resumen" %in% input$graficos) {
      variable_real <- diccionario_nombres[[input$variable]]
      datos <- CAP[[variable_real]]
      
      resumen <- data.frame(
        `n` = length(datos),
        Promedio = round(mean(datos, na.rm = TRUE), 2),
        `DE` = round(sd(datos, na.rm = TRUE), 2),
        Mínimo = round(min(datos, na.rm = TRUE), 2),
        `Q1` = round(quantile(datos, 0.25, na.rm = TRUE), 2),
        Mediana = round(median(datos, na.rm = TRUE), 2),
        `Q3` = round(quantile(datos, 0.75, na.rm = TRUE), 2),
        Máximo = round(max(datos, na.rm = TRUE), 2),
        `IQR` = round(IQR(datos, na.rm = TRUE), 2)
      )
      return(resumen)
    }
  }, rownames = FALSE)
  
  # Descargar los datos seleccionados en formato Excel
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste0("resumen_", input$variable, ".xlsx")
    },
    content = function(file) {
      variable_real <- diccionario_nombres[[input$variable]]#De la 136 a 150 sacar para agregar un reactive event
      datos <- CAP[[variable_real]]
      
      resumen <- data.frame(
        `n` = length(datos),
        Promedio = round(mean(datos, na.rm = TRUE), 2),
        `DE` = round(sd(datos, na.rm = TRUE), 2),
        Mínimo = round(min(datos, na.rm = TRUE), 2),
        `Q1` = round(quantile(datos, 0.25, na.rm = TRUE), 2),
        Mediana = round(median(datos, na.rm = TRUE), 2),
        `Q3` = round(quantile(datos, 0.75, na.rm = TRUE), 2),
        Máximo = round(max(datos, na.rm = TRUE), 2),
        `IQR` = round(IQR(datos, na.rm = TRUE), 2)
      )
      
      write.xlsx(resumen, file)
    }
  )
  
  # Descargar el histograma en formato PNG
  output$descargar_histograma <- downloadHandler(
    filename = function() {
      paste0("histograma_", input$variable, ".png")
    },
    content = function(file) {
      variable_real <- diccionario_nombres[[input$variable]]
      
      g <- ggplot(data = CAP, aes_string(x = variable_real)) +
        geom_histogram(
          bins = input$bins,
          fill = "skyblue",
          color = "black"
        ) +
        labs(
          title = paste("Histograma de", input$variable),
          x = input$variable,
          y = "Frecuencia"
        ) +
        theme_minimal()
      
      ggsave(file, plot = g, device = "png", width = 8, height = 6)
    }
  )
  
  # Descargar el boxplot en formato PNG
  output$descargar_boxplot <- downloadHandler(
    filename = function() {
      paste0("boxplot_", input$variable, ".png")
    },
    content = function(file) {
      variable_real <- diccionario_nombres[[input$variable]]
      
      g <- ggplot(data = CAP, aes_string(x = variable_real, y = "1")) +
        geom_boxplot(
          fill = "lightgreen",
          color = "black",
          outlier.colour = "red",
          outlier.size = 2,
          width = 0.5
        ) +
        labs(
          title = paste("Boxplot de", input$variable),
          x = input$variable,
          y = ""
        ) +
        theme_minimal()
      
      ggsave(file, plot = g, device = "png", width = 8, height = 6)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)