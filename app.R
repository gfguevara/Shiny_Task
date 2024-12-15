# Instala las librerías necesarias si no las tienes
# install.packages("shiny")
# install.packages("ggplot2")
#install.packages("shinythemes")
#install.packages("kableExtra")
#install.packages("shinythemes")
#install.packages("ggsave")


library(shiny)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(DT)
library(readxl)
library(viridis)
library(readxl)

CAP <- read_excel("CAP.xlsx")

#View(CAP)



# ui
# Interfaz de usuario (UI)
# Cargar las librerías necesarias
library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(scales)

# Interfaz de usuario (UI)
ui <- navbarPage("Análisis CAP",
                 
                 # SECCIÓN 1: Análisis Univariado
                 tabPanel("Análisis Univariado",
                          tabsetPanel(
                            tabPanel("Variables Sociodemográficas",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var_soc", "Seleccione una variable:", 
                                                     choices = c("Mes de la encuesta" = "mes", 
                                                                 "Participación previa" = "part_prev", 
                                                                 "Municipio de residencia" = "muni", 
                                                                 "Área de residencia" = "area", 
                                                                 "Edad en años" = "edad", 
                                                                 "Mayor de 65 años" = "edad_dicot", 
                                                                 "Grupos etarios" = "etario", 
                                                                 "Sexo" = "genero", 
                                                                 "Nivel educativo" = "nivel_edu", 
                                                                 "Hogar unipersonal" = "vive_solo", 
                                                                 "Tamaño del núcleo familiar" = "nucleo", 
                                                                 "Situación de empleo" = "empleo_si", 
                                                                 "Ocupación actual" = "ocupacion", 
                                                                 "Ingreso promedio mensual" = "ingreso", 
                                                                 "Estudios superiores" = "edu_sup")),
                                         downloadButton("downloadPlot_soc", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("plot_soc")
                                       )
                                     )
                            ),
                            tabPanel("Variables Clínicas",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var_clin", "Seleccione una variable clínica:", 
                                                     choices = c("Enfermedad crónica" = "enfcro", 
                                                                 "Antecedente de COVID-19" = "prev_per_dicot", 
                                                                 "Dosis de vacuna" = "dosis", 
                                                                 "Vacunas incompletas" = "vacunas_incompletas", 
                                                                 "Mortalidad" = "mortalidad")),
                                         downloadButton("downloadPlot_clin", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("plot_clin")
                                       )
                                     )
                            ),
                            tabPanel("Variables de Exposición",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var_exp", "Seleccione una variable de exposición:", 
                                                     choices = c("Exposición a mensajes en los medios de comunicación" = "exposicion_dicot")),
                                         downloadButton("downloadPlot_exp", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("plot_exp")
                                       )
                                     )
                            ),
                            tabPanel("Puntajes",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var_score", "Seleccione un puntaje o variable numérica:", 
                                                     choices = c("Puntaje de Conocimientos" = "Puntaje_C", 
                                                                 "Puntaje de Actitudes" = "Puntaje_A", 
                                                                 "Puntaje de Prácticas" = "Puntaje_P")),
                                         sliderInput("bins", "Número de bins:", min = 5, max = 50, value = 20),
                                         checkboxInput("normal_curve", "Superponer curva normal", value = FALSE),
                                         downloadButton("downloadPlot_score", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("plot_score"),
                                         textOutput("summary_stats")
                                       )
                                     )
                            )
                            
                          )
                 ),
                 
                 # SECCIÓN 2: Análisis Bivariado
                 tabPanel("Análisis Bivariado",
                          tabsetPanel(
                            tabPanel("Variables Dicotómicas",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var_dicot", "Seleccione una variable dicotómica:", 
                                                     choices = c("Mes de la encuesta" = "mes", 
                                                                 "Participación previa" = "part_prev", 
                                                                 "Mayor de 65 años" = "edad_dicot", 
                                                                 "Estudios superiores" = "edu_sup", 
                                                                 "Enfermedad crónica" = "enfcro", 
                                                                 "Antecedente de COVID-19" = "prev_per_dicot", 
                                                                 "Exposición a mensajes educativos" = "exposicion_dicot")),
                                         selectInput("var_num", "Seleccione una variable numérica:", 
                                                     choices = c("Puntaje de Conocimientos" = "Puntaje_C", 
                                                                 "Puntaje de Actitudes" = "Puntaje_A", 
                                                                 "Puntaje de Prácticas" = "Puntaje_P")),
                                         checkboxInput("eliminar_outliers", "Eliminar valores atípicos", value = FALSE),
                                         checkboxInput("mostrar_IC", "Mostrar Intervalo de Confianza", value = FALSE),
                                         checkboxInput("mostrar_prueba", "Mostrar Prueba de Significancia", value = FALSE),
                                         downloadButton("downloadPlot_biv", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("boxplot_biv"),
                                         tableOutput("tabla_resumen"),
                                         conditionalPanel(
                                           condition = "input.mostrar_IC || input.mostrar_prueba",
                                           tableOutput("tabla_inferencia")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Variables Nominales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # Opciones para análisis de variables nominales
                                       ),
                                       mainPanel(
                                         # Output para gráficos bivariados de variables nominales
                                       )
                                     )
                            ),
                            tabPanel("Variables Numéricas",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # Opciones para análisis de variables numéricas
                                       ),
                                       mainPanel(
                                         # Output para gráficos bivariados de variables numéricas
                                       )
                                     )
                            )
                          )
                 ),
                 
                 # SECCIÓN 3: Modelos de Regresión
                 tabPanel("Modelos de Regresión",
                          tabsetPanel(
                            tabPanel("Regresión Lineal Simple",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # Opciones para modelo de regresión lineal simple
                                       ),
                                       mainPanel(
                                         # Output para regresión lineal simple
                                       )
                                     )
                            ),
                            tabPanel("Regresión Lineal Múltiple",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # Opciones para modelo de regresión lineal múltiple
                                       ),
                                       mainPanel(
                                         # Output para regresión lineal múltiple
                                       )
                                     )
                            ),
                            tabPanel("Regresión Logística",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # Opciones para modelo de regresión logística
                                       ),
                                       mainPanel(
                                         # Output para regresión logística
                                       )
                                     )
                            )
                          )
                 ),
                 
                 # SECCIÓN 4: Análisis de Antes y Después
                 tabPanel("Análisis de Antes y Después",
                          sidebarLayout(
                            sidebarPanel(
                              # Opciones para comparación antes y después
                            ),
                            mainPanel(
                              # Output para gráfico de análisis antes y después
                            )
                          )
                 )
)

# Servidor (server)
server <- function(input, output) {
  
  # Función para generar histogramas con o sin curva normal
  generar_histograma <- function(data, variable, bins, normal_curve = FALSE) {
    data <- data[!is.na(data[[variable]]), ]  # Remover valores NA
    if (!is.numeric(data[[variable]])) {
      return(ggplot() + labs(title = "La variable seleccionada no es numérica."))
    }
    
    p <- ggplot(data, aes_string(x = variable)) +
      geom_histogram(aes(y = ..density..), bins = bins, fill = "#a8ddb5", color = "white", alpha = 0.7) +  # Pastel
      theme_minimal(base_size = 16) +
      labs(title = paste("Histograma de", input$var_score),
           x = input$var_score,
           y = "Densidad") +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)
      )
    
    if (normal_curve) {
      p <- p + stat_function(
        fun = dnorm, 
        args = list(mean = mean(data[[variable]], na.rm = TRUE), 
                    sd = sd(data[[variable]], na.rm = TRUE)),
        color = "#2b8cbe", size = 1
      )
    }
    
    return(p)
  }
  
  # SECCIÓN 1: Análisis Univariado
  
  # Renderizar gráficos para variables sociodemográficas
  output$plot_soc <- renderPlot({
    var <- input$var_soc
    var_label <- var  # Asumiendo que las etiquetas ya están en los niveles del factor
    var_type <- class(CAP[[var]])
    
    if (var_type %in% c("factor", "character")) {
      CAP[[var]] <- factor(CAP[[var]], levels = names(sort(table(CAP[[var]]), decreasing = TRUE)))
      
      ggplot(CAP, aes_string(x = var, fill = var)) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
        scale_fill_brewer(palette = "Pastel1") +
        scale_y_continuous(
          limits = c(0, 1), 
          labels = percent_format(), 
          breaks = seq(0, 1, by = 0.1)
        ) +
        geom_text(
          aes(y = (..count..) / sum(..count..), 
              label = percent((..count..) / sum(..count..), accuracy = 0.1)),
          stat = "count", 
          vjust = -0.5,  
          size = 5
        ) +
        theme_minimal(base_size = 16) +
        labs(title = paste("Distribución de", var_label),
             x = "", 
             y = "Porcentaje", 
             caption = "Fuente: Encuesta de Conocimientos, Actitudes y Prácticas, departamento de La Libertad, 2023") +
        theme(
          axis.text.x = element_text(size = 14),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        )
    } else {
      ggplot(CAP, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "#a6cee3", color = "white") +  # Pastel
        theme_minimal(base_size = 16) +
        labs(title = paste("Distribución de", var_label),
             x = var_label,
             y = "Frecuencia") +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14)
        )
    }
  })
  
  # Renderizar gráficos para variables clínicas
  output$plot_clin <- renderPlot({
    var <- input$var_clin
    var_label <- var  # Asumiendo que las etiquetas ya están en los niveles del factor
    var_type <- class(CAP[[var]])
    
    if (var_type %in% c("factor", "character")) {
      CAP[[var]] <- factor(CAP[[var]], levels = names(sort(table(CAP[[var]]), decreasing = TRUE)))
      
      ggplot(CAP, aes_string(x = var, fill = var)) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
        scale_fill_brewer(palette = "Pastel2") +
        scale_y_continuous(
          limits = c(0, 1), 
          labels = percent_format(), 
          breaks = seq(0, 1, by = 0.1)
        ) +
        geom_text(
          aes(y = (..count..) / sum(..count..), 
              label = percent((..count..) / sum(..count..), accuracy = 0.1)),
          stat = "count", 
          vjust = -0.5,  
          size = 5
        ) +
        theme_minimal(base_size = 16) +
        labs(title = paste("Distribución de", var_label),
             x = "", 
             y = "Porcentaje", 
             caption = "Fuente: Encuesta de Conocimientos, Actitudes y Prácticas, departamento de La Libertad, 2023") +
        theme(
          axis.text.x = element_text(size = 14),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        )
    } else {
      ggplot(CAP, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "#b2df8a", color = "white") +  # Pastel
        theme_minimal(base_size = 16) +
        labs(title = paste("Distribución de", var_label),
             x = var_label,
             y = "Frecuencia") +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14)
        )
    }
  })
  
  # Renderizar gráficos para variables de exposición
  output$plot_exp <- renderPlot({
    var <- input$var_exp
    var_label <- var  # Asumiendo que las etiquetas ya están en los niveles del factor
    var_type <- class(CAP[[var]])
    
    if (var_type %in% c("factor", "character")) {
      CAP[[var]] <- factor(CAP[[var]], levels = names(sort(table(CAP[[var]]), decreasing = TRUE)))
      
      ggplot(CAP, aes_string(x = var, fill = var)) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
        scale_fill_brewer(palette = "Pastel3") +
        scale_y_continuous(
          limits = c(0, 1), 
          labels = percent_format(), 
          breaks = seq(0, 1, by = 0.1)
        ) +
        geom_text(
          aes(y = (..count..) / sum(..count..), 
              label = percent((..count..) / sum(..count..), accuracy = 0.1)),
          stat = "count", 
          vjust = -0.5,  
          size = 5
        ) +
        theme_minimal(base_size = 16) +
        labs(title = paste("Distribución de", var_label),
             x = "", 
             y = "Porcentaje", 
             caption = "Fuente: Encuesta de Conocimientos, Actitudes y Prácticas, departamento de La Libertad, 2023") +
        theme(
          axis.text.x = element_text(size = 14),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        )
    } else {
      ggplot(CAP, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "#fb9a99", color = "white") +  # Pastel
        theme_minimal(base_size = 16) +
        labs(title = paste("Distribución de", var_label),
             x = var_label,
             y = "Frecuencia") +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14)
        )
    }
  })
  
  # Renderizar gráficos para puntajes
  output$plot_score <- renderPlot({
    var <- input$var_score
    var_label <- switch(var,
                        "Puntaje_C" = "Puntaje de Conocimientos",
                        "Puntaje_A" = "Puntaje de Actitudes",
                        "Puntaje_P" = "Puntaje de Prácticas")
    p <- generar_histograma(CAP, var, input$bins, input$normal_curve) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)
      ) +
      labs(title = paste("Histograma de", var_label),
           x = var_label,
           y = "Densidad")
    p
  })
  
  # Renderizar resumen estadístico para puntajes
  output$summary_stats <- renderText({
    var <- input$var_score
    var_label <- switch(var,
                        "Puntaje_C" = "Puntaje de Conocimientos",
                        "Puntaje_A" = "Puntaje de Actitudes",
                        "Puntaje_P" = "Puntaje de Prácticas")
    var_data <- CAP[[var]]
    if (is.numeric(var_data)) {
      paste(
        "Resumen estadístico para", var_label, ":\n",
        "Media: ", round(mean(var_data, na.rm = TRUE), 2), "\n",
        "DE: ", round(sd(var_data, na.rm = TRUE), 2), "\n",
        "Mínimo: ", round(min(var_data, na.rm = TRUE), 2), "\n",
        "Máximo: ", round(max(var_data, na.rm = TRUE), 2)
      )
    } else {
      "La variable seleccionada no es numérica."
    }
  })
  
  # Descarga de gráficos
  output$downloadPlot_soc <- downloadHandler(
    filename = function() {
      paste("grafico_", input$var_soc, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  output$downloadPlot_clin <- downloadHandler(
    filename = function() {
      paste("grafico_", input$var_clin, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  output$downloadPlot_exp <- downloadHandler(
    filename = function() {
      paste("grafico_", input$var_exp, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  output$downloadPlot_score <- downloadHandler(
    filename = function() {
      paste("grafico_", input$var_score, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  # Función para generar boxplot y tablas para análisis bivariado
  generar_boxplot_y_tablas <- function(data, var_dicot, var_num, mostrar_IC, mostrar_prueba) {
    if (input$eliminar_outliers) {
      Q1 <- quantile(data[[var_num]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[var_num]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      data <- data[data[[var_num]] >= (Q1 - 1.5 * IQR) & data[[var_num]] <= (Q3 + 1.5 * IQR), ]
    }
    
    var_dicot_label <- var_dicot  # Asumiendo que las etiquetas ya están en los niveles del factor
    var_num_label <- input$var_num  # Asumiendo que las etiquetas ya están en los nombres de las variables
    
    # Definir colores pastel para los grupos (celeste y rosado)
    colores_pastel <- c("#a6cee3", "#fb9a99")  # Celeste y rosado
    
    p <- ggplot(data, aes_string(x = var_dicot, y = var_num, fill = var_dicot)) +
      geom_boxplot() +
      scale_fill_manual(values = colores_pastel) +
      labs(title = paste("Boxplot de", var_num_label, "según", var_dicot_label),
           x = var_dicot_label, 
           y = var_num_label) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.position = "none"
      )
    
    resumen <- data %>%
      group_by_at(var_dicot) %>%
      summarise(
        N = n(),
        `Promedio` = round(mean(get(var_num), na.rm = TRUE), 2),
        DE = round(sd(get(var_num), na.rm = TRUE), 2)
      ) %>%
      rename("Grupo" = var_dicot)
    
    # Realizar la prueba t de Student solo si hay dos grupos
    if (nlevels(factor(data[[var_dicot]])) == 2) {
      t_test_result <- t.test(as.formula(paste(var_num, "~", var_dicot)), data = data)
      
      # Preparar tablas de inferencia
      tabla_inferencia <- list()
      
      if (mostrar_IC) {
        diferencia_medias <- round(t_test_result$estimate[1] - t_test_result$estimate[2], 2)
        intervalo_inferior <- round(t_test_result$conf.int[1], 3)
        intervalo_superior <- round(t_test_result$conf.int[2], 3)
        
        tabla_IC <- tibble(
          `Diferencia de medias` = diferencia_medias,
          `IC 95%` = paste0("[", ifelse(t_test_result$conf.int[1] < -999, "< -999", intervalo_inferior), 
                            ", ", 
                            ifelse(t_test_result$conf.int[2] > 999, "> 999", intervalo_superior), 
                            "]")
        )
        
        tabla_inferencia <- tabla_IC
      }
      
      if (mostrar_prueba) {
        t_stat <- round(t_test_result$statistic, 3)
        p_value <- ifelse(t_test_result$p.value < 0.001, "< 0.001", round(t_test_result$p.value, 3))
        
        tabla_prueba <- tibble(
          `t` = t_stat,
          `p-valor` = p_value
        )
        
        if (mostrar_IC) {
          tabla_inferencia <- bind_cols(tabla_inferencia, tabla_prueba)
        } else {
          tabla_inferencia <- tabla_prueba
        }
      }
    } else {
      tabla_inferencia <- NULL
    }
    
    return(list(plot = p, resumen = resumen, inferencia = tabla_inferencia))
  }
  
  # SECCIÓN 2: Análisis Bivariado
  
  output$boxplot_biv <- renderPlot({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_dicot, 
      var_num = input$var_num, 
      mostrar_IC = input$mostrar_IC, 
      mostrar_prueba = input$mostrar_prueba
    )
    resultado$plot
  })
  
  output$tabla_resumen <- renderTable({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_dicot, 
      var_num = input$var_num, 
      mostrar_IC = input$mostrar_IC, 
      mostrar_prueba = input$mostrar_prueba
    )
    resultado$resumen
  }, striped = FALSE, hover = TRUE, align = 'c', digits = 2)
  
  output$tabla_inferencia <- renderTable({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_dicot, 
      var_num = input$var_num, 
      mostrar_IC = input$mostrar_IC, 
      mostrar_prueba = input$mostrar_prueba
    )
    resultado$inferencia
  }, striped = FALSE, hover = TRUE, align = 'c', digits = 3)
  
  # Descarga de gráficos bivariados
  output$downloadPlot_biv <- downloadHandler(
    filename = function() {
      paste("boxplot_", input$var_dicot, "_", input$var_num, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
} # Cierre de server

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
