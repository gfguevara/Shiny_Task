# Instala las librerías necesarias si no las tienes
# install.packages("shiny")
# install.packages("ggplot2")
#install.packages("shinythemes")
#install.packages("kableExtra")
#install.packages("shinythemes")
#install.packages("ggsave")


# Cargar las librerías necesarias
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)             # Para usar drop_na()
library(broom)
library(scales)
library(tibble)
library(RColorBrewer)
library(multcomp)
library(corrplot)
library(ggcorrplot)
library(pheatmap)
library(PerformanceAnalytics)
library(GGally)
library(Hmisc)
library(rlang)             # Para tidy evaluation

library(readxl)
CAP <- read_excel("CAP.xlsx")
#View(CAP)

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
                            tabPanel("Variable de Exposición",
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
                                         checkboxInput("eliminar_outliers_biv", "Eliminar valores atípicos", value = FALSE),
                                         checkboxInput("mostrar_IC_biv", "Mostrar Intervalo de Confianza", value = FALSE),
                                         checkboxInput("mostrar_prueba_biv", "Mostrar Prueba de Significancia", value = FALSE),
                                         downloadButton("downloadPlot_biv", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("boxplot_biv"),
                                         tableOutput("tabla_resumen"),
                                         conditionalPanel(
                                           condition = "input.mostrar_IC_biv || input.mostrar_prueba_biv",
                                           tableOutput("tabla_inferencia")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Variables Nominales",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("var_nom_indep", "Seleccione una variable independiente:", 
                                                     choices = c("Municipio" = "muni", 
                                                                 "Grupos Etarios" = "etario", 
                                                                 "Nivel Educativo" = "nivel_edu", 
                                                                 "Situación de Empleo" = "empleo_si", 
                                                                 "Ocupación" = "ocupacion", 
                                                                 "Ingreso" = "ingreso")),
                                         selectInput("var_num_dep", "Seleccione una variable dependiente:", 
                                                     choices = c("Puntaje de Conocimientos" = "Puntaje_C", 
                                                                 "Puntaje de Actitudes" = "Puntaje_A", 
                                                                 "Puntaje de Prácticas" = "Puntaje_P")),
                                         checkboxInput("eliminar_outliers_nom", "Eliminar valores atípicos", value = FALSE),
                                         checkboxInput("mostrar_anova", "Mostrar Tabla ANOVA", value = FALSE),
                                         checkboxInput("mostrar_posthoc", "Mostrar Comparación Post-hoc (Tukey)", value = FALSE),
                                         downloadButton("downloadPlot_nom", "Descargar gráfico")
                                       ),
                                       mainPanel(
                                         plotOutput("boxplot_nom"),
                                         conditionalPanel(
                                           condition = "input.mostrar_anova",
                                           tableOutput("tabla_anova")
                                         ),
                                         conditionalPanel(
                                           condition = "input.mostrar_posthoc",
                                           tableOutput("tabla_posthoc")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Variables Numéricas",
                                     sidebarLayout(
                                       sidebarPanel(
                                         h4("Scatterplots de Correlación"),
                                         selectInput("scatter_pair", "Seleccione un par de variables:", 
                                                     choices = c("Puntaje_C vs edad" = "Puntaje_C_vs_edad",
                                                                 "Puntaje_A vs edad" = "Puntaje_A_vs_edad",
                                                                 "Puntaje_P vs edad" = "Puntaje_P_vs_edad",
                                                                 "Puntaje_C vs Puntaje_A" = "Puntaje_C_vs_Puntaje_A",
                                                                 "Puntaje_C vs Puntaje_P" = "Puntaje_C_vs_Puntaje_P",
                                                                 "Puntaje_A vs Puntaje_P" = "Puntaje_A_vs_Puntaje_P")),
                                         selectInput("cor_method", "Seleccione el coeficiente de correlación:", 
                                                     choices = c("Pearson" = "pearson", "Spearman" = "spearman")),
                                         checkboxInput("add_trend", "Agregar línea de tendencia lineal", value = FALSE),
                                         downloadButton("downloadScatter", "Descargar Scatterplot"),
                                         hr(),
                                         h4("Matriz de Correlaciones"),
                                         # **Removido: selectInput para tipos de gráfica**
                                         # **Solo se usará GGpairs**
                                         downloadButton("downloadCorrMatrix", "Descargar Matriz de Correlaciones")
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Scatterplot",
                                                    plotOutput("scatter_plot"),
                                                    verbatimTextOutput("corr_coeff")
                                           ),
                                           tabPanel("Matriz de Correlaciones",
                                                    plotOutput("corr_matrix_plot")  # Usar plotOutput directamente
                                           )
                                         )
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
    
    var_label <- switch(variable,
                        "Puntaje_C" = "Puntaje de Conocimientos",
                        "Puntaje_A" = "Puntaje de Actitudes",
                        "Puntaje_P" = "Puntaje de Prácticas",
                        variable)
    
    p <- ggplot(data, aes(x = !!sym(variable))) +
      geom_histogram(aes(y = ..density..), bins = bins, fill = "#a8ddb5", color = "white", alpha = 0.7) +  # Pastel
      theme_minimal(base_size = 16) +
      labs(title = paste("Histograma de", var_label),
           x = var_label,
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
    var_label <- switch(var,
                        "mes" = "Mes de la encuesta",
                        "part_prev" = "Participación previa",
                        "muni" = "Municipio de residencia",
                        "area" = "Área de residencia",
                        "edad" = "Edad en años",
                        "edad_dicot" = "Mayor de 65 años",
                        "etario" = "Grupos etarios",
                        "genero" = "Sexo",
                        "nivel_edu" = "Nivel educativo",
                        "vive_solo" = "Hogar unipersonal",
                        "nucleo" = "Tamaño del núcleo familiar",
                        "empleo_si" = "Situación de empleo",
                        "ocupacion" = "Ocupación actual",
                        "ingreso" = "Ingreso promedio mensual",
                        "edu_sup" = "Estudios superiores",
                        var)
    var_type <- class(CAP[[var]])
    
    if (var_type %in% c("factor", "character")) {
      CAP[[var]] <- factor(CAP[[var]], levels = names(sort(table(CAP[[var]]), decreasing = TRUE)))
      
      # Obtener número de niveles
      num_levels <- length(levels(CAP[[var]]))
      
      # Generar una paleta de colores pastel con suficiente número de colores
      colores_pastel <- get_pastel_palette(num_levels, "Pastel1")
      
      ggplot(CAP, aes(x = !!sym(var), fill = !!sym(var))) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
        scale_fill_manual(values = colores_pastel) +
        scale_y_continuous(
          limits = c(0, 1), 
          labels = scales::percent_format(), 
          breaks = seq(0, 1, by = 0.1)
        ) +
        geom_text(
          aes(y = (..count..) / sum(..count..), 
              label = scales::percent((..count..) / sum(..count..), accuracy = 0.1)),
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
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          legend.position = "none",  # Eliminar la leyenda
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        )
    } else {
      ggplot(CAP, aes(x = !!sym(var))) +
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
    var_label <- switch(var,
                        "enfcro" = "Enfermedad crónica",
                        "prev_per_dicot" = "Antecedente de COVID-19",
                        "dosis" = "Dosis de vacuna",
                        "vacunas_incompletas" = "Vacunas incompletas",
                        "mortalidad" = "Mortalidad",
                        var)
    var_type <- class(CAP[[var]])
    
    if (var_type %in% c("factor", "character")) {
      CAP[[var]] <- factor(CAP[[var]], levels = names(sort(table(CAP[[var]]), decreasing = TRUE)))
      
      # Obtener número de niveles
      num_levels <- length(levels(CAP[[var]]))
      
      # Generar una paleta de colores pastel con suficiente número de colores
      colores_pastel <- get_pastel_palette(num_levels, "Pastel2")
      
      ggplot(CAP, aes(x = !!sym(var), fill = !!sym(var))) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
        scale_fill_manual(values = colores_pastel) +
        scale_y_continuous(
          limits = c(0, 1), 
          labels = scales::percent_format(), 
          breaks = seq(0, 1, by = 0.1)
        ) +
        geom_text(
          aes(y = (..count..) / sum(..count..), 
              label = scales::percent((..count..) / sum(..count..), accuracy = 0.1)),
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
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
          axis.ticks.x = element_blank(),
          legend.position = "none",  # Eliminar la leyenda
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        )
    } else {
      ggplot(CAP, aes(x = !!sym(var))) +
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
  generar_boxplot_y_tablas <- function(data, var_dicot, var_num, mostrar_IC, mostrar_prueba, eliminar_outliers, mostrar_posthoc = FALSE) {
    var_dicot_label <- switch(var_dicot,
                              "mes" = "Mes de la encuesta",
                              "part_prev" = "Participación previa",
                              "edad_dicot" = "Mayor de 65 años",
                              "edu_sup" = "Estudios superiores",
                              "enfcro" = "Enfermedad crónica",
                              "prev_per_dicot" = "Antecedente de COVID-19",
                              "exposicion_dicot" = "Exposición a mensajes educativos",
                              var_dicot)
    var_num_label <- switch(var_num,
                            "Puntaje_C" = "Puntaje de Conocimientos",
                            "Puntaje_A" = "Puntaje de Actitudes",
                            "Puntaje_P" = "Puntaje de Prácticas",
                            var_num)
    
    # Verificar que var_num es numérico
    if (!is.numeric(data[[var_num]])) {
      return(list(plot = ggplot() + labs(title = "La variable seleccionada no es numérica."),
                  resumen = NULL,
                  anova = NULL,
                  posthoc = NULL))
    }
    
    # Convertir var_dicot a factor
    if (!is.factor(data[[var_dicot]])) {
      data[[var_dicot]] <- as.factor(data[[var_dicot]])
    }
    
    # Filtrar y eliminar outliers por grupo si se solicita
    if (eliminar_outliers) {
      # Aplicar la fórmula Q1 - 1.5*IQR y Q3 + 1.5*IQR dentro de cada grupo
      data <- data %>%
        group_by(!!sym(var_dicot)) %>%
        filter(
          !!sym(var_num) >= (quantile(!!sym(var_num), 0.25, na.rm = TRUE) - 1.5 * IQR(!!sym(var_num), na.rm = TRUE)) &
            !!sym(var_num) <= (quantile(!!sym(var_num), 0.75, na.rm = TRUE) + 1.5 * IQR(!!sym(var_num), na.rm = TRUE))
        ) %>%
        ungroup()
    }
    
    # Definir colores pastel para los grupos
    num_levels <- length(unique(data[[var_dicot]]))
    colores_pastel <- get_pastel_palette(num_levels, "Pastel2")
    
    # Crear el boxplot utilizando tidy evaluation
    p <- ggplot(data, aes(x = !!sym(var_dicot), y = !!sym(var_num), fill = !!sym(var_dicot))) +
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
        legend.position = "none"  # Eliminar la leyenda
      )
    
    resumen <- data %>%
      group_by(!!sym(var_dicot)) %>%
      summarise(
        N = n(),
        `Promedio` = round(mean(!!sym(var_num), na.rm = TRUE), 2),
        DE = round(sd(!!sym(var_num), na.rm = TRUE), 2)
      ) %>%
      rename("Grupo" = var_dicot)
    
    # Realizar la prueba ANOVA si hay más de dos niveles
    if (num_levels > 2) {
      anova_model <- tryCatch({
        aov(as.formula(paste(var_num, "~", var_dicot)), data = data)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(anova_model)) {
        anova_table <- tibble(
          Error = "Error al ejecutar el modelo ANOVA."
        )
      } else {
        anova_summary <- tryCatch({
          broom::tidy(anova_model)
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(anova_summary)) {
          anova_table <- tibble(
            Error = "Error al procesar el resumen del modelo ANOVA."
          )
        } else {
          # Verificar si las columnas existen antes de seleccionar
          expected_cols <- c("term", "df", "sumsq", "meansq", "statistic", "p.value")
          existing_cols <- intersect(expected_cols, names(anova_summary))
          
          if (length(existing_cols) == 0) {
            anova_table <- tibble(
              Error = "El modelo ANOVA no devolvió las columnas esperadas."
            )
          } else {
            anova_table <- anova_summary %>%
              dplyr::select(dplyr::any_of(existing_cols)) %>%
              dplyr::rename(Factor = term,
                            Df = df,
                            `Sum Sq` = sumsq,
                            `Mean Sq` = meansq,
                            `F value` = statistic,
                            `Pr(>F)` = p.value) %>%
              dplyr::mutate(
                Df = round(Df, 3),
                `Sum Sq` = round(`Sum Sq`, 3),
                `Mean Sq` = round(`Mean Sq`, 3),
                `F value` = round(`F value`, 3),
                `Pr(>F)` = ifelse(`Pr(>F)` < 0.001, "<0.001", round(`Pr(>F)`, 3))
              )
          }
        }
      }
      
      # Realizar comparaciones post-hoc Tukey si se solicita
      if (mostrar_posthoc && !is.null(anova_model)) {
        tukey_result <- tryCatch({
          TukeyHSD(anova_model, conf.level = 0.95)
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(tukey_result) && !is.null(tukey_result[[var_dicot]])) {
          tukey_df <- as.data.frame(tukey_result[[var_dicot]])
          tukey_df <- tibble::rownames_to_column(tukey_df, var = "Comparación")
          
          # Verificar si las columnas existen antes de seleccionar
          expected_posthoc_cols <- c("diff", "lwr", "upr", "p adj")
          missing_posthoc_cols <- setdiff(expected_posthoc_cols, names(tukey_df))
          
          if (length(missing_posthoc_cols) > 0) {
            tukey_table <- tibble::tibble(
              Error = paste("Las columnas siguientes faltan en el resultado post-hoc de Tukey:", paste(missing_posthoc_cols, collapse = ", "))
            )
          } else {
            # Crear la tabla de Tukey de manera manual para evitar errores con select()
            tukey_table <- tibble::tibble(
              Comparación = tukey_df$Comparación,
              `Diferencia de medias` = round(tukey_df$diff, 3),
              `Límite Inferior` = round(tukey_df$lwr, 3),
              `Límite Superior` = round(tukey_df$upr, 3),
              `p-valor` = ifelse(tukey_df$`p adj` < 0.001, "<0.001", round(tukey_df$`p adj`, 3))
            )
          }
        } else {
          tukey_table <- tibble::tibble(
            Error = "Error al realizar la prueba post-hoc Tukey."
          )
        }
      } else {
        tukey_table <- NULL
      }
      
      # Devolver resultados
      if (!is.null(tukey_table)) {
        return(list(plot = p, resumen = resumen, anova = anova_table, posthoc = tukey_table))
      } else {
        return(list(plot = p, resumen = resumen, anova = anova_table))
      }
    } else if (num_levels == 2) {
      # Realizar la prueba t de Student
      t_test_result <- tryCatch({
        t.test(as.formula(paste(var_num, "~", var_dicot)), data = data)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(t_test_result)) {
        tabla_inferencia <- tibble::tibble(
          Error = "Error al ejecutar la prueba t de Student."
        )
      } else {
        # Preparar tablas de inferencia
        tabla_inferencia <- tibble::tibble()
        
        if (mostrar_IC) {
          diferencia_medias <- round(t_test_result$estimate[1] - t_test_result$estimate[2], 2)
          intervalo_inferior <- round(t_test_result$conf.int[1], 3)
          intervalo_superior <- round(t_test_result$conf.int[2], 3)
          
          tabla_IC <- tibble::tibble(
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
          p_value <- ifelse(t_test_result$p.value < 0.001, "<0.001", round(t_test_result$p.value, 3))
          
          tabla_prueba <- tibble::tibble(
            `t` = t_stat,
            `p-valor` = p_value
          )
          
          if (mostrar_IC) {
            tabla_inferencia <- dplyr::bind_cols(tabla_inferencia, tabla_prueba)
          } else {
            tabla_inferencia <- tabla_prueba
          }
        }
      }
      
      # Devolver resultados
      return(list(plot = p, resumen = resumen, inferencia = tabla_inferencia))
    } else {
      return(list(plot = p, resumen = resumen))
    }
  }
  
  # SECCIÓN 2: Análisis Bivariado
  
  # Variables Dicotómicas
  output$boxplot_biv <- renderPlot({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_dicot, 
      var_num = input$var_num, 
      mostrar_IC = input$mostrar_IC_biv, 
      mostrar_prueba = input$mostrar_prueba_biv,
      eliminar_outliers = input$eliminar_outliers_biv,
      mostrar_posthoc = FALSE  # Comparación post-hoc no en dicotómicas
    )
    resultado$plot
  })
  
  output$tabla_resumen <- renderTable({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_dicot, 
      var_num = input$var_num, 
      mostrar_IC = input$mostrar_IC_biv, 
      mostrar_prueba = input$mostrar_prueba_biv,
      eliminar_outliers = input$eliminar_outliers_biv,
      mostrar_posthoc = FALSE  # Comparación post-hoc no en dicotómicas
    )
    resultado$resumen
  }, striped = FALSE, hover = TRUE, align = 'c', digits = 2)
  
  output$tabla_inferencia <- renderTable({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_dicot, 
      var_num = input$var_num, 
      mostrar_IC = input$mostrar_IC_biv, 
      mostrar_prueba = input$mostrar_prueba_biv,
      eliminar_outliers = input$eliminar_outliers_biv,
      mostrar_posthoc = FALSE  # Comparación post-hoc no en dicotómicas
    )
    # Determinar si se trata de ANOVA o t-test
    if (!is.null(resultado$anova) && !"Error" %in% names(resultado$anova)) {
      return(resultado$anova)
    } else if (!is.null(resultado$inferencia) && !"Error" %in% names(resultado$inferencia)) {
      return(resultado$inferencia)
    } else if (!is.null(resultado$posthoc) && !"Error" %in% names(resultado$posthoc)) {
      return(resultado$posthoc)
    } else {
      return(resultado$anova)  # Retornar el error si existe
    }
  }, striped = FALSE, hover = TRUE, align = 'c', digits = 3)
  
  # Descarga de gráficos bivariados
  output$downloadPlot_biv <- downloadHandler(
    filename = function() {
      paste("boxplot_", input$var_dicot, "_", input$var_num, ".png", sep = "")
    },
    content = function(file) {
      resultado <- generar_boxplot_y_tablas(
        data = CAP, 
        var_dicot = input$var_dicot, 
        var_num = input$var_num, 
        mostrar_IC = input$mostrar_IC_biv, 
        mostrar_prueba = input$mostrar_prueba_biv,
        eliminar_outliers = input$eliminar_outliers_biv,
        mostrar_posthoc = FALSE  # Comparación post-hoc no en dicotómicas
      )
      if (!is.null(resultado$plot)) {
        ggsave(file, plot = resultado$plot, device = "png")
      }
    }
  )
  
  # SECCIÓN 2: Variables Nominales
  
  # Renderizar boxplot para Variables Nominales
  output$boxplot_nom <- renderPlot({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_nom_indep, 
      var_num = input$var_num_dep, 
      mostrar_IC = FALSE,  # No se requiere Intervalo de Confianza en Nominales
      mostrar_prueba = FALSE,  # Se maneja en tablas separadas
      eliminar_outliers = input$eliminar_outliers_nom,
      mostrar_posthoc = input$mostrar_posthoc
    )
    resultado$plot
  })
  
  # Renderizar Tabla ANOVA
  output$tabla_anova <- renderTable({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_nom_indep, 
      var_num = input$var_num_dep, 
      mostrar_IC = FALSE, 
      mostrar_prueba = FALSE,
      eliminar_outliers = input$eliminar_outliers_nom,
      mostrar_posthoc = input$mostrar_posthoc
    )
    if (!is.null(resultado$anova) && !"Error" %in% names(resultado$anova)) {
      return(resultado$anova)
    } else {
      return(resultado$anova)  # Retornar el error si existe
    }
  }, striped = FALSE, hover = TRUE, align = 'c', digits = 3)
  
  # Renderizar Tabla Post-hoc Tukey
  output$tabla_posthoc <- renderTable({
    resultado <- generar_boxplot_y_tablas(
      data = CAP, 
      var_dicot = input$var_nom_indep, 
      var_num = input$var_num_dep, 
      mostrar_IC = FALSE, 
      mostrar_prueba = FALSE,
      eliminar_outliers = input$eliminar_outliers_nom,
      mostrar_posthoc = input$mostrar_posthoc
    )
    if (!is.null(resultado$posthoc) && !"Error" %in% names(resultado$posthoc)) {
      return(resultado$posthoc)
    } else {
      return(resultado$posthoc)  # Retornar el error si existe
    }
  }, striped = FALSE, hover = TRUE, align = 'c', digits = 3)
  
  # Descarga de boxplot nominales
  output$downloadPlot_nom <- downloadHandler(
    filename = function() {
      paste("boxplot_nominales_", input$var_nom_indep, "_", input$var_num_dep, ".png", sep = "")
    },
    content = function(file) {
      resultado <- generar_boxplot_y_tablas(
        data = CAP, 
        var_dicot = input$var_nom_indep, 
        var_num = input$var_num_dep, 
        mostrar_IC = FALSE, 
        mostrar_prueba = FALSE,
        eliminar_outliers = input$eliminar_outliers_nom,
        mostrar_posthoc = input$mostrar_posthoc
      )
      if (!is.null(resultado$plot)) {
        ggsave(file, plot = resultado$plot, device = "png")
      }
    }
  )
  
  # SECCIÓN 2: Variables Numéricas
  
  # Renderizar Scatterplot con geom_jitter
  output$scatter_plot <- renderPlot({
    # Definir los pares de variables
    pairs_list <- list(
      "Puntaje_C_vs_edad" = c("Puntaje_C", "edad"),
      "Puntaje_A_vs_edad" = c("Puntaje_A", "edad"),
      "Puntaje_P_vs_edad" = c("Puntaje_P", "edad"),
      "Puntaje_C_vs_Puntaje_A" = c("Puntaje_C", "Puntaje_A"),
      "Puntaje_C_vs_Puntaje_P" = c("Puntaje_C", "Puntaje_P"),
      "Puntaje_A_vs_Puntaje_P" = c("Puntaje_A", "Puntaje_P")
    )
    
    # Obtener las variables seleccionadas
    selected_pair <- pairs_list[[input$scatter_pair]]
    x_var <- selected_pair[1]
    y_var <- selected_pair[2]
    
    # Filtrar los datos para eliminar NAs en las variables seleccionadas
    data_plot <- CAP %>%
      dplyr::select(all_of(x_var), all_of(y_var)) %>%
      drop_na()
    
    # Calcular el coeficiente de correlación
    cor_method <- input$cor_method
    cor_test <- cor.test(data_plot[[x_var]], data_plot[[y_var]], method = cor_method)
    cor_coeff <- round(cor_test$estimate, 2)
    cor_pval <- ifelse(cor_test$p.value < 0.001, "<0.001", round(cor_test$p.value, 3))
    
    # Crear el scatterplot utilizando tidy evaluation con geom_jitter
    p <- ggplot(data_plot, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_jitter(color = "#1f78b4", alpha = 0.6, size = 3, width = 0.2, height = 0.2) +  # Usar geom_jitter
      theme_minimal(base_size = 16) +
      labs(title = paste("Scatterplot de", x_var, "vs", y_var),
           x = switch(x_var,
                      "Puntaje_C" = "Puntaje de Conocimientos",
                      "Puntaje_A" = "Puntaje de Actitudes",
                      "Puntaje_P" = "Puntaje de Prácticas",
                      "edad" = "Edad"),
           y = switch(y_var,
                      "Puntaje_C" = "Puntaje de Conocimientos",
                      "Puntaje_A" = "Puntaje de Actitudes",
                      "Puntaje_P" = "Puntaje de Prácticas",
                      "edad" = "Edad")) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)
      )
    
    # Agregar línea de tendencia si se selecciona
    if (input$add_trend) {
      p <- p + geom_smooth(method = "lm", color = "red", se = FALSE)
    }
    
    # Agregar el coeficiente de correlación en la gráfica
    p <- p + annotate("text", 
                      x = Inf, y = -Inf, 
                      label = paste("r =", cor_coeff, "\n p =", cor_pval), 
                      hjust = 1.1, vjust = -0.5, 
                      size = 5, 
                      color = "black")
    
    p
  })
  
  # Mostrar el coeficiente de correlación debajo del scatterplot (opcional)
  output$corr_coeff <- renderPrint({
    # Definir los pares de variables
    pairs_list <- list(
      "Puntaje_C_vs_edad" = c("Puntaje_C", "edad"),
      "Puntaje_A_vs_edad" = c("Puntaje_A", "edad"),
      "Puntaje_P_vs_edad" = c("Puntaje_P", "edad"),
      "Puntaje_C_vs_Puntaje_A" = c("Puntaje_C", "Puntaje_A"),
      "Puntaje_C_vs_Puntaje_P" = c("Puntaje_C", "Puntaje_P"),
      "Puntaje_A_vs_Puntaje_P" = c("Puntaje_A", "Puntaje_P")
    )
    
    # Obtener las variables seleccionadas
    selected_pair <- pairs_list[[input$scatter_pair]]
    x_var <- selected_pair[1]
    y_var <- selected_pair[2]
    
    # Filtrar los datos para eliminar NAs en las variables seleccionadas
    data_plot <- CAP %>%
      dplyr::select(all_of(x_var), all_of(y_var)) %>%
      drop_na()
    
    # Calcular el coeficiente de correlación
    cor_method <- input$cor_method
    cor_test <- cor.test(data_plot[[x_var]], data_plot[[y_var]], method = cor_method)
    cor_coeff <- round(cor_test$estimate, 2)
    cor_pval <- ifelse(cor_test$p.value < 0.001, "<0.001", round(cor_test$p.value, 3))
    
    cat("Coeficiente de correlación:", cor_coeff, "\n",
        "p-valor:", cor_pval)
  })
  
  # Renderizar Matriz de Correlaciones utilizando GGpairs con geom_jitter
  output$corr_matrix_plot <- renderPlot({
    # Definir las variables para la matriz de correlaciones
    vars_corr <- c("Puntaje_C", "Puntaje_A", "Puntaje_P", "edad")
    
    # Verificar si todas las variables existen
    if (!all(vars_corr %in% names(CAP))) {
      showNotification("Una o más variables de la matriz de correlaciones no existen en el dataset.", type = "error")
      return(NULL)
    }
    
    # Verificar si todas las variables son numéricas
    if (!all(sapply(CAP[, vars_corr], is.numeric))) {
      showNotification("Todas las variables de la matriz de correlaciones deben ser numéricas.", type = "error")
      return(NULL)
    }
    
    data_corr <- CAP %>%
      dplyr::select(all_of(vars_corr)) %>%
      drop_na()
    
    # Verificar que después de drop_na hay suficientes filas
    if (nrow(data_corr) < 2) {
      showNotification("No hay suficientes datos después de eliminar NAs para calcular las correlaciones.", type = "error")
      return(NULL)
    }
    
    # Calcular la matriz de correlación y p-valores (aunque GGpairs no los usa directamente)
    cor_results <- Hmisc::rcorr(as.matrix(data_corr), type = input$cor_method)
    
    cor_matrix <- cor_results$r
    p_matrix <- cor_results$P
    
    # Verificar que las matrices de correlación y p-valores tienen las mismas dimensiones
    if (!all(dim(cor_matrix) == dim(p_matrix))) {
      showNotification("Las matrices de correlación y p-valores no tienen las mismas dimensiones.", type = "error")
      return(NULL)
    }
    
    # **Generar GGpairs con geom_jitter en los paneles de dispersión**
    GGally::ggpairs(
      data_corr,
      lower = list(
        continuous = wrap(
          "points", 
          alpha = 0.6, 
          color = "blue",
          position = position_jitter(width = 0.2, height = 0.2)  # Añadir jitter
        )
      ),
      upper = list(continuous = wrap("cor", size = 5)),
      diag = list(continuous = wrap("densityDiag"))
    )
  })
  
  # Descargar Scatterplot
  output$downloadScatter <- downloadHandler(
    filename = function() {
      paste("scatterplot_", input$scatter_pair, ".png", sep = "")
    },
    content = function(file) {
      # Definir los pares de variables
      pairs_list <- list(
        "Puntaje_C_vs_edad" = c("Puntaje_C", "edad"),
        "Puntaje_A_vs_edad" = c("Puntaje_A", "edad"),
        "Puntaje_P_vs_edad" = c("Puntaje_P", "edad"),
        "Puntaje_C_vs_Puntaje_A" = c("Puntaje_C", "Puntaje_A"),
        "Puntaje_C_vs_Puntaje_P" = c("Puntaje_C", "Puntaje_P"),
        "Puntaje_A_vs_Puntaje_P" = c("Puntaje_A", "Puntaje_P")
      )
      
      # Obtener las variables seleccionadas
      selected_pair <- pairs_list[[input$scatter_pair]]
      x_var <- selected_pair[1]
      y_var <- selected_pair[2]
      
      # Filtrar los datos para eliminar NAs en las variables seleccionadas
      data_plot <- CAP %>%
        dplyr::select(all_of(x_var), all_of(y_var)) %>%
        drop_na()
      
      # Calcular el coeficiente de correlación
      cor_method <- input$cor_method
      cor_test <- cor.test(data_plot[[x_var]], data_plot[[y_var]], method = cor_method)
      cor_coeff <- round(cor_test$estimate, 2)
      cor_pval <- ifelse(cor_test$p.value < 0.001, "<0.001", round(cor_test$p.value, 3))
      
      # Crear el scatterplot utilizando tidy evaluation con geom_jitter
      p <- ggplot(data_plot, aes(x = !!sym(x_var), y = !!sym(y_var))) +
        geom_jitter(color = "#1f78b4", alpha = 0.6, size = 3, width = 0.2, height = 0.2) +  # Usar geom_jitter
        theme_minimal(base_size = 16) +
        labs(title = paste("Scatterplot de", x_var, "vs", y_var),
             x = switch(x_var,
                        "Puntaje_C" = "Puntaje de Conocimientos",
                        "Puntaje_A" = "Puntaje de Actitudes",
                        "Puntaje_P" = "Puntaje de Prácticas",
                        "edad" = "Edad"),
             y = switch(y_var,
                        "Puntaje_C" = "Puntaje de Conocimientos",
                        "Puntaje_A" = "Puntaje de Actitudes",
                        "Puntaje_P" = "Puntaje de Prácticas",
                        "edad" = "Edad")) +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14)
        )
      
      # Agregar línea de tendencia si se selecciona
      if (input$add_trend) {
        p <- p + geom_smooth(method = "lm", color = "red", se = FALSE)
      }
      
      # Agregar el coeficiente de correlación en la gráfica
      p <- p + annotate("text", 
                        x = Inf, y = -Inf, 
                        label = paste("r =", cor_coeff, "\n p =", cor_pval), 
                        hjust = 1.1, vjust = -0.5, 
                        size = 5, 
                        color = "black")
      
      # Guardar la gráfica
      ggsave(file, plot = p, device = "png")
    }
  )
  
  # Renderizar Matriz de Correlaciones utilizando GGpairs con geom_jitter
  output$corr_matrix_plot <- renderPlot({
    # Definir las variables para la matriz de correlaciones
    vars_corr <- c("Puntaje_C", "Puntaje_A", "Puntaje_P", "edad")
    
    # Verificar si todas las variables existen
    if (!all(vars_corr %in% names(CAP))) {
      showNotification("Una o más variables de la matriz de correlaciones no existen en el dataset.", type = "error")
      return(NULL)
    }
    
    # Verificar si todas las variables son numéricas
    if (!all(sapply(CAP[, vars_corr], is.numeric))) {
      showNotification("Todas las variables de la matriz de correlaciones deben ser numéricas.", type = "error")
      return(NULL)
    }
    
    data_corr <- CAP %>%
      dplyr::select(all_of(vars_corr)) %>%
      drop_na()
    
    # Verificar que después de drop_na hay suficientes filas
    if (nrow(data_corr) < 2) {
      showNotification("No hay suficientes datos después de eliminar NAs para calcular las correlaciones.", type = "error")
      return(NULL)
    }
    
    # Calcular la matriz de correlación y p-valores (aunque GGpairs no los usa directamente)
    cor_results <- Hmisc::rcorr(as.matrix(data_corr), type = input$cor_method)
    
    cor_matrix <- cor_results$r
    p_matrix <- cor_results$P
    
    # Verificar que las matrices de correlación y p-valores tienen las mismas dimensiones
    if (!all(dim(cor_matrix) == dim(p_matrix))) {
      showNotification("Las matrices de correlación y p-valores no tienen las mismas dimensiones.", type = "error")
      return(NULL)
    }
    
    # **Generar GGpairs con geom_jitter en los paneles de dispersión**
    GGally::ggpairs(
      data_corr,
      lower = list(
        continuous = wrap(
          "points", 
          alpha = 0.6, 
          color = "blue",
          position = position_jitter(width = 0.2, height = 0.2)  # Añadir jitter
        )
      ),
      upper = list(continuous = wrap("cor", size = 5)),
      diag = list(continuous = wrap("densityDiag"))
    )
  })
  
  # Descargar Matriz de Correlaciones (Solo GGpairs)
  output$downloadCorrMatrix <- downloadHandler(
    filename = function() {
      paste("matriz_correlaciones_ggpairs.png", sep = "")
    },
    content = function(file) {
      # Definir las variables para la matriz de correlaciones
      vars_corr <- c("Puntaje_C", "Puntaje_A", "Puntaje_P", "edad")
      
      # Comprobar si todas las variables existen
      if (!all(vars_corr %in% names(CAP))) {
        showNotification("Una o más variables de la matriz de correlaciones no existen en el dataset.", type = "error")
        return(NULL)
      }
      
      # Verificar si todas las variables son numéricas
      if (!all(sapply(CAP[, vars_corr], is.numeric))) {
        showNotification("Todas las variables de la matriz de correlaciones deben ser numéricas.", type = "error")
        return(NULL)
      }
      
      data_corr <- CAP %>%
        dplyr::select(all_of(vars_corr)) %>%
        drop_na()
      
      # Verificar que después de drop_na hay suficientes filas
      if (nrow(data_corr) < 2) {
        showNotification("No hay suficientes datos después de eliminar NAs para calcular las correlaciones.", type = "error")
        return(NULL)
      }
      
      # Calcular la matriz de correlación y p-valores (aunque GGpairs no los usa directamente)
      cor_results <- Hmisc::rcorr(as.matrix(data_corr), type = input$cor_method)
      
      cor_matrix <- cor_results$r
      p_matrix <- cor_results$P
      
      # Verificar que las matrices de correlación y p-valores tienen las mismas dimensiones
      if (!all(dim(cor_matrix) == dim(p_matrix))) {
        showNotification("Las matrices de correlación y p-valores no tienen las mismas dimensiones.", type = "error")
        return(NULL)
      }
      
      # **Generar únicamente GGpairs y guardarlo con geom_jitter**
      png(file, width = 1200, height = 1000)
      print(GGally::ggpairs(
        data_corr,
        lower = list(
          continuous = wrap(
            "points", 
            alpha = 0.6, 
            color = "blue",
            position = position_jitter(width = 0.2, height = 0.2)  # Añadir jitter
          )
        ),
        upper = list(continuous = wrap("cor", size = 5)),
        diag = list(continuous = wrap("densityDiag"))
      ))
      dev.off()
    }
  )
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
