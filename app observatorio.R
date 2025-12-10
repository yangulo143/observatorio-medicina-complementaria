library(shiny)
library(dplyr)
library(ggplot2)
library(sf)

# Suponemos que tienes los siguientes objetos disponibles:
# - data_hiss: base principal con columnas: año, departamento, grupo_etario, genero, grupo_profesional, terapia, etc.
# - mapa_peru: objeto sf con geometría departamental (columna 'departamento')

ui <- fluidPage(
  titlePanel("Observatorio de datos en Medicina Complementaria - MINSA (2019-2024)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros generales"),
      selectInput("departamento", "Departamento:", choices = sort(unique(atenciones_completa$Departamento)), multiple = TRUE),
      selectInput("grupo_etario", "Grupo etario:", choices = sort(unique(atenciones_completa$grupo_etario)), multiple = TRUE),
      selectInput("genero", "Género:", choices = sort(unique(atenciones_completa$genero)), multiple = TRUE),
      selectInput("terapia", "Terapia:", choices = sort(unique(atenciones_completa$terapia)), multiple = TRUE),
      selectInput("grupo_profesional", "Grupo profesional:", choices = sort(unique(atenciones_completa$grupo_profesional)), multiple = TRUE),
      selectInput("año", "Año de estudio:", choices = sort(unique(atenciones_completa$año)), multiple = TRUE),
      actionButton("actualizar", "Aplicar filtros")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tendencia anual", plotOutput("grafico_linea")),
        tabPanel("Mapa de atenciones", plotOutput("mapa_peru")),
        tabPanel("Pirámide etaria", plotOutput("piramide_etaria")),
        tabPanel("Frecuencia de terapias", plotOutput("grafico_terapias"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtro reactivo general
  datos_filtrados <- eventReactive(input$actualizar, {
    atenciones_completa %>%
      filter(
        departamento %in% input$Departamento,
        grupo_etario %in% input$grupo_etario,
        genero %in% input$genero,
        terapia %in% input$terapia,
        grupo_profesional %in% input$grupo_profesional,
        año %in% input$año,
        !is.na(Departamento)
      )
  })
  
  # Panel 1: gráfico de línea
  output$grafico_linea <- renderPlot({
    datos_filtrados() %>%
      count(año) %>%
      ggplot(aes(x = año, y = n)) +
      geom_line(size = 1.2, color = "#0073C2") +
      geom_point(size = 3, color = "#0073C2") +
      labs(title = "Número de atenciones por año", x = "Año", y = "Frecuencia") +
      theme_minimal()
  })
  
  # Panel 2: mapa de atenciones
  output$mapa_peru <- renderPlot({
    datos_filtrados() %>%
      count(departamento) %>%
      left_join(mapa_peru, by = "departamento") %>%
      ggplot() +
      geom_sf(data = mapa_peru, fill = "gray90", color = "white") +
      geom_sf(aes(size = n), shape = 21, fill = "#1f78b4", alpha = 0.6) +
      scale_size_continuous(name = "Atenciones", range = c(2, 12)) +
      labs(title = "Mapa de atenciones por departamento") +
      theme_minimal()
  })
  
  # Panel 3: pirámide etaria
  output$piramide_etaria <- renderPlot({
    datos_filtrados() %>%
      count(grupo_etario, genero) %>%
      mutate(frecuencia = ifelse(genero == "Masculino", -n, n)) %>%
      ggplot(aes(x = grupo_etario, y = frecuencia, fill = genero)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = abs) +
      labs(title = "Pirámide etaria", x = "Grupo etario", y = "Frecuencia") +
      theme_minimal()
  })
  
  # Panel 4: gráfico de barras horizontales de terapias
  output$grafico_terapias <- renderPlot({
    datos_filtrados() %>%
      count(terapia) %>%
      ggplot(aes(x = reorder(terapia, n), y = n)) +
      geom_bar(stat = "identity", fill = "#1f78b4") +
      coord_flip() +
      labs(title = "Frecuencia de uso de terapias", x = "Terapia", y = "Atenciones") +
      theme_minimal()
  })
}

shinyApp(ui, server)