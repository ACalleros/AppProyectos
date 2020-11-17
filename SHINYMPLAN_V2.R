  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(DT)
  
  distritos$id <- seq.int(nrow(distritos))
  
# UI ----  
ui <- fluidPage(
  titlePanel("Proyectos Estratégicos"),
  sidebarPanel(
    selectInput("estrategia",
                "Proyecto:",
                unique(distritos$estrategia)),
    selectInput("subsistema",
                "subsistema:",
                unique(c(proyectos$SUBSISTEMA)), 
                selected = 'Sistema Municipal de Áreas de Valor Ambiental (SMAVA)')),
  mainPanel(leafletOutput('map'),
            dataTableOutput('tabla'))
  )

# SERVER ----  

server <- function(input,output){
  qSub <-  reactive({
    subset <- subset(distritos, proyectos$SUBSISTEMA>=input$subsistema) 
  })
  # table
  output$tabla <- renderDataTable({
    
    DT::datatable(qSub(), options=list(stateSave = TRUE))
  })
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  

  observeEvent(input$tabla_rows_selected, {
    row_selected = qSub()[input$tabla_rows_selected,]
    proxy <- leafletProxy('map')
    print(row_selected)
    proxy %>%
      addPolygons(data = distritos[input$tabla_rows_selected,],
                  color = 'red') 
    if(!is.null(prev_row()))
    {

    }
    prev_row(row_selected)
  })
  # map
  output$map <- renderLeaflet({
    qMap <- leaflet() %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      addPolygons(data = distritos, 
                  fillColor = "aliceblue", 
                  color = "grey",
                  layerId = ~nombre,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
    qMap
  })
  
  observeEvent(input$map_shape_click, {
    print(input$map_shape_click)
    clickId <- input$map_shape_click$id
    dataTableProxy("tabla") %>%
      selectRows(which(qSub()$nombre == clickId)) %>% 
      selectPage(which(input$tabla_rows_all == clickId) %/% input$tabla_state$length + 1)
  })
}

shinyApp(ui = ui, server = server)
