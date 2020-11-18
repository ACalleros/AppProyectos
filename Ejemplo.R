library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

qDat <-  distritos
qDat$id <- seq.int(nrow(qDat))
str(qDat)

ui <- fluidPage(
  titlePanel("Programa Municipal de Desarrollo"),
  sidebarPanel(
    h4('Proyectos Estratégicos'),
    selectInput("subsistema",
                "Subsistema:",
                unique(c('Todos', c(proyectos$SUBSISTEMA))), 
                selected = 'Todos')),
  mainPanel(
    leafletOutput('map01'),
    dataTableOutput('table01'))
)

server <- function(input,output){
  qSub <- st_drop_geometry(qDat)
  # table
  output$table01 <- renderDataTable({
    DT::datatable(qSub, selection = "single",options=list(stateSave = TRUE))
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  observeEvent(input$table01_rows_selected, {
    row_selected = qSub[input$table01_rows_selected,]
    proxy <- leafletProxy('map01')
    #print(row_selected)
    proxy %>%
      addPolygons(data = distritos[input$table01_rows_selected,], 
                  color = 'red')
    
    if(!is.null(prev_row()))
    {
      
      proxy %>%
        addMarkers(popup=as.character(prev_row()$mag), 
                   layerId = as.character(prev_row()$id),
                   lng=prev_row()$long, 
                   lat=prev_row()$lat)
    }
    
    
  })
  
  # map
  output$map01 <- renderLeaflet({
    qMap <- leaflet() %>% 
      addTiles() %>%
      addPolygons(data = distritos,
                  layerId = ~id)
    qMap
  })
  
  observeEvent(input$map01_shape_click, {
    print(input$table01)
    #print(input$map01_shape_click)
    clickId <- input$map01_shape_click$id
    dataTableProxy("table01") %>% 
      selectRows(qSub[which(qSub$id_dto.x == clickId),]) 
    #selectPage(qSub[which(input$table01_rows_all == clickId),] %/% input$table01_state$length + 1)
  })
}

shinyApp(ui = ui, server = server)
