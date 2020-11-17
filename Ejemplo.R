library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
##############################################################################
# Data
##############################################################################
qDat <- distritos
qDat$id <- seq.int(nrow(qDat))
str(qDat)
##############################################################################
# UI Side
##############################################################################
ui <- fluidPage(
  titlePanel("Visualization of Fiji Earthquake"),
  
  # side panel
  sidebarPanel(
    h3('Fiji Earthquake Data'),
    selectInput("subsistema",
                "subsistema:",
                unique(c(proyectos$SUBSISTEMA)), 
                selected = 'Sistema Municipal de Áreas de Valor Ambiental (SMAVA)')),
  
  # main panel
  mainPanel(
    leafletOutput('map01'),
    dataTableOutput('table01')
  )
  
)
##############################################################################
# Server Side
##############################################################################
server <- function(input,output){
  qSub <-  reactive({
    
      subset <- subset(qDat %>% 
                         st_drop_geometry() %>% 
                         as.data.frame())
                       })
    # table
  output$table01 <- renderDataTable({
    
    DT::datatable(qDat, selection = "single",options=list(stateSave = TRUE))
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  observeEvent(input$table01_rows_selected, {
    row_selected = qSub()[input$table01_rows_selected,]
    proxy <- leafletProxy('map01')
    print(row_selected)
    proxy %>%
      addPolygons(data = distritos[input$table01_rows_selected,], 
                  color = 'red')
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addPolygons(data = st_as_sf(as.data.frame(prev_row)))
    }
    prev_row(NULL)
  })
  
  # map
  output$map01 <- renderLeaflet({
      qMap <- leaflet() %>% 
      addTiles() %>%
     addPolygons(data = distritos)
    qMap
  })
  
  observeEvent(input$map01_shape_click, {
    clickId <- input$map01_shape_click$id
    print(clickId)
    dataTableProxy("table01") %>%
      selectRows(which(qDat %>% 
                         st_drop_geometry() %>% 
                         as.data.frame() %>% View$id == clickId)) %>%
      selectPage(which(input$table01_rows_all == clickId) %/% input$table01_state$length + 1)
  })
}

shinyApp(ui = ui, server = server)
