library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

distritos <- st_read(con, SQL('poligonos.pmdu_distritos')) %>% 
  mutate_if(is.character, iconv) %>% 
  st_transform(., 4326) %>% 
  st_zm(drop = T)
centros <- st_centroid(distritos)

ui <- fluidPage(
  ui <- fluidPage(
    titlePanel("Proyectos Estratégicos"),
    sidebarLayout(
      selectInput("variable",
                  "Proyecto:",
                  unique(distritos$estrategia)))),
      mainPanel(leafletOutput('mymap'),
                dataTableOutput('tabla'))
    )
  )

# server()
server <- function(input, output){
  #MAPA ----
  output$mymap <- renderLeaflet({ if (input$variable == "Gestión urbana") {
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>% 
      addPolygons(data = distritos, fillOpacity = 0) %>% 
      addLabelOnlyMarkers(data = centros, label = 'oli') %>% 
      fitBounds(-101.33959, 19.61621, -101.08393, 19.77006 ) %>% 
      addPopups(data = centros, popup =distritos )
  }
    else {
      leaflet() %>% 
        addProviderTiles("OpenStreetMap.Mapnik") %>% 
        addPolygons(data = distritos, fillOpacity = 0) %>% 
        addPopups(data = centros, popup =distritos )
  }
    })
  #TABLA ----
  output$tabla <- renderDataTable(distritos %>% 
    st_drop_geometry() %>% 
    as.data.frame()
  )
}
  

# shinyApp()
shinyApp(ui = ui, server = server)
