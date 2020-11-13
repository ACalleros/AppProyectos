library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

distritos <- st_read('distritos.gpkg') %>% 
  mutate_if(is.character, iconv) %>% 
  st_transform(., 4326) %>% 
  st_zm(drop = T)
centros <- st_centroid(distritos)

# UI ----
ui <- fluidPage(
    titlePanel("Proyectos Estratégicos"),
      sidebarPanel(
        selectInput("variable",
                    "Proyecto:",
                    unique(distritos$estrategia)),
        textInput("txt", "Enter the text to display below:"),
        wellPanel(textOutput("cnty"))),
    mainPanel(leafletOutput('map'),
              dataTableOutput('tabla'))
    )

# server ----
server <- function(input, output) {
  # Mapa ----
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = distritos, 
                  fillColor = "aliceblue", 
                  color = "grey",
                  layerId = ~nombre,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
  })
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(distritos$nombre[distritos$nombre == event$id])
    
  })
  
  #TABLA ----
  output$tabla <- renderDataTable(distritos %>% 
                                    st_drop_geometry() %>% 
                                    as.data.frame() %>% 
                                    select(nombre, tipo, estrategia, politica, zn_fun_mic, pn_pol_rie)) 
  }
                                  
# shinyApp()
shinyApp(ui = ui, server = server)
