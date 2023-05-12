#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(mapplots)
library(maps)
library(mapdata)
library(pals)

# Load data
load("data.Rdata")

# extract mesh size ranges
mesh_size_split <- strsplit(data$mesh_size_range, "D")
mesh_sizes <- suppressWarnings(as.numeric(do.call("c", mesh_size_split)))
mesh_size_range <- range(unique(mesh_sizes), na.rm = TRUE)
mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(length(x)<2){c(mesh_size_range)}else{x}})
mesh_size_split <- lapply(mesh_size_split, FUN = function(x){if(x[2]=="XX"){c(x[1], mesh_size_range[2])}else{x}})
data$mesh_size_min <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[1]})))
data$mesh_size_max <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x){x[2]})))
  

# Define UI
ui <- fluidPage(
  # Sidebar with filters
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Year range:", step = 1, sep = "",
        min = min(data$year), max = max(data$year),
        value = c(max(data$year)-3, max(data$year))),
      
      selectInput(inputId = "gear_type", label = "Gear type:",
        selected = c("OTB", "OTM"),
        choices = sort(unique(data$gear_type)),
        multiple = TRUE),
      
      sliderInput("mesh_range", "Mesh range [mm]:", step = 10, sep = "",
        min = 0, max = 250,
        value = c(0, 250)),
      
      selectInput(inputId = "species", label = "Species:", 
        selected = c("COD", "HAD", "POK", "WHG"),
        choices = sort(unique(data$species)),
        multiple = TRUE),
      
      selectInput(inputId = "pal", label = "Palette:", 
        selected = "spectral",
        choices = c("spectral", "paired", "cols25", "set1", "set2", "set3"),
        multiple = FALSE),
      
      checkboxInput(inputId = "sc", label = "Scaled landings", value = TRUE),
      
      # downloadLink("downloadMap", "Download Map")
      downloadButton("downloadMap", "Download Map")
      
    ),
    # Main panel with map
    mainPanel(
      plotOutput("map"),
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on input values
  filtered_data <- reactive({
    
    lutPal <- rbind(
      data.frame(pal = "spectral", fun = "brewer.spectral"),
      data.frame(pal = "paired", fun = "brewer.paired"),
      data.frame(pal = "cols25", fun = "cols25"),
      data.frame(pal = "set1", fun = "brewer.set1"),
      data.frame(pal = "set2", fun = "brewer.set2"),
      data.frame(pal = "set3", fun = "brewer.set3")
    )
    
    lutCol <- data.frame(species = sort(unique(data[,c("species")])))
    lutCol$col <- do.call(lutPal$fun[match(input$pal, lutPal$pal)], 
      args = list(n = length(lutCol$species)))
    
    dfsub <- subset(data, species %in% input$species & 
      gear_type %in% input$gear_type &
      mesh_size_min >= input$mesh_range[1] &
      mesh_size_max <= input$mesh_range[2] &
      year >= input$year_range[1] &
      year <= input$year_range[2])
    
    agg1 <- aggregate(totwghtlandg ~ icesname + species, data = dfsub, FUN = sum, na.rm = T)
    names(agg1)[3] <- "landings"
    agg1 <- subset(agg1, landings > 0) # remove zero landings
    agg2 <- aggregate(landings ~ icesname, data = agg1, FUN = sum, na.rm = T)
    names(agg2)[2] <- "sumLandings"
    
    agg <- merge(agg1, agg2, all.x = T)
    agg$percLandings <- agg$landings / agg$sumLandings
    agg$sc <- sqrt(agg$sumLandings/max(agg$sumLandings, na.rm = T))
    agg <- cbind(agg, ices.rect(rectangle = agg$icesname))
    agg$col <- lutCol$col[match(agg$species, lutCol$species)]

    agg
  })
  
  # map
  plot_map <- reactive({
    op <- par(cex = 1.5)
    data2 <- filtered_data()
    plot(1, xlim = c(-6,15), ylim = c(51,62), 
      t = "n", asp = 2, xlab = "", ylab = "")
    # map("world", xlim = range(agg$lon), ylim = range(agg$lat))
    urect <- unique(data2$icesname)
    for(i in seq(urect)){
      aggsub <- subset(data2, icesname == urect[i])
      sc <- ifelse(input$sc, 1, aggsub$sc[1])

      excl <- which(aggsub$percLandings==0)
      if(length(excl)>0) aggsub <- aggsub[-which(aggsub$percLandings==0)]
      barplot2D(z = aggsub$percLandings, x = aggsub$lon[1], y = aggsub$lat[1],
        width = 1*sc, height = 0.5*sc,
        colour = aggsub$col, border = NA,
        lwd.frame = 0.25, col.frame = "black")
    }
    map("world", add = T, fill = T, col = 8, boundary = 1)
    box()
    uSppCol <- unique(data2[,c("species", "col")])
    uSppCol <- uSppCol[order(uSppCol$species),]
    legend("topright", legend = uSppCol$species, col = uSppCol$col, fill = uSppCol$col)
    
    par(op)
    
    recordPlot()
    
  })
  
  
  # Render map
  output$map <- renderPlot({
    replayPlot(req(plot_map()))
    # plot_map()
  }, height = 800, width = 650)
  
  output$downloadMap <- downloadHandler(
    filename = function(){"output.png"},
    content = function(file){
      png(file, height = 800, width = 650)
        replayPlot(plot_map())
      dev.off()
    }
  )

}

# Run the app
shinyApp(ui, server)