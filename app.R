
library(shiny)
library(leaflet)
library(here)
library(shinydashboard)
library(geojsonio)
library(plotly)

# load data sources

PC_to_LSOA <- data.frame(read.csv(here('src','PC_to_LSOA_dec2011.csv')))

police_dataframe <- data.frame(read.csv('/media/sf_Project_1_-_NLP_Safer_Leeds_Crime_MO/Project/Data/Data_v3_year1.csv'))

### Section for matching partial postcode to MSOA

# remove whitespace from partialpostcode column and PCD7 col 
police_dataframe$PartialPostCode <- gsub(' ', '', police_dataframe$PartialPostCode)

PC_to_LSOA$PCD7 <- gsub(' ', '', PC_to_LSOA$PCD7)

# get MSOA codes
police_dataframe$MSOA <- sapply(police_dataframe[,'PartialPostCode'], function(x) as.character(unique(PC_to_LSOA[grep(x, PC_to_LSOA$PCD7),]$MSOA11CD)))

# get LAD
police_dataframe$LAD <- sapply(police_dataframe[,'MSOA'], function(x) as.character(unique(PC_to_LSOA[PC_to_LSOA$MSOA11CD %in% x,]$LAD11CD)))

### Section getting LAD MSOA map

LAD_name <- unique(police_dataframe$LAD)[1]

geojsonfile <- geojsonio::geojson_read(paste0('https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/statistical/eng/msoa_by_lad/',trimws(LAD_name),'.json'), what = 'sp')

### UI section ###

header <-  dashboardHeader(title = 'LDA Viewer')

# main page panel
body <- dashboardBody(
    
    # create a row for controls
    fluidRow(
        box(# time slider
            sliderInput('MonYear','Select a Month over 1 year:',
                        min = min(1),
                        max = max(12),
                        value = c(1,12), step = unique(1),
                        animate = animationOptions(interval = 3000, loop = TRUE)
            )
        )
    ),
    fluidRow(
        # map column within row
        column(width = 8,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("mymap", height = 800))
        ),
        # column for other plots
        column(width = 4,
               box(width = NULL,
                   solidHeader = FALSE,
                   plotlyOutput('topicstimeplot')),
               box(width = NULL,
                   solidHeader = FALSE,
                   # text input section
                   textInput(inputId = 'keyword_text', label = 'Enter keyword(s):', value = ""),
                   htmlOutput(outputId = 'rejected_terms'),
                   actionButton(inputId = 'submit_text', label = 'Submit'),
                   plotlyOutput("wordbymonth")
               )
        )
    )
)

# construct ui from parts
ui <- dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        leafletOptions(maxZoom = 10)
        leaflet(geojsonfile) %>%
            addTiles() %>%
            addPolygons(data = geojsonfile,
                        stroke = TRUE,
                        color = "black",
                        fillOpacity=0.7,
                        #fillColor = ~pal(lsoa_full$freq),
                        dashArray = 2,
                        weight = 0.7
                        #popup = paste(lsoa_full$CrimeNotes)
            ) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
