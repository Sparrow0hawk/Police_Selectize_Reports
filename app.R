library(here)
source(here('R','police-selectize-functions.R'))
library(shiny)
library(leaflet)
library(shinydashboard)
library(geojsonio)
library(plotly)

# load data sources

datalist <- load_data(csv_file='/media/sf_Project_1_-_NLP_Safer_Leeds_Crime_MO/Project/Data/Data_v3_year1.csv')

PC_to_LSOA <- datalist$PCdata

police_dataframe <- datalist$policedata

### Section for matching partial postcode to MSOA

police_dataframe <- extract_MSOA(police_dataframe, PC_to_LSOA)

### Section getting LAD MSOA map

geomapfile <- get_geojson(police_dataframe)


### produce corpus

text_corpus <- tokenize_corpus(police_dataframe$CrimeNotes)

# get doctermmatrix
DTM <- build_DocTermMatrix(0.05, 0.8, text_corpus)


### UI section ###

header <-  dashboardHeader(title = 'LDA Viewer')

# main page panel
body <- dashboardBody(
    
    # create a row for controls
    fluidRow(
        box(width = NULL,
            solidHeader = FALSE,
            # text input section
            textInput(inputId = 'keyword_text', label = 'Enter keyword(s):', value = ""),
            htmlOutput(outputId = 'rejected_terms'),
            actionButton(inputId = 'submit_text', label = 'Submit'),
            plotlyOutput("wordbymonth"),
        # time slider
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
                   plotlyOutput('topicstimeplot'))
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
