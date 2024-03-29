library(here)
source(here('R','police-selectize-functions.R'))
library(shiny)
library(leaflet)
library(shinydashboard)
library(geojsonio)
library(reshape2)
library(plotly)

# load data sources
if (strsplit(getwd(), '/')[[1]][2] == 'Users') {
    
    test_dir <- here('tests','test_data','test_input.csv')
    
} else {
    
    test_dir <- '/media/sf_Project_1_-_NLP_Safer_Leeds_Crime_MO/Project/Data/Data_v3_year1.csv'
}


datalist <- load_data(csv_file=test_dir)

PC_to_LSOA <- datalist$PCdata

police_dataframe <- datalist$policedata

### Section for matching partial postcode to MSOA

police_dataframe <- extract_MSOA(police_dataframe, PC_to_LSOA)

### Add numeric month column

police_dataframe <- map_Months(police_dataframe)

### Section getting LAD MSOA map

geojsonfile <- get_geojson(police_dataframe)


### UI section ###

header <-  dashboardHeader(title = 'Selectize_Keywords',
                           titleWidth = 450
                           )

# main page panel
body <- dashboardBody(
    
    tags$head(
        tags$style(HTML('.main-header .logo {
                         font-family: Serpentine, Verdana, sans-serif;
                         font-size: 24px;
                         }'))
    ),
    
    # create a row for controls
    fluidRow(
        box(width = NULL,
            solidHeader = FALSE,
            # text input section
            textInput(inputId = 'keyword_text', label = 'Enter keyword(s):', value = ""),
            htmlOutput(outputId = 'rejected_terms'),
            actionButton(inputId = 'submit_text', label = 'Submit', value = ''),
            # select crime type
            selectizeInput('crimeType', 'Select Crime Type: ',
                           choices = unique(police_dataframe$CrimeType),
                           selected = NULL,
                           options = list(create = TRUE),
                           multiple = FALSE),
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
                   plotlyOutput("wordbymonth"),
                   plotlyOutput("termfreqmonth"))
        )
    )
)

# construct ui from parts
ui <- dashboardPage(header,
    dashboardSidebar(disable = TRUE),
    body,
    skin = "red")


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(
        {input$submit_text
            input$MonYear
            input$crimeType}, {
                
                # filter by date
                if (is.null(input$crimeType)) {
                    new_pol_frame <- police_dataframe[which(police_dataframe$Month2 %in% c(input$MonYear[1]:input$MonYear[2])), ]
                } else {
                    new_pol_frame <- police_dataframe[which(police_dataframe$Month2 %in% c(input$MonYear[1]:input$MonYear[2]) 
                                             & police_dataframe$CrimeType == input$crimeType), ]
                }
                
                print('Loads frame')
                lsoa_full <- count_MSOAs(new_pol_frame, geojsonfile)$OA_count
                
                reports <- count_MSOAs(new_pol_frame, geojsonfile)$text_reports
                
                ### produce corpus
                
                text_corpus <- tokenize_corpus(new_pol_frame$CrimeNotes)
                
                # get doctermmatrix
                DTM <- build_DocTermMatrix(0.01, 0.8, text_corpus)
                print('Runs dtm')
                
                # lets build in a level of reactivity so a user can specify a key word
                
                terms <- as.character(input$keyword_text)
                
                accepted_terms <- list()
                
                rejected_terms <- list()
                
                if (terms == "") {
                    
                    reduced_DTM <- as.matrix(DTM)
                    
                } else{
                    
                    # perform quick regex formatting to create list of strings
                    
                    terms <- tolower(trimws(unlist(strsplit(terms, ","))))
                    
                    for (x in terms) {
                        if (x %in% DTM$dimnames$Terms) {
                            accepted_terms[[length(accepted_terms) + 1]] <- x
                        } else {
                            rejected_terms[[length(rejected_terms) + 1]] <- x
                        }
                    }
                    
                    # ensure DTM has acceptable terms to slice
                    # if not return whole DTM
                    if (length(accepted_terms) != 0) {
                        reduced_DTM <- as.matrix(DTM[,unlist(accepted_terms)])
                    }else{
                        reduced_DTM <- as.matrix(DTM)
                    }
                    
                    if (length(rejected_terms) != 0) {
                        output$rejected_terms <- renderUI(HTML("<font color =\"#FF0000\"><b>",'The following terms were not found:',
                                                               paste0(unlist(rejected_terms), collapse = ' '),"</b></font>"))
                    }
                    
                    
                }
                
                counts_per_decade <- aggregate(reduced_DTM, 
                                               by = list(Month = new_pol_frame$Month2), 
                                               sum)
                
                wordcountsdecade.tall <- melt(counts_per_decade,
                                              variable.name = 'Word',
                                              value.names = 'count',
                                              id.vars = ('Month')
                )
                
                
                # x axis specification
                axaxis <- list(
                    title = 'Months',
                    tick0 = 1,
                    dtick = 1
                    #ticklen = 25,
                    #ticklen
                )
                
                ### section to build stacked barplot
                if (length(accepted_terms) != 0) {
                    
                    term_in_month <- monthly_term_vol(DTM, new_pol_frame, terms = unlist(accepted_terms))
                    
                }else{
                    term_in_month <- monthly_term_vol(DTM, new_pol_frame, terms = NULL)
                
                    }
                
                term_in_month.tall <- melt(term_in_month,
                                              variable.name = 'Word',
                                              value.names = 'count',
                                              id.vars = ('Month')
                )
                
                termfreqplot <- plot_ly(term_in_month.tall, x = ~Month,
                                        y = term_in_month.tall$value,
                                        type = 'bar',
                                        name = term_in_month.tall$Word,
                                        text = term_in_month.tall$Word,
                                        hoverinfo = 'text+x+y') %>%
                    layout(title = "Term occurence within month",
                           xaxis = axaxis,
                           yaxis = list (title = "Counts",
                                         showline = TRUE),
                           showlegend = FALSE)
                
                # actual plotly call to shiny ui
                # x axis already defined in previous plot
                output$termfreqmonth <- renderPlotly(termfreqplot)
                
                ### plot word frequency over time
                # actual plotly call to shiny ui
                # x axis already defined in previous plot
                output$wordbymonth <- renderPlotly(
                    plot_ly(wordcountsdecade.tall, x = ~Month,
                            y= wordcountsdecade.tall$value,
                            type = 'scatter',
                            name = wordcountsdecade.tall$Word,
                            mode = 'lines',
                            text = wordcountsdecade.tall$Word,
                            hoverinfo = 'text+x+y') %>%
                        layout(title = "Word occurence over time",
                               xaxis = axaxis,
                               yaxis = list (title = "Counts",
                                             showline = TRUE),
                               showlegend = FALSE)
                )
                
                pal <- colorNumeric(c('white', 'red'), domain = lsoa_full$freq)
                
                output$mymap <- renderLeaflet({
                    leafletOptions(maxZoom = 10)
                    leaflet(geojsonfile) %>%
                        addTiles() %>%
                        addPolygons(data = geojsonfile,
                                    stroke = TRUE,
                                    color = "black",
                                    fillOpacity=0.7,
                                    fillColor = ~pal(lsoa_full$freq),
                                    dashArray = 2,
                                    weight = 0.7,
                                    popup = paste(lsoa_full$CrimeNotes)
                        ) 
                    
                })
            })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

