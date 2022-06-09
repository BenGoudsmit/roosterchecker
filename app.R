library(shiny)
library(janitor)
library(tidyverse)
library(gsheet)
library(DT)
library(shinymanager)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Time out after 2 mins inactivity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Password on entrance
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# data.frame with credentials info
credentials <- data.frame(
  user = c("Interne"),
  password = c("dienst"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Get the data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Run once per app start up:
# AIOS and ANIOS names
url <- paste0("https://docs.google.com/spreadsheets/d/",
              "17dliuHMziqm41Mls-Z5XP8b7mBhVcd8PDes8-G8zE7I/edit#gid=1637500244")
rooster <- gsheet2tbl(url)

#aios
names(rooster)[1] <- "artsen"
aios <- rooster %>% select(1,4) %>% slice(7:52) %>% filter(!is.na(artsen)) %>% rename(AIOS=artsen,id=`>>`)

#anios
anios <- rooster %>% select(1,4) %>% slice(54:81) %>% filter(!is.na(artsen)) %>% rename(ANIOS=artsen,id=`>>`)

assistenten <- data.frame(assistent=as.character(cbind(c(anios$ANIOS,aios$AIOS))))

# Get dates and names
url <- paste0("https://docs.google.com/spreadsheets/d/",
              "17dliuHMziqm41Mls-Z5XP8b7mBhVcd8PDes8-G8zE7I/edit#gid=828701554")

rooster <- gsheet2tbl(url)
# clear rows 1 to 6
rooster <- rooster[-c(1:7), ]
colnames(rooster)[1] <- "Datum"
colnames(rooster)[2] <- "Dag"
# select columns with names of assistenten
rooster <- rooster %>% 
    select(contains(as.character(assistenten$assistent))) %>% 
    dplyr::select(!contains("..")) %>% # clean names
    dplyr::select(!contains("_")) %>% # clean names
  mutate(Datum=rooster$Datum,Dag=rooster$Dag) %>% 
  select(Datum,Dag,everything()) %>% 
  filter(!is.na(Datum)) # remove empty dates


## working days per assistant
# rooster <- rooster[, (colnames(rooster) %in% assistenten$assistent)]
# listass <- list(assistenten$assistent)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run the app
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(
                   # classic app
                   headerPanel('Roosterchecker'),
                   # Sidebar with a slider input for number of bins 
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("search","Selecteer of typ je naam",
                                   choice = c('Je naam' = '', 
                                              as.character(assistenten$assistent)), 
                                   multiple = FALSE,
                                   selected = NULL)
                     ),
                     
                     # Show a table
                     mainPanel(
                       # conditionalPanel(
                       #   condition = "input.search != NULL",
                       dataTableOutput('table')
                       #verbatimTextOutput("res_auth")
                   #)
                   )
                   
                 )))

# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Rooster"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             selectInput("search","Selecteer of typ je naam",
#                         choice = c('Je naam' = '', 
#                                    as.character(assistenten$assistent)), 
#                         multiple = FALSE,
#                         selected = NULL)
#         ),
# 
#         # Show a table
#         mainPanel(
#             #DT::dataTableOutput('x1'),
#             dataTableOutput('table')
#            
#     )
# )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
  # first authenticate
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  output$res_auth <- renderPrint({reactiveValuesToList(result_auth)})
  
  # then the app:
    #name <- reactive(input$search)
    name <- reactive({
        validate(
            need(input$search != "", "Selecteer of typ een naam.")
        )
        input$search
    })
    rooster2 <- reactive(rooster[, c('Datum', 'Dag',name())])
    output$table <- renderDataTable(
        DT::datatable(rooster2(),
                      options = list(scrollY = '600px', paging = FALSE,ordering=F),rownames= FALSE)
        
        )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
