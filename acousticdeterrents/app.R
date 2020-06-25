library(shiny)
library(tidyverse)
library(here)
library(kableExtra)
library(formattable)
# det.name (for report)
# det.char (for report)
# contact (for report)

# frequency <- 1 #kHz
# max.loudness <- 174 #dB
# max.ping.dur <- 9 #sec
# min.silent <- 4 #sec
# propagation <- 15
publishing = T


if(publishing){
    source(here('R','Functions.R'))
    source(here('R','Constants.R'))
}else{
    source(here('acousticdeterrents','R','Functions.R'))
    source(here('acousticdeterrents','R','Constants.R'))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Acoustic deterrents"),
    navbarPage(title = "",id="inTabset",
               # Sidebar with a slider input for number of bins 
               tabPanel("Single frequency",
                        sidebarLayout(
                            sidebarPanel(
                                numericInput("frequency",
                                             "DETERRENT PITCH (FREQUENCY) in kilohertz (kHz)",
                                             value = 1),
                                numericInput("max.loudness",
                                             "Maximum loudness in decibels (average source level at 1 m from source in RMS SPL)", value = 174),
                                numericInput("max.ping.dur",
                                             "Maximum ping duration (seconds)",
                                             value = 9),
                                numericInput("min.silent",
                                             "Minimum silent period between pings (seconds)",
                                             value=4)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                tableOutput("single.freq")
                            )
                        )),
               tabPanel("Multiple frequencies",
                        sidebarLayout(
                            sidebarPanel(
                                numericInput("frequency.lowest",
                                             "DETERRENT LOWEST PITCH VALUE (FREQUENCY) in kilohertz (kHz)",
                                             value = 0.5),
                                numericInput("frequency.highest",
                                             "DETERRENT HIGHEST PITCH VALUE (FREQUENCY) in kilohertz (kHz)",
                                             value = 4),
                                numericInput("max.loudness.m",
                                             "Maximum loudness in decibels (average source level at 1 m from source in RMS SPL)", value = 174),
                                numericInput("max.ping.dur.m",
                                             "Maximum ping duration (seconds)",
                                             value = 9),
                                numericInput("min.silent.m",
                                             "Minimum silent period between pings (seconds)",
                                             value=4)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                tableOutput("multiple.freq")
                            )
                        )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Dervied variables
    duty.cycle <- reactive(input$max.ping.dur/(input$max.ping.dur+input$min.silent))
    dur <- reactive(duty.cycle() * 3600)
    log10dur <- reactive(log10(dur())*10)
    
    output$single.freq <- function(){
        
        result.table.s <- m %>%
            mutate(isopleth = get.isopleth(max.loudness = input$max.loudness,
                                           adjustment = get.adjustment(freq = input$frequency,
                                                                       wtpars = list(a=a,b=b,f1=f1,f2=f2,C=C)),
                                           log10dur = log10dur(),
                                           selcumthresh = SELcum,
                                           propagation = propagation))
        
        # in shiny, add conditional formatting to the table
        user.table.s <- result.table.s %>%
            select(hearing.group,isopleth)
        
        user.table.s %>%
            mutate(isopleth = round(isopleth,digits=2)) %>%
            mutate(isopleth = ifelse(isopleth>100,
                                     cell_spec(isopleth,background="red",color="white",bold=T),
                                     cell_spec(isopleth, background="green",color="white",bold=T))) %>%
            kable(escape=FALSE) %>% 
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
        
    }
    
    output$multiple.freq <- function(){
        
        by.hearing <- data.frame(hearing.group = hearing.group,
                                 x = c(1.7,28,42,6.2,4.9)) %>% 
            mutate(freq.by.group = ifelse(x>=input$frequency.lowest & x <=input$frequency.highest,x,
                                          ifelse(x>input$frequency.lowest & x>input$frequency.highest,input$frequency.highest, # this is the same as just saying x>frequency.highest, right? Check w Amy
                                                 input$frequency.lowest)))
        
        # Use m from the single freq case ------------------------------
        mm <- m %>%
            full_join(by.hearing)
        
        result.table.m <- mm %>%
            mutate(isopleth = get.isopleth(max.loudness = input$max.loudness.m,
                                           adjustment = get.adjustment(freq = freq.by.group,
                                                                       wtpars = list(a=a,b=b,f1=f1,f2=f2,C=C)),
                                           log10dur = log10dur(),
                                           selcumthresh = SELcum,
                                           propagation = propagation))
        
        user.table.m <- result.table.m %>% 
            select(hearing.group,isopleth)
        
        user.table.m %>%
            mutate(isopleth = round(isopleth,digits=2)) %>%
            mutate(isopleth = ifelse(isopleth>100,
                                     cell_spec(isopleth,background="red",color="white",bold=T),
                                     cell_spec(isopleth, background="green",color="white",bold=T))) %>%
            kable(escape=FALSE) %>% 
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
