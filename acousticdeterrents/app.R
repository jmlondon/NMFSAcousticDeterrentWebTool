library(shiny)
library(tidyverse)
library(here)
library(kableExtra)
library(formattable)
library(shinyBS) # for tooltips
library(shinythemes) # for theme

# Still need to add
# det.name (for report)
# det.char (for report)
# contact (for report)

publishing = F


if(publishing){
    source(here('R','Functions.R'))
    source(here('R','Constants.R'))
}else{
    source(here('acousticdeterrents','R','Functions.R'))
    source(here('acousticdeterrents','R','Constants.R'))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("darkly"),
    
    # Application title
    titlePanel("Acoustic deterrents"),
    navbarPage(title = "",id="inTabset",
               tabPanel("Introduction",
                        h2("Introduction"),
                        p(intro.text),
                        br(),
                        h3("Which tab should I use?"),
                        p("For devices with a single pitch (frequency, kHz), such as a pinger, please use the 'Single frequency' tab For a device with a range of frequencies, please use the 'Multiple frequencies' tab.")),
               tabPanel("Single frequency",
                        sidebarLayout(
                            sidebarPanel(
                                numericInput("frequency",
                                             "DETERRENT PITCH (FREQUENCY) in kilohertz (kHz)",
                                             value = 1),
                                bsTooltip("frequency", frequency.note),
                                numericInput("max_loudness",
                                             "Maximum loudness in decibels (average source level at 1 m from source in RMS SPL)", value = 174),
                                bsTooltip("max_loudness", loudness.note),
                                numericInput("max_ping_dur",
                                             "Maximum ping duration (seconds)",
                                             value = 9),
                                bsTooltip("max_ping_dur", duration.note),
                                numericInput("min_silent",
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
                                numericInput("frequency_lowest",
                                             "DETERRENT LOWEST PITCH VALUE (FREQUENCY) in kilohertz (kHz)",
                                             value = 0.5),
                                bsTooltip("frequency_lowest", frequency.note),
                                numericInput("frequency_highest",
                                             "DETERRENT HIGHEST PITCH VALUE (FREQUENCY) in kilohertz (kHz)",
                                             value = 4),
                                bsTooltip("frequency_highest", frequency.note),
                                numericInput("max_loudness_m",
                                             "Maximum loudness in decibels (average source level at 1 m from source in RMS SPL)", value = 174),
                                bsTooltip("max_loudness_m", loudness.note),
                                numericInput("max_ping_dur_m",
                                             "Maximum ping duration (seconds)",
                                             value = 9),
                                bsTooltip("max_ping_dur_m", duration.note),
                                numericInput("min_silent_m",
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
    duty.cycle <- reactive(input$max_ping_dur/(input$max_ping_dur+input$min_silent))
    dur <- reactive(duty.cycle() * 3600)
    log10dur <- reactive(log10(dur())*10)
    
    output$single.freq <- function(){
        
        result.table.s <- m %>%
            mutate(isopleth = get.isopleth(max.loudness = input$max_loudness,
                                           adjustment = get.adjustment(freq = input$frequency,
                                                                       wtpars = list(a=a,b=b,f1=f1,f2=f2,C=C)),
                                           log10dur = log10dur(),
                                           selcumthresh = SELcum,
                                           propagation = propagation))
        
        # in shiny, add conditional formatting to the table
        user.table.s <- result.table.s %>%
            select(hearing.group,isopleth)
        
        user.table.s %>%
            rename(`Isopleth (meters)` = isopleth) %>%
            rename(`Hearing group` = hearing.group) %>%
            mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
            mutate(`Isopleth (meters)` = round(`Isopleth (meters)`,digits=2)) %>%
            mutate(`Isopleth (meters)` = ifelse(`Isopleth (meters)`>100,
                                     cell_spec(`Isopleth (meters)`,background="red",color="white",bold=T),
                                     cell_spec(`Isopleth (meters)`, background="green",color="white",bold=T))) %>%
            kable(escape=FALSE) %>% 
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    
    output$multiple.freq <- function(){
        
        by.hearing <- data.frame(hearing.group = hearing.group,
                                 x = c(1.7,28,42,6.2,4.9)) %>% 
            mutate(freq.by.group = ifelse(x>=input$frequency_lowest & x <=input$frequency_highest,x,
                                          ifelse(x>input$frequency_lowest & x>input$frequency_highest,input$frequency_highest, # this is the same as just saying x>frequency_highest, right? Check w Amy
                                                 input$frequency_lowest)))
        
        # Use m from the single freq case ------------------------------
        mm <- m %>%
            full_join(by.hearing)
        
        result.table.m <- mm %>%
            mutate(isopleth = get.isopleth(max.loudness = input$max_loudness_m,
                                           adjustment = get.adjustment(freq = freq.by.group,
                                                                       wtpars = list(a=a,b=b,f1=f1,f2=f2,C=C)),
                                           log10dur = log10dur(),
                                           selcumthresh = SELcum,
                                           propagation = propagation))
        
        user.table.m <- result.table.m %>% 
            select(hearing.group,isopleth) %>%
            mutate(isopleth = round(isopleth,digits=2)) %>%
            rename(`Isopleth (meters)` = isopleth) %>%
            rename(`Hearing group` = hearing.group) %>%
            mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
            mutate(`Isopleth (meters)` = ifelse(`Isopleth (meters)`>100,
                                     cell_spec(`Isopleth (meters)`,background="red",color="white",bold=T),
                                     cell_spec(`Isopleth (meters)`, background="green",color="white",bold=T))) %>%
            kable(escape=FALSE) %>% 
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
