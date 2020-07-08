

library(shiny)
library(tidyverse)
library(here)
library(kableExtra)
library(formattable)
library(shinyBS) # for tooltips
library(shinythemes) # for theme
library(bootstraplib) # for custom NOAA colors theme

# Still need to add
# det.name (for report)
# det.char (for report)
# contact (for report)


publishing = TRUE


if(publishing){
    source(here('R','Functions.R'))
    source(here('R','Constants.R'))
    source(here('R','Themes.R'))
}else{
    source(here('acousticdeterrents','R','Functions.R'))
    source(here('acousticdeterrents','R','Constants.R'))
    source(here('acousticdeterrents','R','Themes.R'))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    #theme = "noaatheme_mcs.css",
    titlePanel(div(column(width = 12,
                          tags$img(src = "NOAA_logo.svg",width="60",height="60",align="right")))),
    br(),
    br(),
    bootstrap(),
    # Application title
    titlePanel("Acoustic deterrents"),
    navbarPage(title = "",id="inTabset",
               tabPanel("Introduction",
                        h2("Introduction"),
                        p(intro.text),
                        br(),
                        h3("Which tab should I use?"),
                        p("For devices with a single frequency (pitch, kHz), such as some standard pingers and underwater speakers, please use the 'Single frequency' tab For a device with a range of frequencies, such as startle devices and transducers, please use the 'Multiple frequencies' tab."),
                        p("If you have a device that is impulsive (i.e., capable of producing high sound levels in a short duration, broadband (multiple frequencies), please consult the NMFS guidelines for safely deterring marine mammals.Examples of impulsive acoustic deterrents include power pulsed devices, low frequency broadband devices, and explosives. "),
                        br(),
                        
                        h3("Once you determine which calculator to use"),
                        tags$ol(
                            tags$li(" Complete fields with the device's specifications"),
                            tags$li(HTML(paste("Output cells will turn ", tags$span(style="color:green", "green"), sep = "")), "if approved for use under NMFS's National Guidelines for Deterring Marine Mammals and a certificate will be provided."),
                            tags$li("The cells will contain the distance (meters) to onset of permanent hearing loss by each marine mammal hearing group."),
                            tags$li("You are required to deploy the device at least as far away as noted in the cell."),
                            tags$li(HTML(paste("Output cells will turn ", tags$span(style="color:red", "red"), sep = ""))," if the device does not meet NMFS's evaluation criteria; this device MAY NOT BE USED to deter marine mammals and a certificate will not be issued.")
                        ),
                        br(),
                        
                        h3("If your device is programmable"),
                        p("Insert the specifications for how you wish to use the device.  If those specifications do not meet NMFS’ evaluation criteria, you may adjust the inputs to determine whether a different combination of specifications would be approved (e.g., using a lower source level, lower duty cycle, increasing the minimum silent interval between acoustic signals, or decreasing the maximum acoustic signal duration will reduce the output distance)."),
                        br(),
                        
                        h3("NMFS Evaluation Criteria"),
                        tags$ol(
                            tags$li("Acoustic deterrents that have the potential to result in the onset of a permanent threshold shift (PTS) at distances > 100 m from the source after an hour of exposure would not be approved."),
                            tags$li("PTS onset thresholds are based on the", a("2018 revision to Technical Guidance for Assessing the Effects of Anthropogenic Sound on Marine Mammal Hearing (Version 2.0)",href=" https://www.fisheries.noaa.gov/national/marine-mammal-protection/marine-mammal-acoustic-technical-guidance")))
                        
               ),
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
                                radioButtons(inputId = 'duty_cycle_decide',
                                             label = "Do you have a duty cycle to enter?",
                                             choices = c("Yes"="direct",
                                                         "No, calculate duty cycle from signal duration and silent period" = "calculate")),
                                bsTooltip("duty_cycle_decide", duty.cycle.note),
                                conditionalPanel(condition = "input.duty_cycle_decide=='direct'",
                                                 numericInput("duty_cycle_manual",
                                                              label = "Duty cycle (%):",value = 50, min = 1,max = 100)),
                                conditionalPanel(condition = "input.duty_cycle_decide=='calculate'",
                                                 numericInput("max_ping_dur",
                                                              "Maximum acoustic signal duration (seconds)",
                                                              value = 9),
                                                 bsTooltip("max_ping_dur", duration.note),
                                                 numericInput("min_silent",
                                                              "Minimum silent period between acoustic signals (seconds)",
                                                              value=4)
                                                 )
                                
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
                                radioButtons(inputId = 'duty_cycle_decide_m',
                                             label = "Do you have a duty cycle to enter?",
                                             choices = c("Yes"="direct",
                                                         "No, calculate duty cycle from signal duration and silent period" = "calculate")),
                                bsTooltip("duty_cycle_decide_m", duty.cycle.note),
                                conditionalPanel(condition = "input.duty_cycle_decide_m=='direct'",
                                                 numericInput("duty_cycle_manual_m",
                                                              label = "Duty cycle (%):",value = 50, min = 1,max = 100)
                                ),
                                conditionalPanel(condition = "input.duty_cycle_decide_m=='calculate'",
                                                 numericInput("max_ping_dur_m",
                                                              "Maximum acoustic signal duration (seconds)",
                                                              value = 9),
                                                 bsTooltip("max_ping_dur_m", duration.note),
                                                 numericInput("min_silent_m",
                                                              "Minimum silent period between acoustic signals (seconds)",
                                                              value=4)
                                                 )
                                
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                tableOutput("multiple.freq")
                            )
                        )
               ),
               tabPanel("Create a certificate",
                        h3("Create a certificate for your device"),
                        textInput("name",
                                  "Name:", value = "Enter your name"),
                        textInput("devicename",
                                  "Deterrent name:", value = "Enter the name of your deterrent"),
                        textAreaInput("characteristics","Deterrent characteristics, including assumptions:"),
                        radioButtons("whichfreq","Type of frequency",choices = c("Single","Multiple")),
                        downloadButton(outputId = "cert", "Generate certificate")) #nd of certificate tab
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # Dervied variables
    duty.cycle.direct <- reactive(input$duty_cycle_manual/100)
    duty.cycle.calculate <- reactive(input$max_ping_dur/(input$max_ping_dur + input$min_silent))
    duty.cycle <- reactive(ifelse(input$duty_cycle_decide=="direct",duty.cycle.direct(),duty.cycle.calculate()))
    
    dur <- reactive(duty.cycle() * 3600)
    log10dur <- reactive(log10(dur())*10)
    
    duty.cycle_m.direct <- reactive(input$duty_cycle_manual_m/100)
    duty.cycle_m.calculate <- reactive(input$max_ping_dur_m/(input$max_ping_dur_m + input$min_silent_m))
    duty.cycle_m <- reactive(ifelse(input$duty_cycle_decide_m=="direct",duty.cycle_m.direct(),duty.cycle_m.calculate()))
    
    dur_m <- reactive(duty.cycle_m() * 3600)
    log10dur_m <- reactive(log10(dur_m())*10)
    
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
            rename(`Distance (meters)` = isopleth) %>%
            rename(`Hearing group` = hearing.group) %>%
            mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
            mutate('Meets criteria' = ifelse(`Distance (meters)`>100,"No","<b> Yes </b>")) %>%
            mutate(`Distance (meters)` = round(`Distance (meters)`,digits=2)) %>%
            mutate(`Distance (meters)` = ifelse(`Distance (meters)`>100,
                                                cell_spec(`Distance (meters)`,background="red",color="white",bold=T),
                                                cell_spec(`Distance (meters)`, background="green",color="white",bold=T))) %>%
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
                                           log10dur = log10dur_m(),
                                           selcumthresh = SELcum,
                                           propagation = propagation))
        
        user.table.m <- result.table.m %>% 
            select(hearing.group,isopleth) %>%
            mutate(isopleth = round(isopleth,digits=2)) %>%
            rename(`Distance (meters)` = isopleth) %>%
            rename(`Hearing group` = hearing.group) %>%
            mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
            mutate('Meets criteria' = ifelse(`Distance (meters)`>100,"No","<b> Yes </b>")) %>%
            mutate(`Distance (meters)` = ifelse(`Distance (meters)`>100,
                                                cell_spec(`Distance (meters)`,background="red",color="white",bold=T),
                                                cell_spec(`Distance (meters)`, background="green",color="white",bold=T))) %>%
            kable(escape=FALSE) %>% 
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    
    output$cert <- downloadHandler(
        filename = "certificate.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "certificate.Rmd")
            file.copy("certificate.Rmd", tempReport, overwrite = TRUE)
            
            # 
            # 
            #if(input$whichfreq == "Single"){
                params <- list(frequency = input$frequency,
                               max_loudness = input$max_loudness, # also called "source level"
                               max_ping_dur = input$max_ping_dur,
                               min_silent = input$min_silent,
                               name = input$name,
                               devicename = input$devicename,
                               characteristics = input$characteristics)
            #}else{ # if input$whichfreq == "Multiple"
                # params <- list(frequency = c(input$frequency_lowest,input$frequency_highest),
                #                max_loudness_m = input$max_loudness_m, # also called "source level"
                #                max_ping_dur_m = input$max_ping_dur_m,
                #                min_silent_m = input$min_silent_m,
                #                name = input$name,
                #                devicename = input$devicename,
                #                characteristics = input$characteristics)
            #}
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
