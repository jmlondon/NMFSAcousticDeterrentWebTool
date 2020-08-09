

library(shiny)
library(tidyverse)
library(here)
library(kableExtra)
library(formattable)
library(shinyBS) # for tooltips
library(shinythemes) # for theme
library(shinyWidgets)


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
                title = "NMFS Acoustic Deterrent Web Tool",
                titlePanel(div(column(width = 12,
                           tags$img(src = "NOAA_logo.svg",width="60",height="60",align="right")))),
  br(),
  br(),
  titlePanel("National Marine Fisheries Service (NMFS) Acoustic Deterrent Web Tool"), 
  #bootstrap(),
  # Application title
  
  navbarPage(title = "",
             id="inTabset",
             tabPanel("Introduction",
                      h2("Introduction"),
                      HTML(preintro.text),
                      br(),
                      br(),
                      HTML(intro.text),
                      br(),
                      h3("Which tab should I use?"),
                      p("For devices with a single frequency (pitch, kHz), such as some standard pingers and underwater speakers, please use the 'Single frequency' tab:"),
                      actionBttn(
                        inputId = "single_tab",
                        label = "I have a single-frequency device",
                        color = "primary",
                        style = "pill",
                        icon = icon("sliders"),
                        size="sm",
                        block = TRUE
                      ),
                      br(),
                      br(),
                      p("For a device with a range of frequencies, such as startle devices and transducers, please use the 'Multiple frequencies' tab:"),
                      actionBttn(
                        inputId = "multi_tab",
                        label = "I have a multiple-frequency device",
                        color = "primary",
                        style = "pill",
                        icon = icon("sliders"),
                        size="sm",
                        block = TRUE
                      ),
                      br(),
                      br(),
                      p("If you have a device that is impulsive (i.e., capable of producing high sound levels in a short duration, broadband [multiple frequencies]), please consult the NMFS guidelines for safely deterring marine mammals. Examples of impulsive acoustic deterrents include power pulsed devices, low frequency broadband devices, and explosives."),
                      br(),
                      
                      h3("Once you determine which calculator to use"),
                      tags$ol(
                        tags$li("Complete fields with the device's specifications"),
                        tags$li(HTML(paste("Output cells will turn ", tags$span(style="color:green", "green"), sep = "")), "if your device meets NMFS's evaluation criteria for deterring marine mammals and a certificate will be provided."),
                        tags$li("The cells will contain the distance (meters) to onset of permanent hearing loss by each marine mammal hearing group."),
                        tags$li("You are required to deploy the device at least as far away as noted in the cell."),
                        tags$li(HTML(paste("Output cells will turn ", tags$span(style="color:red", "red"), sep = ""))," if the device does not meet NMFS's evaluation criteria."),
                        tags$li("If your device meets NMFS's evaluation criteria for all hearing groups, a certificate documenting the device's specifications will be generated.  Certificates are valid for 1 year from date of issue.")
                      ),
                      br(),
                      
                      h3("If your device is programmable"),
                      p("Insert the specifications for how you wish to use the device.  If those specifications do not meet NMFS’ evaluation criteria, you may adjust the inputs to determine whether a different combination of specifications would meet NMFS's evaluation criteria (e.g., using a lower source level, lower duty cycle, increasing the minimum silent interval between acoustic signals, or decreasing the maximum acoustic signal duration will reduce the output distance)."),
                      br(),
                      
                      h3("NMFS Evaluation Criteria"),
                      tags$ol(
                        tags$li("Acoustic deterrents that have the potential to result in the onset of a permanent threshold shift (PTS) at distances ≥100 m from the source after an hour of exposure would not meet NMFS's evaluation criteria."),
                        tags$li("PTS onset thresholds are based on the", a("2018 revision to Technical Guidance for Assessing the Effects of Anthropogenic Sound on Marine Mammal Hearing (Version 2.0)",href=" https://www.fisheries.noaa.gov/national/marine-mammal-protection/marine-mammal-acoustic-technical-guidance")))
                      
             ),
             tabPanel("Single frequency",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("frequency",
                                       "Deterrent frequency (pitch) in kilohertz (kHz)",
                                       value = 1),
                          bsTooltip("frequency", frequency.note),
                          numericInput("max_loudness",
                                       "Maximum loudness in decibels (dB; average source level at 1 m from source in RMS SPL)", value = 174),
                          bsTooltip("max_loudness", loudness.note),
                          radioButtons(inputId = 'duty_cycle_decide',
                                       label = "Do you have a duty cycle to enter?",
                                       choices = c("Yes"="direct",
                                                   "No, calculate duty cycle from signal duration and silent period" = "calculate")),
                          conditionalPanel(condition = "input.duty_cycle_decide=='direct'",
                                           numericInput("duty_cycle_manual",
                                                        label = "Duty cycle (%):",value = 50, min = 1,max = 100),
                                           bsTooltip("duty_cycle_manual", duty.cycle.note)),
                          
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
                          h3("Does your device meet NMFS criteria?"),
                          tableOutput("single.freq"),
                          HTML(criteria.note),
                          br(),
                          br(),
                          ########## Certificate 
                          h3("Create a certificate for your device"),
                          h4("Enter device information"),
                          fluidRow(
                            column( width = 6,
                                    textInput("name",
                                              "Name of person using acoustic deterrent:", value = "Enter your name"),
                                    textInput("devicename",
                                              "Deterrent name (including manufacturer):", value = "Enter the name of your deterrent")
                            ), # end of col 1
                            column(width = 6,
                                   textAreaInput("characteristics","Deterrent characteristics, including assumptions:") )
                          ),
                          
                          h4("What species are you trying to deter?"),
                          fluidRow(
                            column(width = 4,
                                   checkboxGroupInput("seals.sealionslist",
                                                      label = "Seals and sea lions",
                                                      choices = as.list(seals.sealions)),
                                   checkboxInput(inputId = "otherseals",label = "Other seals"),
                                   conditionalPanel(condition = "input.otherseals",
                                                    textInput(inputId = 'seals_other',
                                                              label = "Please enter which seal species you want to deter")),
                                   checkboxInput(inputId = "othersealions",label = "Other sea lions"),
                                   conditionalPanel(condition = "input.othersealions",
                                                    textInput(inputId = 'sealions_other',
                                                              label = "Please enter which sea lion species you want to deter"))
                            ),
                            column(width = 4,
                                   checkboxGroupInput("dolphins.porpoiseslist",
                                                      label = "Dolphins and porpoises",
                                                      choices = as.list(dolphins.porpoises))
                            ),
                            column(width = 4,
                                   checkboxGroupInput("toothed.baleenlist",
                                                      label = "Toothed and baleen whales",
                                                      choices = as.list(toothed.baleen)),
                                   checkboxInput(inputId = "othertoothed",label = "Other toothed whales"),
                                   conditionalPanel(condition = "input.othertoothed",
                                                    textInput(inputId = 'toothed_other',label = "Please enter which toothed whale species you want to deter")),
                                   checkboxInput(inputId = "otherbaleen",label = "Other baleen whales"),
                                   conditionalPanel(condition = "input.otherbaleen",
                                                    textInput(inputId = 'baleen_other',label = "Please enter which baleen whale species you want to deter")),
                            ),
                            column(width = 12, 
                                   textOutput("deterlist"),
                                   br(),
                                   htmlOutput(outputId = 'warningmessage'),
                                   conditionalPanel(
                                     condition = "output.panelStatus",
                                      downloadBttn(outputId = "cert",label = "Download certificate",
                                                color = "success",
                                                style = "pill",
                                                size="md",
                                                block = TRUE)
                                   ),
                                   br(),
                                   br()
                            )
                          ) # end fluidRow
                          
                        ) # end mainpanel
                        
                      ) # end sidebarpanel layout
             ), # end single frequency tab
             tabPanel("Multiple frequencies",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("frequency_lowest",
                                       "Deterrent lowest frequency (pitch) in kilohertz (kHz)",
                                       value = 0.5),
                          bsTooltip("frequency_lowest", frequency.note),
                          numericInput("frequency_highest",
                                       "Deterrent highest frequency (pitch) in kilohertz (kHz)",
                                       value = 4),
                          bsTooltip("frequency_highest", frequency.note),
                          numericInput("max_loudness_m",
                                       "Maximum loudness in decibels (dB; average source level at 1 m from source in RMS SPL)", value = 174),
                          bsTooltip("max_loudness_m", loudness.note),
                          radioButtons(inputId = 'duty_cycle_decide_m',
                                       label = "Do you have a duty cycle to enter?",
                                       choices = c("Yes"="direct",
                                                   "No, calculate duty cycle from signal duration and silent period" = "calculate")),
                          conditionalPanel(condition = "input.duty_cycle_decide_m=='direct'",
                                           numericInput("duty_cycle_manual_m",
                                                        label = "Duty cycle (%):",value = 50, min = 1,max = 100),
                                           bsTooltip("duty_cycle_manual_m", duty.cycle.note)
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
                          h3("Does your device meet NMFS criteria?"),
                          tableOutput("multiple.freq"),
                          HTML(criteria.note),
                          br(),
                          br(),
                          ########## Certificate 
                          h3("Create a certificate for your device"),
                          h4("Enter device information"),
                          fluidRow(
                            column( width = 6,
                                    textInput("name2",
                                              "Name of person using acoustic deterrent:", value = "Enter your name"),
                                    textInput("devicename2",
                                              "Deterrent name (including manufacturer):", value = "Enter the name of your deterrent")
                            ), # end of col 1
                            column(width = 6,
                                   textAreaInput("characteristics2","Deterrent characteristics, including assumptions:") )
                          ),
                          
                          h4("What species are you trying to deter?"),
                          fluidRow(
                            column(width = 4,
                                   checkboxGroupInput("seals.sealionslist2",
                                                      label = "Seals and sea lions",
                                                      choices = as.list(seals.sealions)),
                                   checkboxInput(inputId = "otherseals2",
                                                 label = "Other seals"),
                                   conditionalPanel(condition = "input.otherseals2",
                                                    textInput(inputId = 'seals_other2',
                                                              label = "Please enter which seal species you want to deter")),
                                   checkboxInput(inputId = "othersealions2",label = "Other sea lions"),
                                   conditionalPanel(condition = "input.othersealions2",
                                                    textInput(inputId = 'sealions_other2',
                                                              label = "Please enter which sea lion species you want to deter"))
                            ),
                            column(width = 4,
                                   checkboxGroupInput("dolphins.porpoiseslist2",
                                                      label = "Dolphins and porpoises",
                                                      choices = as.list(dolphins.porpoises))
                            ),
                            column(width = 4,
                                   checkboxGroupInput("toothed.baleenlist2",
                                                      label = "Toothed and baleen whales",
                                                      choices = as.list(toothed.baleen)),
                                   checkboxInput(inputId = "othertoothed2",
                                                 label = "Other toothed whales"),
                                   conditionalPanel(condition = "input.othertoothed2",
                                                    textInput(inputId = 'toothed_other2',
                                                              label = "Please enter which toothed whale species you want to deter")),
                                   checkboxInput(inputId = "otherbaleen2",label = "Other baleen whales"),
                                   conditionalPanel(condition = "input.otherbaleen2",
                                                    textInput(inputId = 'baleen_other2',
                                                              label = "Please enter which baleen whale species you want to deter")),
                            ),
                            column(width = 12, 
                                   textOutput("deterlist2"),
                                   br(),
                                   htmlOutput(outputId = 'warningmessage2'),
                                   conditionalPanel(
                                     condition = "output.panelStatus2",
                                     downloadBttn(outputId = "cert2",label = "Generate certificate",
                                                  color = "success",
                                                  style = "pill",
                                                  size="md",
                                                  block = TRUE)
                                   ),
                                   
                                   br(),
                                   br()
                            )
                          ) # end fluidRow
                          
                        ) # end mainpanel
                      ) # end sidebarlayout format
             ), #end tab
             tabPanel("Help",
                      h3("Useful conversions"),
                      p("Here are unit conversions that may be helpful in finding and entering your device' information's specifications."),
                      h4("Frequency"),
                      p(frequency.note),
                      h4("Pulse duration"),
                      p(duration.note),
                      br(),
                      br(),
                      h3("Where are the specs for my device located?"),
                      p("Here are several examples of device specifications showing where to find the information you need to enter in this web tool."),
                      br(),
                      h4("Example #1"),
                      tags$img(src = 'images/Slide1.png',width = 800),
                      br(),
                      h4("Example #2"),
                      tags$img(src = 'images/Slide2.png',width = 800),
                      br(),
                      h4("Example #3"),
                      tags$img(src = 'images/Slide3.png',width = 800),
                      br(),
                      h4("Example #4"),
                      tags$img(src = 'images/Slide4.png',width = 800),
                      br()
                      )

  ) # end navbarpage
) # end ui

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Links to other tabs
  observeEvent(input$single_tab, {
    updateTabsetPanel(session, "inTabset", selected = "Single frequency")
  })
  observeEvent(input$multi_tab, {
    updateTabsetPanel(session, "inTabset", selected = "Multiple frequencies")
  })
  
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
      rename(`Hearing group` = hearing.group) %>%
      mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
      mutate('Meets criteria' = ifelse(isopleth>=100,"&#10006 <b> NO </b>","&#10004 Yes")) %>%
      mutate(isopleth = round(isopleth,digits = 1)) %>%
      mutate(isopleth = ifelse(isopleth>=100,
                                          cell_spec(isopleth,background="red",color="white",bold=T),
                                          cell_spec(isopleth, background="green",color="white",bold=T))) %>%
      rename(`Minimum Distance(s) for Deploying Deterrent from Marine Mammal (meters)` = isopleth) %>%
      kable(escape=FALSE) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  output$multiple.freq <- function(){
    by.hearing <- data.frame(hearing.group = hearing.group,
                             x = c(1.7,28,42,6.2,4.9)) %>% 
      mutate(freq.by.group = ifelse(x>=input$frequency_lowest & x <=input$frequency_highest,x,
                                    ifelse(x>input$frequency_lowest & x>input$frequency_highest,input$frequency_highest,
                                           input$frequency_lowest)))
    
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
      mutate(isopleth = round(isopleth,digits=1)) %>%
      rename(`Hearing group` = hearing.group) %>%
      mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
      mutate(`Minimum Distance(s) for Deploying Deterrent from Marine Mammal (meters)` = ifelse(isopleth>=100,
                                          cell_spec(isopleth,background="red",color="white",bold=T),
                                          cell_spec(isopleth, background="green",color="white",bold=T))) %>%
      mutate('Meets criteria' = ifelse(isopleth>=100,"&#10006 <b> NO </b>","&#10004 Yes")) %>%
      
      select(-isopleth) %>%
      kable(escape=FALSE) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  
  isopleth.table.out <- reactive( #single freq
    m %>%
      mutate(isopleth = get.isopleth(max.loudness = input$max_loudness,
                                     adjustment = get.adjustment(freq = input$frequency,
                                                                 wtpars = list(a=a,b=b,f1=f1,f2=f2,C=C)),
                                     log10dur = log10dur(),
                                     selcumthresh = SELcum,
                                     propagation = propagation)) %>%
      select(hearing.group,isopleth) %>%
      mutate(isopleth = round(isopleth,digits=1)) %>%
      #rename(`Distance (meters)` = isopleth) %>%
      rename(`Hearing group` = hearing.group) %>%
      mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
      mutate('Meets criteria' = ifelse(isopleth>=100,"&#10006 <b> NO </b>","&#10004 Yes"))
  )
  
  
  isopleth.table.out2 <- reactive({ #multiple freq
    #print(any(isopleth.table.out()$`Distance (meters)`>=100))
    
    
    by.hearing <- data.frame(hearing.group = hearing.group,
                             x = c(1.7,28,42,6.2,4.9)) %>% 
      mutate(freq.by.group = ifelse(x>=input$frequency_lowest & x <=input$frequency_highest,x,
                                    ifelse(x>input$frequency_lowest & x>input$frequency_highest,input$frequency_highest,
                                           input$frequency_lowest)))
    
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
      mutate(isopleth = round(isopleth,digits=1)) %>%
      #rename(`Distance (meters)` = isopleth) %>%
      rename(`Hearing group` = hearing.group) %>%
      mutate(`Hearing group` = recode(`Hearing group`,!!!hgkey)) %>%
      mutate('Meets criteria' = ifelse(isopleth>=100,"&#10006 <b> NO </b>","&#10004 Yes"))
    
      user.table.m
  }
  )
  
  dlist <- reactive({
    spps <- paste(c(input$seals.sealionslist,
                    input$dolphins.porpoiseslist,
                    input$toothed.baleenlist),
                  collapse = ", ")
    
    baleen <- ifelse(input$otherbaleen,input$baleen_other,NA)
    toothed <- ifelse(input$othertoothed,input$toothed_other,NA)
    seal <- ifelse(input$otherseals,input$seals_other,NA)
    sealion <- ifelse(input$othersealions,input$sealions_other,NA)
    x <- c(seal,sealion,baleen,toothed)
    otherspps <- paste(x[complete.cases(x)],collapse=", ")
    
    paste(spps,otherspps,sep=', ')
  })
  
  dlist2 <- reactive({ # for multispecies tab
    spps <- paste(c(input$seals.sealionslist2,
                    input$dolphins.porpoiseslist2,
                    input$toothed.baleenlist2),
                  collapse = ", ")
    
    baleen <- ifelse(input$otherbaleen2,input$baleen_other2,NA)
    toothed <- ifelse(input$othertoothed2,input$toothed_other2,NA)
    seal <- ifelse(input$otherseals2,input$seals_other2,NA)
    sealion <- ifelse(input$othersealions2,input$sealions_other2,NA)
    x <- c(seal,sealion,baleen,toothed)
    otherspps <- paste(x[complete.cases(x)],collapse=", ")
    
    paste(spps,otherspps,sep=', ')
  })
  
  
  output$deterlist <- renderText({
    paste("Species chosen:",dlist())
  })
  
  output$deterlist2 <- renderText({
    paste("Species chosen:",dlist2())
  })
  
  
  # observe({
  #   if (input$start_proc > 0) {
  #     Sys.sleep(1)
  #     # enable the download button
  #     shinyjs::enable("data_file")
  #     # change the html of the download button
  #     shinyjs::html("data_file",
  #                   sprintf("<i class='fa fa-download'></i>
  #                             Download (file size: %s)",
  #                           round(runif(1, 1, 10000))
  #                   )
  #     )
  #   }
  # })
  # shinyjs::disable(id = "cert_yn")
  # 
  # warning_single <- reactive(any(isopleth.table.out()$`Distance (meters)`>100))
  
  output$panelStatus <- reactive({
    all(isopleth.table.out()$isopleth<100)
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)

  output$panelStatus2 <- reactive({
    all(isopleth.table.out2()$isopleth<100)
  })
  outputOptions(output, "panelStatus2", suspendWhenHidden = FALSE)
  
  
  output$warningmessage <- renderText(ifelse(all(isopleth.table.out()$isopleth<100),
                                             approve.note,
                                             notapprove.note))
  output$warningmessage2 <- renderText(ifelse(all(isopleth.table.out2()$isopleth<100),
                                             approve.note,
                                             notapprove.note))
  
  output$cert <- downloadHandler(
    filename = "certificate.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "certificate.Rmd")
      file.copy("certificate.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(frequency = input$frequency,
                     max_loudness = input$max_loudness, # also called "source level"
                     max_ping_dur = input$max_ping_dur,
                     min_silent = input$min_silent,
                     duty_cycle_direct = ifelse(input$duty_cycle_decide=="direct",input$duty_cycle_manual,NA),
                     name = input$name,
                     devicename = input$devicename,
                     characteristics = input$characteristics,
                     isopleth.table = isopleth.table.out(),
                     species.to.deter = dlist()
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$cert2 <- downloadHandler(
    filename = "certificate_multi.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "certificate_multi.Rmd")
      file.copy("certificate_multi.Rmd", tempReport, overwrite = TRUE)

      params <- list(lowest_frequency = input$frequency_lowest,
                     highest_frequency = input$frequency_highest,
                     max_loudness = input$max_loudness_m, # also called "source level"
                     max_ping_dur = input$max_ping_dur_m,
                     min_silent = input$min_silent_m,
                     duty_cycle_direct = ifelse(input$duty_cycle_decide_m=="direct",input$duty_cycle_manual_m,NA),
                     name = input$name2,
                     devicename = input$devicename2,
                     characteristics = input$characteristics2,
                     isopleth.table = isopleth.table.out2(),
                     species.to.deter = dlist2()
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
