library(tidyverse)
library(rmarkdown)
library(stringr)
library(here)

certificate <- function(template, attendeeName, event, eDate, eLocation, outPDF, knitDir){
  cat("\n Starting:", outPDF, "\n")
  
  # Create a temporary Rmd file with the attendee and event information.  
  templateCert <- read_file(template)
  tmpRmd <- templateCert %>%
    str_replace("<<ATTENDEE_NAME>>", attendeeName) %>%
    str_replace("<<EVENT_NAME>>", event) %>%
    str_replace("<<EVENT_DATE>>", eDate) %>%
    str_replace("<<EVENT_LOCATION>>", eLocation)
  
  # The knitdir has to be defined for the rmarkdown::render to work.
  RmdFile <- tempfile(tmpdir = knitDir, fileext = ".Rmd")
  write_file(tmpRmd, RmdFile)
  
  # Creating the certificates using R markdown.
  rmarkdown::render(RmdFile, output_file = here(outPDF), quiet = TRUE)
  
  # Temporary .Rmd file can be deleted.
  file.remove(RmdFile)
  cat("\n Finished:", outPDF, "\n")
}

certificate(template = here("Certificate_Rmd_Template.txt"),
            "Applicant name", "Event Name", "Date", "Location","PDF/1_MiscCert.pdf", here())
