# Constant values


propagation <- 15

# hearing groups table - constant values
hearing.group <- c("Low-frequency cetaceans",
                   "Mid-frequency cetaceans",
                   "High-frequency cetaceans",
                   "Phocids",
                   "Otariids")

hgkey <- c(`Low-frequency cetaceans` = "Low-frequency cetaceans: baleen whales",
                                `Mid-frequency cetaceans` = "Mid-frequency cetaceans: dolphins, toothed whales, and beaked whales",
                                `High-frequency cetaceans` = "High-frequency cetaceans: harbor and Dall's porpoise, and pygmy and dwarf sperm whales",
                                Phocids = "Phocids: true seals",
                                Otariids = "Otariids: sea lions and fur seals")

groups <- data.frame(
  hearing.group = hearing.group, 
  SELcum = c(199,198,173,201,219))

weighting <- data.frame(stringsAsFactors = FALSE,
                        hearing.group = hearing.group,
                        a = c(1, 1.6, 1.8, 1,2),
                        b = c(2, 2, 2, 2,2),
                        f1 = c(0.2, 8.8, 12, 1.9,0.94),
                        f2 = c(19, 110, 140, 30, 25),
                        C = c(0.13, 1.2, 1.36, 0.75,0.64))

m <- dplyr::full_join(weighting,groups)


# Notes for tooltips ------------------------------------------------------

frequency.note <- "Note: 1000 Hz = 1 kHz (e.g., 400 Hz = 0.4 kHz)"
loudness.note <- "Note: If the device is non-programmable and capable of producing multiple source levels, the loudness must be inputed"
duration.note <- "Note: 1000 milliseconds = 1 second (e.g., 100 milleseconds = 0.1 second)"


# Long text for intro etc -------------------------------------------------
 intro.text <- "Welcome to the acoustic deterrents web tool! You can use this tool to determine whether your device is allowed under NMFS’s National Guidelines for Nonlethally Deterring Marine Mammals. This tool is for  non-impulsive devices (e.g., acoustic alarms, pingers, transducers, predator sounds/alarm vocalizations using an underwater speaker) with an underwater source level ≥ 170 dB.(RMS, root-mean-square sound pressure level) Simply enter the device specifications in the appropriate calculator.  If you cannot determine the specifications of the device, contact the manufacturer.  You must enter all required information in the calculator to determine whether your device is approved for deterring marine mammals.  If the device, as you intend to use it, meets NMFS’s evaluation criteria, you will receive a certificate of approval. You will need to print or save a copy of this certificate and furnish upon request. The device must be used according to the specifications listed on the certificate. Certificates are valid for one year from date of issue."     

