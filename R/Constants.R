# Constant values


propagation <- 15

# hearing groups table - constant values
hearing.group <- c("Low-frequency cetaceans",
                   "Mid-frequency cetaceans",
                   "High-frequency cetaceans",
                   "Phocids",
                   "Otariids")

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