# Functions.R

get.adjustment <- function(freq,wtpars){
  a <- wtpars$a
  b <- wtpars$b
  f1 <- wtpars$f1
  f2 <- wtpars$f2
  C <- wtpars$C
  
  x <- (freq/f1)^(2*a) # This is cell C48
  y <- (1+(freq/f1)^2)^a # cell C49
  z <- (1+(freq/f2)^2)^b # cell C50
  aa <- x/(y*z) # cell C51
  adj <- (log10(aa)*10 + C) 
  return(adj)
}

get.isopleth <- function(max.loudness,adjustment,log10dur,selcumthresh,propagation){
  isopleth = 10^(((max.loudness+adjustment) + log10dur-selcumthresh)/propagation)
  return(isopleth)
}


#  Test get.adjustment ----------------------------------------------------

# Define weighting pars for each mammal type
# lowfreq <- list(a = 1,b = 2,f1 = 0.2,f2 = 19,C = 0.13)
# midfreq <- list(a = 1.6,b = 2,f1 = 8.8,f2 = 110,C = 1.2)
# highfreq <- list(a = 1.8,b = 2,f1 = 12,f2 = 140,C = 1.36)
# phocids <- list(a = 1,b = 2,f1 = 1.9,f2 = 30,C = 0.75)
# otariids <- list(a = 2, b=2, f1=0.94,f2=25,C = 0.64)
# 
# # Test get.adjustment function
# get.adjustment(freq = frequency,wtpars = lowfreq)
# get.adjustment(freq = frequency,wtpars = midfreq)
# get.adjustment(freq = frequency,wtpars = highfreq)
# get.adjustment(freq = frequency,wtpars = phocids) 
# get.adjustment(freq = frequency,wtpars = otariids) # yay, it works!
# 
# 
# 
# # Test get.isopleth -------------------------------------------------------
# 
# get.isopleth(max.loudness = max.loudness, # only works for first cetacean group
#              adjustment = get.adjustment(freq = frequency,
#                                          wtpars = list(a = 1,b = 2,f1 = 1.9,f2 = 30,C = 0.75)),
#              log10dur = log10dur,
#              selcumthresh = 201,
#              propagation = propagation)
# 
# 
