
#THEMES: test
library(bootstraplib)

bs_theme_new(bootswatch = 'simplex')
# Color palette derives from https://tombrow.com/dark-mode-website-css
bs_theme_base_colors(fg ="#0055A4", bg = "#FFFFFF") #Palette picked from NOAA style guide 
bs_theme_accent_colors(primary = "#0055A4", secondary =  "#00467F")

bootstrap(bs_theme_get())
bootstrap_sass(theme = bs_theme_get())
xx <- sass::sass(input = bs_theme_get())

#bs_theme_preview()
#bs_theme_get()
fileConn<-file("abc2.txt")
writeLines(xx, fileConn)
close(fileConn)
