theme_c <- function(...){ 
  # font <- "Helvetica"   #assign font family up front
  #  font <- "Arial"
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #text elements
      plot.title = element_text(             #title
        #  family = font,            #set font family
        size = 11,                #set font size
        face = 'bold',            #bold typeface
        hjust = .5,
        vjust = 3),               
      
      plot.subtitle = element_text(          #subtitle
        #   family = font,            #font family
        size = 10,
        hjust = .5,
        face = 'italic',
        vjust = 3),               #font size
      
      axis.title = element_text(             #axis titles
        #   family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        #   family = font,            #axis famuly
        size = 8),
      strip.text = element_text(color="white", size = 12,
                                margin=unit(c(.2,.2,.2,.2), 'cm')),
      strip.background = element_rect(fill = "#363636"),
      # t, r, b, l
      plot.margin = unit(c(1,.5,.5,.5), "cm")
    ) %+replace%
    theme(...)
  
}
