
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





MEAN_X <- 0
MEAN_Y <- 0
COV_XY <- 0

VAR_X <- 1
VAR_Y <- 1



sim <- MASS::mvrnorm(n = 10000, mu =c(MEAN_X,MEAN_X),
                     Sigma =  matrix(data=c(VAR_X, COV_XY ,
                                            COV_XY ,VAR_Y),
                                     byrow=TRUE,
                                     nrow=2))

bivn.kde <- MASS::kde2d(sim[,1], sim[,2], n = 100, h = c( bandwidth.nrd(sim[,1]) + .4,
                                                          bandwidth.nrd(sim[,2]) + .4  ) )



# z <- dmvnorm(, mean = rep(0, p), sigma = diag(p), log = FALSE, checkSymmetry = TRUE)


plot3D::persp3D(z=bivn.kde$z, x=bivn.kde$x,y=bivn.kde$y, colvar=NULL,
                theta=140, phi=25, zlab="Density",
                cex.lab=1.5,cex.axis=1,ticktype="detailed", shade=.1, 
                contour=TRUE, lighting="diffuse",opaque.top=TRUE,
                border="black",lwd=0.08,
                col="gray")


tibble(x=sim[,1],
       y=sim[,2]) %>%
  ggplot(aes(x=x,y=y)) +
  geom_point(alpha = .2) +
  geom_density_2d() +
  geom_point(x= MEAN_X,y = MEAN_Y, color="red", size=1.2) +
  theme_c()


