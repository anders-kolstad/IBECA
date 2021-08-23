library(shiny)
library(ggplot2)
library(shinythemes)
library(ggplot2)



# coxcomb exampl ----------------------------------------------------------



testdat <- data.frame(
  "EcosystemCharacteristic" = c(
    "Primary Production",
    "Abiotic factors",
    "Biodiversity",
    "Func. imp. sp. and str.",
    "Func. imp. gr. within trophic levels",
    "Biomass comp. across trophic levels",
    "Landscape ecol. patterns"
    ),
  "totValue" = c(0.7, 
              0.3,
              0.45,
              0.2,
              1,
              0.9,
              0.5),
  "Tilstand" = "God"
)

testdat <- rbind(testdat, testdat, testdat)
testdat$Tilstand <- rep(c("God", "Noe redusert", "Betydelig redusert"), each=7)
testdat$hgt <- 0
testdat$hgt[testdat$Tilstand=="God"] <- 
  ifelse(testdat$totValue[testdat$Tilstand=="God"] > 0.4, 
         0.4, 
         testdat$totValue[testdat$Tilstand=="God"])

testdat$hgt[testdat$Tilstand=="Noe redusert"] <- 
  ifelse(testdat$totValue[testdat$Tilstand=="Noe redusert"] > 0.5, 
         0.1, 
         testdat$totValue[testdat$Tilstand=="Noe redusert"] 
         - testdat$hgt[testdat$Tilstand=="God"])
  
testdat$hgt[testdat$Tilstand=="Betydelig redusert"] <- 
  ifelse(testdat$totValue[testdat$Tilstand=="Betydelig redusert"] > 0.6, 
         testdat$totValue[testdat$Tilstand=="Betydelig redusert"] 
         - testdat$hgt[testdat$Tilstand=="God"] 
         - testdat$hgt[testdat$Tilstand=="Noe redusert"],0)

  
testdat$colours <- "green"
testdat$colours[testdat$Tilstand=="Noe redusert"] <- "orange"
testdat$colours[testdat$Tilstand=="Betydelig redusert"] <- "red"


testdat$EcosystemCharacteristic <- as.factor(testdat$EcosystemCharacteristic)
testdat$colours <- as.factor(testdat$colours)
testdat$Tilstand <- as.factor(testdat$Tilstand)
testdat$Tilstand <- factor(testdat$Tilstand, levels = c( "Betydelig redusert","Noe redusert", "God"))

testdat$label <- rep(letters[1:7], 3)

# UI ----------------------------------------------------------------------



ui <- navbarPage(theme = shinytheme("slate"),
                 title = "IBECA",
                 

# HOVEDSIDE ---------------------------------------------------------------

                 
                 tabPanel("Hovedside",
                          textOutput('text1'),
                          column(width=5,
                                 plotOutput('coxcomb')
                                     )
                          ),



# INFOSIDE ----------------------------------------------------------------


                 tabPanel('Info',
                          textOutput('text2')
                          )
)

server <- function(input, output, session) {
  output$text1 <- renderText('Her kommer hovedsiden')
  
  output$text2 <- renderText('Her kommer infosiden')
  
  
  output$coxcomb <- renderPlot({
    ggplot(testdat, aes( x = label,
                         y = hgt,
                         fill=Tilstand))+
      geom_bar(stat = "identity", 
               width = 0.9, 
               colour="black", 
               alpha=.7,
               position = "stack") + 
      coord_polar(theta = "x", start = 11, clip="off")+
      theme_bw()+
      theme(                   ## finishing touches to the theme
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        #panel.grid = element_blank(),
        legend.position = "right",
        legend.direction = "vertical"
      )+
      labs(                                             ## labels
        x = "", y = "", fill = "",
        title = "Økologisk tilstand\nAvstand fra referansetilstand"
      )+
      scale_fill_manual(      
        values = c("red", "orange", "green")
      )
  })
  
}


shinyApp(ui = ui, server = server)
                          