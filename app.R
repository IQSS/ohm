#Load libraries
library(shiny)
library(dplyr)
library(readr)
#library(bubbles)
library(ggplot2)
library(shinythemes)
library(ggthemes)
library(colorspace)
library(plotly)
library(tidyr)
library(png)
library(shinyWidgets)
#library(gifski)
#library(gganimate)
#library(extrafont)
library(grid)

#

#Import dataset 

Pager_data <- read_csv("./criminalrecord.csv")

packs <- c("png","grid")

lapply(packs, require, character.only = TRUE) 

img <- readPNG("./DevahGG-30p.png") 

g <- rasterGrob(img, interpolate=FALSE, just = "center") 



Pager_data <- Pager_data %>%
  mutate(race_crim =
           case_when(
             black == 1 & crimrec == 1 ~ "Black with record", 
             black == 1 & crimrec == 0 ~ "Black no record", 
             black == 0 & crimrec == 1 ~ "White with record", 
             black == 0 & crimrec == 0 ~ "White no record"
           )) 

Pager_data <- Pager_data %>%
  mutate(race_crim_mutate =
           case_when(
             black == 1 & crimrec == 1 ~ "Black with criminal record", 
             black == 1 & crimrec == 0 ~ "Black without criminal record", 
             black == 0 & crimrec == 1 ~ "White with criminal record", 
             black == 0 & crimrec == 0 ~ "White without criminal record"
           )) 

Pager_data <- Pager_data %>%
  mutate(black_use = 
           case_when(
             black == 1 ~ "Black", 
             TRUE ~ "White"
           ))

Pager_data <- Pager_data %>%
  mutate(black_mutate = 
           case_when(
             black == 1 ~ "Black", 
             TRUE ~ "White"
           ))


Pager_data <- Pager_data %>%
  mutate(crimrec_use = 
           case_when(
             crimrec == 1 ~ "Criminal record", 
             TRUE ~ "No criminal record"
           ))

Pager_data <- Pager_data %>%
  mutate(crimrec_mutate = 
           case_when(
             crimrec == 1 ~ "Criminal record", 
             TRUE ~ "No criminal record"
           ))

Pager_data <- Pager_data %>%
  gather(key = "compare_across", value = "comparison", race_crim_mutate, black_mutate, crimrec_mutate)

Pager_data <- Pager_data %>% 
  mutate(job_type = 
           case_when(
             custserv == 1 & manualskill == 0 ~ "custserv",
             custserv == 0 & manualskill == 1 ~ "manual",
             custserv == 0 & manualskill == 0 ~ "all",
             custserv == 1 & manualskill == 1 ~ "both",
             TRUE ~ "NA"
           ))

Pager_data <- Pager_data %>% 
  mutate(job_type = 
           case_when(
             job_type == "custserv" ~ "custserv",
             job_type == "manual" ~ "manual",
             job_type == "all" | job_type == "both" |  job_type == "manual" | job_type == "custserv" ~ "all",
             TRUE ~ "NA"
           ))

Pager_data$race_crim <- factor(Pager_data$race_crim, levels = c("Black with record",  "Black no record","White with record", "White no record"))

plot_ly(Pager_data, aes(x = comparison, y = callback, fill = comparison)) + geom_bar(stat = "identity") + theme_bw()

##### UI Side ######

# Use taglist layout - this allows us to have multiple navigation tabs
ui = fluidPage(
  
  #tags$head(tags$style(HTML("a {color: blue}")), id = "sidebar"),
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #DCDEE0;
         border-width: 2px;
         border-left-width: 2px;
         border-right-width: 2px;
         border-radius: 0px / 0px;
         border-color: #464D60
}
         
         body, label, input, button, select { 
         font-family: "Arial";
         }'))),
  #titlePanel(tags$em("...Who gets the callback?")),
  
  #setBackgroundColor("White"),
  fluidRow(
    column(8, offset = 2,
           tags$h2("Who gets ", tags$strong("the callback?"), align = "center", style = "color:#464D60"),            
           tags$br(),
           
           p("In 2003, sociologist Devah Pager asked: how do", tags$strong("race"), "and", tags$strong("criminal record"), "impact how 
             employable someone seems?"),
           tags$br(),
           p("In her experiment, all applicants had identical levels of education and work experience. None of the jobs required more than a high-school degree (e.g. cook, cashier, delivery driver). Who got the callback? Explore her findings below:"),
           
         
           tags$br(),
           tags$br()),
    column(3, offset = 0, 
           style='padding:0px; margin-left:5%; magin-right:0px', 
           
    
           
 
               #Define sidebar
               sidebarPanel(position = "left", width = 15, id = "sidebar",
  
                 
                 #Create checkbox inputs
                 #p("Type of job", color = "black"),
                 radioButtons(inputId = "jobType", "Type of Job", choices = c("Customer Service" = "custserv", "Manual Labor" = "manual", "All Jobs" = "all"), "all"), 
                 
                 # checkboxInput(inputId = "manualskill", label = "Manual Labor", value = TRUE),
                 # checkboxInput(inputId = "customserv", label = "Customer Service Job", value = TRUE),
                 #prettyRadioButtons(inputId = "compare_across", "What do you care about?", choices = c("Race" = "black_mutate", "Criminal Record" = "crim_record_mutate", "Race & Criminal Record" = "race_crim_mutate"), "race_crim_mutate", shape = "square", fill = TRUE, outline = FALSE, animation = "smooth"), 
                radioButtons(inputId = "compare_across", "What do you care about?", choices = c("Race" = "black_mutate", "Criminal Record" = "crim_record_mutate", "Race & Criminal Record" = "race_crim_mutate"), "race_crim_mutate"), 
                 
                 tags$br(), tags$br(), tags$br(), tags$br(),
                 
                 
                 tags$br(),
                 tags$br(),
                 
                 p(align = "right", color = "black", tags$em("Data from"), a("Pager, 2002", 
                                                                                      href="https://davisvanguard.org/wp-content/uploads/2013/07/pager_ajs.pdf", style="color:blue;")),
           
                 #Download Button
                 uiOutput("download_button"))),
               
               #Create main panel with two tabs: one for visual and one for data
      column(8,offset = 0, style='padding:0px; margin-right:-2em',
             
               
            conditionalPanel(
                   condition = "input.compare_across == 'black_mutate'",
                   plotOutput("Race_plot")),
            conditionalPanel(
              condition = "input.compare_across == 'crim_record_mutate'",
              plotOutput("Crimrec_plot")),
            # conditionalPanel(
            #   p(),
            #   condition = "input.compare_across == 'race_crim_mutate'",
            #   tableOutput("dfracecrim")),
            # conditionalPanel(
            #   condition = "input.compare_across == 'black_mutate'",
            #   tableOutput("dfblack")),
            # conditionalPanel(
            #   condition = "input.compare_across == 'crim_record_mutate'",
            #   tableOutput("dfcrim")),
            # conditionalPanel(
            #        condition = "input.compare_across == 'crim_record_mutate',
            #        condition = input.manualskill == TRUE,
            #        condition = input.customserv == FALSE",
            #        plotOutput("Crimrec_plot_manual")),
            #  conditionalPanel(
            #    condition = c("input.manualskill == FALSE", "input.customserv == TRUE", "input.compare_across == 'crim_record_mutate'"),
            #    plotOutput("Crimrec_plot_custserv")),
            # conditionalPanel(
            #   condition = c("input.manualskill == TRUE", "input.customserv == FALSE", "input.compare_across == 'crim_record_mutate'"),
            #   plotOutput("Crimrec_plot_manual")),
             conditionalPanel(
                   condition = "input.compare_across == 'race_crim_mutate'",
                   plotOutput("Race_crim_plot"))),
    #textOutput("caption"),
    # column(10, offset = 1,
    #              #Add text to interactivity bar
    #              p(align = "right", color = "black", tags$em("Data retreived from", a("Pager, 2002.", 
    #                                                                            href="https://davisvanguard.org/wp-content/uploads/2013/07/pager_ajs.pdf")))),
    #              
    column(8, offset = 2, id = "results",
           tags$style(HTML("a {color: #7C1F10}"), id = "results"),
           tags$br(),
           p("A shocking takeaway of this study? In Dr. Pager’s own words:"), 
           p(tags$em("'Essentially, black applicants with clean records were seen as equivalent to white applicants who had just been released from prison.'")),
           
           p("To learn more, listen to our podcast", a("Race bias in hiring: when both applicant and employer lose.", href = "https://outsmartinghumanminds.org/module/race-bias-in-hiring/"))

                
               ))
             )
             
             
  




##### Server Side #####
server<-function(input, output){
  
  formulaText <- reactive(function() {
    paste("Rates of Callback by", input$compare_across)
  })

  
  # Return the formula text for printing as a caption
  output$caption <- renderText(function() {
    formulaText()
  })
  
  output$text_use <- renderText({
    paste("You chose", input$compare_across)
  })

 output$text <- renderText(
   {if_else(input$manualskill == TRUE & input$custserv == FALSE, "Manual", "Customer Service")}
 )
  
  output$Race_plot <- renderPlot({
    ggplot(reactivePager(), aes(x = black_use, y = callback, fill = black_use)) + 
      annotation_custom(rasterGrob(img), xmin=-Inf, xmax=Inf, ymin = 0, ymax = 75) + 
      scale_x_discrete(expand = c(1.2,1.2)) +
      geom_bar(stat = "identity",  width = .8, alpha = .75) + 
      #width = .45 if remove scale discrete expand, .8 if not
      theme_few() +
      scale_fill_manual(values=c("black", "black"), 
                        name="Applicant's Race") + 
      theme(
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            
            #axis.line = element_line(colour = "#7C1F10", size = 1),
            axis.text.x=element_text(size=12, color = "black")
            #legend.position = "bottom"
            ) + 
      ylim(0, 75) + 
      annotate("text", x = 1.5, y = 67, label = 'atop(bold("Manual Labor"))', color = "black", size = 6, parse= TRUE) +
      annotate("text", x = 1.5, y = 65, label = 'atop(italic("(e.g., warehouse worker, delivery driver)"))', color = "black", size = 4, parse = TRUE) 
               
  
    
  })  
  
  
  output$Crimrec_plot <- renderPlot({
    
    ggplot(reactivePager(), aes(x = crimrec_use, y = callback, fill = crimrec_use)) + 
      annotation_custom(rasterGrob(img), xmin=-Inf, xmax=Inf, ymin = 0, ymax = 75) +
      scale_x_discrete(expand = c(1.1,1.3)) +
      geom_bar(stat = "identity", alpha = .75, width = .8) + 
      theme_few() +
      # ylab("Number of Applicants Receiving Callbacks") + 
      # xlab("Comparison") + 
      scale_fill_manual(values=c("black", "black"), 
                         name="Criminal Record") + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(size=12, color = "black"),
            panel.border = element_blank(),
 
            legend.position = "none") + 
      ylim(0, 75) + 
      annotate("text", x = 1.5, y = 67, label = 'atop(bold("Manual Labor"))', color = "black", size = 6, parse= TRUE) +
      annotate("text", x = 1.5, y = 65, label = 'atop(italic("(e.g., warehouse worker, delivery driver)"))', color = "black", size = 4, parse = TRUE)  
    
    
  })  
  
  output$Crimrec_plot_manual <- renderPlot({
    
    ggplot(reactivePager(), aes(x = crimrec_use, y = callback, fill = crimrec_use)) + 
      annotation_custom(rasterGrob(img), xmin=-Inf, xmax=Inf, ymin = 0, ymax = 60) +
      scale_x_discrete(expand = c(1.1,1.3)) +
      geom_bar(stat = "identity", alpha = .75, width = .8) + 
      theme_few() +
      # ylab("Number of Applicants Receiving Callbacks") + 
      # xlab("Comparison") + 
      scale_fill_manual(values=c("black", "black"), 
                        name="Criminal Record") + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(size=12, color = "black"),
            panel.border = element_blank(),
            
            legend.position = "none") + 
      ylim(0, 60) + 
      annotate("text", x = 1.5, y = 52, label = 'atop(bold("Manual Labor"))', color = "black", size = 6, parse= TRUE) +
      annotate("text", x = 1.5, y = 50, label = 'atop(italic("(e.g., warehouse worker, delivery driver)"))', color = "black", size = 4, parse = TRUE)  
    
    
  })  
  
  output$Crimrec_plot_custserv <- renderPlot({
    
    ggplot(reactivePager(), aes(x = crimrec_use, y = callback, fill = crimrec_use)) + 
      annotation_custom(rasterGrob(img), xmin=-Inf, xmax=Inf, ymin = 0, ymax = 60) +
      scale_x_discrete(expand = c(1.1,1.3)) +
      geom_bar(stat = "identity", alpha = .75, width = .8) + 
      theme_few() +
      # ylab("Number of Applicants Receiving Callbacks") + 
      # xlab("Comparison") + 
      scale_fill_manual(values=c("black", "black"), 
                        name="Criminal Record") + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_text(size=12, color = "black"),
            panel.border = element_blank(),
            
            legend.position = "none") + 
      ylim(0, 60) + 
      annotate("text", x = 1.5, y = 52, label = 'atop(bold("Customer Service"))', color = "black", size = 6, parse= TRUE) +
      annotate("text", x = 1.5, y = 50, label = 'atop(italic("(e.g., warehouse worker, delivery driver)"))', color = "black", size = 4, parse = TRUE)  
    
    
  })  
  
  output$Race_crim_plot <- renderPlot({
    ggplot(reactivePager(), aes(x = race_crim, y = callback, fill = race_crim)) + 
      annotation_custom(rasterGrob(img), xmin=-Inf, xmax=Inf, ymin = 0, ymax = 60) + 
      geom_bar(stat = "identity", alpha = .75, width = .8) + 
      theme_few() + 
  
      scale_fill_manual(values=c("black", "black", "black", "black"), 
                        name="Race and Record Status") + 
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            panel.border = element_blank(),
            axis.text.x=element_text(size=12, color = "black")) + 
      ylim(0, 60) + 
      annotate("text", x = 2.5, y = 52, label = 'atop(bold("Manual Labor"))', color = "black", size = 6, parse= TRUE) +
      annotate("text", x = 2.5, y = 50, label = 'atop(italic("(e.g., warehouse worker, delivery driver)"))', color = "black", size = 4, parse = TRUE) +
      annotate("text", x = .78, y = 4, label = "X", color = "yellow", size = 4) + 
      annotate("text", x = 1, y = 4.2, label = "Callbacks", color = "white", size = 4) + 
      annotate("text", x = 1, y = 1.4, label = "(X%)", color = "white", size = 4) + 
      annotate("text", x = 1.78, y = 4, label = "X", color = "yellow", size = 4) + 
      annotate("text", x = 2, y = 4.2, label = "Callbacks", color = "white", size = 4) + 
      annotate("text", x = 2, y = 1.4, label = "(X%)", color = "white", size = 4) + 
      annotate("text", x = 2.78, y = 4, label = "X", color = "yellow", size = 4) + 
      annotate("text", x = 3, y = 4.2, label = "Callbacks", color = "white", size = 4) + 
      annotate("text", x = 3, y = 1.4, label = "(X%)", color = "white", size = 4) + 
      annotate("text", x = 3.78, y = 4, label = "X", color = "yellow", size = 4) + 
      annotate("text", x = 4, y = 4.2, label = "Callbacks", color = "white", size = 4) + 
      annotate("text", x = 4, y = 1.4, label = "(X%)", color = "white", size = 4)
      # annotate("text", x = .78, y = 4, label = "X", color = "yellow", size = 4) + 
      # annotate("text", x = 1, y = 4.2, label = "Callbacks", color = "white", size = 4) + 
      # annotate("text", x = 1, y = 1.4, label = "(X%)", color = "white", size = 4) + 
      # annotate("text", x = 1.78, y = 7, label = "X", color = "yellow", size = 4) + 
      # annotate("text", x = 2, y = 7.2, label = "Callbacks", color = "white", size = 4) + 
      # annotate("text", x = 2, y = 4.4, label = "(X%)", color = "white", size = 4) + 
      # annotate("text", x = 2.78, y = 13, label = "X", color = "yellow", size = 4) + 
      # annotate("text", x = 3, y = 13.2, label = "Callbacks", color = "white", size = 4) + 
      # annotate("text", x = 3, y = 10.4, label = "(X%)", color = "white", size = 4) + 
      # annotate("text", x = 3.78, y = 31, label = "X", color = "yellow", size = 4) + 
      # annotate("text", x = 4, y = 31.2, label = "Callbacks", color = "white", size = 4) + 
      # annotate("text", x = 4, y = 28.4, label = "(X%)", color = "white", size = 4) 
    #geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..cou
      #geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", color = "white", position = position_dodge(0.9), size=3.5) 
    #+ geom_text(aes(y = ..count.., label = ..count.., stat = "count", vjust = 1.2, color = "yellow"))
    
    
      
       })  
  
  
  reactiveDfblack <- reactive({return(tbl_df(Pager_data) %>% 
                                        #filter(interact == input$interact) %>%
                                        filter(manualskill == input$manualskill) %>%
                                        filter(custserv == input$customserv) %>%
                                        select(black_use, callback) %>%
                                        group_by("Race" = black_use) %>%
                                        summarize("Mean Percent of Callbacks" = (mean(callback)*100)))})
  
  
  output$dfblack <- renderTable({reactiveDfblack()})
  
  reactiveDf <- reactive({return(tbl_df(Pager_data) %>% 
                                   #filter(interact == input$interact) %>%
                                   filter(manualskill == input$manualskill) %>%
                                   filter(custserv == input$customserv) %>%
                                   
                                   group_by("Comparison" = comparison) %>%
                                   
                                   summarize("Mean Percent of Callbacks" = (mean(callback)*100)))})
  
  output$df <- renderTable({reactiveDf()})
  
  reactiveDfcrim <- reactive({return(tbl_df(Pager_data) %>% 
                                       #filter(interact == input$interact) %>%
                                       filter(manualskill == input$manualskill) %>%
                                       filter(custserv == input$customserv) %>%
                                       select(crimrec_use, callback) %>%
                                       group_by("Criminal Record" = crimrec_use) %>%
                                       summarize("Mean Percent of Callbacks" = (mean(callback)*100)))})
  
  
  
  output$dfcrim <- renderTable({reactiveDfcrim()})
  
  
  reactiveDfracecrim <- reactive({return(tbl_df(Pager_data) %>% 
                                           #filter(interact == input$interact) %>%
                                           filter(manualskill == input$manualskill) %>%
                                           filter(custserv == input$customserv) %>%
                                           select(race_crim, callback) %>%
                                           group_by("Race and Criminal Record" = race_crim) %>%
                                           
                                           summarize("Mean Percent of Callbacks" = (mean(callback)*100)))})
  
  output$dfracecrim <- renderTable({reactiveDfracecrim()})
  
  
  reactivePager <- reactive({return(tbl_df(Pager_data) %>%
                                      
                                      #filter(interact == input$interact) %>%
                                      filter(job_type == input$jobType) 
                                    
                                      # %>% group_by(compare_across == input$compare_across) 
                                      # filter(manualskill == input$manualskill) %>%
                                      # filter(custserv == input$customserv)
                                      )})
  
  output$dfPager <- renderTable({reactivePager()})
  
}

shinyApp(ui=ui, server=server) 
