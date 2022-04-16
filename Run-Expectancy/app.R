#Loading Packages
library(tidyverse)
library(shiny)
library(plotly)
library(rsconnect)



#Reading in  Results Dataset
df1 <- read_csv(here::here('~/STA 518/STA518_Project/Results/Seaon Total Innings.csv'))


df <- df1 %>% 
  unite(Onbase, First:Third) %>% 
  mutate(`top_bot` = case_when(`Home Team` ==0 ~ "Top",
                               `Home Team` ==1 ~ "Bottom"),
         runners = case_when(Onbase == '0_0_0' ~ "No Runners",
                             Onbase == '1_0_0' ~ "First",
                             Onbase == '1_1_0' ~ "First and Second",
                             Onbase == '1_0_1' ~ "First and Third",
                             Onbase == '0_1_0' ~ "Second",
                             Onbase == '0_1_1' ~ "Second and Third",
                             Onbase == '0_0_1' ~ "Third",
                             Onbase == '1_1_1' ~ "Bases Loaded"))

#Formatiing Data
df1 <- df1 %>% 
  mutate(`Scored` = if_else(`Inning Runs` >= 1,TRUE,FALSE))


df_group <- df1 %>% 
  group_by(First, Second, Third, Outs) %>% 
  summarise(Runs = sum(`Inning Runs`),
            Occurences = n(),
            `Probabilty of Scoring` = mean(Scored))

df_group <- df_group %>%  
  mutate(`Runs Expected` = Runs/Occurences) %>% 
  unite(Onbase, First:Third) 

runs_expected_matrix <- df_group %>% 
  pivot_wider(id_cols=Onbase, names_from=Outs, values_from=`Runs Expected`, names_prefix='Outs: ', names_sep=' ') %>% 
  arrange(desc('Outs: 0'))


run_prob_matrix <- df_group %>% 
  pivot_wider(id_cols=Onbase, names_from=Outs, values_from=`Probabilty of Scoring`, names_prefix='Outs: ', names_sep=' ')



#Setting up Shiny App and gathering Inputs
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel(
    "Run Expectancy"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      "Selections",
      sliderInput("n_inning", label = "Inning:",
                  min = 1, max = 9, value = 1, step = 1),
      
      radioButtons("top_or_bot", label = "Top or Bottom of Inning:",
                   choices = c('Top', 'Bottom')),
      
      radioButtons("n_outs", label = "Number of Outs:",
                   choices = c(0, 1, 2)),
      
      selectInput("base_runners", label = "Base Runners:",
                  choices = c('No Runners','First', 'Second', 'Third', 'First and Second',
                              'First and Third', 'Second and Third', 'Bases Loaded'), selected = "No Runners")
    ),
    
    mainPanel(
      plotOutput("plot"),
      # tabPanel(tableOutput("static"))
      
      plotlyOutput('matrix'),
      plotlyOutput('matrix1')
    
    )
  )
)

#Creating  Shiny App function for inputs and outputs 
server <- function(input, output, session){
  
  #Data Table - Not Uutput
  output$static <- renderTable(head(df %>% 
                                      filter(
                                        (Inning == input$n_inning) &
                                        (top_bot == input$top_or_bot) &
                                          (Outs == input$n_outs) &
                                          (runners == input$base_runners)) %>%
                                      select(`Inning Runs`)))
  
  #Input Affected Histogram
  output$plot <- renderPlot(
    ggplot(
      df %>% 
        filter(
          (Inning == input$n_inning) &
            (top_bot == input$top_or_bot) &
            (Outs == input$n_outs) &
            (runners == input$base_runners)) %>%
        select(`Inning Runs`),
      aes(x=`Inning Runs`)) +
      geom_histogram() + 
      ggtitle("Distribution of Runs Scored") +
      xlab("Number of Runs in Inning") + ylab("Occurences"))
    
  #Run Expectancy Matrix
  output$matrix <- renderPlotly(
    df_group %>% 
      ggplot(aes(y=Onbase, fill=`Runs Expected`, x=Outs))+
      geom_tile()+
      scale_fill_gradient2(low="red", high="blue", mid="white") +
      ggtitle("Run Expectancy Matrix") +
      xlab("Number of Outs") + ylab("Base Runners"))
  
    #Probability Matrix
    output$matrix1 <- renderPlotly(
      df_group %>% 
          ggplot(aes(y=Onbase, fill=`Probabilty of Scoring`, x=Outs))+
          geom_tile() +
          scale_fill_gradient(low="red", high="green") +
          ggtitle("Probability of Scoring Matrix") +
          xlab("Number of Outs") + ylab("Base Runners"))
      

}


#Calling Shiny App function
shinyApp(ui, server)


deployApp()

