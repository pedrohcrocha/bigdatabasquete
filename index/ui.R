# CONDITIONAL PANEL
load("temporada_2019.Rda")
load("temporada_2018.Rda")
load("temporada_2017.Rda")
load("temporada_2016.Rda")
load("estatisticas_2016.Rda")
load("estatisticas_2017.Rda")
load("estatisticas_2018.Rda")
load("estatisticas_2019.Rda")

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)

shinyUI(pageWithSidebar(
    headerPanel("Dados da Liga Nacional de Basquete"),
    
    # SIDEBAR PANEL
    sidebarPanel(
        # Panel 1 - Página Inicial
        conditionalPanel(condition="input.tabselected==1", 
                         h4("Big Data Basquete")
                         ),
        # Panel 2 - Dados
        conditionalPanel(condition="input.tabselected==2",
                        selectInput("dataset",
                                    "Selecione uma temporada",
                                    choices=c("temporada.2019","temporada.2018","temporada.2017","temporada.2016")),
                        uiOutput('team'),
                        radioButtons("choice",
                                     "Escolha uma opção",
                                     choices=c("Dados" = 1, "Resumo" = 2 ))
                        ),
        # Panel 3 - Gráficos
        conditionalPanel(condition="input.tabselected==3", "A seleção das opções 'Jogador' e 'Equipe' resultará em um cluster de dados.",
                         selectInput("count",
                                     "Selecione uma temporada",
                                     choices=c("temporada.2019","temporada.2018","temporada.2017","temporada.2016")),
                         uiOutput("varx"),
                         uiOutput("vary")
                         ),
        # Panel 4 - Top 10
        conditionalPanel(condition="input.tabselected==4", 
                        selectInput("aux",
                                    "Selecione uma temporada",
                                    choices=c("temporada.2019","temporada.2018","temporada.2017","temporada.2016")),
                        uiOutput("stat")
                        ),
        # Panel 5 - Avançadas
        conditionalPanel(condition="input.tabselected==5", 
                         selectInput("avancadas", 
                                     "Selecione uma temporada",
                                     choices=c("estatistica.2019","estatistica.2018","estatistica.2017","estatistica.2016")),
                         uiOutput("team2"),
                         radioButtons("choice2",
                                      "Escolha uma opção",
                                      choices = c("Dados" = 3, "Resumo" = 4))
        ),
 
        # Imagem
        # Para colocar uma imagem, é necessário criar uma pasta 'www' no mesmo diretório
        h6(" Para mais:"),
        tags$img(src='nbb.png', height=70, width=70),
        tags$a(href="http://lnb.com.br/", "Acesse"),
        
        # Edição HTML e CSS
        tags$style(".col-sm-12{
                    background-color: green;
                    font-weight: bold;
                    font-size: 300%;
                    color: white;
                    padding: 10px;
                   }"),
        
        tags$style(".span12{
                   font-style: oblique;
                   font-weight: bold;
                   border-style: solid
                   }"),
        
        tags$style(".span4{
                   border-style: solid
                   }"),
        
        tags$style(".span8{
                   border-style: solid
                   
                   "),
        
        tags$style(".col-sm-4{
                   color:blue;
                   background-color: green")
        ),
    
    # MAIN PANEL
    mainPanel(
        # recomenda-se revisão da syntax p/ tabsetPanel() & tabPanel()
        # id argument é importante p/ tabsetPanel()
        # value argument é importante p/ tabPanel()
        tabsetPanel(
            tabPanel("Sobre", value=1,
                     h3(strong('Sobre o app')),
                     br(),
                     p('Por este aplicativo, desejo informar aos entusiastas do basquete nacional
                       acerca dos dados que rodam nossa liga de basquete. '),
                     p('Tornou-se uma realidade absoluta o uso dos dados na tomada de decisão.
                     Em um esporte de alto dinamismo, como o basquete, essas decisões muitas vezes são a 
                     diferença entre a vitória e o fracasso.'),
                     p('Essa iniciativa é fruto da evolução das estatísticas avançadas nas ligas mundais de basquete, 
                      especialmente, a NBA.'),
                     h4(strong('Legendas:')),
                     span('JO - Jogos ; Min - Minutos ; RT - Rebotes totais ; PTS - Pontos ; LLC - Lance Livre convertido ;'),
                     span('LLT - Lance Livre tentado ; LLPercent - Lance Livre % ; EN - Enterrada ; EF - Eficiência ;'),
                     span('AS - Assistência ; BR - Bola Roubada ; TO - Tocos ; ER - Erros ; RO - Reb.Ofensivos ; 
                          RD - Reb.Defensivos'),
                     br(),
                     br(),
                     p('Para mais informações, acesse o link:'),
                     code('https://github.com/pedrohcrocha')
                     ),
            
            tabPanel("Dados", value=2, 
                     conditionalPanel(condition="input.choice==1",  DT::dataTableOutput("dat")),
                     conditionalPanel(condition="input.choice==2", verbatimTextOutput("summary"))),
            
            tabPanel("Gráfico", value=3,plotOutput("plot")),
            
            tabPanel("TOP 10", value=4, tableOutput("stats")),
            
            tabPanel("Avançadas", value=5, 
                     conditionalPanel(condition="input.choice2==3",  DT::dataTableOutput("dados_avancados")),
                     conditionalPanel(condition="input.choice2==4", verbatimTextOutput("sumario"))),
            
            id = "tabselected"
            ) # tabset Panel
        ) # Main Panel
    ) #pagewithSidebar
) # shinyUI