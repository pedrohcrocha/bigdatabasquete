library(shiny)
library(ggplot2)
library(dplyr)
load("temporada_2019.Rda")
load("temporada_2018.Rda")
load("temporada_2017.Rda")
load("temporada_2016.Rda")
load("estatisticas_2016.Rda")
load("estatisticas_2017.Rda")
load("estatisticas_2018.Rda")
load("estatisticas_2019.Rda")


shinyServer(function(input,output,session)({
    # Funções reativas
    data <- reactive({get(input$dataset)})
    top10 <- reactive({get(input$top)})
    data2 <- reactive({get(input$aux)})
    data3 <- reactive({get(input$count)})
    estatisticas_avancadas <- reactive({get(input$avancadas)})
    
    ## Colocando a variável do time
        # OBS.: Como os times mudam de ano em ano, devemos fazer o renderUI
    output$team <-renderUI({
        selectInput('team', "Escolha um time", choices = data()$Equipe)
    })
    output$team2 <-renderUI({
        selectInput('team2', "Escolha um time", choices = estatisticas_avancadas()$Equipe)
    })
    
# Gráficos
     # Puxando uma lista de variáveis para a escolha da variável x
    output$varx <- renderUI({
        selectInput("variablex", "\nEscolha uma variável X", choices=names(data3()), selected = "PTS")
    })
    # Puxando uma lista de variáveis para a escolha da variável y
    output$vary <- renderUI({
        selectInput("variabley",  "\nEscolha uma variável Y", choices=names(data3()), selected = "PTS")
        
    })
    # Output dos gráficos
    output$plot <- renderPlot({
        ggplot(data3(), aes_string(x = input$variablex, y = input$variabley, colour = input$variablex)) +
            geom_point(alpha = 1, size = 3, position = 'jitter') + 
            stat_smooth(method = "lm", col = "orange", se = TRUE, size = 1)
            
    }) 
        
# TOP 10
    # Puxando uma lista de variáveis para a escolha da estatística do TOP 10
    output$stat <-renderUI({
        selectInput("top", "Escolha uma estatística", choices=names(data2()), selected = "PTS")
    })
    # Output do TOP 10 das estatísticas
    output$stats <-renderTable({
        if(input$top == 'PTS'){
            data2() %>% 
                select(Jogador, Equipe, PTS) %>% 
                top_n(10) %>% 
                arrange(desc(PTS))
        }else if(input$top == "AS"){
            data2() %>% 
                select(Jogador, Equipe, AS) %>% 
                top_n(10) %>% 
                arrange(desc(AS))
        }else if(input$top == "Min"){
            data2() %>% 
                select(Jogador, Equipe, JO, Min) %>% 
                top_n(10) %>% 
                arrange(desc(Min))
        }else if(input$top == "RT"){
            data2() %>% 
                select(Jogador, Equipe, RT) %>% 
                top_n(10) %>% 
                arrange(desc(RT))
        }else if(input$top == "TresFeitas"){
            data2() %>% 
                select(Jogador, Equipe, TresFeitas) %>% 
                top_n(10) %>% 
                arrange(desc(TresFeitas))
        }else if(input$top == "TresTentadas"){
            data2() %>% 
                select(Jogador, Equipe, TresTentadas) %>% 
                top_n(10) %>% 
                arrange(desc(TresTentadas))
        }else if(input$top == "TresPercent"){
            data2() %>% 
                select(Jogador, Equipe, TresTentadas, TresPercent) %>% 
                filter(TresTentadas >= 100) %>% 
                top_n(10) %>% 
                arrange(desc(TresPercent))
        }else if(input$top == "DoisFeitas"){
            data2() %>% 
                select(Jogador, Equipe, DoisFeitas) %>% 
                top_n(10) %>% 
                arrange(desc(DoisFeitas))
        }else if(input$top == "DoisTentadas"){
            data2() %>% 
                select(Jogador, Equipe, DoisTentadas) %>% 
                top_n(10) %>% 
                arrange(desc(DoisTentadas))
        }else if(input$top == "DoisPercent"){
            data2() %>% 
                select(Jogador, Equipe, DoisTentadas, DoisPercent) %>% 
                filter(DoisTentadas >= 100) %>% 
                top_n(10) %>% 
                arrange(desc(DoisPercent))
        }else if(input$top == "LLC"){
            data2() %>% 
                select(Jogador, Equipe, LLC) %>% 
                top_n(10) %>% 
                arrange(desc(LLC))
        }else if(input$top == "LLPercent"){
            data2() %>% 
                select(Jogador, Equipe, LLT, LLPercent) %>% 
                filter(LLT >= 50) %>% 
                top_n(10) %>% 
                arrange(desc(LLPercent))
        }else if(input$top == "EN"){
            data2() %>% 
                select(Jogador, Equipe, EN) %>% 
                top_n(10) %>% 
                arrange(desc(EN))
        }else if(input$top == "EF"){
            data2() %>% 
                select(Jogador, Equipe, EF) %>%
                top_n(10) %>% 
                arrange(desc(EF))
        }else if(input$top == "BR"){
            data2() %>% 
                select(Jogador, Equipe, BR) %>% 
                top_n(10) %>% 
                arrange(desc(BR))
        }else if(input$top == "TO"){
            data2() %>% 
                select(Jogador, Equipe, TO)%>% 
                top_n(10) %>% 
                arrange(desc(TO))
        }else{
            print('Escolhe outra estatística, por favor!')
        }
    })
    
## Output da base de dados
    # ENCONTRAR OUTRA MANEIRA DE REALIZAR ISSO
    output$dat <- DT::renderDataTable({
        if(input$team == 'Flamengo'){
            data() %>% 
                filter(Equipe == 'Flamengo')
        }else if (input$team == 'Minas'){
            data() %>% 
                filter(Equipe == 'Minas')
        }else if (input$team == 'Brasília'){
            data() %>% 
                filter(Equipe == 'Brasília')
        }else if (input$team == 'Bauru'){
            data() %>% 
                filter(Equipe == 'Bauru')
        }else if (input$team == 'Macaé Basquete'){
            data() %>% 
                filter(Equipe == 'Macaé Basquete')
        }else if (input$team == 'Sesi Franca'){
            data() %>% 
                filter(Equipe == 'Sesi Franca')
        }else if (input$team == 'Vitória'){
            data() %>% 
                filter(Equipe == 'Vitória')
        }else if (input$team == 'Campo Mourão'){
            data() %>% 
                filter(Equipe == 'Campo Mourão')
        }else if (input$team == 'Pinheiros'){
            data() %>% 
                filter(Equipe == 'Pinheiros')
        }else if (input$team == 'Caxias do Sul'){
            data() %>% 
                filter(Equipe == 'Caxias do Sul')
        }else if (input$team == 'Paulistano'){
            data() %>% 
                filter(Equipe == 'Paulistano')
        }else if (input$team == 'Basq. Cearense'){
            data() %>% 
                filter(Equipe == 'Basq. Cearense')
        }else if (input$team == 'Vasco da Gama'){
            data() %>% 
                filter(Equipe == 'Vasco da Gama')
        }else if (input$team == 'Mogi'){
            data() %>% 
                filter(Equipe == 'Mogi')
        }else if (input$team == 'L.Sorocabana'){
            data() %>% 
                filter(Equipe == 'L.Sorocabana')
        }else if (input$team == 'Corinthians'){
            data() %>% 
                filter(Equipe == 'Corinthians')
        }else if (input$team == 'São José'){
            data() %>% 
                filter(Equipe == 'São José')
        }else if (input$team == 'Botafogo'){
            data() %>% 
                filter(Equipe == 'Botafogo')
        }else if (input$team == 'UNIFACISA'){
            data() %>% 
                filter(Equipe == 'UNIFACISA')
        }else if (input$team == 'Pato Basquete'){
            data() %>% 
                filter(Equipe == 'Pato Basquete')
        }else if (input$team == 'São Paulo'){
            data() %>% 
                filter(Equipe == 'São Paulo')
        }else if (input$team == 'Rio Claro'){
            data() %>% 
                filter(Equipe == 'Rio Claro')
        }else{
            data()
        }
    })
    # Output do resumo
    output$summary <- renderPrint({
        summary(get(input$dataset))
    })
    
## Output das estatística avançadas
    # Para escolher a temporada
    output$dados_avancados <- DT::renderDataTable({
        if(input$team2 == 'Flamengo'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Flamengo')
        }else if (input$team2 == 'Minas'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Minas')
        }else if (input$team2 == 'Brasília'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Brasília')
        }else if (input$team2 == 'Bauru'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Bauru')
        }else if (input$team2 == 'Macaé Basquete'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Macaé Basquete')
        }else if (input$team2 == 'Sesi Franca'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Sesi Franca')
        }else if (input$team2 == 'Vitória'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Vitória')
        }else if (input$team2 == 'Campo Mourão'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Campo Mourão')
        }else if (input$team2 == 'Pinheiros'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Pinheiros')
        }else if (input$team2 == 'Caxias do Sul'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Caxias do Sul')
        }else if (input$team2 == 'Paulistano'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Paulistano')
        }else if (input$team2 == 'Basq. Cearense'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Basq. Cearense')
        }else if (input$team2 == 'Vasco da Gama'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Vasco da Gama')
        }else if (input$team2 == 'Mogi'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Mogi')
        }else if (input$team2 == 'L.Sorocabana'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'L.Sorocabana')
        }else if (input$team2 == 'Corinthians'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Corinthians')
        }else if (input$team2 == 'São José'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'São José')
        }else if (input$team2 == 'Botafogo'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Botafogo')
        }else if (input$team2 == 'UNIFACISA'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'UNIFACISA')
        }else if (input$team2 == 'Pato Basquete'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Pato Basquete')
        }else if (input$team2 == 'São Paulo'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'São Paulo')
        }else if (input$team2 == 'Rio Claro'){
            estatisticas_avancadas() %>% 
                filter(Equipe == 'Rio Claro')
        }else{
            estatisticas_avancadas()
        }
    })
    # Output do resumo
    output$sumario <- renderPrint({
        summary(get(input$avancadas))
    })
}))