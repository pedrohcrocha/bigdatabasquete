load("temporada_2018.Rda")
load("temporada_2017.Rda")
load("temporada_2016.Rda")

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input,output,session)({
    # Funções reativas
    data <- reactive({get(input$dataset)})
    top10 <- reactive({get(input$top)})
    data2 <- reactive({get(input$aux)})
    data3 <- reactive({get(input$count)})
    data4 <- reactive({get(input$adv)})
    
    ## Colocando a variável do time
        # OBS.: Como os times mudam de ano em ano, devemos fazer o renderUI
    output$team <-renderUI({
        selectInput('team', "Escolha um time", choices = data()$Equipe)
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
        }else if (input$team == 'Joinville / AABJ'){
            data() %>% 
                filter(Equipe == 'Joinville / AABJ')
        }else{
            data()
        }
    })
    # Output da estrutura
    output$str <- renderPrint({
        str(get(input$dataset))
    })
    # Output do resumo
    output$summary <- renderPrint({
        summary(get(input$dataset))
    })
    
## Output das estatística avançadas
    # Para escolher a temporada
    output$advance <-renderUI({
        selectInput("nadv", "Escolha uma estatística", choices = names(data4()))
    })
    # Para escolher a estatística (TABELA - GRÁFICO - (TOP10))
    output$sadv <- renderTable({
        if(input$nadv == "EFG"){
            data4 %>% 
                mutate(EFG = ((((TresFeitas+DoisFeitas) + 0.5*TresFeitas)/(TresTentadas+DoisTentadas))*100)) %>% 
                select(Jogador, Equipe, JO, EFG)
    }else if(input$nadv == "TS"){
            data4 %>% 
                mutate(TS = 100*(PTS/(2*((TresTentadas+DoisTentadas) + (0.44*LLT))))) %>% 
                select(Jogador, Equipe, JO, TS)
    }else if(input$nadv == "EFF"){
            data4 %>% 
                mutate(EFF = ((PTS + RT + AS + BR + TO - ((TresTentadas+DoisTentadas) - (TresFeitas+DoisFeitas)) - (LLT - LLC) - ER)/JO)) %>% 
                select(Jogador, Equipe, JO, EFF)
    }else if(input$nadv == "UR"){
            data4 %>% 
                mutate(UR_2 =  ((((TresTentadas+DoisTentadas)/JO)+ ((LLT/JO)*0.44) + ((AS/JO)*0.33) + (ER/JO)) * 40 * 1.009)/(Min*1.014)) %>% 
                select(Jogador, Equipe, JO, UR)
    }
    })
}))