library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

Sys.setlocale("LC_ALL", "C")

vacinaDados <- read.csv(file = "amostra1.csv", sep = ";")

#quantidade de pessoas vacinadas
pacienteId <- vacinaDados %>% group_by(paciente_id) 
quantidadeVacinacao <- nrow(pacienteId)

#quantidade por etnia
etniaAgrupado <- vacinaDados %>% group_by(paciente_racaCor_valor) %>% summarise(count = n())

#quantidade por faixa etária

#quantidade por dose da vacina
doseVacinaAgrupado <- vacinaDados %>% group_by(vacina_descricao_dose) %>% summarise(count = n())

#porcentagem gênero
sexo <- vacinaDados %>% filter(paciente_enumSexoBiologico %in% c("M", "F")) %>% group_by(paciente_enumSexoBiologico) %>% count() %>% ungroup() %>% mutate(porcentagem = round(n/sum(n),4)*100,  lab.pos = cumsum(porcentagem) -.5*porcentagem)

#quantidade marca vacina
marcasVacina <- vacinaDados %>% group_by(vacina_fabricante_nome) %>% summarise(count = n())

ui <- dashboardPage(
   dashboardHeader(title = "Dashboard covid19"),
   dashboardSidebar(
      sidebarMenuOutput("menu")
   ),
   dashboardBody(
      fluidRow(valueBox(quantidadeVacinacao, "Pessoas vacinadas")),
      fluidRow(column(6,plotOutput("etnia")), column(6, plotOutput("vacinaDose"))),
      fluidRow(column(6,plotOutput("marcas")), column(6, plotOutput("genero"))),
   )
)

server <- function(input, output) {
   
   output$etnia <- renderPlot({
      ggplot(data = etniaAgrupado, aes(x = paciente_racaCor_valor, y = count, fill = paciente_racaCor_valor)) + geom_bar(stat = "identity")
   })
   
   output$vacinaDose <- renderPlot({
      ggplot(data = doseVacinaAgrupado, aes(x = vacina_descricao_dose, y = count, fill = vacina_descricao_dose)) + geom_bar(stat = "identity")
   })
   
   output$marcas <- renderPlot({
      ggplot(data = marcasVacina, aes(x = vacina_fabricante_nome, y = count, fill = vacina_fabricante_nome)) + geom_bar(stat = "identity")
   })
   
   output$genero <- renderPlot({
      ggplot(data = sexo, 
             aes(x = "", y = porcentagem, fill = paciente_enumSexoBiologico))+
         geom_bar(stat = "identity")+
         coord_polar("y", start = 200) +
         geom_text(aes(y = lab.pos, label = paste(porcentagem,"%", sep = "")), col = "white") +
         theme_minimal() + ggtitle( "Sexos das pessoas vacinadas" ) +
         scale_fill_brewer(palette = "Paired")
   })
}

shinyApp(ui, server)