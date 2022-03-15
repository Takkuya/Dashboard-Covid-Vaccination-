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


#testando o plot usando apenas o ggplot
#ggplot(etniaAgrupado, aes(x=etniaAgrupado$paciente_racaCor_valor, y=etniaAgrupado$count)) + geom_bar(stat = "identity")


ui <- dashboardPage(
   dashboardHeader(title = "Dashboard covid19"),
   dashboardSidebar(
      sidebarMenuOutput("menu")
   ),
   dashboardBody(
      fluidRow(valueBox(quantidadeVacinacao, "Pessoas vacinadas")),
      fluidRow(column(7,plotOutput("etnia"))),
      fluidRow(column(7, plotOutput("vacinaDose"))),
   )
)

server <- function(input, output) {
   output$menu <- renderMenu({
      sidebarMenu(
         menuItem("Menu item", icon = icon("calendar"))
      )
   })
   
   output$etnia <- renderPlot({
      ggplot(data = etniaAgrupado, aes(x = paciente_racaCor_valor, y = count)) + geom_bar(stat = "identity")
   })
   
   output$vacinaDose <- renderPlot({
      ggplot(data = doseVacinaAgrupado, aes(x = vacina_descricao_dose, y = count)) + geom_bar(stat = "identity")
   })
}

shinyApp(ui, server)