library(shiny)
library(shinythemes)
library(markdown)
library(shinycssloaders)

navbarPage("VISUALIZAÇÃO GRÁFICA E RESUMIDA DOS RESULTADOS DAS EDIÇÕES DO ENEM NOS ANOS DE 2010 A 2015",
           tabPanel("Gráficos", icon = icon("bar-chart-o"),
                    fluidPage(theme = shinytheme("flatly"),
                      # titulo
                      titlePanel("Gráficos das Edições do ENEM (2010 a 2015)"),
                      hr(), # pular linha
                      
                      sidebarLayout(
                        sidebarPanel(width = 5,
                          # update view
                          submitButton("Atualizar Opções"),
                          
                          # seleção da variável nota
                          radioButtons(inputId = "nota",
                                       label = h4("Selecione a nota a ser exibida"),
                                       choices = c("Média Geral", "Média da Redação", 
                                                   "Nota em Ciências da Natureza",
                                                   "Nota em Ciências Humanas",
                                                   "Nota em Linguagens e Códigos",
                                                   "Nota em Matemática"),
                                       selected = "Média Geral"),
                          # seleção dos anos a serem exibidos
                          selectInput("anos", label = h4("Selecione o(s) ano(s) a ser(em) exibido(s)"), 
                                      choices = list("2010","2011","2012","2013","2014","2015",
                                                     "Todos os anos"),selected = "2015"),
                          
                          # seleção das variaveis categoricas do gráfico
                          selectInput(inputId = "vcategorica",
                                       label = h4("Selecione como deseja ver os dados"),
                                       choices = list("Brasil",
                                                   "Por sexo",
                                                   "Por região",
                                                   "Por sexo e região",
                                                   "Por renda"),
                                       selected = "Brasil"),
                          
                          # seleção do gráfico
                          radioButtons(inputId = "grafico", 
                                       label = h4("Escolha o tipo de gráfico"),
                                       choices = c("Histograma","Boxplot"))
                        ),
                        # organização da tela de visualização
                        mainPanel(width = 7,
                          withSpinner(plotOutput("plot"), color = getOption("spinner.color", "#008080"),
                                      type = getOption("spinner.type",4)),
                          hr(),
                          print("Desenvolvido por: Marylaine Nascimento e Marcus Nunes")
                        )
                      ))
             ),
           tabPanel("Tabelas Resumo", icon = icon("table"),
                    titlePanel("Tabela Resumo com Alguns Parâmetros"),
                    print(h4("Faça download das tabelas e fique a vontade para analisar")),
                    downloadButton("downloadtable", label = "Download"),
                        tableOutput("tabela")
           ),
           navbarMenu("Como Interpretar", icon = icon("question-sign", lib = "glyphicon"),
                      tabPanel("Histograma",
                               includeMarkdown("histograma.md")
                      ),
                      tabPanel("Boxplot",
                               includeMarkdown("boxplot.md")
                      )),
           tabPanel("Sobre", icon = icon("comment", lib = "glyphicon"),
                    includeMarkdown("sobre.md"))
)