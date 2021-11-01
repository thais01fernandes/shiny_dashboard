#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library('shiny')
library('data.table')
library('dplyr')
library('tidyverse')
library('ggplot2')
library('plotly')
library('utf8')
library('shinydashboard')
library('shinyWidgets')
library('bslib')

dados1 <- "https://raw.githubusercontent.com/alura-cursos/dashboard_com_r/dados/dados_limpos.csv"
dados_limpos  <- read_delim(dados1, delim = ",")

dados_limpos  <- dados_limpos  %>% 
    mutate_if(is.character, utf8_encode)

media_chamados_ano <- dados_limpos %>% group_by(anocalendario) %>% 
  summarise(qtd_chamados = n()) %>% 
  summarise(media_chamado_ano = mean(qtd_chamados)) %>% 
  as.numeric()

cabecalho <- dashboardHeader(title = "Dashboard")
barra_lateral <- dashboardSidebar(width = "250px", 
                                  sidebarMenu(menuItem("Dashboard", tabName = "Dashboard", icon =  icon("dashboard")), 
                                              menuItem('Informações', tabName = 'infos', icon = icon('info-circle'))))

painel_principal <- dashboardBody(
  
  tags$head(tags$style(HTML(".info-box, .info-box-icon, .small-box{height: 100px}"))),
 
  tabItems(tabItem(tabName = 'infos', 
                   h1('Informações'), 
                   infoBox(title = 'contato', subtitle = 'Para mais informações entre em contato: thais.fernandes.pereira@usp.br',
                           icon = icon("envelope-square"), color = 'black')),
           tabItem(tabName = 'Dashboard', 
                   fluidRow(
                     valueBox(subtitle = 'Quantidade de Registros', value = nrow(dados_limpos), icon = icon('database'), color = 'black'), 
                     infoBox(title = '', subtitle = "Média de Reclamações por Ano", value = media_chamados_ano, icon = icon('list'), color = 'black'),
                     valueBoxOutput(outputId = 'qtduf')
                   ),
                   fluidRow(
                     column(width = 12,
                            box(title = "Filtros", width = '100%',
                                column(width = 12, 
                                       box(width = '100%',
                                           awesomeCheckboxGroup(inline = T, inputId = "select_UF", 
                                                                label = "Estado:", 
                                                                choices = c("Todos",unique(dados_limpos$UF)),
                                                                selected = "Todos")
                                       )
                                ), 
                                
                                column(width = 6,
                                       box(width = '100%',
                                           dateRangeInput(inputId = 'data_abertura', 
                                                          label = "Data de Abertura:", 
                                                          format = "dd-mm-yyyy",
                                                          start = min(as.Date(dados_limpos$DataAbertura)), 
                                                          end = max(as.Date(dados_limpos$DataAbertura))))
                                       
                                       
                                ),
                                column(width = 6, 
                                       box(
                                         width = '100%',
                                         selectizeInput(inputId = "assunto", 
                                                        label = "Descrição Assunto:", 
                                                        choices = c("Todos", unique(dados_limpos$DescricaoAssunto)),
                                                        multiple = T, options = list(maxItems = 5), 
                                                        selected = "Todos")
                                       ))
                                
                            ) # final box
                     )
                   ), # final linha
                   
                   fluidRow(
                     column(width = 12, 
                            box(title = "Quantidade de Reclamações por Data", width = '100%', 
                                plotlyOutput(outputId = 'data', width = '100%'), 
                                verbatimTextOutput(outputId = 'descData')))),
                   fluidRow(
                     column(width = 6,
                            box(title = "Quantidade de Reclamações Atendidas", width = '100%', 
                                plotlyOutput(outputId = 'atendida'))), 
                     
                     column(width = 6,
                            box(title = "Quantidade de Reclamações Atendidas ou não por ano", width = '100%',
                                plotlyOutput(outputId = 'atendidaAno')))),
                   fluidRow(
                     column(width = 12, 
                            box(title = "Quantidade de Reclamações por UF",
                                width = '100%',
                                plotlyOutput(outputId = 'uf')))
           ))), # fim tabItems
  
  
               
)

ui <- dashboardPage(header = cabecalho, 
                    sidebar = barra_lateral,
                    skin = "purple", 
                    body = painel_principal)


# Define UI for application that draws a histogram
ui2 <- fluidPage(

    # Application title
    titlePanel("Indicadores para os colaboradores"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "select_UF", 
                               label = "Estado:", 
                               choices = c("Todos",unique(dados_limpos$UF)),
                               selected = "Todos"),
            
            dateRangeInput(inputId = 'data_abertura', 
                           label = "Data de Abertura:", 
                           format = "dd-mm-yyyy",
                           start = min(as.Date(dados_limpos$DataAbertura)), 
                           end = max(as.Date(dados_limpos$DataAbertura))),
            
           selectizeInput(inputId = "assunto", 
                        label = "Descrição Assunto:", 
                        choices = c("Todos", unique(dados_limpos$DescricaoAssunto)),
                        multiple = T, options = list(maxItems = 5), 
                        selected = "Todos")
            
        ),
        
        
            # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput(outputId = 'data'), 
           verbatimTextOutput(outputId = 'descData'),
           plotlyOutput(outputId = 'uf'),
           textOutput(outputId = 'descUF'),
           plotlyOutput(outputId = 'atendida'),
           plotlyOutput(outputId = 'atendidaAno')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
   dados_selecionados <- reactive({ 
       if(! "Todos" %in% input$select_UF) {
           dados_limpos <- dados_limpos %>% 
               filter(UF %in% input$select_UF)
       }
       
       if(! "Todos" %in% input$assunto){ 
           dados_limpos <- dados_limpos %>% 
               filter(DescricaoAssunto %in% input$assunto)
        }
       dados_limpos <- dados_limpos %>%  filter(as.Date(DataAbertura) >= input$data_abertura[1] &
           as.Date(DataAbertura) <= input$data_abertura[2])
       dados_limpos
   }) 
       
  output$data <- renderPlotly({ 
      
      ggplotly(
      data.frame(table(as.Date(dados_selecionados()$DataArquivamento))) %>% 
          rename(Data = Var1, Qtd = Freq) %>% 
          ggplot() +
          geom_line(aes(as.Date(Data), Qtd), group = 1, color = "mediumpurple3")+
          scale_x_date(date_labels = "%b-%Y", breaks = "6 month")+
          theme_minimal() +
          ggtitle("") + 
          xlab("") +
          ylab("") +
          theme(plot.title = element_text(family = "serif", size = 12, face = "bold", vjust = 0.5, colour = "black"),
                axis.text.x=element_text(size = 10, family = "serif", angle = 45, hjust = 1),
                axis.text.y=element_text(size = 11, family = "serif"))
      )   
      
  })
  
  

  
  output$uf <- renderPlotly({ 
      
      ggplotly(tooltip = "text", 
          data.frame(table(dados_selecionados()$UF)) %>% 
              rename(UF = Var1, Qtd = Freq) %>% 
                    ggplot()+
                    geom_bar(aes(x = reorder(UF, Qtd), y = Qtd,
                                 text = paste("UF:", UF, "<br>", "Qtd:", Qtd)), fill = "mediumpurple3", stat = "identity")+
                    coord_flip()+
                    xlab("") +
                    ylab("")+ 
                    theme_minimal()+
                    ggtitle("") + 
                    theme(plot.title = element_text(family = "serif", size = 12, face = "bold", vjust = 0.5, colour = "black"),
                          axis.text.x=element_text(size = 11, family = "serif"),
                          axis.text.y=element_text(size = 11, family = "serif")) 
     
        
      )
  
 })



output$atendida <- renderPlotly({ 
    
    ggplotly(
            ggplot(dados_selecionados()) +
            geom_bar(aes(Atendida), fill = c("mediumpurple3", "#f3c5ff"), stat = "count", width = 0.80, colour="white", alpha = 0.9)+
            theme_minimal()+
            xlab("") +
            ylab("") +
            ggtitle("") +
        theme(plot.title = element_text(family = "serif", size = 12, face = "bold", vjust = 0.5, colour = "black"),
              axis.text.x=element_text(size = 14, family = "serif"),
              axis.text.y=element_text(size = 11, family = "serif")))
        
})   


output$atendidaAno <- renderPlotly({ 
    
    ggplotly(
        data.frame(table(dados_selecionados()$anocalendario, dados_selecionados()$Atendida)) %>% 
                    rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>% 
                    ggplot() +
            geom_bar(aes(x = Ano, y = Qtd, fill = Atendida), stat = "identity", position = position_fill()) + 
            theme_minimal() + 
            ggtitle("") + 
            xlab("") +
            ylab("") +
            scale_x_discrete(breaks=seq(2009,2018,1)) + 
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            scale_fill_manual(name = "Atendida?", values = c("mediumpurple3", "#f3c5ff"), labels = c("Sim", "Não")) +
            theme(plot.title = element_text(family = "serif", size = 12, face = "bold", vjust = 0.5, colour = "black"),
                  text = element_text(family = "serif"),
                  axis.text.x=element_text(size = 10, family = "serif"),
                  axis.text.y=element_text(size = 10, family = "serif"),
                  legend.position = "bottom"))
}) 


output$descData <- renderText ({
    paste("Gráfico com a quantidade de reclamações feitas entre: ", 
          min(dados_selecionados()$DataAbertura), "-", 
          max(dados_selecionados()$DataAbertura))
    
    
})

output$descUF <- renderText({
    estados <- paste(unique(dados_selecionados()$UF), collapse = ",")
    paste("Gráfico com a quantidade de reclamações feitas pelas UF: ", estados)
    
})

output$qtduf <- renderValueBox({
  valueBox(value = length(unique(dados_selecionados()$UF)), 
           subtitle = 'UFs Selecionadas', icon = icon('map-marker'), color = 'black')
})

} 

# Run the application 
shinyApp(ui = ui, server = server)
