library(shiny)
library(readxl)

source("dataHandler.R")

ui <- fluidPage(
  fluidRow(
    headerPanel('Um metodo para atribuir automaticamente genero a uma lista de nomes proprios brasileiros.')
  ),
  fluidRow(
    column(12,
           h4('Como determinar o genero de um nome proprio?'),
           p('Em pesquisas que precisamos determinar o genero de cada observacao, as vezes, dispomos apenas de uma lista de nomes proprios, sem a declaracao de genero.'),
           p('Poderiamos utilizar a ultima letra do nome? Nomes terminados em "a" na lingua brasileira sao sempre femininos? Isso e verdade para Ana e Adriana, por exemplo, mas temos os nomes Batista e Evangelista que sao geralmente masculinos. O mesmo ocorre com a letra "o", geralmente masculina, mas encontrada nos nomes femininos como Amparo, Carmo e Socorro. Ao tentar utilizar a ultima letra do nome encontraremos a letra "e" presente nos nomes masculinos Andre e Jose e tambem nos femininos Isabele e Tatiane. Seguindo nesse metodo, identificaremos os nomes terminados em "r" e "s" que, da mesma forma, podem ser atribuidos aos dois generos como: Guiomar e Gilmar, Marcos e Lourdes.'),
           p('O ideal seria dispormos de uma lista de nomes proprios com o genero socialmente atribuido ao nome. O IBGE disponibilizou os dados do censo de 2010 com a frequencia de registros de nascimento por nome.'),
           h4('Metodo'),
           p('Esse projeto visa estabelecer um metodo estatistico de atribuicao de genero utilizando a lista de frequencia de registros de nascimento disponibilizada pelo IBGE no sistema de dados abertos do Governo Federal.'),
           p('Apos fazer o upload do arquivo com o nomes e necessario informar qual coluna contem os nomes. Em seguida o script ira determinar, para cada observacao, que o nome proprio e o conjunto de caracteres do inicio do texto ate o primeiro caractere espaco encontrado. Depois, a informacao de frequencia de resgistro do censo de 2010 sera relacionada ao nome e, por ultimo, sera atribuido o genero com base no maior numero de ocorrencias de registros.')
           )),
  fluidRow(
    fileInput('fileWithNames', label = 'Escolha o arquivo', buttonLabel = 'Procurar', placeholder = 'Nenhum arquivo selecionado',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv'
              )
    )),
  fluidRow(
    checkboxInput('firstRowIsHeader', 'Primeira linha do arquivo contem cabecalhos de coluna.', TRUE),
    uiOutput('columnName'),
    downloadButton('download', 'Baixar dados')
  ),
  fluidRow(
    tableOutput('tableFileHeadContents')
  )
)

server <- function(input, output)
{
  fileContent <- reactive({
    inFile <- input$fileWithNames
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    read.csv(inFile$datapath, header = input$firstRowIsHeader)
  })

  output$tableFileHeadContents <- renderTable({
    if (is.null(fileContent())) {
      return()
    }
    if (input$columnWithNames == 'Selecione') {
      return(head(fileContent()))
    }
    head(main(fileContent(), input$columnWithNames))
  })
  
  output$columnName <- renderUI({
    if (is.null(fileContent())) {
      data <- c('Faca upload do arquivo.')
    } else {
      data <- as.list(c('Selecione', colnames(fileContent())))
    }

    selectInput('columnWithNames', 'Informe a coluna que contem os nomes', choices = data)
  })
}

shinyApp(ui = ui, server = server)