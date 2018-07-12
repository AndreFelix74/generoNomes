library(shiny)
library(readxl)

source("dataHandler.R")

options(shiny.maxRequestSize = 16*1024^2)

ui <- fluidPage(
  fluidRow(
    headerPanel('Um metodo para atribuir genero a uma lista de nomes proprios brasileiros.')
  ),
  fluidRow(
    column(12,
           h4('Como determinar o genero de um nome proprio?'),
           p('Em pesquisas que precisamos do genero de cada observacao e dispomos apenas da lista de nomes proprios sem a declaracao de genero, qual o melhor metodo para atribuir o genero a cada observacao?'),
           p('Poderiamos utilizar a ultima letra do nome? Nomes terminados em "a" na lingua brasileira sao sempre femininos? Isso e verdade para Ana e Adriana, por exemplo, mas temos os nomes Batista e Evangelista que sao geralmente masculinos. O mesmo ocorre com a letra "o", geralmente masculina, mas encontrada nos nomes femininos como Amparo, Carmo e Socorro. Ao tentar utilizar a ultima letra do nome encontraremos a letra "e" presente nos nomes masculinos Andre e Jose e tambem nos femininos Isabele e Tatiane. Seguindo nesse metodo, identificaremos os nomes terminados em "r" e "s" que, da mesma forma, podem ser atribuidos aos dois generos como: Guiomar e Gilmar, Marcos e Lourdes.'),
           p('O ideal seria dispormos de uma lista de nomes proprios com o genero socialmente atribuido ao nome. O IBGE disponibilizou os dados do censo de 2010 com a frequencia de registros de nascimento por nome.'),
           h4('Metodo'),
           p('Esse projeto utiliza um metodo estatistico de atribuicao de genero utilizando a lista de frequencia de registros de nascimento disponibilizada pelo IBGE no sistema de dados abertos do Governo Federal.'),
           p('E a mesma base de dados utilizada nesse site: ', a(href = 'https://censo2010.ibge.gov.br/nomes/#/search', 'https://censo2010.ibge.gov.br/nomes/#/search', target = '_blank')),
           h4('Como utilizar'),
           tags$ol(
             tags$li('Fazer o upload do arquivo com o nomes;'),
             tags$li('Informar se o arquivo contem cabecalhos de coluna na primeira linha;'),
             tags$li('Selecionar qual coluna do arquivo contem os nomes;'),
             tags$li('Aguardar o calculo;'),
             tags$li('Fazer download do arquivo com o resultado.')
             ),
           h4('Resultado'),
           p('Serao criadas 4 colunas:'),
           tags$ol(
             tags$li('Primeiro Nome.'),
             tags$li('freq_f, contem a quantidade de registros de nascimentos do sexo feminio para o nome dado.'),
             tags$li('freq_m, contem a quantidade de registros de nascimentos do sexo masculino para o nome dado.'),
             tags$li('Genero, F caso a freq_f seja maior ou igual a freq_m, ou M caso contrario.')
           ),
           h4('Processamento'),
           selectInput('userEncoding', 'Informe o enconding do arquivo', choices = c('UTF-8', 'ISO-8859-1')),
           checkboxInput('zipFile', 'Arquivo no formato zip', FALSE),
           radioButtons("fieldsSeparator", "Separator", choices = c(Virgula = ",", Ponto_e_virgula = ";", Tabulacao = "\t"), selected = ","),
           fileInput('fileWithNames', label = 'Escolha o arquivo - maximo 16Mb', buttonLabel = 'Procurar', placeholder = 'Nenhum arquivo selecionado',
                  accept = c(
                    '.csv',
                    '.zip'
                  )
          ),
          checkboxInput('firstRowIsHeader', 'Primeira linha do arquivo contem cabecalhos de coluna.', TRUE),
          uiOutput('columnName')
  )),
  fluidRow(
    tableOutput('tableFileHeadContents')
  ),
  fluidRow(
    downloadButton('download', 'Baixar dados')
  )
)

server <- function(input, output)
{
  fileContent <- reactive({
    inFile <- input$fileWithNames
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    strFilePath <- NULL

    if (input$zipFile) {
      listFileName <- unzip(inFile$datapath, list = TRUE)
      strFileName <- listFileName$Name[1]
      unzip(inFile$datapath, strFileName, exdir = dirname(inFile$datapath))
      strFilePath <- paste0(dirname(inFile$datapath), '/', strFileName)
    } else {
      strFilePath <- inFile$datapath
    }

    tryCatch(
      {
        read.csv(strFilePath, header = input$firstRowIsHeader,
                 sep = input$fieldsSeparator, encoding = input$userEncoding)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })

  output$tableFileHeadContents <- renderTable({
    if (is.null(fileContent())) {
      return()
    }

    if (input$columnWithNames == 'Selecione') {
      return(head(fileContent()))
    }

    head(generoNomes(fileContent(), input$columnWithNames))
  })
  
  output$columnName <- renderUI({
    if (is.null(fileContent())) {
      data <- c('Faca upload do arquivo.')
    } else {
      data <- as.list(c('Selecione', colnames(fileContent())))
    }

    selectInput('columnWithNames', 'Informe a coluna que contem os nomes', choices = data)
  })

  output$download <- downloadHandler(
    # o download so funciona se a aplicacao for aberta no navegador. nao funciona no navegador do R Studio.
    # On a Unix-alike, the default for zip will by default use the value of R_ZIPCMD, which is set in 'etc/Renviron' if an unzip command was found during configuration.
    # On Windows, the default relies on a zip program (for example that from Rtools) being in the path.
    filename = 'generoNomes.zip',
    content = function(file) {
      strCSVFileName <- paste0(tempdir(), '/', 'generoNomes.csv')
      write.csv2(generoNomes(fileContent(), input$columnWithNames), strCSVFileName, row.names = F)
      zip(zipfile = file, files = strCSVFileName)
    }
  )
}

shinyApp(ui = ui, server = server)