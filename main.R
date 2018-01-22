library(shiny)

ui <- fluidPage(
  fluidRow(
    headerPanel('Lorem Ipsum')
  ),
  fluidRow(
    column(12,
           p('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut sem dolor, tempus in tincidunt vitae, imperdiet cursus sem. Vestibulum accumsan sapien sem, in sollicitudin leo venenatis ac. Aenean lorem neque, laoreet at pellentesque id, sodales et odio. Maecenas ut rhoncus neque. Etiam tristique magna ac neque ultrices, sed posuere ante fermentum. Nulla eget quam libero. Nam laoreet, tortor id rutrum posuere, leo ex tempor felis, at sollicitudin tellus eros quis sem.'),
           p('Aliquam et blandit turpis. Duis sodales massa eget leo laoreet, at consectetur mi auctor. In mollis ipsum ultricies consequat bibendum. Nullam vel metus a arcu viverra lobortis. Vestibulum a ipsum quis elit maximus facilisis nec ut turpis. In pellentesque in turpis eget condimentum. Maecenas sed risus sed dui posuere pharetra. Ut finibus viverra tempor. Proin nisl massa, placerat sit amet tempus vitae, efficitur sed arcu. Nam elementum vehicula nibh.')
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
    tableOutput('tableFileHeadContents')
  ),
  fluidRow(
    checkboxInput('firstRowIsHeader', 'Primeira linha do arquivo contem cabecalhos de coluna.', TRUE),
    selectInput('columnName', 'Coluna que contem os nomes', 'bla')
  )
)

server <- function(input, output)
{
  output$tableFileHeadContents <- renderTable({

    inFile <- input$fileWithNames
    
    if (is.null(inFile))
      return(NULL)
    
    fileContent <- read.csv(inFile$datapath, header = input$firstRowIsHeader)

    head(fileContent)
  })
}

shinyApp(ui = ui, server = server)