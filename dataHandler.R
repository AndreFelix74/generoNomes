#fileWithNames <- read.csv('C:/Users/Andre.Felix/Downloads/inventoras (1).csv')
#colWithNames <- 4
main <- function(fileWithNames, colWithNames) {
  #fileWithNames <- fileWithNames[regexpr(',', fileWithNames[,colWithNames]) > 0,]

  fileWithNames$first_name <- substring(fileWithNames[,colWithNames], 1, regexpr(' ', fileWithNames[,colWithNames]) - 1)
  
  fileWithNames$first_name <- tolower(fileWithNames$first_name)
  
  fileWithNames$first_name <- gsub('[aàáâäãåæ]', 'a', fileWithNames$first_name)
  fileWithNames$first_name <- gsub('[eèéêë]', 'e', fileWithNames$first_name)
  fileWithNames$first_name <- gsub('[iìíîï]', 'i', fileWithNames$first_name)
  fileWithNames$first_name <- gsub('[oòóôöõø]', 'o', fileWithNames$first_name)
  fileWithNames$first_name <- gsub('[uùúûü]', 'u', fileWithNames$first_name)

  fileWithNames$first_name <- toupper(fileWithNames$first_name)

  getDataCenso()

  nomes_censo_f <- read_excel('data/Brasil_total fem masc.xlsx', sheet = 2)
  nomes_censo_m <- read_excel('data/Brasil_total fem masc.xlsx', sheet = 3)


  fileWithNames <- merge(x = fileWithNames, y = nomes_censo_f, by.x = 'first_name', by.y = 'nome', all.x = TRUE)[, union(names(fileWithNames), 'Freq')]
  colnames(fileWithNames)[which(colnames(fileWithNames) == 'Freq')] <- 'freq_f'
  
  fileWithNames <- merge(x = fileWithNames, y = nomes_censo_m, by.x = 'first_name', by.y = 'nome', all.x = TRUE)[, union(names(fileWithNames), 'Freq')]
  colnames(fileWithNames)[which(colnames(fileWithNames) == 'Freq')] <- 'freq_m'
  
  fileWithNames$freq_f[is.na(fileWithNames$freq_f)] <- 0
  fileWithNames$freq_m[is.na(fileWithNames$freq_m)] <- 0
  
  fileWithNames$genero <- ifelse(fileWithNames$freq_f == 0 & fileWithNames$freq_m == 0, NA, ifelse(fileWithNames$freq_f > fileWithNames$freq_m, 'F', 'M'))
  
  #fileWithNames$freq_f <- NULL
  #fileWithNames$freq_m <- NULL
  #fileWithNames$first_name <- NULL

  fileWithNames
}

getDataCenso <- function() {
  strFileName <- 'Brasil_total fem masc.xlsx'

  if (!(file.exists(strFileName))) {
    unzip('data/Projeto Nomes Censo 2010.zip', files = strFileName, exdir = 'data')
  }

  read_excel(paste('data/', strFileName, sep = ''))
}

