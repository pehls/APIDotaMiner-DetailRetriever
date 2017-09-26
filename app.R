install.packages('caret')
install.packages('caTools')
install.packages('shiny')
install.packages('e1071')
install.packages('jsonlite')
install.packages('ggplot2')
install.packages('rsconnect')
rsconnect::setAccountInfo(name='pehls', token='FB6ADD16A53DC5A313E39B7CDC717F78', secret='AEk3evLrGuMfcLdTpzW4iqshiUeCiDiQt/e74z0b')
library(caret)
library(e1071)
library(rpart.plot)
library(ggplot2)
library(shiny)
getFilesFromWD <- function (wd) {
  setwd(wd)
  filesList <- list.files()
  return(filesList)
}
getMatchIDSfromJSON <- function( hero_id) {
  wd <-"C:\\data\\byHero"
  nFiles <- length(getFilesFromWD(paste(paste(wd,"\\00-",sep=""), hero_id, sep=""))) -1
  list <- list()
  temp <- list()
  library(jsonlite)
  setwd(paste(paste(wd,"\\00-",sep=""), hero_id, sep=""))
  for(j in 0:nFiles) {
    tryCatch({
      paste("0",j, sep="")
      name <- paste(paste(paste(paste("0",j, sep="") ,"-", sep=""),hero_id,sep=""),".json", sep="")
      data <- fromJSON(name, flatten=TRUE)
      temp$match_id <- data$result$matches$match_id
      list <- rbind(list, as.data.frame(temp))
      
    },error = function(cond) {
      return (NA)
    }, warning = function(cond) {
      return (NULL)
    })
  }
  return (list)
}
getByIDSFromURI <- function(list, hero_id){
  # setwd("C:\\Users\\gabri\\git\\APIDotaMiner\\apiMinerDota\\files\\out\\byHero\\test\\00-4")
  
  library(jsonlite)
  link <- "https://api.opendota.com/api/matches/"
  temp <- list()
  retorno <- list()
  data <- list()
  for (i in 1:length(list$match_id)){
    
    tryCatch({
      data <- fromJSON(paste(link,list$match_id[i],sep=""), flatten=TRUE)
      temp = getVariables(data, hero_id)
      retorno <- rbind(retorno,temp)
    },error = function(cond) {
      return (NA)
    }, warning = function(cond) {
      return (NULL)
    })
  }
  setwd('C:\\data\\out')
  write.csv(retorno, file=paste(paste("teste_hero",hero_id, sep=""), ".csv", sep=""))
  return(retorno)
}
getVariables <- function(data, hero_id) {
  id <- NA
  for(i in 1:length(data$players$hero_id)) {
    if(data$players$hero_id[i]==hero_id) {
      id = i
    } 
    
  }
  if(!is.na(id)) {
    temp <- list()
    temp$match_id                                     <- data$match_id
    temp$game_mode                                    <- data$game_mode
    temp$dire_score                                   <- data$dire_score
    temp$human_players                                <- data$human_players
    temp$lobby_type                                   <- data$lobby_type
    temp$engine                                       <- data$engine
    temp$radiant_win                                  <- data$radiant_win
    temp$radiant_score                                <- data$radiant_score
    temp$player.player_slot                           <- data$players$player_slot[id]
    temp$player.assists                               <- data$players$assists[id]
    temp$player.deaths                                <- data$players$deaths[id]
    temp$player.denies                                <- data$players$denies[id]
    temp$player.hero_damage                           <- data$players$hero_damage[id]
    temp$player.hero_healing                          <- data$players$hero_healing[id]
    temp$player.hero_id                               <- data$players$hero_id[id]
    temp$player.item_0                                <- data$players$item_0[id]
    temp$player.item_1                                <- data$players$item_1[id]
    temp$player.item_2                                <- data$players$item_2[id]
    temp$player.item_3                                <- data$players$item_3[id]
    temp$player.item_4                                <- data$players$item_4[id]
    temp$player.item_5                                <- data$players$item_5[id]
    temp$player.kills                                 <- data$players$kills[id]
    temp$player.leaver_status                         <- data$players$leaver_status[id]
    temp$player.level                                 <- data$players$level[id]
    temp$player.tower_damage                          <- data$players$tower_damage[id]
    temp$player.isRadiant                             <- data$players$isRadiant[id]
    temp$player.win                                   <- data$players$win[id]
    temp$player.lose                                  <- data$players$lose[id]
    temp$player.kda                                   <- data$players$kda[id]
    temp$player.abandons                              <- data$players$abandons[id]
    temp$player.benchmarks.gold_per_min.raw 			    <-data$players$benchmarks.gold_per_min.raw[id]
    temp$player.benchmarks.gold_per_min.pct 			    <-data$players$benchmarks.gold_per_min.pct[id]
    temp$player.benchmarks.hero_damage_per_min.raw 	  <-data$players$benchmarks.gold_per_min.raw[id]
    temp$player.benchmarks.hero_damage_per_min.pct 	  <-data$players$benchmarks.hero_damage_per_min.pct[id]
    temp$player.benchmarks.hero_healing_per_min.raw   <-data$players$benchmarks.hero_healing_per_min.raw[id]
    temp$player.benchmarks.hero_healing_per_min.pct	  <-data$players$benchmarks.hero_healing_per_min.pct[id]
    temp$player.benchmarks.tower_damage.raw			      <-data$players$benchmarks.tower_damage.raw[id]
    temp$player.benchmarks.tower_damage.pct			      <-data$players$benchmarks.tower_damage.pct[id]
    temp$player.benchmarks.xp_per_min.raw 				    <-data$players$benchmarks.xp_per_min.raw [id]
    temp$player.benchmarks.xp_per_min.pct  			      <-data$players$benchmarks.xp_per_min.pct  [id]
    temp$player.benchmarks.kills_per_min.raw 			    <-data$players$benchmarks.kills_per_min.raw [id]
    temp$player.benchmarks.kills_per_min.pct			    <-data$players$benchmarks.kills_per_min.pct[id]
    temp$player.benchmarks.hits_per_min.raw  			    <-data$players$benchmarks.hits_per_min.raw [id] 
    temp$player.benchmarks.hits_per_min.pct 			    <-data$players$benchmarks.hits_per_min.pct [id]
  }
  return(temp)
}
getTree <- function(hero_id) {
  setwd('C:\\data\\out')
  data <- read.csv(paste(paste( 'teste_hero',hero_id, sep=""), '.csv', sep=""))
  data$player.item_0 <- as.factor(data$player.item_0)
  data$player.item_1 <- as.factor(data$player.item_1)
  data$player.item_2 <- as.factor(data$player.item_2)
  data$player.item_3 <- as.factor(data$player.item_3)
  data$player.item_4 <- as.factor(data$player.item_4)
  data$player.item_5 <- as.factor(data$player.item_5)
  #install.packages('caret')
  library(caret)
  #install.packages('rpart')
  library(rpart)
  library(rpart.plot)
  #install.packages('ggplot2')
  library(ggplot2)
  #install.packages('caTools')
  library(caTools)
  
  spl <- sample.split(data$player.win, SplitRatio = 0.7)
  Train <- subset(data, spl==TRUE)
  Test <- subset(data, spl==FALSE)
  res1 <- rpart(player.win~player.item_0+player.item_1+player.item_2+player.item_3+player.item_4+player.item_5, 
                data=Train, 
                method="class", 
                minbucket=15)
  predict <- predict(res1, newdata = Test, type="class")
  #table(Test$player.win, predict)
  #pdf('pdfs\\treeCV.pdf') 
  prp(res1)
  #dev.off()
}
getTreeCV <- function(hero_id) {
  setwd('C:\\data\\out')
  data <- read.csv(paste(paste( 'teste_hero',hero_id, sep=""), '.csv', sep=""))
  data$player.item_0 <- as.factor(data$player.item_0)
  data$player.item_1 <- as.factor(data$player.item_1)
  data$player.item_2 <- as.factor(data$player.item_2)
  data$player.item_3 <- as.factor(data$player.item_3)
  data$player.item_4 <- as.factor(data$player.item_4)
  data$player.item_5 <- as.factor(data$player.item_5)
  #install.packages('caret')
  library(caret)
  #install.packages('e1071')
  library(e1071)
  library(rpart.plot)
  #install.packages('ggplot2')
  library(ggplot2)
  #install.packages('caTools')
  library(caTools)
  
  spl <- sample.split(data$player.win, SplitRatio = 0.7)
  Train <- subset(data, spl==TRUE)
  Test <- subset(data, spl==FALSE)
  numfolds <- trainControl(method = "cv", number=6)
  cpgrid = expand.grid(.cp=seq(0.01,0.5,0.01))
  #train <- train(player.win~player.item_0+player.item_1+player.item_2+player.item_3+player.item_4+player.item_5, 
  #               data=Train,
  #               method="rpart",
  #               trControl=numfolds,
  #               tuneGrid=cpgrid)
  treecv <-  rpart(player.win~player.item_0+player.item_1+player.item_2+player.item_3+player.item_4+player.item_5, 
                   data=Train,
                   method="class",
                   cp=0.01)
  predictcv <- predict(treecv, newdata=Test, type="class")
  #table(Test$player.win, predictcv)
  #pdf('pdfs\\treeCV.pdf') 
  prp(treecv)
  #dev.off() 
}
normalize <- function (hero_id) {
  setwd('C:\\data\\out')
  data <- read.csv(paste(paste( 'teste_hero',hero_id, sep=""), '.csv', sep=""))
  data$player.item_0 <- as.factor(data$player.item_0)
  data$player.item_1 <- as.factor(data$player.item_1)
  data$player.item_2 <- as.factor(data$player.item_2)
  data$player.item_3 <- as.factor(data$player.item_3)
  data$player.item_4 <- as.factor(data$player.item_4)
  data$player.item_5 <- as.factor(data$player.item_5)
  
  install.packages('caTools')
  library(caTools)
  
  spl <- sample.split(data$player.win, SplitRatio = 0.7)
  Train <- subset(data, spl==TRUE)
  Test <- subset(data, spl==FALSE)
  t<- list()
  t$player.win <- Train$player.win
  t$player.assists <- Train$player.assists
  t$player.denies <- Train$player.denies
  t$player.deaths <- Train$player.deaths
  t$player.kills <- Train$player.kills
  t$player.level <- Train$player.level
  t$player.hero_healing <- Train$player.hero_healing
  t$player.benchmarks.gold_per_min.raw  <- Train$player.benchmarks.gold_per_min.raw
  t$player.tower_damage <- Train$player.tower_damage
  t$player.hero_damage <- Train$player.hero_damage
  ranges <- list()
  ranges <- sapply(t, function(x) max(x))
  normalized <- t
  normalized$player.assists <- t$player.assists/ranges[2]
  normalized$player.denies <- t$player.denies/ranges[3]
  normalized$player.deaths <- t$player.deaths/ranges[4]
  normalized$player.kills <- t$player.kills/ranges[5]
  normalized$player.level <- t$player.level/ranges[6]
  normalized$player.hero_healing <- t$player.hero_healing/range[7]
  normalized$player.benchmarks.gold_per_min.raw <- t$player.benchmarks.gold_per_min.raw/ranges[8]
  normalized$player.tower_damage <- t$player.tower_damage/ranges[9]
  normalized$player.hero_damage <- t$player.hero_damage/ranges[10]
  setwd('C:\\data\\out')
  write.csv(retorno, file=paste(paste("normalizedTrain_hero",hero_id, sep=""), ".csv", sep=""))
  t<- list()
  t$player.win <- Test$player.win
  t$player.assists <- Test$player.assists
  t$player.denies <- Test$player.denies
  t$player.deaths <- Test$player.deaths
  t$player.kills <- Test$player.kills
  t$player.level <- Test$player.level
  t$player.hero_healing <- Train$player.hero_healing
  t$player.benchmarks.gold_per_min.raw  <- Test$player.benchmarks.gold_per_min.raw
  t$player.tower_damage <- Test$player.tower_damage
  t$player.hero_damage <- Test$player.hero_damage
  ranges <- list()
  ranges <- sapply(t, function(x) max(x))
  normalized <- t
  normalized$player.assists <- t$player.assists/ranges[2]
  normalized$player.denies <- t$player.denies/ranges[3]
  normalized$player.deaths <- t$player.deaths/ranges[4]
  normalized$player.kills <- t$player.kills/ranges[5]
  normalized$player.level <- t$player.level/ranges[6]
  normalized$player.hero_healing <- t$player.hero_healing/range[7]
  normalized$player.benchmarks.gold_per_min.raw <- t$player.benchmarks.gold_per_min.raw/ranges[8]
  normalized$player.tower_damage <- t$player.tower_damage/ranges[9]
  normalized$player.hero_damage <- t$player.hero_damage/ranges[10]
  setwd('C:\\data\\out')
  write.csv(retorno, file=paste(paste("normalizedTest_hero",hero_id, sep=""), ".csv", sep=""))
  
}
# Define UI for data download app ----
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    # a select input
    selectInput('hero_id', 'hero_id', choices = list(
      hero_id = c(`Anti-Mage` = '1', `Axe` = '2', `Bane` = '3')), selectize = FALSE)
  ),
  mainPanel(
    verbatimTextOutput('values'),
    # tableOutput("table"),
    plotOutput("plotTree"),
    plotOutput("plotTreeCV")
  )
), title = 'Selecionar Hero')
# Define server logic to display and download selected file ----
server <- function(input, output, session) {
  
  # updateSelectizeInput(session, 'x2', choices = list(
  #   Eastern = c(`Rhode Island` = 'RI', `New Jersey` = 'NJ'),
  #   Western = c(`Oregon` = 'OR', `Washington` = 'WA'),
  #   Middle = list(Iowa = 'IA')
  # ), selected = 'IA')
  
  output$values <- renderPrint({
    list( hero_id = input$hero_id)
  })
  output$table <- renderTable({
    setwd('C:\\data\\out')
    header(read.csv(paste(paste("teste_hero", input$hero_id, sep=""), ".csv", sep="")))
  })
  output$plotTree <- renderPlot( {
    getTree(input$hero_id)
  })
  output$plotTreeCV <- renderPlot( {
    getTreeCV(input$hero_id)
  })
  
}
#getByIDSFromURI(getMatchIDSfromJSON(4),4)
# getTree(i)
# getTreeCV(i)
# normalize(i)

# Create Shiny app ----
shinyApp(ui, server)
#rsconnect::deployApp(appDir = "E:\\Rapp", appFiles = "app.R",account = 'pehls',upload = TRUE)
