#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(maps)
library(leaflet)
library(naivebayes)
library(tidyverse)
library(DT)
library(corrplot)
library(stargazer)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(sentimentr)
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("stringr")

library(rvest)
library(dplyr)
library(stringr)
library(tidyr)


#install.packages("httr")
#install.packages("RCurl")
#install.packages("XML")
library(XML)
library(httr)
library(RCurl)

library(plotly)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)

library(caret)
library(e1071)
library(Metrics)
set.seed(123)
library(randomForest)



fakeNews=fread("/Users/varunmeka/Downloads/SDMPROJ/Fake.csv")
fakeNews
summary(fakeNews)
is.null(fakeNews)
!is.null(fakeNews)

fakeNews$date<-as.Date(fakeNews$date,format='%B %d, %Y')
fakeNews$date


fakeNews <- mutate(fakeNews, Label = 'false')

trueNews=fread("/Users/varunmeka/Downloads/SDMPROJ/True.csv")
summary(trueNews)
is.null(trueNews)
!is.null(trueNews)

trueNews$date<-as.Date(trueNews$date,format='%B %d, %Y')

trueNews <- mutate(trueNews, Label = 'true')

total <- rbind(fakeNews[1:2000,], trueNews[1:2000,])
total <- mutate(total, Source = 'Kaggle')
total

total<-rename(total, DatePosted=date)
total
summary(total)

total <- total[, c(6,1,2,5,3,4)]
summary(total)
total<-rename(total, Title=title)
total<-rename(total, Description=text)
total<-rename(total, Links=subject)
summary(total)



bbc_data=data.frame(matrix(ncol = 6, nrow = 0))
names (bbc_data)<- c("Source","Title","Description","Label","Links","DatePosted")

BBC_Data <- function(link)
{
  SOURCE <-  getURL(link,encoding="UTF-8") # Specify encoding when dealing with non-latin characters
  PARSED <- htmlParse(SOURCE)
  
  
  Title = unlist(xpathSApply(PARSED, "//a[@class='gs-c-promo-heading gs-o-faux-block-link__overlay-link gel-pica-bold nw-o-link-split__anchor']", xmlValue))
  source=rep("BBC News", length(Title))
  Author= unlist(xpathSApply(PARSED, "//a[@class='gs-c-promo-heading gs-o-faux-block-link__overlay-link gel-pica-bold nw-o-link-split__anchor']", xmlGetAttr,"href"))
  Date=rep(Sys.Date(), length(Title))
  Label=rep("true", length(Title))
  
  for (i in 1:length(Author)){
    if (grepl("https", Author[i]) )
    {}
    else{
      Author[i]=paste("https://www.bbc.com",Author[i],sep="")
    }
  }
  
  
  description=c(1:length(Author))*0
  for (i in 1:length(Author)){
    SOURCE <-  GET(Author[i],encoding="UTF-8") # Specify encoding when dealing with non-latin characters
    if (SOURCE$status_code==200) 
    {
      PARSED <- htmlParse(SOURCE)
      content = unlist(xpathSApply(PARSED, "//b[@class='ssrcss-hmf8ql-BoldText e5tfeyi3']", xmlValue))
      if (length(content)<1)
      {
        content=unlist(xpathSApply(PARSED, "//p[@class='qa-introduction gel-pica-bold']", xmlValue))
      }
      if (length(content)<1){
        content=unlist(xpathSApply(PARSED, "//div[@class='article__intro b-font-family-serif']", xmlValue))
        if (length(content)<1)
          content=unlist(xpathSApply(PARSED, "//h1[@class='ssrcss-1qr3f1s-StyledHeading e1fj1fc10']", xmlValue))
        if (length(content)<1) content="No data"
      }
      
      description[i]=content
    }
  }
  
  res=data.frame(source,Title,description,Label,Author,Date)
  names(res)=c("Source","Title","Description","Label","Links","DatePosted")
  bbc_data=rbind(bbc_data,res)
  
  return (bbc_data)
}

link<- c("https://www.bbc.com/news","https://www.bbc.com/news/world")
BBC_data=data.frame(matrix(ncol = 6, nrow = 0))
names (BBC_data)<- c("Source","Title","Description","Label","Author","DatePosted")
for (i in 1:length(link)) 
{
  
  BBC_data=rbind(BBC_data,BBC_Data(link[i]))
}





Realtime_Data<- function(){
  
  labels=c('true','false','mostly-true','half-true','barely-true')
  blog_data=data.frame(matrix(ncol = 6, nrow = 0))
  names (blog_data)<- c("Source", "Title","Description", "Label", "Links","Author_Date")
  for( label in labels) {
    Source=c()
    Title=c()
    Label=c()
    Author_Date=c()
    for (i in seq_along(1:4)) {
      
      #input the link to scrap website 
      link <-str_c("https://www.politifact.com/factchecks/list/?page=", i,'&ruling=',label) 
      
      # assign link to html 
      page <- read_html(link)
      # get the name 
      s=page %>% html_nodes(".m-statement__name") %>% html_text()
      Source=append(Source,str_replace_all(s, "\n", ""))
      
      t = page %>% html_nodes(".m-statement__quote") %>% html_text()
      Title=append(Title,str_replace_all(t, "\n", ""))
      
      Links=page %>% html_nodes("[class='m-statement__quote']") %>% html_children() %>% html_attr("href")
      Links=(paste("https://www.politifact.com",Links,sep=""))
      
      
      ti=page %>% html_nodes(".m-statement__footer") %>% html_text()
      Author_Date=append(Author_Date,str_replace_all(ti, "\n", ""))
    }
    
    blog_data1 = data.frame(Source,Title, Title,label,Links,Author_Date)
    names (blog_data1)<- c("Source", "Title", "Description","Label", "Links","Author_Date")
    blog_data=rbind(blog_data,blog_data1)
  }
  blog_data = blog_data %>% separate(Author_Date, sep = " . ", into = c('Author','DatePosted'), remove =TRUE)
  blog_data$Author=str_replace_all(blog_data$Author, "By ", "")
  blog_data$DatePosted <- as.Date(blog_data$DatePosted, "%B %d, %Y")
  blog_data=blog_data[-6]
  
  return(blog_data)
  
}


data=Realtime_Data()


result=rbind(BBC_data,data,total)
names(data)
label=result[4]
result1=result[-4]
fake_data=cbind(result1,label)
fake_data=na.omit(fake_data)
#install.packages("tensorflow")
#install.packages("keras")
library(caret)
library(e1071)
fake_data=cbind(result1,label)
fake_data=fake_data %>% drop_na()



#sentimental Analysis

fake_data$Title<-gsub("\r","",as.character(fake_data$Title))
fake_data$Title<-trimws(fake_data$Title, which = c("both"))
mytext <- with(  fake_data,  sentiment_by( get_sentences(Description),list(Description) ) )

mytext1<-fake_data %>%
  get_sentences() 
mytext1 = sentiment_by(fake_data$Description, by = fake_data$Label)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  output$Data <- renderDT(fake_data)
  
  output$Summ <-
    renderPrint(
      stargazer(
        fake_data,
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(fake_data))
  output$structure <- renderPrint(str(fake_data))
  
 
  output$Corr<-
  renderPlot({
    barplot(table(as.factor(fake_data$Label)))
    
  })
  output$CorrMatrix1 <-
    renderPlot({
      ggplot(fake_data,aes(x=as.factor(Source),y=as.factor(Label)))+geom_point(colour='red')+
        labs(y = "Labels", x = "Source of Data")
      
    })
  
  output$CorrMatrix <-
    renderPlot({
      boxplot(as.numeric(as.factor(fake_data$Label)))
      
    })
  
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  
  
  InputDataset <- reactive({
    fake_data
  })
  
  
  InputDataset_model <- reactive({
    fake_data$Label=as.factor(fake_data$Label)
    if (is.null(input$SelectX)) {
      
      dt <- fake_data[,c(1,2,3,4,5,6)]
    }
    else{
      
      dt <- fake_data[, c(input$SelectX)]
    }
  })
  
  
  observe({
    lstname <- names(InputDataset())[6]
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      createDataPartition(fake_data$Label,p=splitSlider(),list=FALSE)
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
    
    
  })
  
  rf <- reactive({
    randomForest(Label ~ ., data=trainingData(),ntree= input$Slider2,mtry=input$Slider3)
  })
  
  output$Model <- renderPrint(summary(rf()))
  
  Importance <- reactive({
    varImp(rf(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(rf()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())

  
  price_predict <- reactive({

   predict(rf(),testData()[,-6])
   
    
  })
 
  
  output$Prediction <- renderPrint(confusionMatrix(testData()[,6], price_predict()))
  
  
  output$residualPlots <- renderPlot({
    barplot(table(testData()[,6]))
    
  })
  
  output$residualPlots1 <- renderPlot({
    barplot(table(price_predict()))
    
  })
  
  
  #Train predict RF
  
  price_predict_train <- reactive({
    
    predict(rf(),trainingData()[,-6])
    
    
  })
  
  
  output$Prediction_train <- renderPrint(confusionMatrix(trainingData()[,6], price_predict_train()))
  
  
  output$residualPlots_train <- renderPlot({
    barplot(table(trainingData()[,6]))
    
  })
  
  output$residualPlots1_train <- renderPlot({
    barplot(table(price_predict_train()))
    
  })
  
  #NB Code.
  nb <- reactive({
  naiveBayes(Label ~ ., data=trainingData(),laplace=1) 
  })
  
  output$Model2 <- renderPrint(nb()$apriori)
  output$Model_sum <- renderPrint(summary(nb()))
  
  Importance2 <- reactive({
    varImp(nb(), scale = FALSE)
  })
  
  tmpImp2 <- reactive({
    
    imp <- as.data.frame(varImp(rf()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar2 <- renderPrint(tmpImp2())
  
  #Test Data
  price_predict2 <- reactive({
    predict(nb(),testData()[,-6])
  })
  
  
  output$Prediction2 <- renderPrint(confusionMatrix(price_predict2(),testData()[,6]))
  
  
  output$residualPlots2 <- renderPlot({
    barplot(table(testData()[,6]))
    
  })
  
  output$residualPlots3 <- renderPlot({
    barplot(table(price_predict2()))
    
  })
  
  #Training data
  price_predict3 <- reactive({
    predict(nb(),trainingData()[,-6])
  })
  
  
  output$Prediction3 <- renderPrint(confusionMatrix(price_predict3(),trainingData()[,6]))
  
  
  output$residualPlots4 <- renderPlot({
    barplot(table(trainingData()[,6]))
    
  })
  
  output$residualPlots5 <- renderPlot({
    barplot(table(price_predict3()))
    
  })
  
  output$residualPlots_kmeans <- renderPlot({
    
    fake_data <- na.omit(fake_data)
    fake_data$Label=as.numeric(as.factor(fake_data$Label))
    fake_data$Source=as.numeric(as.factor(fake_data$Source))
    fake_data=fake_data[,c(1,6)]
    k2 <- kmeans(fake_data, centers = input$Slider4)
    fviz_cluster(k2, data = fake_data)
    
  })
  
  output$residualPlots_kmeans_summ <- renderPrint({
    
    fake_data <- na.omit(fake_data)
    fake_data$Label=as.numeric(as.factor(fake_data$Label))
    fake_data$Source=as.numeric(as.factor(fake_data$Source))
    fake_data1=fake_data[,c(1,6)]
   g= cbind(kmeans(fake_data1, centers = input$Slider4)$size,kmeans(fake_data1, centers = input$Slider4)$withinss)
   colnames(g)<-c("size","withinss")
   g
   
    
  })
  
  output$sentiment <- renderPrint({
    mytext1
  })
  
  output$sentiment_title <- renderPrint({
    mytext
  })
  
  
  
 
  
})
