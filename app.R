#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if(!require(data.table))
    install.packages("data.table")
library(data.table)

if(!require(dplyr))
    install.packages("dplyr")
library(dplyr)

if(!require(tidyr))
    install.packages("tidyr")
library(tidyr)

if(!require(stringi))
    install.packages("stringi")
library(stringi)

if(!require(ggplot2))
    install.packages("ggplot2")
library(ggplot2)

if(!require(ggraph))
    install.packages("ggraph")
library(ggraph)

if(!require(tidygraph))
    install.packages("tidygraph")
library(tidygraph)

if(!require(rtweet))
    install.packages("rtweet")
library(rtweet)

if(!require(leaflet))
    install.packages("leaflet")
library(leaflet)

if(!require(leafletCN))
    install.packages("leafletCN")
library(leafletCN)

if(!require(maptools))
    install.packages("maptools")
library(maptools)

if(!require(sf))
    install.packages("sf")
library(sf)

if(!require(rsconnect))
    install.packages("rsconnect")
library(rsconnect)

if(!require(dygraphs))
    install.packages("dygraphs")
library(dygraphs)

if(!require(RMeCab))
    install.packages("RMeCab", repos = "https://rmecab.jp/R") 
library(RMeCab)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ツイート数の推移"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           textInput("wd",
                     "抽出する単語",
                     "雨"),
           actionButton("button1","抽出開始")
           # submitButton()
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dygraphOutput("Hdy"),
            # plotOutput("ggraph"),
           # plotOutput("Hline"),
           plotOutput("Dline0"),
           # plotOutput("Dline"),
           # plotOutput("Mline"),
           width = 12
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    refreshPlot0 <- reactiveTimer(intervalMs = 1)
    # refreshPlot <- reactiveTimer(intervalMs = 60000)

    TDC <- data.frame()
    dc=""
    wd="コロナ"
    wd="雨"
    
    WD <- eventReactive(input$button1,{
        input$wd
    })
    observeEvent(input$button1, {
        if(file.exists("TDC.csv"))
            file.remove("TDC.csv")
        if(file.exists("dc.txt"))
            file.remove("dc.txt")
    })
    
 observe({
    wd=WD()
    refreshPlot0()
    if(file.exists("TDC.csv"))
        TDC <- fread("TDC.csv") %>%
        data.frame()
    if(file.exists("dc.txt"))
        dc <- as.character(fread("dc.txt")$V1)
    # print(Sys.time())
    if(as.numeric(format(Sys.time(),"%S"))>5)
        return()
    print(Sys.time())
    # refreshPlot()
    td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)#,retryonratelimit = T)
    
    if(nrow(td)==0)
        return()
        
    tds <-
        td %>%
        mutate(JTime=as.POSIXct(format(created_at, tz="Japan"))) %>%
        mutate(Tweet=stri_trans_nfkc(text)) %>%
        # mutate(Tweet=gsub("\"","",Tweet)) %>%
        mutate(Tweet=gsub("http[[:print:]]{,18}","",Tweet)) %>%
        mutate(Tweet=gsub("@[[:alnum:][:punct:]]+","",Tweet)) %>%
        # mutate(Tweet=gsub("<U\\+0001","<U\\+1",Tweet)) %>%
        mutate(Tweet=gsub("!+","!",Tweet)) %>%
        mutate(Tweet=gsub("\\?+","\\?",Tweet)) %>%
        mutate(Tweet=gsub("-+","-",Tweet)) %>%
        mutate(Tweet=gsub("\\(+","\\(",Tweet)) %>%
        mutate(Tweet=gsub(")+",")",Tweet)) %>%
        mutate(Tweet=gsub("\"+","",Tweet)) %>%
        mutate(Year=year(JTime)) %>%
        mutate(Month=month(JTime)) %>%
        mutate(Day=mday(JTime)) %>%
        mutate(Hour=hour(JTime)) %>%
        mutate(Minute=minute(JTime)) %>%
        mutate(M=floor(Minute/10)*10) %>%
        arrange(desc(status_id)) %>%
        mutate(ID=paste0("Row",1:n())) %>% 
        # mutate(Tweet2=iconv(Tweet,from="UTF-8",to="CP932","")) %>%
        data.frame()
    
    day=format(max(tds$JTime),"%Y%m%d_%H%M") #-24*60*60
    mid=max(tds$status_id)
    print(paste(min(tds$JTime),max(tds$JTime),nrow(tds)))
    
    write_as_csv(tds,paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"),fileEncoding = "CP932")
    
    # print(length(dc))
    # print(nrow(tds %>% filter(!status_id %in% dc)))
 
    tdc <-
        tds %>%
        filter(!status_id %in% dc) %>%
        count(Year,Month,Day,Hour,Minute,RT=is_retweet) %>%
        mutate(RT=as.logical(RT)) %>%
        complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
        # mutate(M=floor(Minute/10)*10) %>%
        group_by(Year,Month,Day,Hour,Minute) %>%
        mutate(total=sum(n)) %>%
        ungroup() %>%
        mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M"))
    
    TDC <-
        TDC %>%
        rbind(tdc) %>%
        group_by(Year,Month,Day,Hour,Minute,RT) %>%
        summarise(n=sum(n)) %>%
        ungroup() %>%
        complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
        group_by(Year,Month,Day,Hour,Minute) %>%
        mutate(total=sum(n)) %>%
        ungroup() %>%
        mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M")) %>%
        filter(floor(as.numeric(JTime)/60)<floor(as.numeric(Sys.time())/60))
    
    
    print(head(TDC %>% arrange(desc(JTime))))
    
    write.csv(TDC,"TDC.csv",row.names = F)
    dc <- unique(c(tds$status_id,sort(dc,decreasing = T)))
    dc <- dc[1:min(length(dc),3000)]
    write(dc,"dc.txt")
    
    # print(list.files("Tweet_data"))
    
   output$Hdy <- renderDygraph({
       Comp <- 
           # data.frame(JTime=(max(TDC$JTime)-60*60):(max(TDC$JTime))) %>%
           data.frame(JTime=rep(seq(min(TDC$JTime),max(TDC$JTime),60),each=2),
                      RT=c(F,T))
       
        TDCS <-
            Comp %>%
            left_join(TDC) %>%
            complete(JTime,RT,fill=list(n=0)) %>%
            mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
            select(JTime,RTs,n) %>%
            spread(RTs,n) %>%
            select(Retweet,Origin)
        
        # TDCS <-
        #     TDC %>%
        #     filter(!RT) %>%
        #     select(Origin=n,Total=total)
        
        rownames(TDCS) <- unique(Comp$JTime)
        
        dygraph(TDCS,main = paste0(max(TDC$JTime)-2*60*60,"～",max(TDC$JTime))) %>%
            dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                      axisLabelFontSize = 30,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
            dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$JTime)-2*60*60,max(TDC$JTime))) %>%
            # dyHighlight(highlightCircleSize = 3,
            #             highlightSeriesBackgroundAlpha = 0.5,
            #             highlightSeriesOpts=list(),
            #             hideOnMouseOut = T) %>%
            # dyLegend(show = "always",
            #          width = 100,
            #          showZeroValues = TRUE, labelsDiv = NULL,
            #          labelsSeparateLines = T, hideOnMouseOut = TRUE) 
            dyLegend(width = 175)
        
    })
   
   
    # output$ggraph <- renderPlot({
    #     # Encoding(tds$Tweet) <- "cp932"
    #     # Encoding(TDS$Tweet)
    #     # te <- iconv(tds$Tweet,from="UTF-8",to="cp932","")
    #     
    #     TDS <- fread(paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"))
    #     
    #     TF0 <- docDF(tds, col = 100, type = 1, N = 1, minFreq = 1, nDF = 1,
    #                  pos = c("感動詞","形容詞","動詞","副詞","名詞","接頭詞","連体詞"))
    #     
    #     TF0S <-
    #         TF0 %>%
    #         gather(ID,n,starts_with("Row")) %>%
    #         filter(n>0) %>%
    #         count(N1,POS1,POS2) %>%
    #         mutate(CF=N1>=0) %>%
    #         filter(!CF)
    #         
    #     
    #     TF1 <- docDF(tds, col = 100, type = 1, N = 3, minFreq = 1, nDF = 1,
    #                  pos = c("感動詞","形容詞","動詞","副詞","名詞","接頭詞","連体詞"))
    #     
    #     TF1S <-
    #         TF1 %>%
    #         gather(ID,n,starts_with("Row")) %>%
    #         filter(n>0) %>%
    #         mutate_at(vars(N1,N2,N3),funs(ifelse(. %in% TF0S$N1,"",.))) %>%
    #         filter(N1!="") %>%
    #         left_join(tds %>% select(Tweet,one_of(colnames(tds)))) %>%
    #         mutate(word=ifelse(grepl("^接頭詞-名詞-",POS1),paste0(N1,N2),N1)) %>%
    #         mutate(word=ifelse(grepl("^名詞-名詞-",POS1) & grepl("^[[:alnum:]]+-接尾-",POS2) & 
    #                                !grepl("^記号-接尾-",POS2) & nchar(N2)<3,paste0(N1,N2),word)) %>%
    #         mutate(word=ifelse(grepl("^接頭詞-名詞-名詞",POS1) & grepl("^[[:alnum:]]+-[[:alnum:]]+-接尾",POS2) & 
    #                                !grepl("^[[:alnum:]]+-記号-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word)) %>%
    #         mutate(word=ifelse(grepl("^名詞-名詞-名詞",POS1) & grepl("^[[:alnum:]]+-接尾-接尾",POS2) & 
    #                                !grepl("^記号-接尾-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word)) %>%
    #         mutate(word=ifelse(grepl("^名詞-名詞-名詞",POS1) & grepl("^数-数-接尾",POS2) & nchar(N3)<3,paste0(N1,N2,N3),word)) %>%
    #         mutate(word=ifelse(grepl("^名詞-名詞-名詞",POS1) & grepl("^数-数-数",POS2),paste0(N1,N2,N3),word)) %>%
    #         mutate(word=ifelse(grepl("^名詞-名詞-名詞",POS1) & grepl("^数-記号-数",POS2),paste0(N1,N2,N3),word)) %>%
    #         select(N1,N2,N3,POS1,POS2,word,one_of(colnames(.)))
    #     
    #     TF1S2 <-
    #         TF1S %>%
    #         group_by(word) %>% #,POS1,POS2
    #         summarise(Freq=sum(n)) %>%
    #         ungroup() %>%
    #         mutate(Rank=frank(-Freq,ties.method="dense")) %>%
    #         arrange(Rank)
    #     
    #     print(head(TF1S))
    # })
    
    output$Dline0 <-  renderPlot({
        TDCH <-
            TDC %>%
            mutate(M=floor(Minute/10)*10) %>%
            group_by(Year,Month,Day,Hour,M,RT) %>%
            summarise(n=sum(n)) %>%
            ungroup() %>%
            complete(Year,Month,Day,Hour,M,RT,fill=list(n=0)) %>%
            group_by(Year,Month,Day,Hour,M) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,M),format="%Y %m %d %H %M")) %>%
            filter(JTime<Sys.time())
        
        Comp <- 
            data.frame(JTime=rep(seq(max(TDCH$JTime)-24*60*60,max(TDCH$JTime),60*10),each=2),
                       RT=rep(c(F,T),24*6+1))
        TDC2 <-
            Comp %>%
            left_join(TDCH) %>%
            mutate(n=ifelse(is.na(n),0,n)) %>%
            mutate(total=ifelse(is.na(total),0,total))
        
        keta <-nchar(max(TDC2$total))-1
        if(floor(max(TDC2$total)/(10^keta))<2)
            keta <- keta-1
        
        p <-
            TDC2 %>%
            # filter(total>0) %>%
            mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
            ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
            geom_area(col="black") +
            # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
            labs(x="",y="",fill="") +
            scale_x_datetime(date_breaks="1 hours",date_labels = "%H時") +
            scale_y_continuous(breaks = seq(0,10000000,10^keta),limits = c(0,max(TDC2$total)+10^(keta-1))) +
            ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
            theme(legend.position = "bottom") +
            theme(text = element_text(size=30)) +
            theme(axis.title.x = element_blank())
        
        plot(p)
    })
    
    output$Dline <-  renderPlot({
        TDCH <-
            TDC %>%
            group_by(Year,Month,Day,Hour,RT) %>%
            summarise(n=sum(n)) %>%
            ungroup() %>%
            complete(Year,Month,Day,Hour,RT,fill=list(n=0)) %>%
            group_by(Year,Month,Day,Hour) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H")) %>%
            filter(JTime<Sys.time())
        
        Comp <- 
            data.frame(JTime=rep(seq(max(TDCH$JTime)-24*60*60,max(TDCH$JTime),60*60),each=2),
                       RT=rep(c(F,T),24+1))
        TDC2 <-
            Comp %>%
            left_join(TDCH) %>%
            mutate(n=ifelse(is.na(n),0,n)) %>%
            mutate(total=ifelse(is.na(total),0,total))
        
        keta <-nchar(max(TDC2$total))-1
        if(floor(max(TDC2$total)/(10^keta))<2)
            keta <- keta-1
        
        p <-
            TDC2 %>%
            # filter(total>0) %>%
            mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
            ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
            geom_area(col="black") +
            # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
            labs(x="",y="",fill="") +
            scale_x_datetime(date_breaks="1 hours",date_labels = "%H時",
                             date_minor_break="1 hours") +
            scale_y_continuous(breaks = seq(0,10000000,10^keta),limits = c(0,max(TDC2$total)+10^(keta-1))) +
            ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
            theme(legend.position = "bottom") +
            theme(text = element_text(size=30)) +
            # theme(axis.text.x =  element_text(size=30)) +
            # theme(axis.text.x =  element_text(size=20,angle = 90,vjust = 0.5)) +
            theme(axis.title.x = element_blank())
        
        plot(p)
    })
    
    output$Mline <-  renderPlot({
        TDCM <-
            TDC %>%
            group_by(Year,Month,Day,RT) %>%
            summarise(n=sum(n)) %>%
            ungroup() %>%
            complete(Year,Month,Day,RT,fill=list(n=0)) %>%
            group_by(Year,Month,Day) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day),format="%Y %m %d")) %>%
            filter(JTime<Sys.time())
        
        Comp <- 
            data.frame(JTime=rep(seq(max(TDCM$JTime)-31*24*60*60,max(TDCM$JTime),24*60*60),each=2),
                       RT=rep(c(F,T),32))
        TDC2 <-
            Comp %>%
            left_join(TDCM) %>%
            mutate(n=ifelse(is.na(n),0,n)) %>%
            mutate(total=ifelse(is.na(total),0,total))
        
        keta <-nchar(max(TDC2$total))-1
        if(floor(max(TDC2$total)/(10^keta))<2)
            keta <- keta-1
        
        p <-
            TDC2 %>%
            # filter(total>0) %>%
            mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
            ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
            geom_area(col="black") +
            # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
            labs(x="",y="",fill="") +
            scale_x_datetime(date_breaks="1 days",date_labels = "%d日") +
            scale_y_continuous(breaks = seq(0,10000000,10^keta),limits = c(0,max(TDC2$total)+10^(keta-1))) +
            ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
            theme(legend.position = "bottom") +
            theme(text = element_text(size=30)) +
            theme(axis.title.x = element_blank())
        
        plot(p)
    })
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
