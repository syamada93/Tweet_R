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
           plotOutput("Hline"),
           plotOutput("Dline"),
           plotOutput("Mline"),
           width = 12
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    refreshPlot <- reactiveTimer(intervalMs = 60000)

    TDC <- data.frame()
    dc=""
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
    refreshPlot()
    if(file.exists("TDC.csv"))
        TDC <- fread("TDC.csv") %>%
        data.frame()
    if(file.exists("dc.txt"))
        dc <- fread("dc.txt")$V1
    
    td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)
    
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
        mutate(ID=paste0("Row",1:n()))
    
    day=format(max(tds$JTime),"%Y%m%d_%H%M") #-24*60*60
    mid=max(tds$status_id)
    print(paste(min(tds$JTime),max(tds$JTime),nrow(tds)))
    
    write_as_csv(tds,paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"),fileEncoding = "CP932")
 
    tdc <-
        tds %>%
        filter(!status_id %in% dc) %>%
        count(Year,Month,Day,Hour,Minute,RT=is_retweet) %>%
        mutate(RT=as.logical(RT)) %>%
        complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
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
        filter(JTime<Sys.time())
    
    write.csv(TDC,"TDC.csv",row.names = F)
    dc <- sort(unique(c(tds$status_id,dc)),decreasing = T)
    dc <- dc[1:min(length(dc),10000)]
    write(dc,"dc.txt")
    
    print(list.files("Tweet_data"))
    
    output$Hline <-  renderPlot({
        Comp <- 
            # data.frame(JTime=(max(TDC$JTime)-60*60):(max(TDC$JTime))) %>%
            data.frame(JTime=rep(seq(max(TDC$JTime)-60*60,max(TDC$JTime),60),each=2),
                       RT=rep(c(F,T),61))
        TDC2 <-
            Comp %>%
            left_join(TDC) %>%
            mutate(n=ifelse(is.na(n),0,n))
        
            p <-
                TDC2 %>%
                # filter(total>0) %>%
                mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
                ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
                geom_area(col="black") +
                # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
                labs(x="",y="",fill="") +
                scale_x_datetime(date_breaks="1 min",date_labels = "%H:%M") +
                scale_y_continuous(breaks = seq(0,10000000,100),limits = c(0,max(TDC2$total+10))) +
                ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
                theme(legend.position = "bottom") +
                theme(text = element_text(size=30)) +
                theme(axis.text.x =  element_text(size=20,angle = 90,vjust = 0.5)) +
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
                       RT=rep(c(F,T),25))
        TDC2 <-
            Comp %>%
            left_join(TDCH) %>%
            mutate(n=ifelse(is.na(n),0,n))
        
        p <-
            TDC2 %>%
            # filter(total>0) %>%
            mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
            ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
            geom_area(col="black") +
            # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
            labs(x="",y="",fill="") +
            scale_x_datetime(date_breaks="1 hours",date_labels = "%H時") +
            scale_y_continuous(breaks = seq(0,10000000,100),limits = c(0,max(TDC2$total+10))) +
            ggtitle(paste0(min(TDC2$JTime),"～",max(TDC2$JTime))) +
            theme(legend.position = "bottom") +
            theme(text = element_text(size=30)) +
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
            mutate(n=ifelse(is.na(n),0,n))
        
        p <-
            TDC2 %>%
            # filter(total>0) %>%
            mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
            ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
            geom_area(col="black") +
            # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
            labs(x="",y="",fill="") +
            scale_x_datetime(date_breaks="1 days",date_labels = "%d日") +
            scale_y_continuous(breaks = seq(0,10000000,100),limits = c(0,max(TDC2$total+10))) +
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
