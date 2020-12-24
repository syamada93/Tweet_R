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
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    refreshPlot <- reactiveTimer(intervalMs = 12000)

    TDC <- data.frame()
    dc=""
    wd="雨"
    
output$distPlot <-  renderPlot({
        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    
    
        # repeat{
        #     if(format(Sys.time(),"%S")=="00"){
    refreshPlot()
            td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)
            
            if(nrow(td)==0){
                Sys.sleep(60)
                next
            }
            
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
                mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M"))
            
            dc <- sort(unique(c(tds$status_id,dc)),decreasing = T)
            dc <- dc[1:min(length(dc),10000)]
            
            p <-
                TDC %>%
                filter(total>0) %>%
                # filter(JTime>"2020-11-05 15:30:00") %>%
                mutate(RTs=factor(RT,labels = c("オリジナルツイート","リツイート"))) %>%
                ggplot(aes(x=JTime,y=n,fill=reorder(RTs,-RT))) +
                geom_area(col="black") +
                # geom_text(data=TDC2,aes(y=total+10,label=format(JTime,"%H"),fill=NULL),col="red") +
                labs(x="",y="",fill="") +
                scale_x_datetime(date_breaks="1 hours",date_labels = "%H時") +
                scale_y_continuous(breaks = seq(0,10000000,100),limits = c(0,max(TDC$total+10))) +
                # ggtitle("雨ツイート 更新中") +
                theme(legend.position = "bottom") +
                theme(text = element_text(size=30)) +
                theme(axis.title.x = element_blank())
            
            plot(p)
            
        #     }
        # }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
