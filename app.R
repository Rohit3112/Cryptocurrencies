library(tidyverse)
library(janitor)
library(coinmarketcapr)
library(dotenv)
library(jsonlite)
library(httr)
library(htmlwidgets)
library(sparkline)
library(reshape2)
library(shiny)
library(shinydashboard)
library(lubridate)
library(scales)
library(shinyWidgets)
require(DT)
library(dashboardthemes)
library(shinythemes)
library(Cairo)
options(shiny.usecairo=T)

load_dot_env(".env")

coinmarketcapr::setup(Sys.getenv("COIN_MARKETCAPR_API"))

cryptos <- get_crypto_listings(limit = 50)

cryptos %>% 
  select(id,name,symbol,USD_price, USD_percent_change_24h, USD_percent_change_7d, 
         USD_percent_change_30d, USD_volume_24h,USD_market_cap) %>% 
  rename(price = USD_price,
         Volume_24hr = USD_volume_24h,
         percentage_24hr = USD_percent_change_24h,
         percentage_7d = USD_percent_change_7d,
         percentage_30d = USD_percent_change_30d,
         market_cap = USD_market_cap) %>% 
  mutate(price = round(price,2),
         Volume_24hr = round(Volume_24hr,0),
         percentage_24hr = round(percentage_24hr,2),
         percentage_7d = round(percentage_7d,2),
         percentage_30d = round(percentage_30d,2),
         market_cap = round(market_cap,2)) %>% 
  as_tibble() -> crypto_listings

crypto_listings %>% 
  mutate(link = paste0("https://s2.coinmarketcap.com/static/img/coins/64x64/",id,".png")) -> temp_table

#top movers table
temp_table %>% 
  mutate(logo = paste0('<img src=',link,' height="45"></img>')) %>% 
  arrange(desc(percentage_24hr)) %>%
  slice(1:6) %>% 
  mutate(price = paste0("$",price),
         percentage_24hr = ifelse(percentage_24hr>0,paste0("+",percentage_24hr,"%"),paste0(percentage_24hr,"%")),
         percentage_7d = ifelse(percentage_7d>0,paste0("+",percentage_7d,"%"),paste0(percentage_7d,"%")),
         percentage_30d = ifelse(percentage_30d>0,paste0("+",percentage_30d,"%"),paste0(percentage_30d,"%"))) -> top6_movers

#top coins table
temp_table %>% 
  mutate(logo = paste0('<img src=',link,' height="26"></img>')) %>% 
  mutate(symbol = paste0(symbol," ",logo)) %>% 
  select(name,symbol,price,percentage_24hr,percentage_7d,percentage_30d,Volume_24hr,market_cap) %>% 
  mutate(Volume_24hr = ifelse(Volume_24hr>=1000000000,paste0("$",round(Volume_24hr/1000000000,1)," B"),
                              paste0("$",round(Volume_24hr/1000000000,1)," M")),
         market_cap = ifelse(market_cap>=1000000000,paste0("$",round(market_cap/1000000000,2)," B"),
                             paste0("$",round(market_cap/1000000000,2)," M"))) -> final_table

#Top 10 coins plot
crypto_listings %>% 
  select(symbol,percentage_24hr, percentage_7d, percentage_30d) %>% 
  slice(1:10) %>% 
  rename(percentage_onemonth = percentage_30d) %>% 
  gather(key = percentage, value = value, c(percentage_24hr,percentage_7d,percentage_onemonth)) %>% 
  mutate(symbol = symbol %>% factor() %>% fct_inorder()) -> crypto_listings_1

crypto_listings %>%
  select(symbol,percentage_24hr, percentage_7d, percentage_30d) %>% 
  slice(1:10) %>% 
  mutate(x1 = seq(0.70,9.70, length.out = 10),
         x2 = seq(1,10, length.out = 10),
         x3 = seq(1.30,10.30, length.out = 10)) -> crypto_listings_plot_points 

ggplot()+
  geom_col(data = crypto_listings_1, aes(x=symbol, y=value,fill=percentage),position = "dodge") +
  geom_segment(data=crypto_listings_plot_points, aes(x=x1, xend = x2, y=percentage_24hr, yend=percentage_7d),
               size = 0.5, color="white",alpha=0.6) +
  geom_segment(data=crypto_listings_plot_points, aes(x=x2, xend = x3, y=percentage_7d, yend=percentage_30d),
               size = 0.5, color="white", alpha=0.6) +
  geom_point(data = crypto_listings_1, aes(x=symbol, y=value, group=percentage),stat="identity",size = 0.8,
             position = position_dodge(width = .9), color = ifelse(crypto_listings_1$value>0,"green","red")) +
  scale_y_continuous(breaks = seq(round(min(crypto_listings_1$value), digits =-1),
                                  round(max(crypto_listings_1$value), digits =-1), length.out = 9)) +
  geom_smooth(method = "lm", alpha = 0.1, colour = "black", size = 0.5) +
  scale_fill_manual(labels = c("24Hr %", "7 Days %", "1 Month %"), values = c("#D172A2", "#74EE90","#0CC1D5")) +
  ylab("% Change") +
  ggtitle("Top 10 Cryptocurrencies") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  colour = "white",
                                  hjust = 0.5),
        axis.title.y = element_text(size = 12,
                                    face = "plain",
                                    colour = "white"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        legend.background = element_rect(fill = "#343E48"),
        legend.text = element_text(colour = "white"),
        legend.key = element_rect(fill = "darkblue", color = NA),
        legend.position = "bottom",
        legend.direction ="horizontal",
        legend.key.size = unit(0.7, "cm"),
        legend.key.width = unit(0.7,"cm"),
        panel.grid = element_line(colour = "#949494"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#343E48",color ="#46505A",size= 1),
        plot.background = element_rect(fill = "#343E48",colour = NA)) -> plot 

temp_table %>% 
  mutate(logo = paste0('<img src=',link,' height="42" style = "margin-left: 20%;"></img>')) %>% 
  mutate(market_cap = paste0("$", round(market_cap/1000000000,2), " B"),
         Volume_24hr = paste0("$", round(Volume_24hr/1000000000,2), " B")) -> temp_table2

#Shiny 
ui <- dashboardPage(
  dashboardHeader(title = "CryptoCurrencies" ),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Cryptocurrencies", tabName = "top_crypto", icon = icon("dashboard")),
      menuItem("Historical Data", tabName = "hist_data", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "http://vaccaro.tech/favicon.ico")),
    
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    tabItems(
      # dashboard content
      tabItem(tabName = "top_crypto",
              fluidRow(
                valueBoxOutput("top1"),
                valueBoxOutput("top2"),
                valueBoxOutput("top3"),
                valueBoxOutput("top4"),
                valueBoxOutput("top5"),
                valueBoxOutput("top6")
              ),
              fluidRow(
                tabPanel("Visualization",
                         fluidRow(
                           column(6,
                                  dataTableOutput("table"),style = "margin-left: 1.2%"),
                           column(6,
                                  plotOutput("ggp"),style = "margin-left: -1.5%")
                         )
                         
                )
              )
      ),
      tabItem(tabName = "hist_data",
              fluidRow(
                tabPanel("Plots",
                         fluidRow(
                           column(4,
                                  searchInput(
                                    inputId = "search", label = "Search CryptoCurrency",
                                    placeholder = "Search",
                                    btnSearch = icon("search"),
                                    btnReset = icon("remove"),
                                    width = "310px"
                                  ), align = "center", style = "margin-top: 2%; font-size: 21px"),  
                           valueBoxOutput("symb"),
                           valueBoxOutput("cap")
                         ),
                         fluidRow(
                           column(6,
                                  plotOutput("hourly")),
                           column(6,
                                  plotOutput("daily"))
                         ),br(),
                         fluidRow(
                           column(6,
                                  plotOutput("weekly")),
                           column(6,
                                  plotOutput("monthly"))
                         )
                         
                )
              )
      )
    )
  ))

server <- function(input, output) {
  
  output$top1 <- renderValueBox({
    valueBox(HTML(paste(top6_movers %>% slice(1) %>% pull(logo),"   ",
                        top6_movers %>% slice(1) %>% pull(symbol),
                        tags$span(top6_movers %>% slice(1) %>% pull(percentage_24hr), style ="float:right; font-size:75%"))),
             HTML(paste(top6_movers %>% slice(1) %>% pull(name),br(),
                        tags$span(top6_movers %>% slice(1) %>% pull(price), style="font-size: 105%; font-weight: 700")
             )), color = "purple"
    )
  })
  
  output$top2 <- renderValueBox({
    valueBox(HTML(paste(top6_movers %>% slice(2) %>% pull(logo),"   ",
                        top6_movers %>% slice(2) %>% pull(symbol),
                        tags$span(top6_movers %>% slice(2) %>% pull(percentage_24hr), style ="float:right; font-size:75%"))),
             HTML(paste(top6_movers %>% slice(2) %>% pull(name),br(),
                        tags$span(top6_movers %>% slice(2) %>% pull(price), style="font-size: 105%; font-weight: 700")
             )), color = "orange"
    )
  })
  output$top3 <- renderValueBox({
    valueBox(HTML(paste(top6_movers %>% slice(3) %>% pull(logo),"   ",
                        top6_movers %>% slice(3) %>% pull(symbol),
                        tags$span(top6_movers %>% slice(3) %>% pull(percentage_24hr), style ="float:right; font-size:75%"))),
             HTML(paste(top6_movers %>% slice(3) %>% pull(name),br(),
                        tags$span(top6_movers %>% slice(3) %>% pull(price), style="font-size: 105%; font-weight: 700")
             )), color = "red"
    )
  })
  
  output$top4 <- renderValueBox({
    valueBox(HTML(paste(top6_movers %>% slice(4) %>% pull(logo),"   ",
                        top6_movers %>% slice(4) %>% pull(symbol),
                        tags$span(top6_movers %>% slice(4) %>% pull(percentage_24hr), style ="float:right; font-size:75%"))),
             HTML(paste(top6_movers %>% slice(4) %>% pull(name),br(),
                        tags$span(top6_movers %>% slice(4) %>% pull(price), style="font-size: 105%; font-weight: 700")
             )), color = "blue"
    )
  })
  
  output$top5 <- renderValueBox({
    valueBox(HTML(paste(top6_movers %>% slice(5) %>% pull(logo),"   ",
                        top6_movers %>% slice(5) %>% pull(symbol),
                        tags$span(top6_movers %>% slice(5) %>% pull(percentage_24hr), style ="float:right; font-size:75%"))),
             HTML(paste(top6_movers %>% slice(5) %>% pull(name),br(),
                        tags$span(top6_movers %>% slice(5) %>% pull(price), style="font-size: 105%; font-weight: 700")
             )), color = "green"
    )
  })
  
  output$top6 <- renderValueBox({
    valueBox(HTML(paste(top6_movers %>% slice(6) %>% pull(logo),"   ",
                        top6_movers %>% slice(6) %>% pull(symbol),
                        tags$span(top6_movers %>% slice(6) %>% pull(percentage_24hr), style ="float:right; font-size:75%"))),
             HTML(paste(top6_movers %>% slice(6) %>% pull(name),br(),
                        tags$span(top6_movers %>% slice(6) %>% pull(price), style="font-size: 105%; font-weight: 700")
             )), color = "light-blue"
    )
  })
  
  output$table <- renderDataTable({
                                  datatable(final_table %>% 
                                    mutate(cf_24hr = ifelse(percentage_24hr<0,0,1),
                                           cf_7d = ifelse(percentage_7d<0,0,1),
                                           cf_30d = ifelse(percentage_30d<0,0,1)) %>% 
                                    mutate(price = paste0("$",price),
                                           percentage_24hr = ifelse(percentage_24hr>0,paste0("+",percentage_24hr,"%"),paste0(percentage_24hr,"%")),
                                           percentage_7d = ifelse(percentage_7d>0,paste0("+",percentage_7d,"%"),paste0(percentage_7d,"%")),
                                           percentage_30d = ifelse(percentage_30d>0,paste0("+",percentage_30d,"%"),paste0(percentage_30d,"%"))), 
                                  escape = FALSE,
                                  options = list(
                                    autoWidth=TRUE,
                                    paging=TRUE,
                                    scrollX=TRUE,
                                    columnDefs = list(list(targets = "all", searchable = FALSE),
                                                      list(visible= FALSE, targets=c(-1,-2,-3))),
                                    pageLength = 6,
                                    filter = list(position = "top")),
                                  colnames=c("Name", "Symbol", "Price($)", "24H Change", "7D Change", "30D Change", "24H Volume",
                                             "Market Cap", "cf_24hr", "cf_7d", "cf_30d")) %>% 
                                  formatStyle(
                                  'percentage_24hr', 'cf_24hr',
                                  color = styleEqual(c(0,1),values = c('#FF8789', 'lightgreen'))) %>% 
                                  formatStyle(
                                 'percentage_7d', 'cf_7d',
                                  color = styleEqual(c(0,1),values = c('#FF8789', 'lightgreen'))) %>% 
                                  formatStyle(
                                  'percentage_30d', 'cf_30d',
                                  color = styleEqual(c(0,1),values = c('#FF8789', 'lightgreen'))) 
                                  })
  
  output$ggp <- renderPlot({
    plot}, res = 100)
  
  #ohlc tables
  symbol <- "ETH"
  symb <- symbol
  
  load_dot_env("av.env")
  
  url <- str_c("https://www.alphavantage.co/query?function=CRYPTO_INTRADAY","&symbol=",symbol,"&market=USD", 
               "&interval=60min", "&apikey=", Sys.getenv("AV_KEY"), "&datatype=csv")
  
  url1 <- str_c("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY","&symbol=",symbol,"&market=USD", 
                "&interval=60min", "&apikey=", Sys.getenv("AV_KEY"), "&datatype=csv")
  
  url2 <- str_c("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_WEEKLY","&symbol=",symbol,"&market=USD", 
                "&interval=60min", "&apikey=", Sys.getenv("AV_KEY"), "&datatype=csv")
  
  url3 <- str_c("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_MONTHLY","&symbol=",symbol,"&market=USD", 
                "&interval=60min", "&apikey=", Sys.getenv("AV_KEY"), "&datatype=csv")
  
  one_hr <- read_csv(url)
  
  daily <- read_csv(url1)
  
  weekly <- read_csv(url2)
  
  monthly <- read_csv(url3)
  
  one_hr %>% 
    slice(1:24) -> one_hr
  
  one_hr %>% 
    clean_names() %>% 
    mutate_if(is.numeric, funs(round(.,2))) -> one_hr_data
  
  daily %>% 
    clean_names() %>% 
    select(timestamp, open_usd, high_usd, low_usd, close_usd, volume) %>% 
    mutate_if(is.numeric, funs(round(.,2))) %>% 
    rename(open = "open_usd",
           high = "high_usd",
           low = "low_usd",
           close = "close_usd") %>% 
    slice(1:30) -> daily_data
  
  weekly %>% 
    clean_names() %>% 
    select(timestamp, open_usd, high_usd, low_usd, close_usd, volume) %>% 
    mutate_if(is.numeric, funs(round(.,2))) %>% 
    rename(open = "open_usd",
           high = "high_usd",
           low = "low_usd",
           close = "close_usd") %>% 
    slice(1:30) -> weekly_data
  
  monthly %>% 
    clean_names() %>% 
    select(timestamp, open_usd, high_usd, low_usd, close_usd, volume) %>% 
    mutate_if(is.numeric, funs(round(.,2))) %>% 
    rename(open = "open_usd",
           high = "high_usd",
           low = "low_usd",
           close = "close_usd") -> monthly_data
  
  #candlestick charts (ohcl)
  candlestick_plot_func <- function(ohcl_data){
    ohcl_data %>%
      select(timestamp,open, high, low, volume) %>% 
      mutate(id = row_number()) %>% 
      arrange(desc(id)) %>% 
      mutate(id = row_number()) -> candlestick_table
    
    candlestick_table %>% slice(-1) %>% pull(open) -> end_vect
    
    candlestick_table %>% 
      mutate(close = c(end_vect,end_vect[length(end_vect)])) %>% 
      mutate(color = c("#59FF3C",ifelse(diff(close) < 0,"#FE1A1E","#59FF3C"))) -> candlestick_table
    
    candlestick_table %>% pull(timestamp) -> ts_onehr
    candlestick_table %>% pull(id) -> id_onehr
    candlestick_table %>% pull(color) -> plot_colors
    plot_colors[1:length(plot_colors)-1] -> plot_colors
    
    candlestick_table %>% 
      select(timestamp,low,close,open,high,volume,id,color) %>% 
      slice(-nrow(candlestick_table)) %>% 
      ggplot() + 
      geom_rect(aes(x = id,
                    xmin = id - 0.25, # control bar gap width
                    xmax = id + 0.25,
                    ymin = close,
                    ymax = open), fill = plot_colors) +
      # geom_point(aes(x=id,y=high),size=1.24, linetype = "solid", color = "green") +
      # geom_point(aes(x=id,y=low),size=1.24, linetype = "solid", color = "red") +
      coord_cartesian(xlim = c(min(candlestick_table$id),max(candlestick_table$id))) +
      scale_x_continuous(breaks = seq(min(id_onehr), max(id_onehr), length.out = 5), 
                         labels = seq(min(ts_onehr), max(ts_onehr), length.out = 5)) +
      geom_segment(aes(x=id, xend=id, y=ifelse(open>close,close,open), yend=low), 
                   size=0.8, colour="red", linetype="solid") +
      geom_segment(aes(x=id, xend=id, y=high, yend=ifelse(open<close,close,open)), 
                   size=0.8, colour="green", linetype="solid") +
      xlab("Timestamp") +
      ylab("Price($)") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "#433854",color ="#343E48",size= 1),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid = element_line(colour = "#949494"),
            legend.position = "none",
            axis.title.y = element_text(size = 9,
                                        face = "plain",
                                        colour = "white"),
            axis.title.x = element_text(size = 9,
                                         face = "plain",
                                         colour = "white"),
            axis.text.x = element_text(colour = "white"),
            axis.text.y = element_text(colour = "white"),
            plot.background = element_rect(fill = "#3a4149",colour = NA),
            plot.title = element_text(hjust = 0.5,
                                      size = 15,
                                      face = "bold", 
                                      colour = "white")) -> candlestick_plot
    
    candlestick_plot
  }
  
  candlestick_plot_func(one_hr_data) + ggtitle("Hourly Trend") -> candlestick_plot_onehr
  candlestick_plot_func(daily_data) + ggtitle("Daily Trend") -> candlestick_plot_daily
  candlestick_plot_func(weekly_data) + ggtitle("Weekly Trend") -> candlestick_plot_weekly
  candlestick_plot_func(monthly_data) + ggtitle("Monthly Trend") -> candlestick_plot_monthly
  
  #
  output$hourly <- renderPlot({
    candlestick_plot_onehr}, res = 75)
  
  output$daily <- renderPlot({
    candlestick_plot_daily}, res = 75)
  
  output$weekly <- renderPlot({
    candlestick_plot_weekly}, res = 75)
  
  output$monthly <- renderPlot({
    candlestick_plot_monthly}, res = 75)
  
  output$search_plot <- renderUI({
    searchInput("search",
                "Select Crypto", 
                choices = symbols_vector,
                multiple = FALSE)
  })
  
  output$symb <- renderValueBox({
    valueBox(HTML(paste(temp_table2 %>% slice(which(temp_table$symbol==symb)) %>% pull(logo),
                        tags$span(temp_table2 %>% slice(which(temp_table$symbol==symb)) %>% pull(symbol), 
                                  style ="float:right; font-size:100%; font-weight:bold; margin-right: 5%; margin-top: 1.5%"),br(),
                        tags$span(temp_table2 %>% slice(which(temp_table$symbol==symb)) %>% pull(name), 
                                  style ="font-size: 75%;font-weight: 600;margin-left: 11%;margin-top: 3%;"))),
             HTML(paste("")),
             color = "yellow"
    )
  })
  
  output$cap <- renderValueBox({
    valueBox(HTML(paste(tags$span("Market Capital", style ="font-size:73%; font-weight:500"),
                        tags$span("24Hr Volume", style ="float:right; font-size:73%; margin-top:2%; font-weight:300; margin-right: 2%"),
                        tags$span(temp_table2 %>% slice(which(temp_table$symbol==symb)) %>% pull(Volume_24hr), style ="float: right;
                                                                                                                            font-size: 75%;
                                                                                                                            font-weight: 600;
                                                                                                                            margin-right: 2%;
                                                                                                                            margin-top: 2.5%;"))),
             HTML(paste(tags$span(temp_table2 %>% slice(which(temp_table$symbol==symb)) %>% pull(market_cap), style ="font-size: 170%; font-weight: bolder")
             )), color = "yellow"
    )
  })
  
}

shinyApp(ui,server)

