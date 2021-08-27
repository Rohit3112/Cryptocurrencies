library(tidyverse)
library(janitor)
library(coinmarketcapr)
library(dotenv)

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
  select(name,symbol,price,percentage_24hr,percentage_7d,percentage_30d,Volume_24hr,market_cap) -> final_table

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
  scale_fill_manual(labels = c("24Hr %", "7 Days %", "1 Month %"), values = c("#F8766D", "#00BA38","#619CFF")) +
  ylab("% Increase") +
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
        legend.background = element_rect(fill = "#361752"),
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
        panel.background = element_rect(fill = "#361752",color ="#6b4683",size= 1),
        plot.background = element_rect(fill = "#361752",colour = NA)) -> plot 

#ohlc tables
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

