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

