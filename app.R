library(tidyverse)
library(janitor)
library(coinmarketcapr)
library(dotenv)

load_dot_env(".env")

coinmarketcapr::setup(Sys.getenv("COIN_MARKETCAPR_API"))

cryptos <- get_crypto_listings(limit = 50)