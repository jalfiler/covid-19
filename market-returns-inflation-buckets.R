# cpi-cut-number
# market-returns-inflation-buckets

library(tidyverse)
library(tidyquant)
library(timetk)
library(readxl)
library(plotly)
library(scales)
library(formattable)
library(fredr)
library(broom)
library(treasuryTR)
library(modeltime)
library(tidymodels)
library(ppsr)
library(Quandl)
library(riingo)

riingo_browse_signup()
riingo_browse_token() # This requires that you are signed in on the site once you sign up
# API Token: d631414848ef54096c08e8686fe916608f670224

sp_500_daily <- 
  "VFINX" %>% 
  riingo_prices(start_date = "1979-01-01") 

sp_returns_monthly <-
  sp_500_daily %>% 
  summarise_by_time(.date_var = date, 
                    .by = "1 month",
                    .type = "floor",
                    return = last(close)/first(close) - 1 )  %>% 
  mutate(date = ymd(date),
         month_year = str_glue("{lubridate::month(date, label = T)} {year(date)}") %>%
           as.yearmon(),
         quarter_year= str_glue(" {year(date)} Q{quarter(date)} ") %>% 
           as.yearqtr()) 

cpi_monthly_yoy_change <- 
  cpi %>% 
  select(date, cpi_year_change) %>% 
  mutate(quarter_year= str_glue(" {year(date)} Q{quarter(date)} ") %>% 
           as.yearqtr(),
         month_year = str_glue("{lubridate::month(date, label = T)} {year(date)}") %>% as.yearmon())

inflation_bucketed <- 
  cpi_monthly_yoy_change %>% 
  mutate(cpi_yoy_bucket = cut(cpi_year_change,
                              c(-Inf, 
                                .01, 
                                .02, 
                                .03, 
                                .04, 
                                .05, 
                                .06,
                                Inf),
                              labels = c("0-1%",
                                         "1-2%",
                                         "2-3%",
                                         "3-4%",
                                         "4-5%",
                                         "5-6%",
                                         "6+%")
  )
  )

sp_returns_inflation_bucketed <- 
  sp_returns_monthly %>%
  # bind_rows(sector_quarterly_returns) %>% 
  left_join(inflation_bucketed %>% select(-quarter_year), by = c("date", "month_year")) %>% 
  filter(!is.na(cpi_yoy_bucket))

sp_returns_inflation_bucketed %>%
  ggplot(aes(x = date, y = return, fill = cpi_yoy_bucket)) + 
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  labs(x = "", 
       y = "% Mkt Return",  
       title = "Market Monthly Returns Colored by Inflation Buckets Since 1980",
       fill = "CPI YoY Bucket")

# If we filter to just CPI buckets of greater than 6%, 
# we get a sense of how rare our current times are. 


sp_returns_inflation_bucketed %>% 
  filter(cpi_yoy_bucket == "6+%") %>%
  ggplot(aes(x = date, y = return, fill = cpi_yoy_bucket)) + 
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "4 years") +
  labs(x = "", 
       y = "% Mkt Return",  
       title = "Market Monthly Returns Just 6+% Inflation Bucket Since 1980",
       fill = "CPI YoY Bucket")

# We see our current inflationary environment, which was fueled by COVID restrictions, 
# supply chain issues, and war in Europe. 
# The early 1990's saw a similar spike during the first Gulf War, 
# and then we have to go back to the early 1980's. 

#Let's peak at the other inflation buckets for the sake of comparison. 

sp_returns_inflation_bucketed %>% 
  filter(cpi_yoy_bucket != "6+%") %>%
  ggplot(aes(x = date, y = return, fill = cpi_yoy_bucket)) + 
  geom_col(show.legend = F) +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "10 years") +
  labs(x = "", 
       y = "% Mkt Return", 
       title = "Market Monthly Returns Colored by Inflation Buckets Since 1980",
       fill = "CPI YoY Bucket") +
  facet_wrap(~cpi_yoy_bucket) 

