library(tidyverse)
library(tidyquant)
library(timetk)
library(readxl)
library(plotly)
library(scales)
library(formattable)
library(fredr)
library(broom)



# Suppose we didn't wish to choose levels, 
# but rather wished to have our levels chosen so that 
# an equal number of observations falls in each bucket. 
# To accomplish this, we call the `cut_number()` function from `ggplot2`. 
# We tell `cut_number()` function how many buckets to create, 
# and the function creates them such that each has an equal number of observations. 

cpi <- 
  "CPIAUCSL" %>% 
tq_get(get = "economic.data", from = "1979-01-01") %>% 
  select(date, cpi = price) %>% 
  mutate(cpi_month_change = cpi/lag(cpi, 1) - 1,
         cpi_year_change = cpi/lag(cpi, 12) - 1,
         cpi_5_year_change = cpi/lag(cpi, 60) - 1)

cpi_monthly_yoy_change <- 
cpi %>% 
  select(date, cpi_year_change) %>% 
  mutate(quarter_year= str_glue(" {year(date)} Q{quarter(date)} ") %>% 
           as.yearqtr())



  cpi_monthly_yoy_change %>% 
  drop_na() %>% 
  mutate(cpi_yoy_bucket = cut_number(cpi_year_change, n = 7)) %>% 
  group_by(cpi_yoy_bucket) %>% 
  count()


# Notice how each bucket has 71 or 72 observations, and the ranges are quite precise. 
# Since we eventually want to chart and use these levels, we want better labels 
# than what's displayed currently. We created a custom function called `cut_label_fun()` 
# to accomplish this. 

# We use a custom function called cut_label_fun(). 

# And here are the new results: 
  
cpi_monthly_yoy_change %>% 
  drop_na() %>% 
  mutate(cpi_yoy_bucket = cut_number(cpi_year_change, n = 7)) %>% 
  cut_label_fun(bucket_col = cpi_yoy_bucket) %>% 
  group_by(cpi_yoy_bucket, label) %>% 
  count()


# No change in substance, but we have a column called `label` with more 
# aesthetically pleasing descriptions for our buckets. 
# Now let's plot a visualization of those buckets and 
# how actual CPI changes fall within them.


# inflation_bucketed_equal <- 
  cpi_monthly_yoy_change %>% 
  mutate(cpi_yoy_bucket = cut_number(cpi_year_change, n = 7)
  ) %>%
  filter(!is.na(cpi_yoy_bucket),
         date >= "1980-01-01") %>% 
    cut_label_fun(bucket_col = cpi_yoy_bucket) %>% 
  ggplot(aes(x = label, y = cpi_year_change, color = label)) +
    geom_jitter()  +
  scale_y_continuous(labels = percent, breaks = pretty_breaks(n = 10)) + 
  theme_minimal() +
  labs(title =  "Distribution of Monthly CPI YoY", 
       subtitle = "Equal Number obs per bucket",
         color = 
         "Inflation Bucket", x = "", y = "Actual YoY Change CPI",
       caption = "Source: FRED Data and author calcs") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = .5),
       plot.subtitle = element_text(hjust = .5) ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
  
  