#2025-03-25 

################################
#Regional Fisheries Data
################################
library(tidyverse)

load('data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData')

head(timeseries)
dim(timeseries)
glimpse(tsmetrics)


timeseries_tsmetrics = timeseries %>%
  left_join(tsmetrics, by= c("tsid" = "tsunique")) #use when joining when column names are different so use =

dim(timeseries)
dim(timeseries_tsmetrics)
#both look good have the same number of rows but just added more columns 6 to 11

glimpse(timeseries_values_views)
glimpse(taxonomy)
glimpse(stock)

#best annual total catch estimate = TCbest
#joining timeseries table with stock (using stockid and stocklong) and then join in taxonomy table(using tsn and scientificname)

fish = timeseries_values_views %>%
  left_join(stock, by= c("stockid", "stocklong")) %>%
  left_join(taxonomy, by= c("tsn", "scientificname")) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, 
         commonname, region, FisheryType, taxGroup) #selected the columns I want to look at so I made my dataset smaller

glimpse(fish)
dim(timeseries_values_views)
dim(fish)

#how dd total catch change over time in these stocks 

ggplot()+
  geom_line(aes(x=year, y=TCbest, color=stockid), 
            data = fish %>% filter(TCbest>3e6)) +
  theme(legend.position = "none") #takes away legend

ggplot()+
  geom_line(aes(x=year, y=TCbest, color=stockid), 
            data = fish %>% filter(TCbest>3e6)) #filter out so only showing stock catch greater than 3e6

###collapse of the COD industry exploration

#look at what regions have Atlantic cod assessments, now only look at canada east coast

cod_can = fish %>%
  filter(scientificname == "Gadus morhua",
         region == "Canada East Coast",
  !is.na(TCbest))

head(cod_can)
unique(cod_can$region)

ggplot()+
  geom_line(aes(x = year, y = TCbest, color= stocklong), data = cod_can)

#summarized the total catch for all stocks in canada to show one line of what happened to all stocks throughout the years
cod_can_total = cod_can%>%
  group_by(year) %>%
  summarize(total_catch_mt = sum(TCbest))

head(cod_can_total)
ggplot()+
  geom_line(aes(x = year, y = total_catch_mt), data = cod_can_total)

#Did the stock officially collapse
fakedata = c(1,3,6,2,3,9,-1)
fakedata_max = cummax(fakedata) #cummatlitive maximum number of orginal data
fakedata_sum = cumsum(fakedata) 
test_cum = data.frame(fakedata, fakedata_max, fakedata_sum)
fakedata_max
test_cum
#remember mutate makes a new column
cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch_mt),
         collapse = total_catch_mt <= 0.1 * historical_max_catch) #is the 1920 max less than 10 percent of orginal *histori
tail(cod_collapse)

#at what year did cod collapse? collapsed in 1993

cod_collapse_year = cod_collapse %>%
  filter(collapse == "TRUE") %>%
  summarize(year = min(year)) %>%
  pull(year)
cod_collapse_year

#shows red when it hasnt collapse and blue when it has collapsed
#geom_vline adds line where the cod stock collapsed
ggplot() +
  geom_line(aes(x = year, y = total_catch_mt, color= collapse), data = cod_collapse) +
  geom_vline(xintercept = cod_collapse_year) 

#####Examine fishery collapse across ALL stocks

allcollapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest), 
         current_collapse = TCbest < 0.1 * historical_max_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()

head(allcollapse)
glimpse(allcollapse)

#giving first year all these stocks in what year 
allcollapse_year = allcollapse %>%
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet ==TRUE) %>%
  summarize(first_collapse_year = min(year)) %>%
  ungroup()
glimpse(allcollapse_year)

ggplot()+
  geom_histogram(aes(x= first_collapse_year), data = allcollapse_year, 
                 fill = "salmon", color= "black", binwidth = 5) #binwidth is 5 year intervals

#how many stocks have ever collapsed over time

n_stocks = length(unique(allcollapse$stockid))
collaspe_ts = allcollapse_year %>%
  count(first_collapse_year) %>%
  mutate(cum_collapse_yr = cumsum(n), 
         ratio_collapsed_yet = cum_collapse_yr/n_stocks)

ggplot()+
  geom_line(aes(x= first_collapse_year, y = cum_collapse_yr), data = collaspe_ts)
#shows all stocks collapsed over time, so at year 2000, 300 stocks collapsed 
#ration collapsedd yet just shows the percent sp 0.1= 10 percent 
