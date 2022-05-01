source(file = "./Misc.R")

# possible color intervals of cloropleths
colorIntervals <- list(c("floralwhite","blue"),c("floralwhite","firebrick4"),c("floralwhite","darkorange2"),
                       c("floralwhite","darkorchid"),c("floralwhite","forestgreen"),c("floralwhite","violetred"),
                       c("floralwhite","royalblue4"),c("floralwhite","red3"));

# countries metadata
countryList <- fromJSON(file = "./countries.json")

# activities labels to text
activitiesLabels <- read_tsv(file = "./CodeLists/ESTAT_ACL00_en.tsv",col_names = FALSE)

# age lables to text
ageIntervalsLabels <- read_tsv(file = "./CodeLists/ESTAT_AGE_en.tsv",col_names = FALSE)

# sexes labels to text
sexesLabels <- read_tsv(file = "./CodeLists/ESTAT_SEX_en.tsv",col_names = FALSE)

# dataset
data <- NULL


# read corresponding dataset, if last choice then combine 2000 and 2010 by taking mean of values 
DataReader <- function(year)
{
  if(year == "2000")
  {
    data <<- read.csv("period_2000_rearranged.csv",header = TRUE) %>% select(-TIME_PERIOD,-PTP_RT,-PTP_TIME)
    data$TIME_SP <<- as.numeric(lubridate::hm(data$TIME_SP) %>% lubridate::seconds())
  }
  else
  {
    if(year == "2010")
    {
      data <<- read.csv("period_2010_rearranged.csv",header = TRUE) %>% select(-TIME_PERIOD,-PTP_RT,-PTP_TIME)
      data$TIME_SP <<- as.numeric(lubridate::hm(data$TIME_SP) %>% lubridate::seconds())
    }
    else
    {
      data2000 <- read.csv("period_2000_rearranged.csv",header = TRUE) %>% select(-TIME_PERIOD,-PTP_RT,-PTP_TIME)
      data2000$TIME_SP <- as.numeric(lubridate::hm(data2000$TIME_SP) %>% lubridate::seconds())
      
      data2010 <- read.csv("period_2010_rearranged.csv",header = TRUE) %>% select(-TIME_PERIOD,-PTP_RT,-PTP_TIME)
      data2010$TIME_SP <- as.numeric(lubridate::hm(data2010$TIME_SP) %>% lubridate::seconds())
      
      data <<- rbind(data2000,data2010) %>% group_by(sex,age,acl00,geo) %>% summarise(TIME_SP = mean(TIME_SP))
    }
  }
  
  data[data == 0] = NA
  CreateUsedLists(data)
}