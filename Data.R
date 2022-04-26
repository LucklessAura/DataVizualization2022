source(file = "./Misc.R")


colorIntervals <- list(c("floralwhite","blue"),c("floralwhite","firebrick4"),c("floralwhite","darkorange2"),
                       c("floralwhite","darkorchid"),c("floralwhite","forestgreen"),c("floralwhite","violetred"),
                       c("floralwhite","royalblue4"),c("floralwhite","red3"));


countryList <- fromJSON(file = "./countries.json")
activitiesLabels <- read_tsv(file = "./CodeLists/ESTAT_ACL00_en.tsv",col_names = FALSE)
ageIntervalsLabels <- read_tsv(file = "./CodeLists/ESTAT_AGE_en.tsv",col_names = FALSE)
sexesLabels <- read_tsv(file = "./CodeLists/ESTAT_SEX_en.tsv",col_names = FALSE)
data <- NULL



DataReader <- function(year)
{
  choices <<- list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3)
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