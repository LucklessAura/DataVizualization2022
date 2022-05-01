
# actually sued activities in dataset
UsedActivities <- NULL
#actually used age intervals in dataset
UsedAgeIntervals <- NULL
# actually used sexes in dataset
UsedSexes <- NULL
# actually used countries in dataset
UsedCountriesList <- NULL

# iso2 used countries codes to iso3 codes
CreateCountryList <- function(data)
{
  iso2Countries <- unique(data$geo)
  reshaped <- flatten(countryList) %>% bind_rows()
  UsedCountriesList <<- reshaped$iso3[match(iso2Countries,reshaped$iso2)]
  
}


CreateUsedActivityLables <- function(data)
{
  actLables <- unique(data$acl00)
  UsedActivities <<- as.data.frame(activitiesLabels[activitiesLabels$X1 %in% actLables,])
  UsedActivities <<- rename(UsedActivities, value = X1)
  UsedActivities <<- rename(UsedActivities, label = X2)
}


CreateUsedAgeIntervals <- function(data)
{
  ages <- unique(data$age)
  UsedAgeIntervals <<- as.data.frame(ageIntervalsLabels[ageIntervalsLabels$X1 %in% ages,])
  UsedAgeIntervals <<- rename(UsedAgeIntervals, value = X1)
  UsedAgeIntervals <<- rename(UsedAgeIntervals, label = X2)
}

CreateUsedSexes <- function(data)
{
  sexes <- unique(data$sex)
  UsedSexes <<- as.data.frame(sexesLabels[sexesLabels$X1 %in% sexes,])
  UsedSexes <<- rename(UsedSexes, value = X1)
  UsedSexes <<- rename(UsedSexes, label = X2)
}


CreateUsedLists <- function(data)
{
  CreateCountryList(data)
  CreateUsedActivityLables(data)
  CreateUsedAgeIntervals(data)
  CreateUsedSexes(data)
}

# get seconds for given filters
getTimesForChoices <- function(iso3List,activity,sex,age)
{
  
  timeSpent = data[data$acl00 == activity & data$sex == sex & data$age == age,]
  timeSpent = timeSpent %>% group_by(geo) %>% summarise(summ = sum(TIME_SP))
  reshaped <- flatten(countryList) %>% bind_rows()
  iso3List <- reshaped$iso2[match(iso3List,reshaped$iso3)]
  timeSpent[timeSpent == 0] = NA
  return(timeSpent[match(iso3List, timeSpent$geo),]$summ)
}


# periods dont always have the same string length, solve that by padding when needed
periodToPaddedString <- function(periods)
{
  paste0(str_pad(width = 2,pad = '0',side = 'left',string = periods@hour),":",
        str_pad(width = 2,pad = '0',side = 'left',string = periods@minute),":",
        str_pad(width = 2,pad = '0',side = 'left',string = periods@.Data))
}


# seconds to padded period
secondsToPeriodLabeler <- function(name, value)
{
  mapply(paste, sep = ": ",name,periodToPaddedString(lubridate::seconds_to_period(value)))
}

# labeller needs this type of structure to works properly 
toLabelDataframe <- function(x)
{
  df <- set_names(x = mapply(FUN = str_wrap,x$label,width = 30),nm = x$value)
  return(df)
}

# get iso2 country code for a iso3 code
getIso2ForIso3 <- function(iso3)
{
  reshaped <- flatten(countryList) %>% bind_rows()
  return(reshaped$iso2[match(iso3,reshaped$iso3)])
}