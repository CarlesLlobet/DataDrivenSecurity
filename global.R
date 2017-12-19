library(dplyr)

inputData <- read.csv("data/cleanData.csv", header = TRUE, stringsAsFactors = FALSE)

#allzips <- readRDS("data/superzip.rds")
#allzips$latitude <- jitter(allzips$latitude)
#allzips$longitude <- jitter(allzips$longitude)
#allzips$college <- allzips$college * 100
#allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
#row.names(allzips) <- allzips$zipcode

cleantable <- inputData %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )