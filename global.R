library(dplyr)

inputData <- read.csv("data/formatedData.csv", header = TRUE, stringsAsFactors = FALSE)
countries <- read.csv("data/countries.csv", header = TRUE, stringsAsFactors = FALSE)
#Data cleaning
cleanData <- inputData
cleanData$X <- NULL
cleanData$itime <- NULL
cleanData$Device <- NULL
cleanData$DOM <- NULL
cleanData$Type <- NULL
cleanData$Subtype <- NULL
cleanData$Action <- NULL
cleanData$AppCat <- NULL
cleanData$App <- NULL
cleanData$level <- NULL
cleanData$Craction <- NULL
cleanData$CRLevel <- NULL
cleanData$Crscore <- NULL
cleanData$dstcountry <- NULL
cleanData$dstinf <- NULL
cleanData$dstip <- NULL
cleanData$dstport <- NULL
cleanData$duration <- NULL
cleanData$logid <- NULL
cleanData$logver <- NULL
cleanData$policyid <- NULL
cleanData$policytype <- NULL
cleanData$none <- NULL
cleanData$proto <- NULL
cleanData$rcvdbyte <- NULL
cleanData$sentbyte <- NULL
cleanData$sentpkt <- NULL
cleanData$sessionid <- NULL
cleanData$srcinf <- NULL
cleanData$srcport <- NULL
cleanData$threatcnts <- NULL
cleanData$threatlvls <- NULL
cleanData$threats <- NULL
cleanData$threattyps <- NULL
cleanData$threatwgts <- NULL
cleanData$trandisp <- NULL
cleanData$tranip <- NULL
cleanData$tranport <- NULL
# cleanData <- inputData Perque ho tornes a repetir al final?

# cleantable <- initialData# %>%
#  select(
#    Date = initialData$Date,
#    Hour = initialData$Hour,
#    Country = initialData$srccountry,
#    IP = initialData$srcip,
#    Service = initialData$service
#  )