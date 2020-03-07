## ---- message=FALSE, warning=FALSE, eval=FALSE--------------------------------
#  # aggregate the sample data to make a small data frame
#  library(basictabler)
#  library(dplyr)
#  tocsummary <- bhmsummary %>%
#    group_by(TOC) %>%
#    summarise(OnTimeArrivals=sum(OnTimeArrivals),
#              OnTimeDepartures=sum(OnTimeDepartures),
#              TotalTrains=sum(TrainCount)) %>%
#    ungroup() %>%
#    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
#           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
#    arrange(TOC)
#  
#  # formatting values (explained in the introduction vignette)
#  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")
#  
#  # create the table and render
#  tbl <- BasicTable$new()
#  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
#              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
#                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
#              columnFormats=columnFormats)
#  
#  # export to Excel
#  library(openxlsx)
#  wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
#  addWorksheet(wb, "Data")
#  tbl$writeToExcelWorksheet(wb=wb, wsName="Data",
#                           topRowNumber=1, leftMostColumnNumber=1, applyStyles=FALSE)
#  saveWorkbook(wb, file="C:\\test.xlsx", overwrite = TRUE)

## ---- message=FALSE, warning=FALSE, eval=FALSE--------------------------------
#  # aggregate the sample data to make a small data frame
#  library(basictabler)
#  library(dplyr)
#  tocsummary <- bhmsummary %>%
#    group_by(TOC) %>%
#    summarise(OnTimeArrivals=sum(OnTimeArrivals),
#              OnTimeDepartures=sum(OnTimeDepartures),
#              TotalTrains=sum(TrainCount)) %>%
#    ungroup() %>%
#    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
#           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
#    arrange(TOC)
#  
#  # formatting values (explained in the introduction vignette)
#  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")
#  
#  # create the table and render
#  tbl <- BasicTable$new()
#  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
#              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
#                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
#              columnFormats=columnFormats)
#  
#  # export to Excel
#  library(openxlsx)
#  wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
#  addWorksheet(wb, "Data")
#  tbl$writeToExcelWorksheet(wb=wb, wsName="Data",
#                           topRowNumber=1, leftMostColumnNumber=1,
#                           applyStyles=TRUE, mapStylesFromCSS=TRUE)
#  saveWorkbook(wb, file="C:\\test.xlsx", overwrite = TRUE)

## ---- message=FALSE, warning=FALSE, eval=FALSE--------------------------------
#  # aggregate the sample data to make a small data frame
#  library(basictabler)
#  library(dplyr)
#  tocsummary <- bhmsummary %>%
#    group_by(TOC) %>%
#    summarise(OnTimeArrivals=sum(OnTimeArrivals),
#              OnTimeDepartures=sum(OnTimeDepartures),
#              TotalTrains=sum(TrainCount)) %>%
#    ungroup() %>%
#    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
#           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
#    arrange(TOC)
#  
#  # formatting values (explained in the introduction vignette)
#  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")
#  
#  # create the table and render
#  tbl <- BasicTable$new()
#  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
#              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
#                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
#              columnFormats=columnFormats)
#  
#  # set the styling on the count cells
#  # the arguments are (rFrom, cFrom, rTo, cTo, declarations)
#  tbl$setStyling(2, 2, 5, 4, declarations=list("xl-value-format"="#,##0"))
#  # set the styling on the average delay cells
#  tbl$setStyling(2, 5, 5, 6, declarations=list("xl-value-format"="##0.0"))
#  
#  # export to Excel
#  library(openxlsx)
#  wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
#  addWorksheet(wb, "Data")
#  tbl$writeToExcelWorksheet(wb=wb, wsName="Data",
#                           topRowNumber=1, leftMostColumnNumber=1,
#                           applyStyles=TRUE, mapStylesFromCSS=TRUE,
#                           outputValuesAs="rawValue")
#  saveWorkbook(wb, file="C:\\test.xlsx", overwrite = TRUE)

