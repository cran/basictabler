## ---- message=FALSE, warning=FALSE, eval=FALSE--------------------------------
#  tableStyles$addStyle(styleName="ColumnHeader", list(
#      "font-family"="arial",
#      "font-size"="0.75em",
#      padding="2px",
#      border="1px solid blue",
#      "vertical-align"="middle",
#      "text-align"="center",
#      "font-weight"="bold",
#      color="blue",
#      "background-color"="#FFFFFF",
#      "xl-wrap-text"="wrap"
#    ))

## ---- message=FALSE, warning=FALSE--------------------------------------------
# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- "default"  # this theme is already the default, so this line isn't really needed
tbl$renderTable(styleNamePrefix="t0")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- "compact"
tbl$renderTable(styleNamePrefix="t1")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- "largeplain"
tbl$renderTable(styleNamePrefix="t2")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# define the font and colours
simpleBlueTheme <- list(
  fontName="Verdana, Arial",
  fontSize="0.75em",
  headerBackgroundColor = "rgb(68, 114, 196)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(186, 202, 233)",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "rgb(48, 84, 150)"
)

# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- simpleBlueTheme
tbl$renderTable(styleNamePrefix="t3")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# define the colours
simpleGrayTheme <- list(
  fontName="Courier New, Courier",
  fontSize="0.75em",
  headerBackgroundColor = "rgb(128, 128, 128)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(192, 192, 192)",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "rgb(64, 64, 64)"
)

# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- simpleGrayTheme
tbl$renderTable(styleNamePrefix="t4")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# define the colours
simpleGreenTheme <- list(
  fontName="Helvetica, arial",
  fontSize="0.75em",
  headerBackgroundColor = "rgb(112, 173, 71)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor="rgb(255, 255, 255)",
  cellColor="rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(182, 216, 158)",
  totalColor="rgb(0, 0, 0)",
  borderColor = "rgb(84, 130, 53)"
)

# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=FALSE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- simpleGreenTheme
tbl$renderTable(styleNamePrefix="t6")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# aggregate the sample data to make a small data frame
library(basictabler)
library(dplyr)
tocsummary <- bhmsummary %>%
  group_by(TOC) %>%
  summarise(OnTimeArrivals=sum(OnTimeArrivals),
            OnTimeDepartures=sum(OnTimeDepartures),
            TotalTrains=sum(TrainCount)) %>%
  ungroup() %>%
  mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
         OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
  arrange(TOC)

# column formats
columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

# simple theme
simpleBlueTheme <- list(
  fontName="Verdana, Arial",
  fontSize="0.75em",
  headerBackgroundColor = "rgb(68, 114, 196)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(186, 202, 233)",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "rgb(48, 84, 150)"
)

# headings in red text, cells in light gray
tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                    "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
            columnFormats=columnFormats, theme=simpleBlueTheme)

# render table
tbl$renderTable(styleNamePrefix="t9")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# aggregate the sample data to make a small data frame
library(basictabler)
library(dplyr)
tocsummary <- bhmsummary %>%
  group_by(TOC) %>%
  summarise(OnTimeArrivals=sum(OnTimeArrivals),
            OnTimeDepartures=sum(OnTimeDepartures),
            TotalTrains=sum(TrainCount)) %>%
  ungroup() %>%
  mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
         OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
  arrange(TOC)

# column formats
columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

# create the table
tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=FALSE,
            explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                    "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
            columnFormats=columnFormats, 
            tableStyle=list("border-color"="maroon"),
            headingStyle=list("color"="cornsilk", "background-color"="maroon", 
                              "font-style"="italic", "border-color"="maroon"), 
            cellStyle=list("color"="maroon", "background-color"="cornsilk", 
                           "border-color"="maroon"))

# set column alignment of first column
tbl$setStyling(2, 1, 5, 1, declarations=list("text-align"="left"))

# render table
tbl$renderTable(styleNamePrefix="t10")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)
df <- data.frame(saleIds, items, quantities, prices)
colNames <- c("Sale ID", "Item", "Quantity", "Price")
colFormats <- list(NULL, NULL, NULL, "%.2f")

# construct the table
library(basictabler)
tbl <- BasicTable$new()

# define a new style
tbl$addStyle(styleName="AltCell", list(
  "font-family"="Arial",
  "font-size"="0.8em",
  "font-weight"="bold",
  padding="2px 2px 2px 8px",
  "border-bottom"="1px solid #9C0006",
  "text-align"="right",
  color="#9C0006",
  "background-color"="#FFC7CE"
))
colStyleNames <- c("Cell", "Cell", "AltCell", "Cell")

# populate the table
tbl$addData(df, explicitColumnHeaders=colNames, columnFormats=colFormats, baseStyleNames=colStyleNames)
tbl$renderTable(styleNamePrefix="t11")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()

# define a new style
tbl$addStyle(styleName="AltColumn", list(
  "font-family"="Arial",
  "font-size"="0.8em",
  "font-weight"="bold",
  padding="2px 2px 2px 8px",
  "border-bottom"="1px solid #9C0006",
  "text-align"="right",
  color="#9C0006",
  "background-color"="#FFC7CE"
))
tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=saleIds)
tbl$cells$setColumn(2, cellTypes="cell", rawValues=items)
tbl$cells$setColumn(3, cellTypes="cell", rawValues=quantities, baseStyleName="AltColumn")
tbl$cells$setColumn(4, cellTypes="cell", rawValues=prices,
                    formats=list("%.2f"))
tbl$renderTable(styleNamePrefix="t13")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# cell types for the cells in each row
cellTypes <- c("rowHeader", "cell", "cell", "cell")

# formats for the values in each row
# (only the value in the fourth column needs formatting)
formats <- list(NULL, NULL, NULL, "%.2f")

# construct the table
library(basictabler)
tbl <- BasicTable$new()

# define a new style
tbl$addStyle(styleName="AltRow", list(
  "font-family"="Arial",
  "font-size"="0.8em",
  "font-weight"="bold",
  padding="2px 2px 2px 8px",
  "border-bottom"="1px solid #9C0006",
  "text-align"="right",
  color="#9C0006",
  "background-color"="#FFC7CE"
))
tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
tbl$cells$setRow(2, cellTypes=cellTypes, formats=formats,
                 rawValues=list(5334, "Apple", 5, 0.34452354))
tbl$cells$setRow(3, cellTypes=cellTypes, formats=formats,
                 rawValues=list(5336, "Orange", 8, 0.4732543), baseStyleNames="AltRow")
tbl$cells$setRow(4, cellTypes=cellTypes, formats=formats,
                 rawValues=list(5338, "Banana", 6, 1.3443243))
tbl$renderTable(styleNamePrefix="t14")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# cell types for the cells in each row
cellTypes <- c("rowHeader", "cell", "cell", "cell")

# formats for the values in each row
# (only the value in the fourth column needs formatting)
formats <- list(NULL, NULL, NULL, "%.2f")

# construct the table
library(basictabler)
tbl <- BasicTable$new()

# define a new style
tbl$addStyle(styleName="AltRowLeftAlign", list(
  "font-family"="Arial",
  "font-size"="0.8em",
  "font-weight"="bold",
  padding="2px 2px 2px 2px",
  "border-bottom"="1px solid #9C0006",
  "text-align"="left",
  color="#9C0006",
  "background-color"="#FFC7CE"
))
tbl$addStyle(styleName="AltRowRightAlign", list(
  "font-family"="Arial",
  "font-size"="0.8em",
  "font-weight"="bold",
  padding="2px 2px 2px 8px",
  "border-bottom"="1px solid #9C0006",
  "text-align"="right",
  color="#9C0006",
  "background-color"="#FFC7CE"
))
styleNames <- c("AltRowLeftAlign", "AltRowRightAlign", 
                    "AltRowRightAlign", "AltRowRightAlign")
tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
tbl$cells$setRow(2, cellTypes=cellTypes, formats=formats,
                 rawValues=list(5334, "Apple", 5, 0.34452354))
tbl$cells$setRow(3, cellTypes=cellTypes, formats=formats,
                 rawValues=list(5336, "Orange", 8, 0.4732543), baseStyleNames=styleNames)
tbl$cells$setRow(4, cellTypes=cellTypes, formats=formats,
                 rawValues=list(5338, "Banana", 6, 1.3443243))
tbl$renderTable(styleNamePrefix="t15")

## ---- message=FALSE, warning=FALSE--------------------------------------------

library(basictabler)
tbl <- BasicTable$new()

# specify a new cell style
tbl$addStyle(styleName="AltCell", list(
  "font-family"="Arial",
  "font-size"="0.8em",
  "font-weight"="bold",
  padding="2px 2px 2px 8px",
  "border"="2px solid #9C0006",
  "text-align"="right",
  color="#9C0006",
  "background-color"="#FFC7CE"
))

# build the table
tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
tbl$cells$setCell(2, 1, cellType="rowHeader", rawValue=5334)
tbl$cells$setCell(2, 2, cellType="cell", rawValue="Apple")
tbl$cells$setCell(3, 1, cellType="rowHeader", rawValue=5336)
tbl$cells$setCell(3, 2, cellType="cell", rawValue="Orange")

# use the new style for the following cell (used instead of the theme styling)
tbl$cells$setCell(2, 3, cellType="cell", rawValue=5, baseStyleName="AltCell")

# specify an additional style declaration for the following cell (used on top of the theme styling)
tbl$cells$setCell(3, 3, cellType="cell", rawValue=8, styleDeclarations=list("background-color"="#FFFF00"))

# render the table
tbl$renderTable(styleNamePrefix="t12")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# define the colours
orangeTheme <- list(
  fontName="Garamond, arial",
  fontSize="0.75em",
  headerBackgroundColor = "rgb(237, 125, 49)",
  headerColor = "rgb(255, 255, 255)",
  cellBackgroundColor = "rgb(255, 255, 255)",
  cellColor = "rgb(0, 0, 0)",
  totalBackgroundColor = "rgb(248, 198, 165)",
  totalColor = "rgb(0, 0, 0)",
  borderColor = "rgb(198, 89, 17)"
)

# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- orangeTheme

# apply an additional highlight to one cell (3rd row, 2nd column)
tbl$setStyling(3, 2, declarations=list("background-color"="#FFFF00"))

# apply an additional highlight to one cell (3rd row, 3rd column)
cellHighlight <- tbl$createInlineStyle(declarations=list("background-color"="#00FFFF"))
cell <- tbl$cells$getCell(3, 3)
cell$style <- cellHighlight

# render the table
tbl$renderTable(styleNamePrefix="t16")

## ---- eval=FALSE--------------------------------------------------------------
#  # apply inline style to multiple cells
#  highlight <- tbl$createInlineStyle(declarations=list("background-color"="#FFCC66"))
#  cells <- tbl$getCells(specifyCellsAsList=TRUE, rowNumbers=c(1, 3))
#  lst <- lapply(cells, function(cell) {cell$style <- highlight})

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(basictabler)
library(dplyr)
tocsummary <- bhmsummary %>%
  group_by(TOC) %>%
  summarise(OnTimeArrivals=sum(OnTimeArrivals),
            OnTimeDepartures=sum(OnTimeDepartures),
            TotalTrains=sum(TrainCount)) %>%
  ungroup() %>%
  mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
         OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
  arrange(TOC)

# formatting values (explained in the introduction vignette)
columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

## ---- message=FALSE, warning=FALSE--------------------------------------------
tbl <- BasicTable$new()
tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                    "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
            columnFormats=columnFormats)
cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")
tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="logic",
              mappings=list("v==2348", "pink", "v<3000", "red", "3000<=v<15000", 
                            "yellow", "v>15000", "green"))
tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="text", mapType="logic",
              mappings=list("v==1404", "red"))
tbl$renderTable()

## ---- message=FALSE, warning=FALSE--------------------------------------------
tbl <- BasicTable$new()
tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                    "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
            columnFormats=columnFormats)
cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")
tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="range",
              mappings=list(0, "red", 3000, "orange", 5000, "yellow", 15000, "green"))
tbl$renderTable()

## ---- message=FALSE, warning=FALSE--------------------------------------------
tbl <- BasicTable$new()
tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                    "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
            columnFormats=columnFormats)
cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")
redclr <- function(x, cell) {
  clr <- 255-floor(140*cell$columnNumber/3)
  return(paste0("#",
                format(as.hexmode(255), width=2),
                format(as.hexmode(clr), width=2),
                format(as.hexmode(clr), width=2)))
}
tbl$mapStyling(cells=cells, styleProperty="background-color", mappings=redclr)
tbl$renderTable()

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(basictabler)
# define the theme and styles
createCustomTheme <- function(parentTable=NULL, themeName="myCustomTheme") {
  tableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  # borders in purple
  tableStyles$addStyle(styleName="Table", list(
      "display"="table",
      "border-collapse"="collapse",
      "border"="2px solid #B28DFF"
    ))
  # column headings in pink
  tableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      "font-weight"="bold",
      padding="2px",
      "border"="2px solid #B28DFF",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      color="#DB49AC",
      "background-color"="#FFCCF9",
      "xl-wrap-text"="wrap"
    ))
  # row headings in blue
  tableStyles$addStyle(styleName="RowHeader", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      "font-weight"="bold",
      padding="2px 8px 2px 2px",
      "border"="1px solid #B28DFF",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      color="#438EC8",
      "background-color"="#ACE7FF",
      "xl-wrap-text"="wrap"
    ))
  # cells in yellow
  tableStyles$addStyle(styleName="Cell", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      padding="2px 2px 2px 8px",
      "border"="1px solid #B28DFF",
      "text-align"="right",
      color="#FF800D",
      "background-color"="#FFFFD1"
    ))
  # totals in orange
  tableStyles$addStyle(styleName="Total", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      "font-weight"="bold",
      padding="2px 2px 2px 8px",
      "border"="1px solid rgb(84, 130, 53)",
      "text-align"="right",
      color="#3BC6B6",
      "background-color"="#BFFCC6"
    ))
  tableStyles$tableStyle <- "Table"
  tableStyles$rootStyle <- "ColumnHeader"
  tableStyles$rowHeaderStyle <- "RowHeader"
  tableStyles$colHeaderStyle <- "ColumnHeader"
  tableStyles$cellStyle <- "Cell"
  tableStyles$totalStyle <- "Total"
  return(invisible(tableStyles))
}

# data for the table
saleIds <- c(5334, 5336, 5338)
items <- c("Apple", "Orange", "Banana")
quantities <- c(5, 8, 6)
prices <- c(0.34452354, 0.4732543, 1.3443243)

# construct the table
library(basictabler)
tbl <- BasicTable$new()
tbl$addData(data.frame(saleIds, items, quantities, prices), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
            columnFormats=list(NULL, NULL, NULL, "%.2f"))

# theme the table and render
tbl$theme <- createCustomTheme(tbl)
tbl$renderTable(styleNamePrefix="t8")

