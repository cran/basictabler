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
tbl$renderTable()

## ---- message=FALSE, warning=FALSE, comment=""--------------------------------
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

# output table
tbl

## ---- message=FALSE, warning=FALSE, comment=""--------------------------------
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

#out the HTML and CSS
cat(paste(tbl$getHtml(), sep="", collapse="\n"))
cat(tbl$getCss())

## ---- message=FALSE, warning=FALSE, eval=TRUE, comment=""---------------------
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

# convert to flextable
library(flextable)
ft <- tbl$asFlexTable()
ft

## ---- eval=FALSE--------------------------------------------------------------
#  # data for the table
#  saleIds <- c(5334, 5336, 5338)
#  items <- c("Apple", "Orange", "Banana")
#  quantities <- c(5, 8, 6)
#  prices <- c(0.34452354, 0.4732543, 1.3443243)
#  
#  # construct the table
#  library(basictabler)
#  tbl <- BasicTable$new()
#  tbl$addData(data.frame(saleIds, items, quantities, prices),
#              firstColumnAsRowHeaders=TRUE,
#              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
#              columnFormats=list(NULL, NULL, NULL, "%.2f"))
#  
#  # convert to flextable
#  library(flextable)
#  ft <- tbl$asFlexTable()
#  
#  # save word document
#  library(officer)
#  docx <- read_docx()
#  docx <- body_add_par(docx, "Example Table")
#  docx <- body_add_flextable(docx, value = ft)
#  print(docx, target = "example_table_word.docx")

## ---- eval=FALSE--------------------------------------------------------------
#  # data for the table
#  saleIds <- c(5334, 5336, 5338)
#  items <- c("Apple", "Orange", "Banana")
#  quantities <- c(5, 8, 6)
#  prices <- c(0.34452354, 0.4732543, 1.3443243)
#  
#  # construct the table
#  library(basictabler)
#  tbl <- BasicTable$new()
#  tbl$addData(data.frame(saleIds, items, quantities, prices),
#              firstColumnAsRowHeaders=TRUE,
#              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
#              columnFormats=list(NULL, NULL, NULL, "%.2f"))
#  
#  # convert to flextable
#  library(flextable)
#  ft <- tbl$asFlexTable()
#  
#  # save PowerPoint document
#  library(officer)
#  ppt <- read_pptx()
#  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
#  ppt <- ph_with(ppt, value = ft, location = ph_location_left())
#  print(ppt, target = "example_table_powerpoint.pptx")

## ---- message=FALSE, warning=FALSE, eval=TRUE, comment=""---------------------
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

# output as matrix
tbl$asMatrix()

## ---- message=FALSE, warning=FALSE, eval=TRUE, comment=""---------------------
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

# output as matrix
tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE)

## ---- message=FALSE, warning=FALSE, eval=TRUE, comment=""---------------------
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

# output as data frame
df <- tbl$asDataFrame(firstRowAsColumnNames=TRUE, rawValue=TRUE)
df
str(df)

