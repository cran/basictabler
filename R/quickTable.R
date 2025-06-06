
#' Quickly build a basic table.
#'
#' The \code{qtbl} function builds a basic table with one line of R.
#'
#' @export
#' @param dataFrameOrMatrix The data frame or matrix containing the data to be
#'   displayed in the table.
#' @param columnNamesAsColumnHeaders TRUE to use the data frame column names
#'   as the column headers in the table.
#' @param explicitColumnHeaders A character vector of column headers.
#' @param rowNamesAsRowHeaders TRUE to use the data frame row names as the
#'   row headers in the table.
#' @param firstColumnAsRowHeaders TRUE to use the first column in the data
#'   frame as row headings.
#' @param explicitRowHeaders A character vector of row headers.
#' @param numberOfColumnsAsRowHeaders The number of columns to be set as row
#'   headers.
#' @param columnFormats A list containing format specifiers, each of which is
#'   either an sprintf() character value, a list of format() arguments or an
#'   R function that provides custom formatting logic.
#' @param columnCellTypes A vector that is the same length as the
#'   number of columns in the data frame, where each element is one of
#'   the following values that specifies the type of cell: root, rowHeader,
#'   columnHeader, cell, total.  The cellType controls the default styling
#'   that is applied to the cell.  Typically only rowHeader, cell or total
#'   would be used.
#' @param theme Either the name of a built-in theme (default, largeplain,
#'   compact or blank/none) or a list which specifies the default formatting for
#'   the table.
#' @param replaceExistingStyles TRUE to completely replace the default styling
#'   with the specified tableStyle, headingStyle, cellStyle and/or totalStyle
#' @param tableStyle A list of CSS style declarations that apply to the table.
#' @param headingStyle A list of CSS style declarations that apply to the
#'   heading cells in the table.
#' @param cellStyle A list of CSS style declarations that apply to the normal
#'   cells in the table.
#' @param totalStyle A list of CSS style declarations that apply to the total
#'   cells in the table.
#' @param ... Additional arguments, currently compatibility and/or
#'   argumentCheckMode.
#' @return A basic table.
#' @examples
#' qtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")])
#' qtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")], columnNamesAsColumnHeaders=FALSE,
#'   explicitColumnHeaders=c("Week", "From", "To", "Trains", "On-Time"))
#'
qtbl <- function(dataFrameOrMatrix, columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                 rowNamesAsRowHeaders=FALSE, firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL, numberOfColumnsAsRowHeaders=0,
                 columnFormats=NULL, columnCellTypes=NULL, theme=NULL, replaceExistingStyles=FALSE,
                 tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qtbl", dataFrameOrMatrix, missing(dataFrameOrMatrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("data.frame", "matrix"))
  checkArgument(3, TRUE, "", "qtbl", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qtbl", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", firstColumnAsRowHeaders, missing(firstColumnAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qtbl", numberOfColumnsAsRowHeaders, missing(numberOfColumnsAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
  checkArgument(3, TRUE, "", "qtbl", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
  checkArgument(3, TRUE, "", "qtbl", columnCellTypes, missing(columnCellTypes), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
  checkArgument(3, TRUE, "", "qtbl", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyles"), allowedListElementClasses="character")
  checkArgument(3, TRUE, "", "qtbl", replaceExistingStyles, missing(replaceExistingStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", tableStyle, missing(tableStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  checkArgument(3, TRUE, "", "qtbl", headingStyle, missing(headingStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  checkArgument(3, TRUE, "", "qtbl", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  checkArgument(3, TRUE, "", "qtbl", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  compatibility <- arguments$compatibility
  tbl <- BasicTable$new(argumentCheckMode=argumentCheckMode, theme=theme, replaceExistingStyles=replaceExistingStyles,
                        tableStyle=tableStyle, headingStyle=headingStyle, cellStyle=cellStyle, totalStyle=totalStyle,
                        compatibility=compatibility)
  if("data.frame" %in% class(dataFrameOrMatrix)) {
    tbl$addData(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, firstColumnAsRowHeaders=firstColumnAsRowHeaders, explicitRowHeaders=explicitRowHeaders,
                numberOfColumnsAsRowHeaders=numberOfColumnsAsRowHeaders, columnFormats=columnFormats, columnCellTypes=columnCellTypes)
  }
  else if("matrix" %in% class(dataFrameOrMatrix)) {
    tbl$addMatrix(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, explicitRowHeaders=explicitRowHeaders, columnFormats=columnFormats)
  }
  return(tbl)
}

#' Quickly render a basic table in HTML.
#'
#' The \code{qhpvt} function renders a basic table as a HTML widget with
#' one line of R.
#'
#' @export
#' @param dataFrameOrMatrix The data frame or matrix containing the data to be
#'   displayed in the table.
#' @param columnNamesAsColumnHeaders TRUE to use the data frame column names
#'   as the column headers in the table.
#' @param explicitColumnHeaders A character vector of column headers.
#' @param rowNamesAsRowHeaders TRUE to use the data frame row names as the
#'   row headers in the table.
#' @param firstColumnAsRowHeaders TRUE to use the first column in the data
#'   frame as row headings.
#' @param explicitRowHeaders A character vector of row headers.
#' @param numberOfColumnsAsRowHeaders The number of columns to be set as row
#'   headers.
#' @param columnFormats A list containing format specifiers, each of which is
#'   either an sprintf() character value, a list of format() arguments or an
#'   R function that provides custom formatting logic.
#' @param columnCellTypes A vector that is the same length as the
#'   number of columns in the data frame, where each element is one of
#'   the following values that specifies the type of cell: root, rowHeader,
#'   columnHeader, cell, total.  The cellType controls the default styling
#'   that is applied to the cell.  Typically only rowHeader, cell or total
#'   would be used.
#' @param theme Either the name of a built-in theme (default, largeplain,
#'   compact or blank/none) or a list which specifies the default formatting for
#'   the table.
#' @param replaceExistingStyles TRUE to completely replace the default styling
#'   with the specified tableStyle, headingStyle, cellStyle and/or totalStyle
#' @param tableStyle A list of CSS style declarations that apply to the table.
#' @param headingStyle A list of CSS style declarations that apply to the
#'   heading cells in the table.
#' @param cellStyle A list of CSS style declarations that apply to the normal
#'   cells in the table.
#' @param totalStyle A list of CSS style declarations that apply to the total
#'   cells in the table.
#' @param ... Additional arguments, currently compatibility, argumentCheckMode
#'   and/or styleNamePrefix.
#' @return A basic table.
#' @examples
#' qhtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")])
#' qhtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")], columnNamesAsColumnHeaders=FALSE,
#'   explicitColumnHeaders=c("Week", "From", "To", "Trains", "On-Time"))
#'
qhtbl <- function(dataFrameOrMatrix, columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                  rowNamesAsRowHeaders=FALSE, firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL, numberOfColumnsAsRowHeaders=0,
                  columnFormats=NULL, columnCellTypes=NULL, theme=NULL, replaceExistingStyles=FALSE,
                  tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qhtbl", dataFrameOrMatrix, missing(dataFrameOrMatrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("data.frame", "matrix"))
  checkArgument(3, TRUE, "", "qhtbl", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhtbl", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", firstColumnAsRowHeaders, missing(firstColumnAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhtbl", numberOfColumnsAsRowHeaders, missing(numberOfColumnsAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
  checkArgument(3, TRUE, "", "qhtbl", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
  checkArgument(3, TRUE, "", "qhtbl", columnCellTypes, missing(columnCellTypes), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
  checkArgument(3, TRUE, "", "qhtbl", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyles"), allowedListElementClasses="character")
  checkArgument(3, TRUE, "", "qhtbl", replaceExistingStyles, missing(replaceExistingStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", tableStyle, missing(tableStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  checkArgument(3, TRUE, "", "qhtbl", headingStyle, missing(headingStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  checkArgument(3, TRUE, "", "qhtbl", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  checkArgument(3, TRUE, "", "qhtbl", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  compatibility <- arguments$compatibility
  styleNamePrefix <- arguments$styleNamePrefix
  tbl <- BasicTable$new(argumentCheckMode=argumentCheckMode, theme=theme, replaceExistingStyles=replaceExistingStyles,
                        tableStyle=tableStyle, headingStyle=headingStyle, cellStyle=cellStyle, totalStyle=totalStyle,
                        compatibility=compatibility)
  if("data.frame" %in% class(dataFrameOrMatrix)) {
    tbl$addData(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, firstColumnAsRowHeaders=firstColumnAsRowHeaders, explicitRowHeaders=explicitRowHeaders,
                numberOfColumnsAsRowHeaders=numberOfColumnsAsRowHeaders, columnFormats=columnFormats, columnCellTypes=columnCellTypes)
  }
  else if("matrix" %in% class(dataFrameOrMatrix)) {
    tbl$addMatrix(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, explicitRowHeaders=explicitRowHeaders, columnFormats=columnFormats)
  }
  w <- tbl$renderTable(styleNamePrefix=styleNamePrefix)
  return(w)
}
