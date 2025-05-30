#' R6 class that converts a table to a flextable from the `flextabler` package.
#'
#' @description
#' The `FlexTableRenderer` class creates a representation of a table using
#' the `flextable` package.  See the Output vignette for more details.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link[R6]{R6Class}} object.
#' @examples
#' # This class should not be used by end users.  It is an internal class
#' # created only by the BasicTable class.  It is used when converting to a
#' # flextable.

FlexTableRenderer <- R6::R6Class("FlexTableRenderer",
  public = list(

    #' @description
    #' Create a new `FlexTableRenderer` object.
    #' @param parentTable Owning table.
    #' @return No return value.
    initialize = function(parentTable) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
      }
      private$p_parentTable <- parentTable
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$new", "Creating new FlexTbl Renderer...")
      private$p_styles <- FlexTableStyles$new(parentTable=private$p_parentTable)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$new", "Created new FlexTbl Renderer.")
    },

    #' @description
    #' Write a value to a cell, optionally with styling and cell merging.
    #' @param ft The flextable to write to.
    #' @param rowNumber The row number of the cell where the value is to be
    #'   written.
    #' @param columnNumber The column number of the cell where the value is to be
    #'   written.
    #' @param totalRowCount Used when writing the cell border.
    #' @param totalColumnCount Used when writing the cell border.
    #' @param value The value to be written.  Since the flextable is created
    #'   from a data frame, this argument can be omitted.
    #' @param applyStyles `TRUE` (default) to also set the styling of the cell,
    #'   `FALSE` to only write the value.
    #' @param baseStyleName The name of the style from the table theme to apply
    #'   to the cell.
    #' @param style A `TableStyle` object that contains additional styling to
    #'   apply to the cell.
    #' @param mapFromCss `TRUE` (default) to map the basictabler CSS styles to
    #'   corresponding Excel styles, `FALSE` to apply only the specified xl
    #'   styles.
    #' @param mergeRows If the cell is to be merged with adjacent cells, then an
    #'   integer or numeric vector specifying the row numbers of the merged
    #'   cell.  NULL (default) to not merge cells.
    #' @param mergeColumns If the cell is to be merged with adjacent cells, then
    #'   an integer or numeric vector specifying the column numbers of the
    #'   merged cell.  NULL (default) to not merge cells.
    #' @return The updated flextable definition.
    writeToCell = function(ft=NULL, rowNumber=NULL, columnNumber=NULL, totalRowCount=NULL, totalColumnCount=NULL,
                           value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL) {
       if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", rowNumber, missing(rowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", columnNumber, missing(columnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", totalRowCount, missing(totalRowCount), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", totalColumnCount, missing(totalColumnCount), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "character", "Date", "POSIXct", "POSIXlt"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="TableStyle")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", mergeRows, missing(mergeRows), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeToCell", mergeColumns, missing(mergeColumns), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$writeToWorksheet", "Writing to cell...")
      # write the value
      if(!missing(value)) ft <- flextable::compose(ft, i=rowNumber, j=columnNumber, value=flextable::as_paragraph(value))
      # merge cells
      isMergedCells <- isNumericValue(mergeRows)&&isNumericValue(mergeColumns)
      if(isMergedCells) ft <- flextable::merge_at(ft, i=mergeRows, j=mergeColumns)
      # styling
      if(applyStyles) {
        ftStyle <- NULL
        # just a base style (these were all already added to the FlexTblStyles collection, so can just do a find based on the name only)
        if(isTextValue(baseStyleName)&&is.null(style)) {
          ftStyle <- private$p_styles$findNamedStyle(baseStyleName)
          if(is.null(ftStyle)) stop(paste0("FlexTableRenderer$writeToWorksheet(): Unable to find named style '", baseStyleName, "'."), call. = FALSE)
        }
        # base style and overlaid style, or just an overlaid style
        else if(!is.null(style)) {
          if(isTextValue(baseStyleName)) {
            # need to get the base style and overlay the additional style attributes
            baseStyle <- private$p_parentTable$styles$getStyle(baseStyleName)
            fullStyle <- baseStyle$getCopy(newStyleName="")
            fullStyle$setPropertyValues(style$declarations)
          }
          else fullStyle <- style
          ftStyle <- private$p_styles$findOrAddStyle(action="findOrAdd", baseStyleName=baseStyleName, isBaseStyle=FALSE, style=fullStyle, mapFromCss=mapFromCss)
          if(is.null(ftStyle)) stop("FlexTableRenderer$writeToWorksheet(): Failed to find or add style.", call. = FALSE)
        }
        # style range
        styleRows <- ifelse(isMergedCells, mergeRows, rowNumber)
        styleCols <- ifelse(isMergedCells, mergeColumns, columnNumber)
        # font name
        if(isTextValue(ftStyle$fontName)) ft <- flextable::font(ft, i=styleRows, j=styleCols, fontname=ftStyle$fontName)
        # font size
        if(!is.null(ftStyle$fontSize)) ft <- flextable::fontsize(ft, i=styleRows, j=styleCols, size=ftStyle$fontSize)
        # bold
        if(isTRUE(ftStyle$bold)) ft <- flextable::bold(ft, i=styleRows, j=styleCols, bold=TRUE)
        # italic
        if(isTRUE(ftStyle$italic)) ft <- flextable::italic(ft, i=styleRows, j=styleCols, italic=TRUE)
        # background colour
        if(isTextValue(ftStyle$bgColor)) ft <- flextable::bg(ft, i=styleRows, j=styleCols, bg=ftStyle$bgColor)
        # text colour
        if(isTextValue(ftStyle$textColor)) ft <- flextable::color(ft, i=styleRows, j=styleCols, color=ftStyle$textColor)
        # hAlign
        if(isTextValue(ftStyle$hAlign)) ft <- flextable::align(ft, i=styleRows, j=styleCols, align=ftStyle$hAlign)
        # vAlign
        if(isTextValue(ftStyle$vAlign)) ft <- flextable::valign(ft, i=styleRows, j=styleCols, valign=ftStyle$vAlign)
        # textRotation
        if(isTextValue(ftStyle$textRotation)) ft <- flextable::rotate(ft, i=styleRows, j=styleCols, rotation=ftStyle$textRotation)
        # padding
        paddingSet <- FALSE
        paddingSet <- paddingSet || !is.null(ftStyle$paddingAll)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingLeft)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingRight)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingTop)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingBottom)
        if(paddingSet==TRUE) {
          ft <- flextable::padding(ft, i=styleRows, j=styleCols, padding=PxToPt(ftStyle$paddingAll),
                                   padding.left=PxToPt(ftStyle$paddingLeft), padding.right=PxToPt(ftStyle$paddingRight),
                                   padding.top=PxToPt(ftStyle$paddingTop), padding.bottom=PxToPt(ftStyle$paddingBottom))
        }

        # border
        borderSet <- FALSE
        borderSet <- borderSet || !is.null(ftStyle$borderAll)
        borderSet <- borderSet || !is.null(ftStyle$borderLeft)
        borderSet <- borderSet || !is.null(ftStyle$borderRight)
        borderSet <- borderSet || !is.null(ftStyle$borderTop)
        borderSet <- borderSet || !is.null(ftStyle$borderBottom)
        if(paddingSet==TRUE) {
          ft <- self$writeBorder(ft, rowNumber=rowNumber, columnNumber=columnNumber, totalRowCount=totalRowCount, totalColumnCount=totalColumnCount,
                                 border=ftStyle$borderAll, borderLeft=ftStyle$borderLeft, borderRight=ftStyle$borderRight, borderTop=ftStyle$borderTop, borderBottom=ftStyle$borderBottom)
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$writeToWorksheet", "Written to cell.")
      return(ft)
    },

    #' @description
    #' Write the borders of a cell.
    #' @param ft The flextable to write to.
    #' @param rowNumber The row number of the cell where the border is to be
    #'   written.
    #' @param columnNumber The column number of the cell where the border is to be
    #'   written.
    #' @param totalRowCount The total number of rows in the table.
    #' @param totalColumnCount The total number of columns in the table.
    #' @param border The border to be applied to all sides of the cell (unless
    #'   a border is specified using the other arguments).
    #' @param borderLeft The border to apply to the left side of the cell.
    #' @param borderRight The border to apply to the right side of the cell.
    #' @param borderTop The border to apply to the top side of the cell.
    #' @param borderBottom The border to apply to the bottom side of the cell.
    #' @return The updated flextable definition.
    writeBorder = function(ft=NULL, rowNumber=NULL, columnNumber=NULL, totalRowCount=NULL, totalColumnCount=NULL,
                           border=NULL, borderLeft=NULL, borderRight=NULL, borderTop=NULL, borderBottom=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeBorder", rowNumber, missing(rowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "writeBorder", columnNumber, missing(columnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$writeBorder", "Writing borders...")
      # arrange arguments
      if(!is.null(border)) {
        if(is.null(borderLeft)) { borderLeft <- border }
        if(is.null(borderRight)) { borderRight <- border }
        if(is.null(borderTop)) { borderTop <- border }
        if(is.null(borderBottom)) { borderBottom <- border }
      }
      # set left border (using public functions)
      if(!is.null(borderLeft)) {
        fpBorder = officer::fp_border(color=borderLeft$color, width=borderLeft$width, style=borderLeft$style)
        if (columnNumber==1) {
          ft <- flextable::vline_left(ft, i=rowNumber, border=fpBorder, part="body")
        } else {
          ft <- flextable::vline(ft, i=rowNumber, j=columnNumber-1, border=fpBorder, part="body")
        }
      }
      # set right border (using public functions)
      if(!is.null(borderRight)) {
        fpBorder = officer::fp_border(color=borderRight$color, width=borderRight$width, style=borderRight$style)
        if (columnNumber==totalColumnCount) {
          ft <- flextable::vline_right(ft, i=rowNumber, border=fpBorder, part="body")
        } else {
          ft <- flextable::vline(ft, i=rowNumber, j=columnNumber, border=fpBorder, part="body")
        }
      }
      # set top border (using public functions)
      if(!is.null(borderTop)) {
        fpBorder = officer::fp_border(color=borderTop$color, width=borderTop$width, style=borderTop$style)
        if (rowNumber==1) {
          ft <- flextable::hline_top(ft, j=columnNumber, border=fpBorder, part="body")
        } else {
          ft <- flextable::hline(ft, i=rowNumber-1, j=columnNumber, border=fpBorder, part="body")
        }
      }
      # set bottom border (using public functions)
      if(!is.null(borderBottom)) {
        fpBorder = officer::fp_border(color=borderBottom$color, width=borderBottom$width, style=borderBottom$style)
        if (rowNumber==totalRowCount) {
          ft <- flextable::hline_bottom(ft, j=columnNumber, border=fpBorder, part="body")
        } else {
          ft <- flextable::hline(ft, i=rowNumber, j=columnNumber, border=fpBorder, part="body")
        }
      }
      # done
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$writeBorder", "Written.")
      return(ft)
    },

    #' @description
    #' Convert table to a flextable table.
    #' @param applyStyles `TRUE` (default) to also set the styling of the cells,
    #'   `FALSE` to only write the value.
    #' @param mapStylesFromCSS `TRUE` (default) to map the basictabler CSS styles to
    #'   corresponding flextable styles where possible, `FALSE` to apply only the
    #'   specified ft styles.
    #' @return No return value.
    asFlexTable = function(applyStyles=TRUE, mapStylesFromCSS=TRUE) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "asFlexTable", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableRenderer", "asFlexTable", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$asFlexTable", "Converting to flextable...")

      # clear the styles
      private$p_styles$clearStyles()

      # create an FlexTblStyle for each named style in the table
      if(applyStyles) private$p_styles$addNamedStyles(mapFromCss=mapStylesFromCSS)

      # output the styles
      # message(private$p_styles$asString(seperator="\r\n\r\n"))

      # do the export, first creating a basic flextable table from a data frame of formatted values, then going cell by cell to apply the styling/formatting
      # for each header and cell, if the style is basic, find the basic style matching on name only
      # if the style is not basic (i.e. has additional style settings applied directly to that header/cell)
      # then need to do a full match on all of the details of the style (slower)

      # get the style names
      tableStyle = private$p_parentTable$styles$tableStyle
      rootStyle = private$p_parentTable$styles$rootStyle
      rowHeaderStyle = private$p_parentTable$styles$rowHeaderStyle
      colHeaderStyle = private$p_parentTable$styles$colHeaderStyle
      cellStyle = private$p_parentTable$styles$cellStyle
      totalStyle = private$p_parentTable$styles$totalStyle

      # ...cells:
      rowCount <- private$p_parentTable$cells$rowCount
      columnCount <- private$p_parentTable$cells$columnCount

      # special case of no rows and no columns, return a blank empty table
      if((rowCount==0)&&(columnCount==0)) {
        df <- data.frame(data="(no data)")
        ft <- flextable::flextable(df)
        ft <- flextable::autofit(ft)
        ft <- flextable::delete_part(ft, part="header")
        ft <- flextable::delete_part(ft, part="footer")
        ft <- self$writeToCell(ft, rowNumber=NULL, columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL)
        return(invisible(ft))
      }

      # update the merged cell info
      private$p_parentTable$applyCellMerges()

      # create the flextable (based on formatted values, since flextables store the data as dataframes, so don't support different data types in the same column)
      ft <- flextable::flextable(private$p_parentTable$asDataFrame(rawValue=FALSE), cwidth=0, cheight=0)
      ft <- flextable::delete_part(ft, part="header")
      ft <- flextable::delete_part(ft, part="footer")
      ft <- flextable::border_remove(ft)

      # set the basic border settings based on the base styles
      # default borders for entire table from the cell style
      # TODO - add functionality to output cell border styles - depending on outcome of https://github.com/davidgohel/flextable/issues/322
      # ftStyle <- private$p_styles$findNamedStyle(cellStyle)
      # if(!is.null(ftStyle$borderAll)) {
      #   ftb <- ftStyle$borderAll
      #   ft <- flextable::border_inner(ft, border=officer::fp_border(color=ftb[["color"]], width=ftb[["width"]], style=ftb[["style"]]))
      #   ft <- flextable::border_outer(ft, border=officer::fp_border(color=ftb[["color"]], width=ftb[["width"]], style=ftb[["style"]]))
      # }

      # render the rows
      for(r in 1:rowCount) {
        # render the cell values
        for(c in 1:columnCount) {

          # get cell
          cell <- private$p_parentTable$cells$getCell(r, c)

          # if a merged cell and not the root of the merge, then skip to next cell
          if(cell$isMerged && (!cell$isMergeRoot)) { next }

          # merge cell if needed
          mergeRows <- NULL
          mergeColumns <- NULL
          if(cell$isMerged) {
            mergeRange <- private$p_parentTable$mergedCells$ranges[[cell$mergeIndex]]
            mergeRows <- r:(r + mergeRange$rSpan - 1)
            mergeColumns <- c:(c + mergeRange$cSpan - 1)
          }

          # get style info
          if(cell$cellType=="root") cs <- rootStyle
          else if(cell$cellType=="rowHeader") cs <- rowHeaderStyle
          else if(cell$cellType=="columnHeader") cs <- colHeaderStyle
          else if(cell$cellType=="total") cs <- totalStyle
          else cs <- cellStyle
          if(!is.null(cell$baseStyleName)) cs <- cell$baseStyleName

          # write cell (value is omitted since it was set in the data frame used to create the flex table)
          ft <- self$writeToCell(ft, rowNumber=r, columnNumber=c, totalRowCount=rowCount, totalColumnCount=columnCount,
                                 applyStyles=applyStyles, baseStyleName=cs, style=cell$style, mapFromCss=mapStylesFromCSS,
                                 mergeRows=mergeRows, mergeColumns=mergeColumns)
        }
      }

      # set as autofit
      # removed since this can't be undone (and the caller could easily call this explicitly)
      # ft <- flextable::autofit(ft)

      # FlexTableStyles collection builds up a collection of styles. This
      # follows the same pattern used by the OpenXlsx renderer - even though
      # the flextable package does not use a collection of styles (same
      # approach taken so that both renderers are consistent).
      # The styles are used as follows: Before rendering
      # begins, a FlexTableStyle object is created for each named style
      # defined in the table. As each cell is rendered to the workbook, if
      # a cell with a named style but no other style overrides is
      # encountered, then these styles are used.  If a cell is encountered
      # with overrides, then the styles collection is searched to find the
      # matching style.  If not found, then a new style is added which can be
      # reused later. The FlexTableStyle object looks for styles named
      # "ft-".  If found, these are used.  These closely map to the formatting
      # options available in the flextable package.
      # If style settings named "ft-" are not found, then an attempt is made
      # to use the CSS equivalents.  Currently, the following still needs
      # doing in terms of style settings:
      #
      # * font-name, font-size, etc have been mapped, but the general CSS font
      # setting (e.g. font: 15px arial, sans-serif;) has not been mapped.
      # * border settings have been partially mapped.  border, border-top,
      # border-bottom, etc have been mapped.  The very specific (e.g.
      # border-bottom-width, border-bottom-style, etc) have not been mapped.

      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableRenderer$asFlexTable", "Converted to flextable.")
      return(ft)
    }
  ),
  private = list(
    p_parentTable = NULL,
    p_styles = NULL
  )
)
