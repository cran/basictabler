% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TableStyles.R
\docType{class}
\name{TableStyles}
\alias{TableStyles}
\title{R6 class that defines a collection of styles.}
\format{
\code{\link[R6]{R6Class}} object.
}
\description{
The `TableStyles` class defines all of the base styles needed to style/theme a
table.  It defines the names of the styles that are used for
styling the different parts of the table.
}
\examples{
# Creating styles is part of defining a theme for a table.
# Multiple styles must be created for each theme.
# The example below shows how to create one style.
# For an example of creating a full theme please
# see the Styling vignette.
tbl <- BasicTable$new()
# ...
TableStyles <- TableStyles$new(tbl, themeName="compact")
TableStyles$addStyle(styleName="MyNewStyle", list(
    font="0.75em arial",
    padding="2px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "text-align"="center",
    "font-weight"="bold",
    "background-color"="#F2F2F2"
  ))
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{count}}{The number of styles in this styles collection.}

\item{\code{theme}}{The name of the theme.}

\item{\code{styles}}{The collection of `TableStyle` objects in this styles collection.}

\item{\code{allowExternalStyles}}{Enable integration scenarios where an external
system is supplying the CSS definitions.}

\item{\code{tableStyle}}{The name of the style for the HTML table element.}

\item{\code{rootStyle}}{The name of the style for the HTML cell at the top left of
the table (when both row and column headers are displayed).}

\item{\code{rowHeaderStyle}}{The name of the style for the row headers in the table.}

\item{\code{colHeaderStyle}}{The name of the style for the column headers in the
table.}

\item{\code{cellStyle}}{The name of the cell style for the non-total cells in the
body of the table.}

\item{\code{totalStyle}}{The name of the cell style for the total cells in the
table.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TableStyles-new}{\code{TableStyles$new()}}
\item \href{#method-TableStyles-isExistingStyle}{\code{TableStyles$isExistingStyle()}}
\item \href{#method-TableStyles-getStyle}{\code{TableStyles$getStyle()}}
\item \href{#method-TableStyles-addStyle}{\code{TableStyles$addStyle()}}
\item \href{#method-TableStyles-copyStyle}{\code{TableStyles$copyStyle()}}
\item \href{#method-TableStyles-asCSSRule}{\code{TableStyles$asCSSRule()}}
\item \href{#method-TableStyles-asNamedCSSStyle}{\code{TableStyles$asNamedCSSStyle()}}
\item \href{#method-TableStyles-asList}{\code{TableStyles$asList()}}
\item \href{#method-TableStyles-asJSON}{\code{TableStyles$asJSON()}}
\item \href{#method-TableStyles-asString}{\code{TableStyles$asString()}}
\item \href{#method-TableStyles-clone}{\code{TableStyles$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-new"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-new}{}}}
\subsection{Method \code{new()}}{
Create a new `TableStyles` object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$new(parentTable, themeName = NULL, allowExternalStyles = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parentTable}}{Owning table.}

\item{\code{themeName}}{The name of the theme.}

\item{\code{allowExternalStyles}}{Enable integration scenarios where an external
system is supplying the CSS definitions.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No return value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-isExistingStyle"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-isExistingStyle}{}}}
\subsection{Method \code{isExistingStyle()}}{
Check whether a style with the specified name exists in the collection.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$isExistingStyle(styleName = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{styleName}}{The style name.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
`TRUE` if a style with the specified name exists, `FALSE` otherwise.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-getStyle"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-getStyle}{}}}
\subsection{Method \code{getStyle()}}{
Retrieve a style with the specified name from the collection.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$getStyle(styleName = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{styleName}}{The style name.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A `TableStyle` object if a style with the specified name exists in the collection, an error is raised otherwise.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-addStyle"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-addStyle}{}}}
\subsection{Method \code{addStyle()}}{
Add a new style to the collection of styles.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$addStyle(styleName = NULL, declarations = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{styleName}}{The style name of the new style.}

\item{\code{declarations}}{A list containing CSS style declarations.
Example: `declarations = list(font="...", color="...")`}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The newly created `TableStyle` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-copyStyle"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-copyStyle}{}}}
\subsection{Method \code{copyStyle()}}{
Create a copy of an exist style.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$copyStyle(styleName = NULL, newStyleName = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{styleName}}{The style name of the style to copy.}

\item{\code{newStyleName}}{The name of the new style.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The newly created `TableStyle` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-asCSSRule"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-asCSSRule}{}}}
\subsection{Method \code{asCSSRule()}}{
Generate a CSS style rule from the specified table style.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$asCSSRule(styleName = NULL, selector = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{styleName}}{The style name.}

\item{\code{selector}}{The CSS selector name.  Default value `NULL`.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The CSS style rule, e.g. \{ text-align: center; color: red; \}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-asNamedCSSStyle"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-asNamedCSSStyle}{}}}
\subsection{Method \code{asNamedCSSStyle()}}{
Generate a named CSS style from the specified table style.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$asNamedCSSStyle(styleName = NULL, styleNamePrefix = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{styleName}}{The style name.}

\item{\code{styleNamePrefix}}{A character variable specifying a prefix for all named
CSS styles, to avoid style name collisions where multiple tables exist.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The CSS style rule, e.g. cell \{ text-align: center; color: red; \}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-asList"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-asList}{}}}
\subsection{Method \code{asList()}}{
Return the contents of this object as a list for debugging.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$asList()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of various object properties.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-asJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-asJSON}{}}}
\subsection{Method \code{asJSON()}}{
Return the contents of this object as JSON for debugging.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$asJSON()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A JSON representation of various object properties.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-asString"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-asString}{}}}
\subsection{Method \code{asString()}}{
Return the contents of this object as a string for debugging.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$asString(seperator = ", ")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{seperator}}{Delimiter used to combine multiple values into a string.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A character representation of various object properties.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableStyles-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TableStyles-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableStyles$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
