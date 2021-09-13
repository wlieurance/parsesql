#' parsesql: A package for parsing multi-statement Structured Query Language
#' (SQL) strings.
#'
#' @description The sqlparse package provides a single R6 class `sql_parser`
#' whose methods and variables are used to parse a given SQL string or file
#' during initialization of the class. This class can be used to:
#' \enumerate{
#'   \item Split a string/file into a statement vector.
#'   \item `glue` parameters into SQL strings using bracket notation
#'     (e.g. SELECT \{my_col\} FROM \{my_table\};).
#'   \item Print color/style formatted SQL according to one of four specific
#'     SQL languages or standards.
#'   \item Separate SQL statements into ordered tibbles, with one row per state
#'     change. Useful in identifying statement characters which have been
#'     commented out or quoted.
#' }
#' @docType package
#' @name parsesql
NULL
#> NULL
