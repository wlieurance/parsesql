#' Key words used by various SQL languages and standards.
#'
#' A dataset containing key words and other attributes used by PostgreSQL,
#' and the SQL-92, SQL:2011, SQL:2016 standards.
#'
#' @format A tibble with 2041 rows and 4 variables:
#' \describe{
#'     \item{key_word}{A chracter string recognized as special or reserved by
#'     the relevant standard}
#'     \item{standard}{The language or standard which recognizes the key_word}
#'     \item{reserved}{A logical value indicating whether the key_word is
#'     reserved or not. A reserved word generally cannot be used for column or
#'     table names.}
#'     \item{function_or_type}{A logical value stating whether the key_word can
#'     be used as a function or data type name. Some non-reserved words cannot
#'     be used for a function or data type name and some reserved key_words can
#'     be.}
#' }
#' @source \url{https://www.postgresql.org/docs/current/sql-keywords-appendix.html}
"special"

#' Available Structured Query Language languages or standards available to use
#' in SQL formatting.
#'
#' @format A vector of four character strings.
"standards"
