#' Parse, separate and format multi-statement SQL strings
#' @description An R6 Class with variables and methods to store,
#'   manipulate and print Structured Query Language (SQL) with multiple
#'   statements in string format. The class parses each statement and assigns
#'   a 'state' to each character, identifying that character as having no
#'   specific state, as being part of a comment, or as being quoted. The class
#'   also parses out key words from the statement for use in display or
#'   printing.
#' @export
sql_parser <- R6::R6Class("sql_parser",
  public = list(
    #' @field text A character string containing one or more SQL statements
    #'   separated by a semicolon.
    text = NULL,

    #' @field file A character string. A path to a file containing SQL to parse.
    file = NULL,

    #' @field standard A character string. An SQL standard from which to
    #'   identify Reserved Words from. One of:
    #'   \code{c('SQL-92', 'SQL:2011', 'SQL:2016', 'PostgreSQL')}
    standard = NULL,

    #' @field params A named list of character strings. Contains parameters to
    #'   replace in the SQL code. Parameters need to be identically named and
    #'   contained by brackets \{\} in the SQL file.
    params = NULL,

    #' @field reserved A character string vector which is populated from the
    #'   global `standards` variable and filtered during class initialization by
    #'   choice of the `standard` field.
    reserved = NULL,

    #' @field param_text A character string. A string sourced from `text` or
    #'   `file` where parameters which have been enclosed in brackets in the
    #'   string have been replaced by their values in `params`.
    param_text = NULL,

    #' @field char_states A list of tibbles, one tibble for each statement.
    #'   Each tibble contains two columns, 'char' and 'state', a single
    #'   character field and a list encapsulated named logical vector
    #'   containing that character's state respectively.
    char_states = list(),

    #' @field stripped_states A list of tibbles. A stripped version of
    #'   `char_states` where white space characters have been removed from the
    #'   beginning and end of statement tibbles.
    stripped_states = list(),

    #' @field string_states A list of tibbles, one tibble for each statement.
    #'   Each tibble contains three columns, 'state', 'state_no' and 'string'.
    #'   'state' is a list encapsulated named logical vector containing
    #'   a string's state. 'state_no' is the order in the statement in which the
    #'   string/state occurs. 'string' is the concatenation of individual
    #'   characters from `stripped_states` 'char' field which share the same
    #'   state before a new state is detected.
    string_states = list(),

    #' @field sql A list of character strings, one string for each statement.
    sql = list(),

    #' @field formatted A list of character strings, on string for each
    #' statement where strings have been color and style formatted by state via
    #' the `crayon` package.
    formatted = list(),

    #' @description Create a new SqlParser object.
    #'
    #' @param text A character string containing one or more SQL statements
    #'   separated by a semicolon (mutually exclusive with `file_path`).
    #' @param file_path A character string. A path to an existing file
    #'   containing one or more SQL statements separated by a semicolon
    #'   (mutaually exclusive with `text`).
    #' @param standard A character string. An SQL standard from which to
    #'   identify Reserved Words from. One of:
    #'   \code{c('SQL-92', 'SQL:2011', 'SQL:2016', 'PostgreSQL')}
    #' @param params A named list of character strings. Contains parameters to
    #'   replace in the SQL code. Parameters need to be identically named and
    #'   contained by brackets \{\} in the SQL file.
    #' @examples
    #' sql_parser$new(text = "SELECT cal1 FROM tbl1; SELECT col2 FROM tbl2;")
    #' sql_parser$new(text = "SELECT {cols} FROM tbl1",
    #'                params = list(cols = "col1, col2"))
    #' sql_parser$new(file = "/path/to/file.sql",
    #'                standard = "PostgreSQL")
    #' @return A new `SqlParser` object.
    initialize = function(text = NA, file_path = NA, standard = "SQL:2016",
                          params = NA){
      if (!standard %in% parsesql::standards){
        stop(glue(paste0("standard: must be one of {paste(standards, ",
                         "collapse = ', ')}.")))
      }
      if (!is.list(params) && !is.na(params)){
        stop("params: must be a named list.")
      }
      if (!is.character(text) && !is.na(text)){
        stop("text: must be character.")
      }
      if (is.na(text) && is.na(file_path)){
        stop("either text or file_path must not be NA.")
      }
      if (!is.na(text) && !is.na(file_path)){
        stop("text and file_path cannot both be not both be provided.")
      }
      self$standard <- standard
      self$reserved <- dplyr::filter(special,
                                     standard == self[["standard"]])$key_word
      self$params <- params
      if (!is.na(text)){
        self$text = text
      } else {
        self$read_fp(file_path)
      }
      if (!is.na(self$params)){
        self$param_text <- glue::glue_data(.x = self$params, self$text)
      } else {
        self$param_text <- self$text
      }
      self$char_states <- list()
      self$stripped_states <- list()
      self$string_states <- list()
      self$sql <- list()
      self$formatted <- list()

      self$get_state()
      self$strip_ws()
      self$combine_states()
      self$combine_stmts()
    },

    #' @description Reads in file text and stores within the class if there is
    #' no text argument given.
    #' @param f A character string which is a file path to an existing file
    #'   containing one or more SQL statements separated by a semicolon.
    read_fp = function(f){
      if (!file.exists(f)){
        stop("file_path: must be an existing file.")
      }
      self$text = readr::read_file(f)
    },

    #' @description Parses character string multi-statement SQL one character
    #' at a time and assigns a state to that character pertaining to whether it
    #' is single-quoted, dollar quoted, a quoted identifier, or a comment.
    get_state = function(){
      param_sql <- strsplit(self$param_text, "")[[1]]
      param_len <- length(param_sql)
      if (param_len == 0){
        return(param_sql)
      }

      current <- character()
      state_list <- list()
      state <- c(
        lc = FALSE,  # line comment
        bc = FALSE,  # block comment
        dt1 = FALSE,  # first tag within the dollar quote
        dt2 = FALSE,  # second tag within the dollar quote
        dc = FALSE,  # string constant within the dollar quote
        sq = FALSE,  # single quoted string constant
        qi = FALSE  # quoted identifier
      )
      begin <-  TRUE
      for (i in seq(1, param_len)){
        new_state <- state
        if (paste0(param_sql[i], param_sql[i+1]) == "--"){
          if (all(!new_state)){
            new_state['lc'] <- TRUE
            begin <- TRUE
          }
        }
        if (param_sql[i] == "\n"){
          if (new_state["lc"]){
            new_state["lc"] <- FALSE
            begin <- FALSE
          }
        }
        if (paste0(param_sql[i], param_sql[i+1]) == "/*"){
          if (all(!new_state)){
            new_state["bc"] <- TRUE
            begin <- TRUE
          }
        }
        if (paste0(param_sql[i-1], param_sql[i])== "*/"){
          if (new_state["bc"]){
            new_state["bc"] <- FALSE
            begin <- FALSE
          }
        }
        if (param_sql[i] == "$"){
          if (all(!new_state)){
            new_state["dt1"] <- TRUE
            begin <- TRUE
          } else if (new_state["dt1"] &&
                     all(!new_state[!names(new_state) %in% c("dt1")])){
            new_state["dt1"] <- FALSE
            new_state["dc"] <- TRUE
            begin <- FALSE
          } else if (new_state["dc"] &&
                     all(!new_state[!names(new_state) %in% c("dc")])){
            begin <- TRUE
            new_state["dc"] <- FALSE
            new_state["dt2"] <- TRUE
          } else if (new_state["dt2"] &&
                     all(!new_state[!names(new_state) %in% c("dt2")])){
            new_state["dt2"] <- FALSE
            begin <- FALSE
          }
        }
        if (param_sql[i] == "'" &&
            paste0(param_sql[i], param_sql[i+1]) != "''" &&
            paste0(param_sql[i-1], param_sql[i]) != "''"){
          if (all(!new_state)){
            new_state["sq"] <-  TRUE
            begin <- TRUE
          } else if (new_state["sq"]
                     && all(!new_state[!names(new_state) %in% c("sq")])){
            new_state["sq"] <- FALSE
            begin <- FALSE
          }
        }
        if (param_sql[i] == '"' &&
            paste0(param_sql[i], param_sql[i+1]) != '""' &&
            paste0(param_sql[i-1], param_sql[i]) != '""'){
          if (all(!new_state)){
            new_state["qi"] <- TRUE
            begin <- TRUE
          } else if (new_state["qi"] &&
                     all(!new_state[!names(new_state) %in% c("qi")])){
            new_state["qi"] <- FALSE
            begin <- FALSE
          }
        }

        if (begin){
          state_list <- c(state_list, list(new_state))
        } else {
          state_list <- c(state_list, list(state))
        }
        current <- c(current, param_sql[i])
        state <- new_state

        if (param_sql[i] == ";" && all(!new_state)){
          self$char_states[[length(self$char_states) + 1]] <-
            tibble::as_tibble(list(char = current, state = state_list))
          current <- character()
          state_list <- list()
        }
      }
    },

    #' @description Finds white space characters at the beginning and end of a
    #'   tibble produced by the `get_state` method and removes them.
    #' @param stmt A tibble with two columns, 'char' and 'state', a single
    #'   character field and a list encapsulated named logical vector
    #'   containing that character's state respectively.
    #'
    #' @return A tibble in the form of `stmt` with beginning and end rows of a
    #'   statement removed if they contain white space characters.
    strip_single = function(stmt){
      df <- stmt
      for (i in range(1,2)){
        new <- tibble::tibble(char = character(), state = list())
        non_ws <- FALSE
        for (j in as.integer(rownames(df))){
          if (grepl("^[^\\s]+$", df[j, "char"], perl = TRUE)){
            new <- dplyr::bind_rows(new, df[j:nrow(df),])
            break
          }
        }
        df <- new[dim(new)[1]:1,]
      }
      return(df)
    },

    #' @description Applies the `strip_single` method over a list of tibbles.
    strip_ws = function(){
      self$stripped_states <- lapply(self$char_states, self$strip_single)
    },

    #' @description Combines characters that have the same state into strings
    #' and stores them along with state and order in a tibble.
    #' @param stripped A tibble with two columns, 'char' and 'state', a single
    #'   character field and a list encapsulated named logical vector
    #'   containing that character's state respectively. Produced from the
    #'   `strip_single` method.
    #' @return  A tibble containing three columns, 'state', 'state_no' and
    #'   'string'. 'state' is a list encapsulated named logical vector
    #'   containing a string's state. 'state_no' is the order in the statement
    #'   in which the string/state occurs. 'string' is the concatenation of
    #'   individual characters from `stripped_states` 'char' field which share
    #'   the same state before a new state is detected.
    string_group = function(stripped){
      groups <- stripped %>% dplyr::group_by(state) %>%
        dplyr::summarize(.groups="drop") %>%
        dplyr::mutate(gn = dplyr::row_number())
      combined <- stripped %>%
        dplyr::inner_join(groups, by = c("state"="state")) %>%
        # next line is where the magic happens to detect changes in states
        dplyr::mutate(state_no = cumsum(
          gn != dplyr::lag(gn, 1, default = dplyr::first(gn)))
          ) %>%
        dplyr::group_by(state, state_no) %>%
        dplyr::summarize(string = paste(char, collapse = ""),
                         .groups = "drop") %>%
        dplyr::arrange(state_no)
      return(combined)
    },

    #' @description Applies the `string_group` method over a list of tibbles.
    combine_states = function(){
      self$string_states <- lapply(self$stripped_states, self$string_group)
    },

    #' @description Takes a character string  and a state and assigns a color
    #'   and style to the string using the crayon package.
    #'
    #' @param my_chars A character string containing the text to format.
    #' @param my_state A named logical vector containing state information for
    #'   \code{my_chars}.
    #' @param reserved A character vector of key words to color and
    #'   \strong{bold}.
    #'
    #' @return A color/style formatted version of \code{my_chars}.
    format_char = function(my_chars, my_state, reserved){
      f <- Vectorize(FUN = function(my_chars, my_state, reserved){
        if (my_state[["lc"]] || my_state['bc']) {
          text <- crayon::yellow(my_chars)
        } else if (any(my_state[c("dt1", "dt2")])) {
          text <- crayon::bgYellow(crayon::red(my_chars))
        } else if (any(my_state[c("sq", "dc")])) {
          text <- crayon::red(my_chars)
        } else if (my_state["qi"]) {
          text <- crayon::cyan(my_chars)
        } else {
          split_text <- unlist(stringr::str_split(my_chars, "\\b"))
          format_list <- dplyr::case_when(
            stringr::str_to_lower(split_text) %in%
              stringr::str_to_lower(reserved) ~
              crayon::blue(crayon::bold(split_text)),
            TRUE ~ crayon::reset(split_text))
          text <-  paste(format_list, collapse = "")
        }
        return(text)
      }, vectorize.args = c("my_chars", "my_state"))
      return(f(my_chars, my_state, reserved))
    },

    #' @description Combines SQL with different states into a single statements,
    #' both unformatted for execution and formatted for printing/display.
    #' @param df A tibble produced from the `string_group` method.
    #' @param reserved A character vector of key words to color and
    #'   \strong{bold}.
    #'
    #' @return A named list containing the SQL statement (sql) and a formatted
    #'   version for printing (formatted).
    group_df = function(df, reserved) {
      df_frmt <- df %>%
        dplyr::mutate(frmt = self$format_char(my_chars = string,
                                              my_state = state,
                                              reserved = reserved)) %>%
        dplyr::arrange(state_no) %>%
        dplyr::group_by() %>%
        dplyr::summarize(sql = paste(string, collapse = ""),
                  formatted = paste(frmt, collapse = ""),
                  .groups = "drop")
      sql <- unlist(df_frmt$sql)
      formatted <- unlist(df_frmt$formatted)
      return(list(sql = sql, formatted = formatted))
    },

    #' @description Applies the `group_df` method over a list of tibbles and
    #'   separates the output into two separate class variables, `sql` and
    #'   `formatted`.
    combine_stmts = function(){
      combined_list <-  lapply(X = self$string_states, FUN = self$group_df,
                               reserved = self$reserved)
      self$sql <- sapply(combined_list, FUN = function(x) x$sql)
      self$formatted <- sapply(combined_list, FUN = function(x) x$formatted)
    },

    #' @description Prints the formatted SQL statements separated by a dashed
    #'   line
    #' @param ... additional arguments to print
    print = function(...){
      d <- paste(c("\n\n", rep("-", 70), "\n\n"), collapse = "")
      s <- paste(self$formatted, collapse = d)
      cat(s)
      cat("\n")
      invisible(self)
    }
  )
)

