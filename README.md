# parsesql
Parses SQL into vector statements, formats with parameters, identifies character states/key words, and formats the result for printing.

## Description

This project provides classes in both python and R which can be used to do simple Structured Query Language (SQL) parsing in both languages. The _sql_parser_ class functions by first doing a parameter replacement in the SQL if any are passed by the user and found within the string. Parameters in the SQL code are surrounded by \{\} brackets and named parameters are passed to the class by the user at initialization. The class then assigns a 'state' to each character in the SQL code, where the state can be one of the following: 

- default state (none of the other states)
- line comment
- block comment
- dollar quote
- dollar tag
- single quote
- quoted identifier (double quote)

Characters sharing the same state are then concatenated between each state change and stored within dictionary vectors in python or tibbles in R.  This allows users to do string replacement depending on state (e.g. only replace a word if it is quoted or replace a function name only if it is neither quoted nor commented). State-identified strings are split into a vector of statements by splitting the text by semi-colons within the default state. Key words for PostgreSQL, SQL-92, SQL:2011, and SQL:2016 are then identified within the default state per user choice of language/standard at class initialization. Finally, the strings are recombined for each statement to create two vector class variables, one with SQL suitable for single statement execution by python or R, and another for printing out a style/color formatted version of each statement.

## Getting Started

Users can initialize an instance of the class most simply in python using the following commands:


`from parsesql.classes import SqlParser`

`my_instance = SqlParser(args)` 


or in R using the R6 class instancing syntax:


`library(parsesql)`

`my_instance = sql_parser$new(args)`


Doing this will parse the SQL provided and make the output available in _my_instance.sql_ (python) or _my_instance$sql_ (R).
Users can also print a formatted version of the SQL statements by calling print on the instance.

For those users who may want to use regex or string replacement functions, the _string_states_ class variable will contains the individual states for each statement.
### Dependencies

**python**: 

- colorama

**R**:

- dplyr
- crayon
- glue
- R6
- readr
- stringr
- tibble

### Installing

**python** (from the terminal):

`pip install git+https://github.com/wlieurance/parsesql`

**R** (from within the R console):

`devtools::install_github("wlieurance/parsesql/R")`

## Help

The parser currently relies on the fact that every parameter in the SQL text (i.e. text within brackets \{\}) needs to be provided
by the user at class initialization. If users do not provide a parameter dictionary (python) or a named list(R) the class with still initialize, but providing only partial parameters will cause errors.  Support for partial parameter formatting is planned for future releases.

## Authors

Wade Lieurance

## Version History

- 0.1
  - Initial Release

## License

This project is licensed under the GPL3 License - see the LICENSE.md file for details.