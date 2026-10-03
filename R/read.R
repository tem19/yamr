#' Read an exported log file
#'
#' Parses a part of a log downloaded with [ym_logs_download_part()]: a
#' tab-separated file with a header row, in ClickHouse `TabSeparated` escaping.
#' Columns are converted according to the field types from
#' [ym_logs_fields()]:
#'
#' | ClickHouse type | R type |
#' |---|---|
#' | `String` | character (escapes decoded) |
#' | `Int8`-`Int32`, `UInt8`, `UInt16` | integer |
#' | `UInt32`, `Int64`, `Float*` | double |
#' | `UInt64` | character, since IDs such as `clientID` don't fit a double |
#' | `Date` | Date |
#' | `DateTime` | POSIXct |
#' | `Array(...)` | the raw string, or a list if `parse_arrays = TRUE` |
#'
#' @param file Path to the file.
#' @param parse_types Convert columns to R types (see the table). If `FALSE`,
#'   all columns are returned as character (still unescaped).
#' @param parse_arrays Convert `Array(...)` columns to list columns; see
#'   [ym_parse_array()].
#' @param clean_names Drop the `ym:s:` / `ym:pv:` prefix from column names.
#' @param tz Time zone assigned to `DateTime` columns. Metrica exports wall
#'   clock time (the tag's time zone for `dateTime`, UTC+3 for `dateTimeUTC`),
#'   and it is kept as is.
#' @return A tibble.
#' @export
ym_read_log <- function(file, parse_types = TRUE, parse_arrays = FALSE,
                        clean_names = TRUE, tz = "UTC") {
  df <- readr::read_tsv(
    file,
    col_types = readr::cols(.default = readr::col_character()),
    quote = "", na = character(), trim_ws = FALSE, comment = "",
    progress = FALSE, lazy = FALSE
  )
  types <- field_types(names(df))
  for (i in seq_along(df)) {
    df[[i]] <- convert_column(df[[i]], types[i], parse_types, parse_arrays, tz)
  }
  if (clean_names) {
    names(df) <- sub("^ym:(s|pv):", "", names(df))
  }
  tibble::as_tibble(df)
}

convert_column <- function(x, type, parse_types, parse_arrays, tz) {
  if (is.na(type)) {
    return(tsv_unescape(x))
  }
  if (startsWith(type, "Array(")) {
    if (!parse_arrays) {
      return(x)
    }
    inner <- sub("^Array\\((.*)\\)$", "\\1", type)
    return(lapply(ym_parse_array(x), convert_scalar, type = inner, parse_types = parse_types, tz = tz))
  }
  convert_scalar(tsv_unescape(x), type, parse_types, tz)
}

convert_scalar <- function(x, type, parse_types = TRUE, tz = "UTC") {
  if (!parse_types) {
    return(x)
  }
  x[!nzchar(x)] <- NA
  switch(type,
    Int8 = , Int16 = , Int32 = , UInt8 = , UInt16 = as.integer(x),
    UInt32 = , Int64 = , Float32 = , Float64 = as.double(x),
    Date = as.Date(x, format = "%Y-%m-%d"),
    DateTime = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = tz),
    # String, UInt64 and anything unknown stay character; restore empty strings.
    {
      x[is.na(x)] <- ""
      x
    }
  )
}

# Decodes ClickHouse TabSeparated escapes: \t \n \r \b \f \0 \' \\ and \N (NULL).
tsv_unescape <- function(x) {
  has <- grepl("\\", x, fixed = TRUE)
  if (!any(has)) {
    return(x)
  }
  x[has] <- vapply(x[has], unescape_one, character(1), USE.NAMES = FALSE)
  x
}

escape_map <- c(
  t = "\t", n = "\n", r = "\r", b = "\b", f = "\f", "0" = "",
  "'" = "'", "\"" = "\"", "\\" = "\\", N = ""
)

unescape_one <- function(s) {
  m <- gregexpr("\\\\.", s)[[1]]
  if (m[1] == -1) {
    return(s)
  }
  esc <- regmatches(s, list(m))[[1]]
  repl <- escape_map[substring(esc, 2)]
  repl[is.na(repl)] <- substring(esc[is.na(repl)], 2) # unknown escape: drop the backslash
  regmatches(s, list(m)) <- list(unname(repl))
  s
}

#' Parse array values from a log
#'
#' Array fields (`goalsID`, `purchaseID`, `parsedParamsKey1`, ...) are exported
#' as ClickHouse literals like `[1,2,3]` or `['a','b']`. This splits them into
#' character vectors, one per input value; [ym_read_log()] with
#' `parse_arrays = TRUE` additionally converts the elements to the field's type.
#'
#' @param x Character vector of array literals.
#' @return A list of character vectors, the same length as `x`.
#' @export
#' @examples
#' ym_parse_array(c("[1,2]", "[]", "['a','it\\'s']"))
ym_parse_array <- function(x) {
  lapply(x, function(s) {
    s <- sub("^\\[(.*)\\]$", "\\1", s)
    if (!nzchar(s)) {
      return(character())
    }
    if (startsWith(s, "'")) {
      items <- regmatches(s, gregexpr("'((?:[^'\\\\]|\\\\.)*)'", s, perl = TRUE))[[1]]
      return(tsv_unescape(substring(items, 2, nchar(items) - 1)))
    }
    strsplit(s, ",", fixed = TRUE)[[1]]
  })
}
