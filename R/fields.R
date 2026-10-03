#' Logs API fields
#'
#' Reference of the fields available for export, with their ClickHouse data
#' types and descriptions, taken from the official documentation
#' (<https://yandex.com/dev/metrika/en/logs/fields/visits>,
#' <https://yandex.com/dev/metrika/en/logs/fields/hits>).
#'
#' Fields marked `parametrized` contain an `<attribution>` placeholder: replace
#' it with an attribution model, e.g. `ym:s:<lastsign>TrafficSource` or
#' `ym:s:lastsignTrafficSource`, or leave it as is and set the `attribution`
#' argument of the request. See
#' <https://yandex.com/dev/metrika/en/logs/param>.
#'
#' @param source `"visits"` (sessions, `ym:s:` prefix), `"hits"` (events,
#'   `ym:pv:` prefix), or `NULL` for both.
#' @param lang Language of the descriptions: `"en"` or `"ru"`.
#' @return A tibble with columns `source`, `field`, `type`, `description` and
#'   `parametrized`.
#' @export
#' @examples
#' ym_logs_fields("visits")
#' ym_logs_fields("hits", lang = "ru")
ym_logs_fields <- function(source = NULL, lang = c("en", "ru")) {
  lang <- rlang::arg_match(lang)
  x <- ym_fields_tbl
  if (!is.null(source)) {
    source <- rlang::arg_match(source, c("visits", "hits"))
    x <- x[x$source == source, ]
  }
  tibble::tibble(
    source = x$source,
    field = x$field,
    type = x$type,
    description = x[[paste0("description_", lang)]],
    parametrized = x$parametrized
  )
}

source_prefix <- function(source) {
  c(visits = "ym:s:", hits = "ym:pv:")[[source]]
}

# Infers `source` from prefixed field names when it isn't given.
resolve_source <- function(source, fields, call = rlang::caller_env()) {
  if (!is.null(source)) {
    return(rlang::arg_match(source, c("visits", "hits"), error_call = call))
  }
  prefixes <- unique(stats::na.omit(regmatches(fields, regexpr("^ym:(s|pv):", fields))))
  if (length(prefixes) != 1) {
    cli::cli_abort(
      "Can't infer {.arg source} from {.arg fields}; set it to {.val visits} or {.val hits}.",
      call = call
    )
  }
  if (prefixes == "ym:s:") "visits" else "hits"
}

# Adds the source prefix to bare names ("visitID" -> "ym:s:visitID") and
# checks the result against the API limits.
normalize_fields <- function(fields, source, call = rlang::caller_env()) {
  if (!is.character(fields) || !length(fields) || anyNA(fields)) {
    cli::cli_abort("{.arg fields} must be a non-empty character vector.", call = call)
  }
  fields <- trimws(unlist(strsplit(fields, ",", fixed = TRUE)))
  fields <- fields[nzchar(fields)]
  prefix <- source_prefix(source)
  bare <- !startsWith(fields, "ym:")
  fields[bare] <- paste0(prefix, fields[bare])
  fields <- unique(fields)

  wrong <- fields[!startsWith(fields, prefix)]
  if (length(wrong)) {
    cli::cli_abort(
      "Fields for {.val {source}} must start with {.val {prefix}}, not: {.val {wrong}}.",
      call = call
    )
  }
  unknown <- fields[is.na(field_types(fields))]
  if (length(unknown)) {
    cli::cli_warn(c(
      "Unknown field{?s}: {.val {unknown}}.",
      i = "See {.fun ym_logs_fields}. The request is sent as is; the API will reject invalid fields."
    ))
  }
  if (nchar(paste(fields, collapse = ",")) > 3000) {
    cli::cli_abort("The field list must not exceed 3000 characters (API limit).", call = call)
  }
  fields
}

# ClickHouse types of the given fields (NA for unknown ones). Parametrized
# fields match with any attribution, e.g. "ym:s:lastsignTrafficSource" and
# "ym:s:<first>TrafficSource" both match "ym:s:<attribution>TrafficSource".
field_types <- function(fields) {
  tbl <- ym_fields_tbl
  out <- tbl$type[match(fields, tbl$field)]
  miss <- is.na(out)
  if (any(miss)) {
    par <- tbl[tbl$parametrized, ]
    parts <- strsplit(par$field, "<attribution>", fixed = TRUE)
    rx <- vapply(parts, function(p) {
      paste0("^", escape_regex(p[1]), "(<[a-z_]+>|[a-z_]*)", escape_regex(p[2]), "$")
    }, character(1))
    out[miss] <- vapply(fields[miss], function(f) {
      hit <- which(vapply(rx, grepl, logical(1), x = f))
      if (length(hit)) par$type[hit[1]] else NA_character_
    }, character(1))
  }
  out
}

escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
