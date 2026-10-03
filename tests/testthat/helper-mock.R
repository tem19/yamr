# A fake Metrica API: routes requests by method + path to handlers and records
# every call so tests can assert on what was sent.
fake_api <- function(routes, env = parent.frame()) {
  calls <- new.env()
  calls$log <- list()
  mock <- function(req) {
    url <- httr2::url_parse(req$url)
    method <- req$method %||% "GET"
    key <- paste(method, sub("^/management/v1/counter/[0-9]+/", "", url$path))
    calls$log[[length(calls$log) + 1]] <- list(key = key, query = url$query, req = req)
    for (pattern in names(routes)) {
      if (grepl(pattern, key)) {
        return(routes[[pattern]](req, url))
      }
    }
    stop("Unexpected request: ", key)
  }
  httr2::local_mocked_responses(mock, env = env)
  withr::local_envvar(YANDEX_METRIKA_TOKEN = "test-token", .local_envir = env)
  calls
}

json_resp <- function(x, status = 200) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  )
}

tsv_resp <- function(lines) {
  httr2::response(
    status_code = 200,
    headers = list(`Content-Type` = "text/tab-separated-values"),
    body = charToRaw(paste0(paste(lines, collapse = "\n"), "\n"))
  )
}

log_request_json <- function(id = 1, status = "processed", parts = 1, date1 = "2026-09-01",
                             date2 = "2026-09-02", source = "visits") {
  list(
    request_id = id, counter_id = 42, source = source,
    date1 = date1, date2 = date2, fields = list("ym:s:visitID", "ym:s:date"),
    status = status, size = 1000,
    parts = lapply(seq_len(parts) - 1, function(i) list(part_number = i, size = 500))
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
