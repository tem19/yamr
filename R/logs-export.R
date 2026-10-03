#' Export raw data from the Logs API
#'
#' Does the whole Logs API workflow in one call: checks the quota, creates a
#' log request (several, if the period doesn't fit the quota or exceeds a
#' year), waits for it to be prepared, downloads and parses all parts, and
#' cleans the prepared files on the server.
#'
#' When the period is too large for the remaining quota, it's split into
#' chunks of `max_possible_day_quantity` days reported by
#' [ym_logs_evaluate()], and the chunks are exported one after another. With
#' `clean = TRUE` each chunk's files are deleted right after download, so the
#' quota is freed for the next one.
#'
#' If an error or interrupt happens mid-way, the request being processed is
#' cancelled or cleaned (when `clean = TRUE`), so it doesn't hold the quota.
#'
#' @inheritParams ym_logs_api
#' @inheritParams ym_read_log
#' @inheritParams ym_logs_wait
#' @param clean Delete the prepared logs on the server after download.
#' @param chunk_days Split the period into chunks of at most this many days
#'   regardless of the quota. Smaller chunks are prepared faster and are
#'   easier to retry.
#' @return A tibble; see [ym_read_log()] for column types.
#' @export
#' @examples
#' \dontrun{
#' visits <- ym_logs(
#'   counter = 12345,
#'   date1 = "2026-09-01", date2 = "2026-09-30",
#'   fields = c("visitID", "clientID", "dateTime", "startURL",
#'              "lastsignTrafficSource", "goalsID"),
#'   source = "visits"
#' )
#'
#' hits <- ym_logs(12345, Sys.Date() - 7, Sys.Date() - 1,
#'                 c("ym:pv:watchID", "ym:pv:dateTime", "ym:pv:URL"))
#' }
ym_logs <- function(counter, date1, date2, fields, source = NULL, attribution = NULL,
                    clean = TRUE, chunk_days = NULL,
                    parse_types = TRUE, parse_arrays = FALSE, clean_names = TRUE, tz = "UTC",
                    interval = 10, timeout = 6 * 3600, token = NULL) {
  dates <- check_period(date1, date2)
  source <- resolve_source(source, fields)
  fields <- normalize_fields(fields, source)
  check_id(counter, "counter")

  # The API accepts at most one year per request.
  max_days <- min(chunk_days %||% 365, 365)
  chunks <- split_period(dates[[1]], dates[[2]], max_days)

  # If a chunk doesn't fit into the quota, split it further.
  ev <- ym_logs_evaluate(counter, chunks$date1[1], chunks$date2[1], fields, source, token = token)
  if (!isTRUE(ev$possible)) {
    days <- ev$max_possible_day_quantity %||% 0
    if (days < 1) {
      cli::cli_abort(c(
        "The Logs API storage quota of tag {counter} is exhausted.",
        i = "Free it by cleaning old requests: see {.fun ym_logs_requests} and {.fun ym_logs_clean}."
      ))
    }
    if (!clean) {
      cli::cli_warn("The period doesn't fit the quota and {.code clean = FALSE}: later chunks may fail.")
    }
    chunks <- split_period(dates[[1]], dates[[2]], min(days, max_days))
  }
  if (nrow(chunks) > 1) {
    cli::cli_inform("Exporting {.val {source}} in {nrow(chunks)} chunks of up to {max(chunks$days)} day{?s}.")
  }

  dfs <- vector("list", nrow(chunks))
  for (i in seq_len(nrow(chunks))) {
    dfs[[i]] <- export_chunk(
      counter, chunks$date1[i], chunks$date2[i], fields, source, attribution,
      clean = clean, interval = interval, timeout = timeout, token = token,
      parse_types = parse_types, parse_arrays = parse_arrays,
      clean_names = clean_names, tz = tz
    )
  }
  bind_rows(dfs)
}

export_chunk <- function(counter, date1, date2, fields, source, attribution,
                         clean, interval, timeout, token, ...) {
  req <- ym_logs_create(counter, date1, date2, fields, source, attribution, token = token)
  cli::cli_inform("Created log request {format(req$request_id, scientific = FALSE)} for {date1} .. {date2}.")
  if (clean) {
    on.exit(discard_request(req, token), add = TRUE)
  }
  req <- ym_logs_wait(req, interval = interval, timeout = timeout, token = token)
  ym_logs_download(req, ..., token = token)
}

# Frees the quota held by a request whatever state it's in. Never fails, so
# it doesn't mask the original error when called from on.exit().
discard_request <- function(req, token) {
  tryCatch(
    {
      status <- ym_logs_info(req, token = token)$status
      if (status == "processed") {
        ym_logs_clean(req, token = token)
      } else if (status %in% c("created", "awaiting_retry")) {
        ym_logs_cancel(req, token = token)
      }
    },
    error = function(e) {
      cli::cli_warn(c(
        "Couldn't clean up log request {format(req$request_id, scientific = FALSE)}.",
        i = "Clean it manually with {.fun ym_logs_clean} to free the quota."
      ), parent = e)
    }
  )
  invisible()
}

split_period <- function(date1, date2, days) {
  starts <- seq(date1, date2, by = days)
  ends <- pmin(starts + days - 1, date2)
  data.frame(date1 = starts, date2 = ends, days = as.integer(ends - starts) + 1L)
}
