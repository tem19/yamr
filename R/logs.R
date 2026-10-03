attribution_models <- c(
  "FIRST", "LAST", "LASTSIGN", "LAST_YANDEX_DIRECT_CLICK",
  "CROSS_DEVICE_LAST_SIGNIFICANT", "CROSS_DEVICE_FIRST",
  "CROSS_DEVICE_LAST_YANDEX_DIRECT_CLICK", "CROSS_DEVICE_LAST", "AUTOMATIC"
)

#' Logs API: low-level methods
#'
#' Thin wrappers around the Logs API methods
#' (<https://yandex.com/dev/metrika/en/logs/>). For a one-call export use
#' [ym_logs()] instead.
#'
#' * `ym_logs_evaluate()` checks whether a request for the period fits the
#'   storage quota (10 GB per tag, including prepared logs not yet cleaned).
#' * `ym_logs_create()` creates a log request.
#' * `ym_logs_info()` returns the current state of a request.
#' * `ym_logs_requests()` lists all requests of a tag.
#' * `ym_logs_cancel()` cancels a request that is still being prepared.
#' * `ym_logs_clean()` deletes the prepared files of a processed request,
#'   freeing the quota.
#'
#' Functions taking `request_id` also accept a `ym_log_request` object as
#' `counter` instead, e.g. `ym_logs_info(req)`.
#'
#' @param counter Tag (counter) ID, or a `ym_log_request` object.
#' @param request_id Log request ID.
#' @param date1,date2 First and last day of the period, as `Date` or
#'   `"YYYY-MM-DD"`. At most one year; `date2` can't be today or later.
#' @param fields Fields to export: a character vector, or one comma-separated
#'   string. The `ym:s:` / `ym:pv:` prefix may be omitted. See
#'   [ym_logs_fields()].
#' @param source `"visits"` or `"hits"`. Inferred from the field prefixes when
#'   `NULL`.
#' @param attribution Attribution model for parametrized fields, one of
#'   `FIRST`, `LAST`, `LASTSIGN` (API default), `LAST_YANDEX_DIRECT_CLICK`,
#'   `CROSS_DEVICE_LAST_SIGNIFICANT`, `CROSS_DEVICE_FIRST`,
#'   `CROSS_DEVICE_LAST_YANDEX_DIRECT_CLICK`, `CROSS_DEVICE_LAST`, `AUTOMATIC`
#'   (case-insensitive).
#' @param token OAuth token. By default taken from `YANDEX_METRIKA_TOKEN` or
#'   [ym_auth()].
#' @return
#' * `ym_logs_evaluate()`: a list with `possible` (logical) and
#'   `max_possible_day_quantity` (the longest period, in days, that fits the
#'   remaining quota).
#' * `ym_logs_create()`, `ym_logs_info()`, `ym_logs_cancel()`,
#'   `ym_logs_clean()`: a `ym_log_request` object, a list with `request_id`,
#'   `counter_id`, `source`, `date1`, `date2`, `fields`, `status`, `size`,
#'   `parts` (a data frame of `part_number` and `size`) and `attribution`.
#'   `status` is one of `created`, `processed`, `canceled`, `processing_failed`,
#'   `awaiting_retry`, `cleaned_by_user`, `cleaned_automatically_as_too_old`.
#' * `ym_logs_requests()`: a tibble, one row per request.
#' @name ym_logs_api
#' @examples
#' \dontrun{
#' ym_logs_evaluate(12345, "2026-09-01", "2026-09-30", c("visitID", "date"), "visits")
#' req <- ym_logs_create(12345, "2026-09-01", "2026-09-30", c("visitID", "date"), "visits")
#' req <- ym_logs_wait(req)
#' df <- ym_logs_download(req)
#' ym_logs_clean(req)
#' }
NULL

#' @rdname ym_logs_api
#' @export
ym_logs_evaluate <- function(counter, date1, date2, fields, source = NULL, token = NULL) {
  p <- logs_params(date1, date2, fields, source)
  req <- ym_request(logs_path(counter, "logrequests", "evaluate"), !!!p, token = token)
  ym_perform_json(req)$log_request_evaluation
}

#' @rdname ym_logs_api
#' @export
ym_logs_create <- function(counter, date1, date2, fields, source = NULL,
                           attribution = NULL, token = NULL) {
  p <- logs_params(date1, date2, fields, source)
  if (!is.null(attribution)) {
    attribution <- toupper(attribution)
    p$attribution <- rlang::arg_match(attribution, attribution_models)
  }
  req <- ym_request(logs_path(counter, "logrequests"), !!!p, .method = "POST", token = token)
  as_log_request(ym_perform_json(req)$log_request)
}

#' @rdname ym_logs_api
#' @export
ym_logs_info <- function(counter, request_id = NULL, token = NULL) {
  r <- resolve_request(counter, request_id)
  req <- ym_request(logs_path(r$counter, "logrequest", r$request_id), token = token)
  as_log_request(ym_perform_json(req)$log_request)
}

#' @rdname ym_logs_api
#' @export
ym_logs_requests <- function(counter, token = NULL) {
  req <- ym_request(logs_path(counter, "logrequests"), token = token)
  reqs <- lapply(ym_perform_json(req)$requests, as_log_request)
  tibble::tibble(
    request_id = vapply(reqs, `[[`, numeric(1), "request_id"),
    counter_id = vapply(reqs, `[[`, numeric(1), "counter_id"),
    source = vapply(reqs, `[[`, character(1), "source"),
    date1 = as.Date(vapply(reqs, function(r) format(r$date1), character(1))),
    date2 = as.Date(vapply(reqs, function(r) format(r$date2), character(1))),
    status = vapply(reqs, `[[`, character(1), "status"),
    size = vapply(reqs, `[[`, numeric(1), "size"),
    n_parts = vapply(reqs, function(r) nrow(r$parts), integer(1)),
    attribution = vapply(reqs, function(r) r$attribution %||% NA_character_, character(1)),
    fields = lapply(reqs, `[[`, "fields")
  )
}

#' @rdname ym_logs_api
#' @export
ym_logs_cancel <- function(counter, request_id = NULL, token = NULL) {
  r <- resolve_request(counter, request_id)
  req <- ym_request(logs_path(r$counter, "logrequest", r$request_id, "cancel"),
    .method = "POST", token = token
  )
  as_log_request(ym_perform_json(req)$log_request)
}

#' @rdname ym_logs_api
#' @export
ym_logs_clean <- function(counter, request_id = NULL, token = NULL) {
  r <- resolve_request(counter, request_id)
  req <- ym_request(logs_path(r$counter, "logrequest", r$request_id, "clean"),
    .method = "POST", token = token
  )
  as_log_request(ym_perform_json(req)$log_request)
}

#' Download prepared logs
#'
#' `ym_logs_download_part()` saves one part of a processed log request to a
#' file. `ym_logs_download()` downloads all parts and returns them as one
#' tibble parsed with [ym_read_log()].
#'
#' Parts are streamed to disk, so even large exports don't have to fit in
#' memory as raw text.
#'
#' @inheritParams ym_logs_api
#' @param part Part number (from the `parts` element of the request; numbering
#'   starts at 0).
#' @param path Destination file.
#' @param dir Directory for the downloaded parts. If `NULL`, a temporary
#'   directory that is removed after parsing.
#' @param ... Passed to [ym_read_log()]: `parse_types`, `parse_arrays`,
#'   `clean_names`, `tz`.
#' @return `ym_logs_download_part()`: `path`, invisibly.
#'   `ym_logs_download()`: a tibble.
#' @export
ym_logs_download_part <- function(counter, request_id = NULL, part = 0,
                                  path = tempfile(fileext = ".tsv"), token = NULL) {
  r <- resolve_request(counter, request_id)
  req <- ym_request(
    logs_path(r$counter, "logrequest", r$request_id, "part", part, "download"),
    token = token
  )
  resp <- httr2::req_perform(req, path = path)
  if (!file.exists(path)) {
    # Mocked responses (httr2::local_mocked_responses) keep the body in memory.
    writeBin(httr2::resp_body_raw(resp), path)
  }
  invisible(path)
}

#' @rdname ym_logs_download_part
#' @export
ym_logs_download <- function(counter, request_id = NULL, ..., dir = NULL, token = NULL) {
  r <- resolve_request(counter, request_id)
  info <- if (inherits(counter, "ym_log_request") && identical(counter$status, "processed")) {
    counter
  } else {
    ym_logs_info(r$counter, r$request_id, token = token)
  }
  if (!identical(info$status, "processed")) {
    cli::cli_abort(c(
      "Log request {r$request_id} is not ready for download (status {.val {info$status}}).",
      i = if (info$status %in% c("created", "awaiting_retry")) "Wait for it with {.fun ym_logs_wait}."
    ))
  }
  if (is.null(dir)) {
    dir <- tempfile("yamr-")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  parts <- info$parts$part_number
  dfs <- vector("list", length(parts))
  cli::cli_progress_bar("Downloading log request {r$request_id}", total = length(parts))
  for (i in seq_along(parts)) {
    path <- file.path(dir, sprintf("%s-%s-part%d.tsv", r$counter, r$request_id, parts[i]))
    ym_logs_download_part(r$counter, r$request_id, parts[i], path, token = token)
    dfs[[i]] <- ym_read_log(path, ...)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  bind_rows(dfs)
}

#' Wait for a log request to be processed
#'
#' Polls [ym_logs_info()] until the request is `processed`. Fails if it ends up
#' canceled, failed, or cleaned.
#'
#' @inheritParams ym_logs_api
#' @param interval Seconds between status checks.
#' @param timeout Maximum seconds to wait.
#' @return The processed `ym_log_request`.
#' @export
ym_logs_wait <- function(counter, request_id = NULL, interval = 10, timeout = 6 * 3600,
                         token = NULL) {
  r <- resolve_request(counter, request_id)
  start <- Sys.time()
  cli::cli_progress_message("Waiting for log request {r$request_id} to be prepared...")
  repeat {
    info <- ym_logs_info(r$counter, r$request_id, token = token)
    if (info$status == "processed") {
      cli::cli_progress_done()
      return(info)
    }
    if (!info$status %in% c("created", "awaiting_retry")) {
      cli::cli_abort("Log request {r$request_id} finished with status {.val {info$status}}.")
    }
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    if (elapsed + interval > timeout) {
      cli::cli_abort(c(
        "Log request {r$request_id} was not processed within {timeout} s.",
        i = "It's still being prepared; resume with {.code ym_logs_wait({r$counter}, {r$request_id})}."
      ))
    }
    Sys.sleep(interval)
  }
}

# Helpers ---------------------------------------------------------------------

logs_path <- function(counter, ...) {
  check_id(counter, "counter")
  paste("management/v1/counter", format(counter, scientific = FALSE), ..., sep = "/")
}

check_id <- function(x, arg, call = rlang::caller_env()) {
  ok <- length(x) == 1 && !is.na(x) &&
    ((is.numeric(x) && x == trunc(x) && x > 0) || (is.character(x) && grepl("^[0-9]+$", x)))
  if (!ok) {
    cli::cli_abort("{.arg {arg}} must be a single positive integer ID.", call = call)
  }
}

resolve_request <- function(counter, request_id, call = rlang::caller_env()) {
  if (inherits(counter, "ym_log_request")) {
    return(list(counter = counter$counter_id, request_id = counter$request_id))
  }
  if (is.null(request_id)) {
    cli::cli_abort("{.arg request_id} is required.", call = call)
  }
  check_id(request_id, "request_id", call = call)
  list(counter = counter, request_id = format(request_id, scientific = FALSE))
}

logs_params <- function(date1, date2, fields, source, call = rlang::caller_env()) {
  dates <- check_period(date1, date2, call = call)
  source <- resolve_source(source, fields, call = call)
  fields <- normalize_fields(fields, source, call = call)
  list(
    date1 = format(dates[[1]]),
    date2 = format(dates[[2]]),
    fields = paste(fields, collapse = ","),
    source = source
  )
}

check_period <- function(date1, date2, call = rlang::caller_env()) {
  d1 <- as_date(date1, "date1", call)
  d2 <- as_date(date2, "date2", call)
  if (d2 < d1) {
    cli::cli_abort("{.arg date2} ({d2}) is earlier than {.arg date1} ({d1}).", call = call)
  }
  list(d1, d2)
}

as_date <- function(x, arg, call = rlang::caller_env()) {
  d <- if (inherits(x, "Date")) x else tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) NA)
  if (length(d) != 1 || is.na(d)) {
    cli::cli_abort("{.arg {arg}} must be a single date (Date or {.val YYYY-MM-DD}).", call = call)
  }
  d
}

as_log_request <- function(x) {
  parts <- x$parts %||% list()
  structure(
    list(
      request_id = as.numeric(x$request_id),
      counter_id = as.numeric(x$counter_id),
      source = paste(unlist(x$source), collapse = ","),
      date1 = as.Date(x$date1),
      date2 = as.Date(x$date2),
      fields = unlist(x$fields) %||% character(),
      status = x$status %||% NA_character_,
      size = as.numeric(x$size %||% 0),
      parts = data.frame(
        part_number = vapply(parts, function(p) as.integer(p$part_number), integer(1)),
        size = vapply(parts, function(p) as.numeric(p$size %||% NA), numeric(1))
      ),
      attribution = x$attribution
    ),
    class = "ym_log_request"
  )
}

#' @export
format.ym_log_request <- function(x, ...) {
  c(
    sprintf("<ym_log_request %s>", format(x$request_id, scientific = FALSE)),
    sprintf("  counter: %s", format(x$counter_id, scientific = FALSE)),
    sprintf("  source:  %s", x$source),
    sprintf("  period:  %s .. %s", x$date1, x$date2),
    sprintf("  status:  %s", x$status),
    sprintf("  size:    %s in %d part(s)", format_bytes(x$size), nrow(x$parts)),
    sprintf("  fields:  %s", paste(x$fields, collapse = ", ")),
    if (!is.null(x$attribution)) sprintf("  attribution: %s", x$attribution)
  )
}

#' @export
print.ym_log_request <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

format_bytes <- function(x) {
  format(structure(x, class = "object_size"), units = "auto", standard = "SI")
}

bind_rows <- function(dfs) {
  dfs <- dfs[lengths(dfs) > 0]
  if (!length(dfs)) {
    return(tibble::tibble())
  }
  tibble::as_tibble(do.call(rbind, dfs))
}
