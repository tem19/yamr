ym_base_url <- function() {
  getOption("yamr.base_url", "https://api-metrika.yandex.net")
}

# Builds an authorized request to the Metrica API.
#
# Retries on quota (420/429) and transient server errors, and throttles to the
# Logs API limit of 10 requests per second per IP.
ym_request <- function(path, ..., .method = "GET", token = NULL) {
  req <- httr2::request(ym_base_url())
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_url_query(req, ...)
  req <- httr2::req_method(req, .method)
  req <- httr2::req_user_agent(req, paste0("yamr/", utils::packageVersion("yamr"), " (R)"))
  req <- ym_req_auth(req, token)
  req <- httr2::req_throttle(req, capacity = 10, fill_time_s = 1, realm = "yamr")
  req <- httr2::req_retry(
    req,
    max_tries = getOption("yamr.max_tries", 5),
    is_transient = function(resp) httr2::resp_status(resp) %in% c(420, 429, 500, 502, 503, 504)
  )
  httr2::req_error(req, body = ym_error_body)
}

ym_perform_json <- function(req) {
  httr2::resp_body_json(httr2::req_perform(req), check_type = FALSE)
}

# Turns the API error payload into bullet points for the condition message:
# {"errors": [{"error_type": ..., "message": ...}], "code": ..., "message": ...}
ym_error_body <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp, check_type = FALSE), error = function(e) NULL)
  if (!is.list(body)) {
    return(NULL)
  }
  msgs <- vapply(body$errors %||% list(), function(e) {
    paste0(if (!is.null(e$error_type)) paste0("[", e$error_type, "] "), e$message %||% "")
  }, character(1))
  if (!length(msgs) && !is.null(body$message)) {
    msgs <- body$message
  }
  status <- httr2::resp_status(resp)
  hint <- if (status == 401) {
    "Check the token: see {.fun ym_auth} or the {.envvar YANDEX_METRIKA_TOKEN} variable."
  } else if (status == 403) {
    "The token needs the {.field metrika:read} scope and access to this tag."
  }
  c(msgs, if (!is.null(hint)) c(i = cli::format_inline(hint)))
}
