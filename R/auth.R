yandex_oauth_url <- function() getOption("yamr.oauth_url", "https://oauth.yandex.ru")

#' Authorize with Yandex OAuth
#'
#' Obtains an OAuth token for the Metrica API and keeps it for the rest of the
#' session (and, with `cache = TRUE`, between sessions). You only need this if
#' you don't already have a token: a ready token can be passed to any function
#' via `token`, or set once in the `YANDEX_METRIKA_TOKEN` environment variable.
#'
#' You need your own OAuth app, created at <https://oauth.yandex.ru/client/new>
#' with the `metrika:read` access (plus `metrika:write` if you plan to use
#' write methods later). Then:
#'
#' * `flow = "auth_code"` (default) opens the browser and catches the redirect
#'   on a local port. Add `redirect_uri` (by default `http://localhost:1410/`)
#'   to the app's "Redirect URI" list.
#' * `flow = "device"` prints a short code to enter at <https://ya.ru/device>.
#'   Use it on remote machines (RStudio Server, SSH) where the local redirect
#'   can't reach R.
#'
#' Yandex tokens live for about a year and come with a refresh token, which
#' yamr uses automatically once the token expires.
#'
#' Token resolution order used by every API function: the `token` argument,
#' then `YANDEX_METRIKA_TOKEN`, then the token obtained by `ym_auth()`.
#'
#' @param client_id,client_secret Credentials of your OAuth app. Default to the
#'   `YANDEX_CLIENT_ID` and `YANDEX_CLIENT_SECRET` environment variables.
#' @param flow `"auth_code"` or `"device"`, see Details.
#' @param cache Store the token on disk (in [tools::R_user_dir()]) and reuse
#'   it in later sessions.
#' @param redirect_uri Redirect URI for the `"auth_code"` flow; must be
#'   registered in the app.
#' @param force Ignore the cached token and authorize again.
#' @return The access token (a string), invisibly.
#' @export
#' @examples
#' \dontrun{
#' ym_auth("your-client-id", "your-client-secret")
#' ym_auth(flow = "device")
#' }
ym_auth <- function(client_id = Sys.getenv("YANDEX_CLIENT_ID"),
                    client_secret = Sys.getenv("YANDEX_CLIENT_SECRET"),
                    flow = c("auth_code", "device"),
                    cache = TRUE,
                    redirect_uri = "http://localhost:1410/",
                    force = FALSE) {
  flow <- rlang::arg_match(flow)
  if (!rlang::is_string(client_id) || !nzchar(client_id)) {
    cli::cli_abort(c(
      "{.arg client_id} is required.",
      i = "Create an app at {.url https://oauth.yandex.ru/client/new} with {.field metrika:read} access,",
      i = "or set {.envvar YANDEX_CLIENT_ID} and {.envvar YANDEX_CLIENT_SECRET}."
    ))
  }
  if (!rlang::is_string(client_secret) || !nzchar(client_secret)) {
    cli::cli_abort("{.arg client_secret} is required (or set {.envvar YANDEX_CLIENT_SECRET}).")
  }

  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = paste0(yandex_oauth_url(), "/token"),
    auth = "body",
    name = "yamr"
  )
  path <- if (cache) token_cache_path(client_id)

  token <- if (!force && !is.null(path) && file.exists(path)) readRDS(path)
  if (is.null(token)) {
    token <- switch(flow,
      auth_code = httr2::oauth_flow_auth_code(
        client,
        auth_url = paste0(yandex_oauth_url(), "/authorize"),
        redirect_uri = redirect_uri
      ),
      device = yandex_flow_device(client, client_secret)
    )
  }

  the$client <- client
  the$token <- token
  the$cache_path <- path
  token <- current_token() # refreshes an expired cached token and re-saves it
  if (!is.null(path)) save_token(the$token, path)
  cli::cli_alert_success("Authorized with Yandex OAuth.")
  invisible(token)
}

#' Forget the OAuth token
#'
#' Drops the token obtained by [ym_auth()] from the session and, optionally,
#' deletes its on-disk cache. Doesn't touch `YANDEX_METRIKA_TOKEN`.
#'
#' @param clear_cache Also delete cached tokens from disk.
#' @return `NULL`, invisibly.
#' @export
ym_deauth <- function(clear_cache = FALSE) {
  rm(list = intersect(c("client", "token", "cache_path"), ls(the)), envir = the)
  if (clear_cache) {
    unlink(dir(token_cache_dir(), pattern = "^token-.*\\.rds$", full.names = TRUE))
  }
  invisible(NULL)
}

#' Is a Metrica token available?
#'
#' @return `TRUE` if a token is set via `YANDEX_METRIKA_TOKEN` or [ym_auth()].
#' @export
ym_has_token <- function() {
  nzchar(Sys.getenv("YANDEX_METRIKA_TOKEN")) || !is.null(the$token)
}

ym_req_auth <- function(req, token = NULL) {
  token <- ym_token(token)
  # Metrica documents the "OAuth <token>" scheme, not "Bearer".
  httr2::req_headers(req, Authorization = paste("OAuth", token), .redact = "Authorization")
}

ym_token <- function(token = NULL) {
  if (!is.null(token)) {
    if (!rlang::is_string(token) || !nzchar(token)) {
      cli::cli_abort("{.arg token} must be a non-empty string.")
    }
    return(token)
  }
  env <- Sys.getenv("YANDEX_METRIKA_TOKEN")
  if (nzchar(env)) {
    return(env)
  }
  if (!is.null(the$token)) {
    return(current_token())
  }
  cli::cli_abort(c(
    "No Yandex Metrica token found.",
    i = "Pass {.arg token}, set {.envvar YANDEX_METRIKA_TOKEN}, or call {.fun ym_auth}."
  ))
}

# Access token of the ym_auth() session, refreshed when expired.
current_token <- function() {
  token <- the$token
  expired <- !is.null(token$expires_at) && token$expires_at < unclass(Sys.time()) + 60
  if (expired) {
    if (is.null(token$refresh_token) || is.null(the$client)) {
      cli::cli_abort("The OAuth token has expired; run {.fun ym_auth} again.")
    }
    refreshed <- httr2::oauth_flow_refresh(the$client, token$refresh_token)
    refreshed$refresh_token <- refreshed$refresh_token %||% token$refresh_token
    the$token <- token <- refreshed
    if (!is.null(the$cache_path)) save_token(token, the$cache_path)
  }
  token$access_token
}

token_cache_dir <- function() {
  getOption("yamr.cache_dir", tools::R_user_dir("yamr", "cache"))
}

token_cache_path <- function(client_id) {
  file.path(token_cache_dir(), paste0("token-", client_id, ".rds"))
}

save_token <- function(token, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(token, path)
  Sys.chmod(path, "600")
}

# Yandex's device flow differs from RFC 8628 in field and parameter names
# (`verification_url`, `code`), so httr2::oauth_flow_device() can't be used.
yandex_flow_device <- function(client, client_secret) {
  dev <- httr2::request(paste0(yandex_oauth_url(), "/device/code"))
  dev <- httr2::req_body_form(dev, client_id = client$id)
  dev <- ym_perform_json(dev)

  cli::cli_alert_info("Open {.url {dev$verification_url}} and enter the code {.strong {dev$user_code}}")
  if (interactive()) utils::browseURL(dev$verification_url)

  interval <- dev$interval %||% 5
  deadline <- Sys.time() + (dev$expires_in %||% 300)
  cli::cli_progress_message("Waiting for confirmation...")
  while (Sys.time() < deadline) {
    Sys.sleep(interval)
    req <- httr2::request(client$token_url)
    req <- httr2::req_body_form(req,
      grant_type = "device_code", code = dev$device_code,
      client_id = client$id, client_secret = client_secret
    )
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp, check_type = FALSE)
    if (httr2::resp_status(resp) == 200) {
      return(httr2::oauth_token(
        access_token = body$access_token,
        token_type = body$token_type %||% "bearer",
        expires_in = body$expires_in,
        refresh_token = body$refresh_token
      ))
    }
    if (identical(body$error, "authorization_pending")) next
    if (identical(body$error, "slow_down")) {
      interval <- interval + 5
      next
    }
    cli::cli_abort("Device authorization failed: {body$error_description %||% body$error}")
  }
  cli::cli_abort("Device code expired before it was confirmed. Run {.fun ym_auth} again.")
}
