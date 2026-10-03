test_that("token resolution order: argument, env var, session", {
  withr::local_envvar(YANDEX_METRIKA_TOKEN = "from-env")
  expect_equal(ym_token("explicit"), "explicit")
  expect_equal(ym_token(), "from-env")

  withr::local_envvar(YANDEX_METRIKA_TOKEN = "")
  withr::defer(ym_deauth())
  ym_deauth()
  expect_false(ym_has_token())
  expect_error(ym_token(), "No Yandex Metrica token")

  assign("token", httr2::oauth_token("from-session", expires_in = 3600), envir = yamr:::the)
  expect_equal(ym_token(), "from-session")
  expect_true(ym_has_token())
})

test_that("expired session token is refreshed and cached", {
  withr::local_envvar(YANDEX_METRIKA_TOKEN = "")
  withr::defer(ym_deauth())
  cache <- withr::local_tempfile(fileext = ".rds")
  env <- yamr:::the
  env$client <- httr2::oauth_client("id", "https://oauth.example/token", secret = "s")
  env$token <- httr2::oauth_token("old", expires_in = -10, refresh_token = "rt")
  env$cache_path <- cache
  httr2::local_mocked_responses(function(req) {
    json_resp(list(access_token = "new", token_type = "bearer", expires_in = 3600))
  })
  expect_equal(ym_token(), "new")
  expect_equal(readRDS(cache)$refresh_token, "rt") # kept when not rotated
})

test_that("ym_auth requires app credentials", {
  expect_error(ym_auth(client_id = ""), "client_id")
  expect_error(ym_auth(client_id = "x", client_secret = ""), "client_secret")
})

test_that("ym_auth reuses a cached token without a browser flow", {
  withr::local_envvar(YANDEX_METRIKA_TOKEN = "")
  withr::local_options(yamr.cache_dir = withr::local_tempdir())
  withr::defer(ym_deauth())
  save_token(httr2::oauth_token("cached", expires_in = 3600), token_cache_path("app"))
  expect_message(tok <- ym_auth("app", "secret"), "Authorized")
  expect_equal(tok, "cached")
})

test_that("device flow follows Yandex's protocol", {
  polls <- 0
  httr2::local_mocked_responses(function(req) {
    if (grepl("device/code", req$url)) {
      return(json_resp(list(device_code = "dc", user_code = "ABCD",
                            verification_url = "https://ya.ru/device", interval = 0, expires_in = 60)))
    }
    polls <<- polls + 1
    expect_equal(req$body$data$grant_type, "device_code", ignore_attr = TRUE)
    expect_equal(req$body$data$code, "dc", ignore_attr = TRUE)
    if (polls == 1) {
      return(json_resp(list(error = "authorization_pending"), status = 400))
    }
    json_resp(list(access_token = "dev-token", token_type = "bearer", expires_in = 3600, refresh_token = "r"))
  })
  client <- httr2::oauth_client("id", "https://oauth.yandex.ru/token", secret = "s")
  expect_message(tok <- yandex_flow_device(client, "s"), "ABCD")
  expect_equal(tok$access_token, "dev-token")
  expect_equal(polls, 2)
})
