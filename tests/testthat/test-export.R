# Fake Logs API backend with in-memory request state.
fake_logs_backend <- function(max_days = 365, fail_download = FALSE) {
  state <- new.env()
  state$next_id <- 1
  state$requests <- list()
  calls <- fake_api(list(
    "GET logrequests/evaluate" = function(req, url) {
      days <- as.integer(as.Date(url$query$date2) - as.Date(url$query$date1)) + 1
      json_resp(list(log_request_evaluation = list(possible = days <= max_days, max_possible_day_quantity = max_days)))
    },
    "POST logrequests$" = function(req, url) {
      id <- state$next_id
      state$next_id <- id + 1
      r <- log_request_json(id, "created", 0, url$query$date1, url$query$date2)
      state$requests[[as.character(id)]] <- r
      json_resp(list(log_request = r))
    },
    "GET logrequest/[0-9]+$" = function(req, url) {
      id <- basename(url$path)
      r <- state$requests[[id]]
      if (r$status == "created") {
        r <- log_request_json(as.numeric(id), "processed", 1, r$date1, r$date2)
        state$requests[[id]] <- r
      }
      json_resp(list(log_request = r))
    },
    "download$" = function(req, url) {
      if (fail_download) {
        return(json_resp(list(errors = list(list(error_type = "backend_error", message = "boom"))), status = 400))
      }
      id <- strsplit(url$path, "/")[[1]][7]
      r <- state$requests[[id]]
      tsv_resp(c("ym:s:visitID\tym:s:date", paste0(id, "\t", r$date1)))
    },
    "POST logrequest/[0-9]+/(clean|cancel)" = function(req, url) {
      id <- strsplit(url$path, "/")[[1]][7]
      r <- state$requests[[id]]
      r$status <- if (endsWith(url$path, "clean")) "cleaned_by_user" else "canceled"
      state$requests[[id]] <- r
      json_resp(list(log_request = r))
    }
  ), env = parent.frame())
  list(state = state, calls = calls)
}

test_that("ym_logs runs the full cycle and cleans up", {
  be <- fake_logs_backend()
  df <- ym_logs(42, "2026-09-01", "2026-09-10", c("visitID", "date"), "visits", interval = 0)
  expect_equal(df$visitID, "1")
  expect_equal(df$date, as.Date("2026-09-01"))
  expect_equal(be$state$requests[["1"]]$status, "cleaned_by_user")
})

test_that("ym_logs splits the period when it doesn't fit the quota", {
  be <- fake_logs_backend(max_days = 4)
  expect_message(
    df <- ym_logs(42, "2026-09-01", "2026-09-10", "ym:s:visitID", interval = 0),
    "3 chunks"
  )
  expect_equal(df$visitID, c("1", "2", "3"))
  expect_equal(df$date, as.Date(c("2026-09-01", "2026-09-05", "2026-09-09")))
  expect_true(all(vapply(be$state$requests, `[[`, "", "status") == "cleaned_by_user"))
})

test_that("ym_logs stops when the quota is exhausted", {
  fake_logs_backend(max_days = 0)
  expect_error(ym_logs(42, "2026-09-01", "2026-09-10", "ym:s:visitID"), "quota")
})

test_that("ym_logs cleans the request when download fails", {
  be <- fake_logs_backend(fail_download = TRUE)
  expect_error(ym_logs(42, "2026-09-01", "2026-09-02", "ym:s:visitID", interval = 0), "boom")
  expect_equal(be$state$requests[["1"]]$status, "cleaned_by_user")
})

test_that("clean = FALSE leaves the request on the server", {
  be <- fake_logs_backend()
  ym_logs(42, "2026-09-01", "2026-09-02", "ym:s:visitID", clean = FALSE, interval = 0)
  expect_equal(be$state$requests[["1"]]$status, "processed")
})
