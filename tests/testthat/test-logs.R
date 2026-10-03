test_that("evaluate sends the documented parameters", {
  calls <- fake_api(list(
    "GET logrequests/evaluate" = function(req, url) {
      json_resp(list(log_request_evaluation = list(possible = TRUE, max_possible_day_quantity = 300)))
    }
  ))
  ev <- ym_logs_evaluate(42, as.Date("2026-09-01"), "2026-09-30", c("visitID", "ym:s:date"))
  expect_true(ev$possible)
  q <- calls$log[[1]]$query
  expect_equal(q$date1, "2026-09-01")
  expect_equal(q$date2, "2026-09-30")
  expect_equal(q$fields, "ym:s:visitID,ym:s:date")
  expect_equal(q$source, "visits")
  expect_equal(httr2::req_get_headers(calls$log[[1]]$req, "reveal")$Authorization, "OAuth test-token")
})

test_that("create posts and returns a ym_log_request", {
  calls <- fake_api(list(
    "POST logrequests$" = function(req, url) json_resp(list(log_request = log_request_json(7, "created", 0)))
  ))
  req <- ym_logs_create(42, "2026-09-01", "2026-09-02", "ym:s:visitID", attribution = "first")
  expect_s3_class(req, "ym_log_request")
  expect_equal(req$request_id, 7)
  expect_equal(req$status, "created")
  expect_equal(nrow(req$parts), 0)
  expect_equal(calls$log[[1]]$query$attribution, "FIRST")
  expect_output(print(req), "ym_log_request 7")
})

test_that("bad arguments fail before any request", {
  fake_api(list())
  expect_error(ym_logs_create(42, "2026-09-02", "2026-09-01", "visitID", "visits"), "earlier")
  expect_error(ym_logs_create(42, "01.09.2026", "2026-09-01", "visitID", "visits"), "date1")
  expect_error(ym_logs_create("abc", "2026-09-01", "2026-09-01", "visitID", "visits"), "counter")
  expect_error(ym_logs_create(42, "2026-09-01", "2026-09-01", "visitID", "visits", attribution = "x"), "attribution")
})

test_that("API errors are reported with their messages", {
  fake_api(list(
    "GET logrequest/" = function(req, url) {
      json_resp(list(
        errors = list(list(error_type = "invalid_parameter", message = "Wrong field ym:s:foo")),
        code = 400, message = "Wrong field ym:s:foo"
      ), status = 400)
    }
  ))
  expect_error(ym_logs_info(42, 1), "invalid_parameter.*Wrong field")
})

test_that("requests are listed as a tibble", {
  fake_api(list(
    "GET logrequests$" = function(req, url) {
      json_resp(list(requests = list(log_request_json(1), log_request_json(2, "created", 0, source = "hits"))))
    }
  ))
  reqs <- ym_logs_requests(42)
  expect_equal(reqs$request_id, c(1, 2))
  expect_equal(reqs$status, c("processed", "created"))
  expect_equal(reqs$n_parts, c(1L, 0L))
  expect_equal(reqs$date1, as.Date(c("2026-09-01", "2026-09-01")))
})

test_that("clean and cancel accept a request object", {
  calls <- fake_api(list(
    "POST logrequest/5/clean" = function(req, url) json_resp(list(log_request = log_request_json(5, "cleaned_by_user"))),
    "POST logrequest/5/cancel" = function(req, url) json_resp(list(log_request = log_request_json(5, "canceled")))
  ))
  req <- as_log_request(log_request_json(5))
  expect_equal(ym_logs_clean(req)$status, "cleaned_by_user")
  expect_equal(ym_logs_cancel(42, 5)$status, "canceled")
})

test_that("wait polls until processed and fails on terminal statuses", {
  statuses <- c("created", "awaiting_retry", "processed")
  i <- 0
  fake_api(list(
    "GET logrequest/1$" = function(req, url) {
      i <<- i + 1
      json_resp(list(log_request = log_request_json(1, statuses[i])))
    },
    "GET logrequest/2$" = function(req, url) json_resp(list(log_request = log_request_json(2, "processing_failed")))
  ))
  expect_equal(ym_logs_wait(42, 1, interval = 0)$status, "processed")
  expect_equal(i, 3)
  expect_error(ym_logs_wait(42, 2, interval = 0), "processing_failed")
})

test_that("download fetches and binds all parts", {
  fake_api(list(
    "GET logrequest/1$" = function(req, url) json_resp(list(log_request = log_request_json(1, parts = 2))),
    "GET logrequest/1/part/0/download" = function(req, url) tsv_resp(c("ym:s:visitID\tym:s:date", "1\t2026-09-01")),
    "GET logrequest/1/part/1/download" = function(req, url) tsv_resp(c("ym:s:visitID\tym:s:date", "2\t2026-09-02"))
  ))
  df <- ym_logs_download(42, 1)
  expect_equal(df$visitID, c("1", "2"))
  expect_equal(df$date, as.Date(c("2026-09-01", "2026-09-02")))
})

test_that("download refuses unprocessed requests", {
  fake_api(list("GET logrequest/1$" = function(req, url) json_resp(list(log_request = log_request_json(1, "created", 0)))))
  expect_error(ym_logs_download(42, 1), "not ready")
})

test_that("split_period makes contiguous chunks", {
  ch <- split_period(as.Date("2026-01-01"), as.Date("2026-01-10"), 4)
  expect_equal(ch$date1, as.Date(c("2026-01-01", "2026-01-05", "2026-01-09")))
  expect_equal(ch$date2, as.Date(c("2026-01-04", "2026-01-08", "2026-01-10")))
  expect_equal(nrow(split_period(as.Date("2026-01-01"), as.Date("2026-01-01"), 365)), 1)
})
