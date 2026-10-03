write_tsv_lines <- function(lines) {
  path <- withr::local_tempfile(fileext = ".tsv", .local_envir = parent.frame())
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("columns are typed by field", {
  path <- write_tsv_lines(c(
    "ym:s:visitID\tym:s:clientID\tym:s:date\tym:s:dateTime\tym:s:pageViews\tym:s:visitDuration\tym:s:isNewUser\tym:s:startURL\tym:s:goalsID",
    "9223372036854775807\t1693493472123456789\t2026-09-01\t2026-09-01 12:34:56\t3\t4000000000\t1\thttps://a.ru/?q=1\t[1,2]",
    "1\t2\t2026-09-02\t2026-09-02 00:00:00\t\t0\t0\t\t[]"
  ))
  df <- ym_read_log(path)
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("visitID", "clientID", "date", "dateTime", "pageViews",
                     "visitDuration", "isNewUser", "startURL", "goalsID"))
  expect_identical(df$visitID, c("9223372036854775807", "1")) # UInt64 kept exact
  expect_identical(df$clientID[1], "1693493472123456789")
  expect_equal(df$date, as.Date(c("2026-09-01", "2026-09-02")))
  expect_equal(df$dateTime[1], as.POSIXct("2026-09-01 12:34:56", tz = "UTC"))
  expect_identical(df$pageViews, c(3L, NA))
  expect_identical(df$visitDuration, c(4e9, 0)) # UInt32 exceeds integer range
  expect_identical(df$isNewUser, c(1L, 0L))
  expect_identical(df$startURL, c("https://a.ru/?q=1", ""))
  expect_identical(df$goalsID, c("[1,2]", "[]"))
})

test_that("escapes are decoded and quotes are literal", {
  path <- write_tsv_lines(c(
    "ym:pv:title\tym:pv:URL",
    "a\\tb\\nc \\\\ d\t\"quoted\" 'single'"
  ))
  df <- ym_read_log(path)
  expect_identical(df$title, "a\tb\nc \\ d")
  expect_identical(df$URL, "\"quoted\" 'single'")
})

test_that("arrays are parsed on request", {
  path <- write_tsv_lines(c(
    "ym:s:goalsID\tym:s:parsedParamsKey1\tym:s:goalsDateTime",
    "[10,20]\t['a','it\\'s, ok']\t['2026-09-01 10:00:00']",
    "[]\t[]\t[]"
  ))
  df <- ym_read_log(path, parse_arrays = TRUE)
  expect_identical(df$goalsID, list(c(10, 20), numeric()))
  expect_identical(df$parsedParamsKey1, list(c("a", "it's, ok"), character()))
  expect_equal(df$goalsDateTime[[1]], as.POSIXct("2026-09-01 10:00:00", tz = "UTC"))
})

test_that("raw mode keeps character and full names", {
  path <- write_tsv_lines(c("ym:s:visitID\tym:s:pageViews", "1\t2"))
  df <- ym_read_log(path, parse_types = FALSE, clean_names = FALSE)
  expect_named(df, c("ym:s:visitID", "ym:s:pageViews"))
  expect_identical(df[["ym:s:pageViews"]], "2")
})

test_that("ym_parse_array handles numbers, strings and empties", {
  expect_identical(
    ym_parse_array(c("[1,2]", "[]", "['x','y\\\\z']")),
    list(c("1", "2"), character(), c("x", "y\\z"))
  )
})
