test_that("field reference covers both sources", {
  f <- ym_logs_fields()
  expect_setequal(unique(f$source), c("visits", "hits"))
  expect_true(all(c("ym:s:visitID", "ym:pv:watchID") %in% f$field))
  expect_false(anyNA(f$type))
  expect_true(any(f$parametrized))
  expect_true(all(startsWith(ym_logs_fields("hits")$field, "ym:pv:")))
  expect_false(identical(ym_logs_fields(lang = "ru")$description, f$description))
})

test_that("bare field names get the source prefix", {
  expect_equal(
    normalize_fields(c("visitID", "ym:s:date", "visitID"), "visits"),
    c("ym:s:visitID", "ym:s:date")
  )
  expect_equal(normalize_fields("watchID, URL", "hits"), c("ym:pv:watchID", "ym:pv:URL"))
})

test_that("fields from the wrong source are rejected", {
  expect_error(normalize_fields(c("ym:pv:URL"), "visits"), "must start with")
})

test_that("unknown fields warn but pass through", {
  expect_warning(out <- normalize_fields("noSuchField", "visits"), "Unknown field")
  expect_equal(out, "ym:s:noSuchField")
})

test_that("field list length is limited", {
  long <- paste0("ym:s:f", 1:400)
  expect_error(suppressWarnings(normalize_fields(long, "visits")), "3000 characters")
})

test_that("source is inferred from prefixes", {
  expect_equal(resolve_source(NULL, c("ym:s:visitID", "date")), "visits")
  expect_equal(resolve_source(NULL, "ym:pv:URL"), "hits")
  expect_error(resolve_source(NULL, "visitID"), "infer")
  expect_error(resolve_source(NULL, c("ym:s:a", "ym:pv:b")), "infer")
  expect_equal(resolve_source("hits", "URL"), "hits")
})

test_that("parametrized fields resolve to a type", {
  expect_equal(
    field_types(c("ym:s:lastsignTrafficSource", "ym:s:<first>TrafficSource",
                  "ym:s:<attribution>TrafficSource", "ym:s:visitID", "ym:s:nope")),
    c("String", "String", "String", "UInt64", NA)
  )
})
