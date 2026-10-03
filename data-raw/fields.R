# Builds the internal table of Logs API fields (R/sysdata.rda) from the
# official documentation pages:
#   https://yandex.com/dev/metrika/en/logs/fields/visits.md
#   https://yandex.com/dev/metrika/en/logs/fields/hits.md
# (and their /ru/ counterparts). Re-run after refreshing data-raw/docs/:
#
#   mkdir -p data-raw/docs
#   for s in visits hits; do
#     curl -sL https://yandex.com/dev/metrika/en/logs/fields/$s.md -o data-raw/docs/logs_fields_$s.md
#     curl -sL https://yandex.com/dev/metrika/ru/logs/fields/$s.md -o data-raw/docs/logs_fields_${s}_ru.md
#   done
#   Rscript data-raw/fields.R

parse_fields_md <- function(path) {
  txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  # Diplodoc tables: rows are delimited by "||" ... "||", cells by " | ".
  # A description may span several lines, so split on row starts instead of lines.
  rows <- strsplit(txt, "\n\\|\\| ")[[1]][-1]
  rows <- rows[grepl("^ym:", rows)]
  rows <- sub("\\s*\\|\\|.*$", "", rows) # drop row terminator and anything after the table
  cells <- strsplit(rows, "\\s*\\|\\s*")
  field <- trimws(vapply(cells, `[`, "", 1))
  type <- gsub("[[:space:] ]", "", vapply(cells, `[`, "", 2))
  type <- sub("^string$", "String", type) # one row in the docs is lowercase
  desc <- vapply(cells, function(x) paste(x[-(1:2)], collapse = " | "), "")
  desc <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", desc) # markdown links -> text
  desc <- trimws(gsub("\\s+", " ", desc))
  # "ym:s:<[attribution](...)>TrafficSource" -> "ym:s:<attribution>TrafficSource"
  field <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", field)
  data.frame(field = field, type = type, description = desc, stringsAsFactors = FALSE)
}

build <- function(source) {
  en <- parse_fields_md(file.path("data-raw/docs", paste0("logs_fields_", source, ".md")))
  ru <- parse_fields_md(file.path("data-raw/docs", paste0("logs_fields_", source, "_ru.md")))
  en <- en[!duplicated(en$field), ]
  ru <- ru[!duplicated(ru$field), ]
  out <- data.frame(
    source = source,
    field = en$field,
    type = en$type,
    description_en = en$description,
    description_ru = ru$description[match(en$field, ru$field)],
    stringsAsFactors = FALSE
  )
  out$parametrized <- grepl("<attribution>", out$field, fixed = TRUE)
  out
}

ym_fields_tbl <- rbind(build("visits"), build("hits"))
stopifnot(
  !anyNA(ym_fields_tbl$type), all(nzchar(ym_fields_tbl$type)),
  all(grepl("^ym:(s|pv):", ym_fields_tbl$field))
)
message(nrow(ym_fields_tbl), " fields; types: ",
        paste(sort(unique(ym_fields_tbl$type)), collapse = ", "))
save(ym_fields_tbl, file = "R/sysdata.rda", compress = "xz", version = 2)
