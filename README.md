# yamr

R-клиент для API [Яндекс Метрики](https://yandex.ru/dev/metrika/). Сейчас покрыт
[Logs API](https://yandex.ru/dev/metrika/ru/logs/), через который выгружаются сырые,
неагрегированные данные о визитах (`visits`) и хитах (`hits`).

## Установка

```r
# install.packages("pak")
pak::pak("tem19/yamr")
```

## Авторизация

Проще всего положить готовый OAuth-токен в `~/.Renviron`:

```
YANDEX_METRIKA_TOKEN=y0_AgAAAA...
```

Токен также можно передать любой функции аргументом `token`.

Если токена нет, его можно получить прямо из R. Для этого
[создайте приложение](https://oauth.yandex.ru/client/new) с доступом `metrika:read`.
Затем выберите способ входа в зависимости от того, какой Redirect URI (Callback URL)
указан у приложения:

```r
# Redirect URI https://oauth.yandex.ru/verification_code (ставится автоматически
# для приложений «Для доступа к API или отладки»): откроется браузер, Яндекс
# покажет код, вставьте его в консоль. Работает и на удалённых машинах.
ym_auth(client_id = "...", client_secret = "...")

# Redirect URI http://localhost:1410/ (должен совпадать буква в букву):
# код подхватывается сам, вставлять ничего не нужно.
ym_auth(client_id = "...", client_secret = "...", flow = "localhost")

# Ввод короткого кода на ya.ru/device.
ym_auth(client_id = "...", client_secret = "...", flow = "device")
```

Токен кешируется на диске (`tools::R_user_dir("yamr", "cache")`) и сам обновляется
refresh-токеном, когда истекает. `client_id` и `client_secret` можно задать через
`YANDEX_CLIENT_ID` и `YANDEX_CLIENT_SECRET`.

## Выгрузка одним вызовом

```r
library(yamr)

visits <- ym_logs(
  counter = 12345,
  date1 = "2026-09-01", date2 = "2026-09-30",
  fields = c("visitID", "clientID", "dateTime", "startURL",
             "lastsignTrafficSource", "goalsID"),
  source = "visits"
)
```

`ym_logs()` проходит весь цикл:

1. проверяет через `evaluate`, хватает ли квоты (10 ГБ на счётчик);
2. если период не влезает в квоту или длиннее года, разбивает его на куски;
3. создаёт запрос, ждёт подготовки и скачивает все части;
4. парсит TSV с типами колонок и склеивает результат в один tibble;
5. очищает подготовленные логи на сервере, в том числе при ошибке или прерывании.

Префиксы `ym:s:` / `ym:pv:` в `fields` можно не писать. Если они указаны,
`source` определится сам. В результате префиксы убираются из имён колонок
(отключается через `clean_names = FALSE`).

### Типы колонок

| ClickHouse | R |
|---|---|
| `String` | character (escape-последовательности `\t`, `\n`, `\\` раскодируются) |
| `Int8`–`Int32`, `UInt8`, `UInt16` | integer |
| `UInt32`, `Int64`, `Float*` | double |
| `UInt64` (`visitID`, `clientID`, `watchID`…) | character, потому что в double они теряют точность |
| `Date` / `DateTime` | Date / POSIXct |
| `Array(...)` | строка вида `[1,2]`; с `parse_arrays = TRUE` — list-колонка |

## Справочник полей

```r
ym_logs_fields("visits")             # 179 полей: тип и описание
ym_logs_fields("hits", lang = "ru")  # 131 поле, описания на русском
```

Справочник собран из официальной документации скриптом `data-raw/fields.R`.

## Низкоуровневые методы

Каждому методу Logs API соответствует своя функция:

| Функция | Метод API |
|---|---|
| `ym_logs_evaluate()` | `GET logrequests/evaluate` |
| `ym_logs_create()` | `POST logrequests` |
| `ym_logs_requests()` | `GET logrequests` |
| `ym_logs_info()` | `GET logrequest/{id}` |
| `ym_logs_wait()` | опрашивает `info`, пока статус не станет `processed` |
| `ym_logs_download_part()` / `ym_logs_download()` | `GET logrequest/{id}/part/{n}/download` |
| `ym_logs_cancel()` | `POST logrequest/{id}/cancel` |
| `ym_logs_clean()` | `POST logrequest/{id}/clean` |
| `ym_read_log()` | парсит скачанный файл |

```r
req <- ym_logs_create(12345, "2026-09-01", "2026-09-30", c("watchID", "URL"), "hits")
req <- ym_logs_wait(req)
hits <- ym_logs_download(req)
ym_logs_clean(req)

# какие запросы занимают квоту
ym_logs_requests(12345)
```

Запросы к API автоматически ограничиваются 10 в секунду (лимит Logs API).
Ответы 420/429/5xx повторяются с backoff.
