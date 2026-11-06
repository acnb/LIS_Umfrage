# Wissenschaftliche Auswertung der LIS-Umfrage in Deutschland und Österreich.
# Start aus dem Projektordner über einen Laufwerksbuchstaben: Rscript auswertung.R

library(tidyverse)
library(gt)

# Einstellungen ----------------------------------------------------------------

# Grundgesamtheit: mindestens so viele Angaben, ohne Land und Labortyp.
MIN_ANSWERS <- 5

# Produkte mit weniger Laboren gehen in "Other" auf, damit einzelne Labore nicht
# erkennbar werden.
MIN_PRODUCT_N <- 5

# Daten einlesen ---------------------------------------------------------------

N_ITEMS <- 75

IMPORTANCE_VALUES <- c("1 (unwichtig)", "2", "3", "4", "5 (sehr wichtig)")

# "Nicht vorhanden" und "Nicht genutzt" gehören nicht zur Skala. Wie sie zu
# behandeln sind, entscheidet die jeweilige Auswertung.
IMPLEMENTATION_LEVELS <- c(
  "Vorbildlich umgesetzt",
  "Gut umgesetzt",
  "Zufriedenstellend umgesetzt",
  "Ausreichend umgesetzt",
  "Unzureichend umgesetzt",
  "Nicht vorhanden",
  "Nicht genutzt"
)

# Blöcke, die als inhaltliche Angabe zählen.
ANSWER_BLOCKS <- c("laboratory", "importance", "implementation", "comment")

# Alle Spalten außerhalb der Item-Blöcke, gefunden über den Fragetext.
SINGLE_COLUMNS <- tribble(
  ~name,               ~block,       ~pattern,
  "response_id",       "metadata",   "^Antwort ID$",
  "submit_date",       "metadata",   "^Datum Abgeschickt$",
  "last_page",         "metadata",   "^Letzte Seite$",
  "start_language",    "metadata",   "^Start-Sprache$",
  "random_seed",       "metadata",   "^Zufallsstartwert$",
  "lab_hospital",      "lab_type",   "\\[Krankenhauslabor\\]$",
  "lab_university",    "lab_type",   "\\[universitäres Labor\\]$",
  "lab_private",       "lab_type",   "\\[niedergelassenes Labor\\]$",
  "lab_office",        "lab_type",   "\\[Labor einer Arztpraxis\\]$",
  "lab_chain",         "lab_type",   "\\[Teil eines Verbundes / Kette\\]$",
  "country",           "country",    "^In welchem Land befindet sich Ihr Labor:$",
  "country_other",     "country",    "^In welchem Land .*\\[Sonstiges\\]$",
  "lis",               "laboratory", "^Welches Laborinformationssystem .*\\)$",
  "lis_other",         "laboratory", "^Welches Laborinformationssystem .*\\[Sonstiges\\]$",
  "order_entry",       "laboratory", "^Welches Order-Entry System .*\\)$",
  "order_entry_other", "laboratory", "^Welches Order-Entry System .*\\[Sonstiges\\]$",
  "year",              "laboratory", "^In welchem Jahr wurde",
  "hardware",          "laboratory", "auf welcher Hardware",
  "modules",           "laboratory", "mit welchen Modulen",
  "it_fte",            "laboratory", "^Wie viele Vollzeitäquivalente",
  "samples_day",       "laboratory", "^Wie viele Proben",
  "final_comment",     "comment",    "^Gibt es noch etwas"
)

assert <- function(condition, message) {
  if (!condition) stop(message, call. = FALSE)
}

find_column <- function(headers, pattern) {
  position <- str_which(headers, pattern)
  assert(length(position) == 1, str_glue("Nicht genau eine Spalte für: {pattern}"))
  position
}

# Ordnet jeder Spalte des Exports einen kurzen Namen und ihre Bedeutung zu. Die
# Nummer eines Items verbindet Wichtigkeit, Umsetzung und Kommentar. Sie folgt dem
# Alphabet des Itemtexts, damit sie nicht von der Spaltenreihenfolge abhängt; die
# Reihenfolge im Fragebogen steht in der Codebuch-Spalte position.
build_codebook <- function(headers) {
  all_columns <- tibble(position = seq_along(headers), question = headers)

  single <- SINGLE_COLUMNS |>
    mutate(position = map_int(pattern, \(x) find_column(headers, x))) |>
    select(position, name, block) |>
    left_join(all_columns, by = "position")

  importance <- all_columns |>
    filter(str_detect(question, "^Wie wichtig sind folgende Aspekte")) |>
    mutate(
      block = "importance",
      item_group = str_match(question, "für Ihr Labor: (.*?) \\[")[, 2],
      item = str_match(question, " \\[(.*)\\]$")[, 2],
      item_number = str_rank(item, locale = "de")
    )

  item_columns <- all_columns |>
    filter(str_detect(question, "Wie gut ist dieser Punkt")) |>
    mutate(
      block = if_else(str_ends(question, fixed("[Kommentar]")), "comment", "implementation"),
      item = str_remove(question, "Wie gut ist dieser Punkt.*$")
    ) |>
    left_join(select(importance, item, item_number, item_group), by = "item")

  calculated <- all_columns |>
    filter(str_starts(question, fixed("{"))) |>
    mutate(block = "calculated", name = str_c("calc_", str_rank(question)))

  items_per_block <- bind_rows(importance, item_columns) |>
    summarise(items = n_distinct(item_number, na.rm = TRUE), columns = n(), .by = block)
  assert(
    nrow(items_per_block) == 3 &&
      all(items_per_block$items == N_ITEMS) &&
      all(items_per_block$columns == N_ITEMS),
    "Die Item-Blöcke passen nicht zueinander."
  )

  codebook <- bind_rows(
    single,
    calculated,
    importance |> mutate(name = sprintf("importance_%03d", item_number)),
    item_columns |>
      mutate(
        name = sprintf(
          if_else(block == "comment", "implementation_comment_%03d", "implementation_%03d"),
          item_number
        )
      )
  ) |>
    arrange(position) |>
    select(position, name, block, item_number, item_group, item, question)

  assert(
    identical(codebook$position, seq_along(headers)),
    "Nicht jede Spalte des Exports ist genau einmal zugeordnet."
  )

  codebook
}

assert_values <- function(data, allowed, label) {
  unknown <- data |>
    unlist(use.names = FALSE) |>
    unique() |>
    setdiff(c(allowed, NA))
  assert(
    length(unknown) == 0,
    str_glue("Unbekannte Werte in {label}: {str_c(unknown, collapse = ', ')}")
  )
}

# Bricht ab, statt nicht lesbare Zahlen stillschweigend als fehlend zu zählen.
as_number <- function(x) {
  number <- suppressWarnings(as.numeric(x))
  assert(identical(is.na(number), is.na(x)), "Eine Spalte enthält nicht lesbare Zahlen.")
  number
}

# Liefert alle Datensätze und alle Spalten des Exports sowie das Codebuch.
read_survey <- function(path) {
  raw <- read_delim(
    path,
    delim = ";",
    col_types = cols(.default = "c"),
    name_repair = "minimal",
    locale = locale(encoding = "UTF-8")
  )
  assert(nrow(problems(raw)) == 0, "Der Export konnte nicht sauber gelesen werden.")

  codebook <- build_codebook(names(raw))
  answer_columns <- codebook |>
    filter(block %in% ANSWER_BLOCKS) |>
    pull(name)

  # Die kurzen Namen sind auch nötig, weil einzelne Spaltennamen des Exports
  # länger sind, als R für Variablennamen zulässt.
  responses <- raw |>
    set_names(codebook$name) |>
    mutate(across(everything(), \(x) na_if(x, "N. v.")))

  assert_values(select(responses, starts_with("lab_")), c("Ja", "Nein"), "Labortyp")
  assert_values(select(responses, country), c("Deutschland", "Österreich"), "Land")
  assert_values(select(responses, starts_with("importance_")), IMPORTANCE_VALUES, "Wichtigkeit")
  assert_values(
    select(responses, matches("^implementation_[0-9]")),
    IMPLEMENTATION_LEVELS,
    "Umsetzung"
  )

  responses <- responses |>
    mutate(
      completed = !is.na(submit_date),
      n_answers = rowSums(!is.na(pick(all_of(answer_columns)))),
      across(starts_with("lab_"), \(x) x == "Ja"),
      year = as_number(str_sub(year, 1, 4)),
      across(c(last_page, it_fte, samples_day, starts_with("calc_")), as_number),
      across(starts_with("importance_"), \(x) as.integer(str_sub(x, 1, 1))),
      across(matches("^implementation_[0-9]"), \(x) factor(x, levels = IMPLEMENTATION_LEVELS))
    )

  list(responses = responses, codebook = codebook)
}

# Funktionen für Table 1 -------------------------------------------------------

NOT_REPORTED <- "Not reported"

group_products <- function(x) {
  x |>
    fct_recode(Other = "Sonstiges") |>
    fct_lump_min(min = MIN_PRODUCT_N, other_level = "Other") |>
    fct_infreq() |>
    fct_relevel("Other", after = Inf)
}

format_n_percent <- function(n, total) sprintf("%d (%.1f)", n, 100 * n / total)

format_median_iqr <- function(x, digits) {
  quartiles <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf(
    "%.*f [%.*f–%.*f]",
    digits, quartiles[[2]], digits, quartiles[[1]], digits, quartiles[[3]]
  )
}

summarise_completed <- function(data) {
  data |>
    summarise(value = format_n_percent(sum(completed), n()), .by = stratum) |>
    mutate(group = "Survey participation", label = "Survey completed")
}

# Mehrfachauswahl: Jede Antwortoption ist eine eigene Ja/Nein-Variable.
summarise_lab_types <- function(data) {
  labels <- c(
    lab_hospital = "Hospital laboratory",
    lab_university = "University laboratory",
    lab_private = "Private laboratory",
    lab_office = "Physician's office laboratory",
    lab_chain = "Part of a network or chain"
  )

  data |>
    pivot_longer(all_of(names(labels)), names_to = "variable", values_to = "selected") |>
    summarise(
      value = format_n_percent(sum(selected, na.rm = TRUE), sum(!is.na(selected))),
      .by = c(stratum, variable)
    ) |>
    mutate(group = "Laboratory type", label = unname(labels[variable]))
}

summarise_category <- function(data, var, group) {
  data |>
    mutate(level = fct_na_value_to_level({{ var }}, NOT_REPORTED)) |>
    count(stratum, level, .drop = FALSE) |>
    filter(any(n > 0), .by = level) |>
    mutate(reported = sum(n[level != NOT_REPORTED]), .by = stratum) |>
    arrange(level) |>
    mutate(
      value = if_else(
        level == NOT_REPORTED,
        as.character(n),
        format_n_percent(n, reported)
      ),
      group = group,
      label = as.character(level)
    )
}

summarise_numeric <- function(data, var, label, digits) {
  statistic <- data |>
    summarise(value = format_median_iqr({{ var }}, digits), .by = stratum) |>
    mutate(label = str_c(label, ", median [IQR]"))

  missing <- data |>
    summarise(value = as.character(sum(is.na({{ var }}))), .by = stratum) |>
    mutate(label = str_c(label, ", not reported, n"))

  bind_rows(statistic, missing) |>
    mutate(group = "Laboratory characteristics")
}

# Auswertung -------------------------------------------------------------------

data_path <- Sys.getenv("LIS_DATEN_PFAD")
assert(data_path != "", "LIS_DATEN_PFAD ist nicht gesetzt, siehe README.")

dir.create("res", showWarnings = FALSE)

survey <- read_survey(data_path)

# Grundgesamtheit aller Auswertungen.
labs <- survey$responses |>
  filter(n_answers >= MIN_ANSWERS) |>
  mutate(
    country = country |>
      fct_recode(Germany = "Deutschland", Austria = "Österreich") |>
      fct_relevel("Germany", "Austria"),
    lis = group_products(lis),
    order_entry = group_products(order_entry)
  )

# Table 1 ----------------------------------------------------------------------

# Jedes Labor zählt in "Total", Labore mit Land zusätzlich in ihrem Land.
strata <- bind_rows(
  labs |> mutate(stratum = "Total"),
  labs |>
    filter(!is.na(country)) |>
    arrange(country) |>
    mutate(stratum = as.character(country))
) |>
  add_count(stratum, name = "n_stratum") |>
  mutate(stratum = fct_inorder(as.character(str_glue("{stratum} (n={n_stratum})"))))

table_1 <- bind_rows(
  summarise_completed(strata),
  summarise_lab_types(strata),
  summarise_category(strata, lis, "Laboratory information system"),
  summarise_category(strata, order_entry, "Order entry system"),
  summarise_numeric(strata, year, "Year LIS went live", digits = 0),
  summarise_numeric(strata, it_fte, "IT staff, FTE", digits = 1),
  summarise_numeric(strata, samples_day, "Samples per day", digits = 0)
) |>
  pivot_wider(id_cols = c(group, label), names_from = stratum, values_from = value)

samples_range <- format(range(labs$samples_day, na.rm = TRUE), scientific = FALSE, trim = TRUE)

table_1 |>
  gt(rowname_col = "label", groupname_col = "group") |>
  tab_caption("Characteristics of the participating laboratories.") |>
  tab_footnote(
    "Multiple answers were possible, so percentages do not add up to 100.",
    locations = cells_row_groups(groups = "Laboratory type")
  ) |>
  tab_footnote(
    str_glue("Reported values ranged from {samples_range[1]} to {samples_range[2]}."),
    locations = cells_stub(rows = label == "Samples per day, median [IQR]")
  ) |>
  tab_source_note(str_glue(
    "Data are n (%) unless stated otherwise. Percentages refer to laboratories ",
    "with an answer. Of {nrow(survey$responses)} survey records, ",
    "{sum(survey$responses$completed)} were completed. The table includes the ",
    "{nrow(labs)} records with at least {MIN_ANSWERS} answers, not counting country ",
    "and laboratory type. Total includes laboratories without a reported country ",
    "(n={sum(is.na(labs$country))}). Other comprises the answer option other and ",
    "products used by fewer than {MIN_PRODUCT_N} laboratories."
  )) |>
  tab_source_note(
    "FTE, full-time equivalent; IQR, interquartile range; LIS, laboratory information system."
  ) |>
  opt_footnote_marks(marks = "letters") |>
  opt_table_font(font = c("Arial", "Helvetica", "sans-serif")) |>
  tab_options(
    table_body.hlines.style = "none",
    stub.border.style = "none",
    stub_row_group.border.style = "none"
  ) |>
  gtsave("res/table_1.html")
