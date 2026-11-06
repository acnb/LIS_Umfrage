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

# Funktionen für die Fragebogen-Gruppen ----------------------------------------

GROUP_LABELS <- c(
  "Elektronisches Order Entry" = "Electronic order entry",
  "Systemfunktionen" = "System functions",
  "Benutzerfreundlichkeit und Interface" = "Usability and interface",
  "Performance und Stabilität" = "Performance and stability",
  "Integration und Schnittstellen" = "Integration and interfaces",
  "Dokumentation und Compliance" = "Documentation and compliance",
  "Reporting und Analytics" = "Reporting and analytics",
  "Neue Funktionalitäten" = "New functionalities"
)

# Die Reihenfolge entspricht den Werten 5 bis 1 der Skala.
RATING_LABELS <- c("Exemplary", "Good", "Satisfactory", "Adequate", "Inadequate")

RATING_COLOURS <- c(
  Inadequate = "#c8352f",
  Adequate = "#f0a59f",
  Satisfactory = "#dedcd6",
  Good = "#86b6ef",
  Exemplary = "#1c5cab"
)

NOT_AVAILABLE_COLOUR <- "#898781"

# Eine Zeile je Labor und Item. score ist 5 (vorbildlich) bis 1 (unzureichend) und
# fehlt bei "Nicht vorhanden" und "Nicht genutzt", die nicht zur Skala gehören.
ratings_long <- function(labs, codebook) {
  item_groups <- codebook |>
    filter(block == "importance") |>
    transmute(item_number, group = unname(GROUP_LABELS[item_group]))
  assert(!anyNA(item_groups$group), "Eine Fragebogen-Gruppe hat keinen englischen Namen.")

  labs |>
    select(response_id, starts_with("importance_"), matches("^implementation_[0-9]")) |>
    pivot_longer(
      -response_id,
      names_to = c(".value", "item_number"),
      names_pattern = "(importance|implementation)_([0-9]+)",
      names_transform = list(item_number = as.integer)
    ) |>
    left_join(item_groups, by = "item_number") |>
    mutate(score = 6L - match(implementation, IMPLEMENTATION_LEVELS[1:5]))
}

# "Good" und "poor" beziehen sich auf die Skala, "not available" auf alle Antworten.
summarise_groups <- function(ratings) {
  ratings |>
    summarise(
      items = n_distinct(item_number),
      mean_importance = mean(importance, na.rm = TRUE),
      mean_implementation = mean(score, na.rm = TRUE),
      n_ratings = sum(!is.na(implementation)),
      n_scale = sum(!is.na(score)),
      good_pct = 100 * sum(score >= 4, na.rm = TRUE) / n_scale,
      poor_pct = 100 * sum(score <= 2, na.rm = TRUE) / n_scale,
      not_available_pct = 100 * sum(implementation == "Nicht vorhanden", na.rm = TRUE) / n_ratings,
      .by = group
    ) |>
    arrange(desc(good_pct))
}

# Berechnet die Lage der Segmente. "Satisfactory" liegt je zur Hälfte links und
# rechts der Mitte, die übrigen Stufen schließen nach außen an.
likert_segments <- function(ratings) {
  shares <- ratings |>
    filter(!is.na(score)) |>
    mutate(rating = RATING_LABELS[6L - score]) |>
    count(group, rating) |>
    mutate(share = 100 * n / sum(n), .by = group)

  side <- function(ratings_from_centre, direction) {
    shares |>
      filter(rating %in% ratings_from_centre) |>
      mutate(
        order = match(rating, ratings_from_centre),
        width = if_else(rating == "Satisfactory", share / 2, share)
      ) |>
      arrange(group, order) |>
      mutate(
        end = direction * cumsum(width),
        start = lag(end, default = 0),
        .by = group
      )
  }

  bind_rows(
    side(c("Satisfactory", "Adequate", "Inadequate"), -1),
    side(c("Satisfactory", "Good", "Exemplary"), 1)
  )
}

plot_implementation <- function(segments, groups) {
  panels <- c("Rating of implementation", "Not available")
  bar_half_height <- 0.36
  label_size <- 7 / .pt
  label_gap <- 1.5

  rows <- groups |>
    mutate(
      row = rev(row_number()),
      row_label = str_glue("{group}\n{items} items, n={n_ratings}")
    )

  bars <- segments |>
    left_join(rows, by = "group") |>
    mutate(panel = factor(panels[1], levels = panels))

  outer_ends <- bars |>
    summarise(left = min(start, end), right = max(start, end), .by = c(group, row, panel)) |>
    left_join(select(rows, group, good_pct, poor_pct), by = "group")

  not_available <- rows |>
    mutate(panel = factor(panels[2], levels = panels))

  panel_limits <- tibble(
    panel = factor(panels[c(1, 1, 2, 2)], levels = panels),
    x = c(-85, 75, 0, 40),
    row = 1
  )

  ggplot() +
    geom_blank(data = panel_limits, aes(x = x, y = row)) +
    geom_rect(
      data = bars,
      aes(
        xmin = start, xmax = end,
        ymin = row - bar_half_height, ymax = row + bar_half_height,
        fill = rating
      ),
      colour = "white",
      linewidth = 0.25
    ) +
    geom_rect(
      data = not_available,
      aes(
        xmin = 0, xmax = not_available_pct,
        ymin = row - bar_half_height, ymax = row + bar_half_height
      ),
      fill = NOT_AVAILABLE_COLOUR
    ) +
    geom_vline(
      data = tibble(panel = factor(panels[1], levels = panels)),
      aes(xintercept = 0),
      colour = "#52514e",
      linewidth = 0.3
    ) +
    geom_text(
      data = outer_ends,
      aes(x = left - label_gap, y = row, label = sprintf("%.0f%%", poor_pct)),
      hjust = 1, size = label_size, family = "Arial"
    ) +
    geom_text(
      data = outer_ends,
      aes(x = right + label_gap, y = row, label = sprintf("%.0f%%", good_pct)),
      hjust = 0, size = label_size, family = "Arial"
    ) +
    geom_text(
      data = not_available,
      aes(x = not_available_pct + label_gap, y = row, label = sprintf("%.0f%%", not_available_pct)),
      hjust = 0, size = label_size, family = "Arial"
    ) +
    facet_grid(cols = vars(panel), scales = "free_x", space = "free_x") +
    scale_x_continuous(
      breaks = seq(-75, 75, 25),
      labels = \(x) str_c(abs(x), "%"),
      expand = expansion(0)
    ) +
    scale_y_continuous(
      breaks = rows$row,
      labels = rows$row_label,
      expand = expansion(add = 0.5)
    ) +
    scale_fill_manual(values = RATING_COLOURS, breaks = names(RATING_COLOURS), name = NULL) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 8, base_family = "Arial") +
    theme(
      axis.text = element_text(colour = "#0b0b0b", size = 7),
      axis.text.y = element_text(hjust = 0, lineheight = 1.05),
      legend.key.size = unit(3, "mm"),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#e1e0d9", linewidth = 0.2),
      panel.spacing.x = unit(5, "mm"),
      strip.text = element_text(colour = "#0b0b0b", size = 8, face = "bold")
    )
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

# Fragebogen-Gruppen -----------------------------------------------------------

ratings <- ratings_long(labs, survey$codebook)
groups <- summarise_groups(ratings)

n_rated_labs <- ratings |>
  filter(!is.na(implementation)) |>
  pull(response_id) |>
  n_distinct()

population <- str_glue(
  "Based on {sum(groups$n_ratings)} ratings from {n_rated_labs} of the {nrow(labs)} ",
  "included laboratories; each laboratory contributes several ratings. Implementation ",
  "was only asked for features rated 4 or 5 for importance."
)

groups |>
  transmute(
    Group = group,
    Items = items,
    `Ratings, n` = n_ratings,
    `Importance, mean` = sprintf("%.1f", mean_importance),
    `Implementation, mean` = sprintf("%.1f", mean_implementation),
    `Good or exemplary, %` = sprintf("%.1f", good_pct),
    `Adequate or inadequate, %` = sprintf("%.1f", poor_pct),
    `Not available, %` = sprintf("%.1f", not_available_pct)
  ) |>
  gt() |>
  tab_caption("Importance and implementation of LIS features by questionnaire group.") |>
  tab_source_note(str_glue(
    "Importance ranged from 1 (unimportant) to 5 (very important), implementation ",
    "from 1 (inadequate) to 5 (exemplary). Percentages of good or exemplary and of ",
    "adequate or inadequate refer to ratings on this scale, the percentage not ",
    "available refers to all ratings. {population}"
  )) |>
  tab_source_note("LIS, laboratory information system.") |>
  opt_table_font(font = c("Arial", "Helvetica", "sans-serif")) |>
  tab_options(table_body.hlines.style = "none") |>
  gtsave("res/table_2.html")

ggsave(
  "res/figure_1.png",
  plot = plot_implementation(likert_segments(ratings), groups),
  device = "png", width = 170, height = 110, units = "mm",
  dpi = 600, bg = "white"
)

write_lines(
  str_glue(
    "Implementation of LIS features by questionnaire group. Bars are centred on the ",
    "middle of the rating scale, with satisfactory ratings split evenly across the ",
    "centre. Numbers at the bar ends give the percentage of adequate or inadequate ",
    "ratings (left) and of good or exemplary ratings (right) among ratings on the ",
    "scale; satisfactory ratings are not part of these numbers. The right panel gives ",
    "the percentage of all ratings stating that a feature is not available. ",
    "{population} LIS, laboratory information system."
  ),
  "res/figure_1_caption.txt"
)
