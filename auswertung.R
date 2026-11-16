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

# Funktionen für die Importance-Performance-Analyse ---------------------------

# Wegen des Deckeneffekts beginnt die Hauptachse hier, darunter folgt eine Randnotiz.
IMPORTANCE_CUTOFF <- 3.5

# Beschriftet werden die größten Lücken unter Merkmalen mit genügend Bewertungen.
N_GAP_LABELS <- 10
MIN_GAP_N <- 20

# Englische Kurznamen, zugeordnet über den Anfang des Itemtexts.
ITEM_LABELS_EN <- tribble(
  ~prefix,                                         ~label,
  "einfach zu erfüllende Hard- und Software",      "Modest hardware and software requirements",
  "Module für Mikrobiologie",                      "Microbiology module",
  "Module für Blutdepot",                          "Blood bank module",
  "Modul für Humangenetik",                        "Human genetics module",
  "Modul für Blutspende",                          "Blood donation module",
  "Modul für Pathologie",                          "Pathology module",
  "Modul für Neugeborenenscreening",               "Newborn screening module",
  "KV-Abrechnung",                                 "Statutory health insurance billing",
  "Probenverwaltung",                              "Sample management and tracking",
  "Flexible Auftragserfassung",                    "Flexible order capture",
  "Strukturierte Befunderstellung",                "Structured reporting",
  "automatisierte Erstellung von Diagrammen",      "Automated charts in reports",
  "Integrierte Qualitätskontrolle",                "Integrated quality control",
  "Stammdatenverwaltung",                          "Master data management",
  "Präanalytik",                                   "Preanalytical documentation",
  "Modul für Id-Check",                            "Patient identification module",
  "Modul für Fremdversand",                        "Send-out testing module",
  "Modul für Referenzbereichsverifizierung",       "Reference interval verification",
  "Programmierbare Regeln",                        "Rules for comments and reflex testing",
  "regelbasierte Autovalidation",                  "Rule-based autovalidation",
  "Deltacheck",                                    "Delta check",
  "Berechnung von z-log Werten",                   "zlog value calculation",
  "Intuitive, übersichtliche Bedienoberfläche",    "Intuitive user interface",
  "farbliche differenzierte Hervorhebung",         "Colour highlighting of critical values",
  "Anpassbare Benutzeroberflächen",                "Customisable user interface",
  "Erweiterte Suchfunktionen",                     "Advanced search",
  "Rollenbasierte Benutzerrechte",                 "Role-based access rights",
  "Mobile/Tablet-Unterstützung",                   "Mobile and tablet support",
  "Schnittstellen zur Laborgeräte-Anbindung",      "Rapid instrument interfacing",
  "einfache Befundlayout Anpassung",               "Report layout customisation",
  "KIS/HIS-Integration",                           "Hospital information system integration",
  "Anbindung externer Labore",                     "Connection to external laboratories",
  "HL7 V2/V3",                                     "HL7 v2/v3 interfaces",
  "FHIR-Standardschnittstellen",                   "FHIR interfaces",
  "Schnittstelle für Drittsysteme",                "Third-party system interfaces",
  "lesender direkter Datenbankzugriff",            "Direct read access to the database",
  "Datenexport",                                   "Data export over long periods",
  "Unterstützung von SNOMED CT",                   "SNOMED CT support",
  "Unterstützung von LOINC",                       "LOINC support",
  "Unterstützung von UCUM",                        "UCUM support",
  "Anbindung an KIM",                              "Connection to KIM secure messaging",
  "IVDR/FDA-Konformität",                          "IVDR/FDA compliance",
  "Vollständiger Audit-Trail",                     "Complete audit trail",
  "Langzeitarchivierung",                          "Long-term archiving",
  "Elektronische Signatur zum Signieren von Bef",  "Electronic signature of reports",
  "Elektronische Signatur zum Signieren von Anf",  "Electronic signature of orders",
  "Validierungsunterstützung",                     "Validation support (rules, AI)",
  "Standardberichterstellung",                     "Standard reports",
  "Individuelle Report-Konfiguration",             "Custom report configuration",
  "Statistische Auswertungen",                     "Statistical analyses",
  "Dashboard/KPI-Übersichten",                     "Dashboards and key performance indicators",
  "erweiterte Suchfunktionen auf der Datenbank",   "Advanced database search",
  "Schnelle Systemantwortzeiten",                  "Fast response times",
  "Support",                                       "Vendor support",
  "Testumgebung",                                  "Test environment",
  "Hohe Systemverfügbarkeit",                      "High availability",
  "Automatische Backup-Funktionen",                "Automatic backup",
  "Disaster-Recovery-Funktionen",                  "Disaster recovery",
  "Einfachheit von Updates/Upgrades",              "Ease of updates and upgrades",
  "Biobanking",                                    "Biobanking",
  "dynamische Befunde",                            "Interactive result portal",
  "automatisierte Softwaretests",                  "Automated software testing",
  "mögliche Migration der Daten",                  "Data migration to a new LIS",
  "Stammdaten synchronisiert",                     "Master data synchronised with the LIS",
  "Nachforderungsmanagement",                      "Add-on test management",
  "dynamische Maskensteuerung",                    "Dynamic order forms",
  "einfache Order-Entry Masken Anpassung",         "Order form customisation",
  "Anzeige von benötigter Probenart",              "Display of required sample type",
  "Anzeige von benötigter Probenmenge",            "Display of required sample volume",
  "Einsenderspezifische Profile",                  "Requester-specific profiles",
  "diagnosebezogenen Panel-Vorschläge",            "Diagnosis-based panel suggestions",
  "Anzeige von Vorbefunden",                       "Display of previous results when ordering",
  "Behandlung von Re-Testing Intervallen",         "Retesting interval rules",
  "Kostenanzeige",                                 "Cost display",
  "Überprüfung von zu erfüllenden Kriterien",      "Order eligibility checks"
)

GROUP_COLOURS <- c(
  "System functions" = "#2a78d6",
  "Usability and interface" = "#eb6834",
  "Integration and interfaces" = "#1baf7a",
  "Documentation and compliance" = "#eda100",
  "Reporting and analytics" = "#e87ba4",
  "Performance and stability" = "#008300",
  "New functionalities" = "#6250d6",
  "Electronic order entry" = "#e34948"
)

# Die Form unterscheidet die Gruppen zusätzlich zur Farbe.
GROUP_SHAPES <- c(
  "System functions" = 21,
  "Usability and interface" = 22,
  "Integration and interfaces" = 24,
  "Documentation and compliance" = 23,
  "Reporting and analytics" = 25,
  "Performance and stability" = 3,
  "New functionalities" = 4,
  "Electronic order entry" = 8
)

english_item_labels <- function(items) {
  items <- str_squish(items)
  matches <- map(ITEM_LABELS_EN$prefix, \(x) which(str_starts(items, fixed(x))))
  assert(all(lengths(matches) == 1), "Ein englischer Itemname passt nicht zu genau einem Item.")
  matches <- unlist(matches)
  assert(setequal(matches, seq_along(items)), "Nicht jedes Item hat einen englischen Namen.")
  ITEM_LABELS_EN$label[match(seq_along(items), matches)]
}

summarise_items <- function(ratings, codebook) {
  labels <- codebook |>
    filter(block == "importance") |>
    transmute(item_number, label = english_item_labels(item))

  ratings |>
    summarise(
      group = first(group),
      mean_importance = mean(importance, na.rm = TRUE),
      n_importance = sum(!is.na(importance)),
      mean_implementation = mean(score, na.rm = TRUE),
      n_implementation = sum(!is.na(score)),
      .by = item_number
    ) |>
    left_join(labels, by = "item_number") |>
    mutate(gap = mean_importance - mean_implementation) |>
    add_gap_rank()
}

# Rang 1 bis N_GAP_LABELS für die größten Lücken; Abbildung und Bildunterschrift
# verwenden dieselbe Nummerierung.
add_gap_rank <- function(items) {
  ranks <- items |>
    filter(mean_importance >= IMPORTANCE_CUTOFF, n_implementation >= MIN_GAP_N) |>
    slice_max(gap, n = N_GAP_LABELS, with_ties = FALSE) |>
    transmute(item_number, gap_rank = row_number())

  left_join(items, ranks, by = "item_number")
}

# Zwei Teile mit gemeinsamer y-Achse: links eine schmale Randnotiz für Merkmale
# unter der Abschneidegrenze, rechts das Hauptfeld.
plot_importance_performance <- function(items) {
  label_size <- 6.5 / .pt
  max_point_size <- 6
  means <- c(
    importance = mean(items$mean_importance),
    implementation = mean(items$mean_implementation)
  )

  points <- items |>
    mutate(
      group = factor(group, levels = names(GROUP_COLOURS)),
      # Ungefähre Punktgröße in mm, damit Beschriftungen den Punkten ausweichen.
      repel_size = max_point_size * sqrt(n_implementation / max(n_implementation))
    )

  # Identische Skalen in beiden Teilen, damit patchwork die Legenden zusammenführt.
  shared <- list(
    geom_hline(yintercept = means[["implementation"]], colour = "#898781", linewidth = 0.3),
    geom_point(
      aes(size = n_implementation, colour = group, fill = group, shape = group),
      alpha = 0.75,
      stroke = 0.4
    ),
    scale_y_continuous(
      limits = range(points$mean_implementation) + c(-0.1, 0.1),
      breaks = seq(1.5, 4.5, 0.5)
    ),
    scale_colour_manual(values = GROUP_COLOURS, limits = names(GROUP_COLOURS), name = NULL),
    scale_fill_manual(values = GROUP_COLOURS, limits = names(GROUP_COLOURS), name = NULL),
    scale_shape_manual(values = GROUP_SHAPES, limits = names(GROUP_COLOURS), name = NULL),
    scale_size_area(
      max_size = max_point_size,
      limits = c(0, max(points$n_implementation)),
      breaks = c(10, 25, 50),
      name = "Ratings, n"
    ),
    guides(
      colour = guide_legend(ncol = 4, order = 1, override.aes = list(size = 2.5)),
      fill = guide_legend(ncol = 4, order = 1),
      shape = guide_legend(ncol = 4, order = 1),
      size = guide_legend(order = 2)
    ),
    theme_minimal(base_size = 8, base_family = "Arial"),
    theme(
      axis.text = element_text(colour = "#0b0b0b", size = 7),
      legend.text = element_text(size = 7),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.2),
      plot.title = element_text(colour = "#0b0b0b", size = 8, face = "bold", hjust = 0.5)
    )
  )

  # Ein halbtransparenter Kasten ohne Rahmen hält Beschriftungen über Linien und
  # Punkten lesbar und gibt ggrepel verlässliche Abmessungen zum Ausweichen.
  repel <- function(..., min.segment.length = 0) {
    ggrepel::geom_label_repel(
      aes(label = label, point.size = repel_size),
      size = label_size, family = "Arial", colour = "#0b0b0b",
      fill = alpha("white", 0.8), linetype = "blank", label.padding = unit(0.08, "lines"),
      min.segment.length = min.segment.length, segment.size = 0.2, segment.colour = "#52514e",
      max.overlaps = Inf, max.time = 5, max.iter = 500000, seed = 1,
      ...
    )
  }

  margin_plot <- points |>
    filter(mean_importance < IMPORTANCE_CUTOFF) |>
    mutate(label = str_glue("{label} ({sprintf('%.1f', mean_importance)})")) |>
    ggplot(aes(x = 0, y = mean_implementation)) +
    shared +
    repel(direction = "y", hjust = 0, nudge_x = 0.12, xlim = c(0.08, NA), box.padding = 0.15) +
    scale_x_continuous(limits = c(-0.05, 1), breaks = NULL) +
    # Die Legende kommt aus dem Hauptfeld, das alle Gruppen enthält.
    guides(colour = "none", fill = "none", shape = "none", size = "none") +
    labs(
      title = str_glue("Importance < {IMPORTANCE_CUTOFF}"),
      x = NULL,
      y = "Implementation, mean (1–5)"
    )

  # Alle Punkte werden übergeben, damit die Beschriftungen auch unbeschrifteten
  # Punkten ausweichen; die größten Lücken tragen ihre Nummer, die Namen stehen in
  # der Bildunterschrift.
  main_plot <- points |>
    filter(mean_importance >= IMPORTANCE_CUTOFF) |>
    mutate(label = if_else(is.na(gap_rank), "", as.character(gap_rank))) |>
    ggplot(aes(mean_importance, mean_implementation)) +
    geom_vline(xintercept = means[["importance"]], colour = "#898781", linewidth = 0.3) +
    annotate(
      "text",
      x = means[["importance"]], y = Inf,
      label = sprintf("Mean %.2f", means[["importance"]]),
      hjust = -0.1, vjust = 1.5, size = label_size, colour = "#52514e", family = "Arial"
    ) +
    annotate(
      "text",
      x = -Inf, y = means[["implementation"]],
      label = sprintf("Mean %.2f", means[["implementation"]]),
      hjust = -0.1, vjust = 1.6, size = label_size, colour = "#52514e", family = "Arial"
    ) +
    shared +
    repel(box.padding = 0.35, point.padding = 0, force = 4, fontface = "bold") +
    scale_x_continuous(
      limits = c(IMPORTANCE_CUTOFF, 5),
      breaks = seq(IMPORTANCE_CUTOFF, 5, 0.5),
      expand = expansion(add = 0.05)
    ) +
    labs(
      title = str_glue("Importance ≥ {IMPORTANCE_CUTOFF}"),
      x = "Importance, mean (1–5)",
      y = NULL
    ) +
    theme(axis.text.y = element_blank())

  patchwork::wrap_plots(margin_plot, main_plot, widths = c(1.3, 3), guides = "collect") &
    theme(
      legend.box = "vertical",
      legend.key.size = unit(3.5, "mm"),
      legend.position = "bottom"
    )
}

# Funktionen für die Umsetzung je Labor ----------------------------------------

IT_STAFF_BREAKS <- c(-Inf, 0, 2, Inf)
IT_STAFF_LABELS <- c("0", ">0–2", ">2")
LAB_TYPE_LEVELS <- c("University", "Hospital", "Private")

# Ein Labor zählt zum ersten zutreffenden Typ: Universität vor Krankenhaus vor
# Privat. Hersteller erscheinen ab MIN_PRODUCT_N bewerteten Laboren, wie in Table 1.
summarise_labs <- function(ratings, labs) {
  ratings |>
    summarise(
      n_scale = sum(!is.na(score)),
      mean_score = mean(score, na.rm = TRUE),
      .by = response_id
    ) |>
    filter(n_scale > 0) |>
    left_join(
      select(labs, response_id, lis_product, starts_with("lab_"), it_fte),
      by = "response_id"
    ) |>
    mutate(
      lab_type = case_when(
        lab_university ~ "University",
        lab_hospital ~ "Hospital",
        lab_private | lab_chain | lab_office ~ "Private"
      ) |>
        factor(levels = LAB_TYPE_LEVELS),
      it_staff = cut(it_fte, IT_STAFF_BREAKS, labels = IT_STAFF_LABELS),
      vendor = na_if(lis_product, "Sonstiges")
    ) |>
    add_count(vendor, name = "n_vendor") |>
    mutate(
      vendor = if_else(
        !is.na(vendor) & n_vendor >= MIN_PRODUCT_N,
        str_remove(vendor, " *[/|].*$"),
        NA
      )
    )
}

# Beschriftet jede Gruppe mit ihrer Laborzahl und behält die Reihenfolge der Gruppen.
label_groups <- function(data) {
  data |>
    add_count(facet, group) |>
    arrange(facet, group) |>
    mutate(x = fct_inorder(as.character(str_glue("{str_wrap(group, 8)}\n(n={n})"))))
}

plot_lab_implementation <- function(per_lab) {
  overall_median <- median(per_lab$mean_score)
  point_colour <- "#2a78d6"

  base_theme <- list(
    theme_minimal(base_size = 8, base_family = "Arial"),
    theme(
      axis.text = element_text(colour = "#0b0b0b", size = 7),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.2),
      plot.tag = element_text(size = 9, face = "bold"),
      strip.text = element_text(colour = "#0b0b0b", size = 8, face = "bold")
    )
  )

  box_layers <- list(
    geom_hline(yintercept = overall_median, colour = "#898781", linewidth = 0.3),
    geom_boxplot(
      outlier.shape = NA, fill = "#f0efec", colour = "#52514e",
      width = 0.55, linewidth = 0.3
    ),
    geom_point(
      position = position_jitter(width = 0.15, height = 0, seed = 1),
      colour = point_colour, alpha = 0.7, size = 1.2
    ),
    facet_grid(cols = vars(facet), scales = "free_x", space = "free_x"),
    scale_y_continuous(limits = c(1, 5), breaks = 1:5),
    labs(x = NULL, y = "Implementation, mean per laboratory"),
    base_theme
  )

  distribution <- ggplot(per_lab, aes(mean_score)) +
    geom_histogram(binwidth = 0.2, boundary = 1, fill = point_colour, colour = "white", linewidth = 0.3) +
    geom_vline(xintercept = overall_median, colour = "#52514e", linewidth = 0.3) +
    annotate(
      "text",
      x = overall_median, y = Inf,
      label = sprintf("Median %.2f", overall_median),
      hjust = -0.08, vjust = 1.5, size = 6.5 / .pt, colour = "#52514e", family = "Arial"
    ) +
    scale_x_continuous(
      limits = c(1, 5),
      breaks = 1:5,
      labels = c("1\nInadequate", "2", "3", "4", "5\nExemplary")
    ) +
    scale_y_continuous(
      breaks = \(limits) seq(0, floor(limits[2]), by = 4),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      x = str_glue("Implementation, mean per laboratory (n={nrow(per_lab)})"),
      y = "Laboratories, n"
    ) +
    base_theme +
    theme(panel.grid.major.x = element_line(colour = "#e1e0d9", linewidth = 0.2))

  vendors <- per_lab |>
    filter(!is.na(vendor)) |>
    transmute(facet = "LIS product", group = fct_infreq(vendor), mean_score) |>
    label_groups()

  characteristics <- bind_rows(
    per_lab |>
      filter(!is.na(lab_type)) |>
      transmute(facet = "Laboratory type", group = as.character(lab_type), mean_score),
    per_lab |>
      filter(!is.na(it_staff)) |>
      transmute(facet = "IT staff, FTE", group = as.character(it_staff), mean_score)
  ) |>
    mutate(
      facet = factor(facet, levels = c("Laboratory type", "IT staff, FTE")),
      group = factor(group, levels = c(LAB_TYPE_LEVELS, IT_STAFF_LABELS))
    ) |>
    label_groups()

  vendor_plot <- ggplot(vendors, aes(x, mean_score)) + box_layers
  characteristics_plot <- ggplot(characteristics, aes(x, mean_score)) +
    box_layers +
    theme(axis.title.y = element_blank())

  patchwork::wrap_plots(
    distribution, vendor_plot, characteristics_plot,
    design = "AA\nBC", widths = c(5, 6), heights = c(0.8, 1.2)
  ) +
    patchwork::plot_annotation(tag_levels = "A")
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
    lis_product = lis,
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

# Importance-Performance-Analyse -----------------------------------------------

items <- summarise_items(ratings, survey$codebook)

gap_legend <- items |>
  filter(!is.na(gap_rank)) |>
  arrange(gap_rank) |>
  mutate(entry = sprintf("(%d) %s, gap %.2f", gap_rank, label, gap)) |>
  pull(entry) |>
  str_c(collapse = "; ")

ipa_note <- str_glue(
  "{population} Importance is averaged over all laboratories that rated the ",
  "feature, so the gap between importance and implementation compares partly ",
  "different raters."
)

items |>
  mutate(group = factor(group, levels = names(GROUP_COLOURS))) |>
  arrange(group, desc(gap)) |>
  transmute(
    Group = as.character(group),
    Feature = label,
    `Importance, mean` = sprintf("%.2f", mean_importance),
    `Importance, n` = n_importance,
    `Implementation, mean` = sprintf("%.2f", mean_implementation),
    `Implementation, n` = n_implementation,
    Gap = sprintf("%.2f", gap)
  ) |>
  gt(groupname_col = "Group") |>
  tab_caption("Importance and implementation of all 75 LIS features.") |>
  tab_source_note(str_glue(
    "Both scales range from 1 to 5. Gap is mean importance minus mean implementation. ",
    "{ipa_note}"
  )) |>
  tab_source_note("LIS, laboratory information system.") |>
  opt_table_font(font = c("Arial", "Helvetica", "sans-serif")) |>
  tab_options(table_body.hlines.style = "none") |>
  gtsave("res/table_s1.html")

ggsave(
  "res/figure_2.png",
  plot = plot_importance_performance(items),
  device = "png", width = 170, height = 150, units = "mm",
  dpi = 600, bg = "white"
)

write_lines(
  str_glue(
    "Importance-performance analysis of {nrow(items)} LIS features. Each point is one ",
    "feature; point area shows the number of implementation ratings. Grey lines mark ",
    "the means across all features (importance {sprintf('%.2f', mean(items$mean_importance))}, ",
    "implementation {sprintf('%.2f', mean(items$mean_implementation))}). Because of a ",
    "ceiling effect, the main panel starts at an importance of {IMPORTANCE_CUTOFF}; the ",
    "{sum(items$mean_importance < IMPORTANCE_CUTOFF)} features below are shown in the left ",
    "panel with their mean importance in parentheses. Numbers mark the {N_GAP_LABELS} ",
    "largest gaps between importance and implementation among features with at least ",
    "{MIN_GAP_N} implementation ratings: {gap_legend}. {ipa_note} ",
    "LIS, laboratory information system."
  ),
  "res/figure_2_caption.txt"
)

# Umsetzung je Labor -----------------------------------------------------------

per_lab <- summarise_labs(ratings, labs)

ggsave(
  "res/figure_3.png",
  plot = plot_lab_implementation(per_lab),
  device = "png", width = 170, height = 140, units = "mm",
  dpi = 600, bg = "white"
)

write_lines(
  str_glue(
    "Implementation by laboratory. The implementation score of a laboratory is the ",
    "mean of its ratings on the scale from 1 (inadequate) to 5 (exemplary); ",
    "{nrow(per_lab)} of the {nrow(labs)} included laboratories gave ",
    "{min(per_lab$n_scale)} to {max(per_lab$n_scale)} such ratings ",
    "(median {median(per_lab$n_scale)}). (A) Distribution of laboratory scores ",
    "(median {sprintf('%.2f', median(per_lab$mean_score))}, range ",
    "{sprintf('%.2f', min(per_lab$mean_score))} to {sprintf('%.2f', max(per_lab$mean_score))}). ",
    "(B) Laboratories using one of the {n_distinct(per_lab$vendor, na.rm = TRUE)} LIS ",
    "products used by at least {MIN_PRODUCT_N} of these laboratories ",
    "(n={sum(!is.na(per_lab$vendor))}). (C) Laboratory type ",
    "(n={sum(!is.na(per_lab$lab_type))}), assigned in the order university, hospital, ",
    "private, where private includes office-based laboratories, laboratory networks ",
    "and physician's office laboratories; and IT staff (n={sum(!is.na(per_lab$it_staff))}). ",
    "Boxes show median and interquartile range, whiskers extend to 1.5 times the ",
    "interquartile range, points show individual laboratories. Grey lines mark the ",
    "overall median. FTE, full-time equivalent; LIS, laboratory information system."
  ),
  "res/figure_3_caption.txt"
)
