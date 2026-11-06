# LIS-Umfrage

Wissenschaftliche Auswertung der Umfrage zu Laborinformationssystemen (LIS) in Deutschland und Österreich. Die Ergebnisse sind für eine Publikation in *Clinical Chemistry and Laboratory Medicine* (CCLM) vorgesehen.

## Prinzipien

- **Alles in einer Datei:** Die gesamte Auswertung steht in `auswertung.R`, vom Einlesen der Daten bis zur fertigen Grafik.
- **Tidyverse-Dialekt:** Der Code verwendet durchgängig tidyverse-Funktionen und die native Pipe `|>`.
- **Alle Resultate in `res/`:** Das Skript legt den Ordner bei Bedarf an und schreibt sämtliche Tabellen und Grafiken dorthin. Außerhalb von `res/` entstehen keine Ausgabedateien.
- **Grafiken für CCLM:** Alle Abbildungen werden direkt in publikationsfähiger Form erzeugt und nicht nachträglich von Hand bearbeitet.
- **Datenpfad über `.Renviron`:** Der Pfad zu den Originaldaten steht in der lokalen Datei `.Renviron` und nicht im Code.
- **Nur Code wird versioniert:** Originaldaten und Ergebnisse gelangen nicht ins Git-Repository.
- **Grundgesamtheit immer angeben:** Jede Tabelle und jede Abbildung nennt, auf welcher Grundgesamtheit sie beruht.

## Grundgesamtheit

| Gruppe | n | Verwendung |
|---|---|---|
| Datensätze im Export | 235 | Ausgangsmenge |
| Abgeschickt („completed“) | 80 | Zahl der vollständig ausgefüllten Umfragen |
| Mindestens 5 Angaben, Land und Labortyp zählen nicht mit (`labs`) | 107 | Table 1 und alle weiteren Auswertungen |

Von den 107 Laboren stammen 93 aus Deutschland und 13 aus Österreich, 1 Labor hat kein Land angegeben. Es zählt in „Total“, aber in keiner Länderspalte.

## Import

`read_survey()` trennt das Einlesen von der Auswertung und liefert eine Liste mit zwei Tabellen:

- **`responses`:** Alle Datensätze und alle Spalten des Exports mit kurzen Namen, dazu `completed` und `n_answers`. Freitexte stehen in R zur Verfügung, gehen aber nicht in Tabellen.
- **`codebook`:** Für jede Spalte Kurzname, Position, Block, Fragengruppe, Item und Originaltext.

Die Spalten werden über den Fragetext gefunden, nicht über ihre Position. Der Import bricht ab, wenn eine Spalte fehlt oder neu ist, wenn die Item-Blöcke nicht zusammenpassen oder wenn ein unbekannter Antwortwert auftritt.

Für die weiteren Auswertungen gilt:

- **Item-Nummern:** `importance_001`, `implementation_001` und `implementation_comment_001` gehören zum selben Item. Die Nummern folgen dem Alphabet des Itemtexts, die Reihenfolge im Fragebogen steht in `position`.
- **Umsetzungsfragen:** Sie liegen nur für Items vor, die das Labor mit Wichtigkeit 4 oder 5 bewertet hat. Die Bewertungen sind daher keine Stichprobe über alle Items und Labore.
- **`Nicht vorhanden` und `Nicht genutzt`:** Sie gehören nicht zur Skala der Umsetzung. Die jeweilige Auswertung legt fest, wie sie behandelt werden.

## Fragebogen-Gruppen

Die Items werden nach den acht Gruppen des Fragebogens ausgewertet (`item_group` im Codebuch). Sie standen vor der Erhebung fest. Eine nachträgliche Einteilung in inhaltliche Bereiche wurde verworfen, weil die Zuordnung einzelner Items das Ergebnis mitbestimmt: Die Anforderungssteuerung war je nach Itemauswahl der schwächste oder der beste Bereich.

Grundlage sind die Umsetzungsbewertungen der `labs`, also 3038 Bewertungen aus 81 Laboren. Jedes Labor trägt mehrere Bewertungen bei.

- **Gut oder vorbildlich, ausreichend oder unzureichend:** Prozent der fünf Skalenantworten.
- **Nicht vorhanden:** Prozent aller Antworten. `Nicht genutzt` zählt hier im Nenner mit.

`res/table_2.html` enthält die Kennzahlen je Gruppe, `res/figure_1.png` zeigt sie als zentrierte Likert-Balken. Die Bildunterschrift steht in `res/figure_1_caption.txt`.

## Projektstruktur

```text
LIS_Umfrage/
├── .gitignore
├── .Renviron       # lokaler Datenpfad, nicht versioniert
├── README.md
├── auswertung.R    # gesamte Auswertung
└── res/            # alle Resultate, vom Skript erzeugt, nicht versioniert
```

## Einrichtung

Im Projektordner wird eine Datei `.Renviron` mit dem Pfad zur CSV-Datei der Originaldaten angelegt:

```sh
LIS_DATEN_PFAD=/pfad/zu/results-survey821811.csv
```

Das Skript liest den Pfad beim Start aus und bricht ab, wenn er nicht gesetzt ist:

```r
data_path <- Sys.getenv("LIS_DATEN_PFAD")
assert(data_path != "", "LIS_DATEN_PFAD ist nicht gesetzt, siehe README.")
```

## Ausführung

Voraussetzung sind R, die Pakete `tidyverse` und `gt` sowie die eingerichtete `.Renviron`. Das Skript wird aus dem Projektordner gestartet, damit R die Datei findet:

```sh
Rscript auswertung.R
```

Da alle Resultate aus dem Skript entstehen, lässt sich `res/` jederzeit löschen und durch einen neuen Lauf vollständig wiederherstellen.

## Grafiken für CCLM

Vorgaben aus den [Instructions for Authors](https://www.degruyterbrill.com/publication/journal_key/CCLM/downloadAsset/CCLM_Instructions%20for%20Authors.pdf) (Stand 16.05.2023):

- **Dateien:** Jede Abbildung ist eine eigene Datei im Format `.png`.
- **Auflösung in Zielgröße:**  600 dpi.
- **Schrift:** Mindestens 6 pt, bevorzugt Arial oder Helvetica, einheitlich in allen Abbildungen.
- **Gestaltung:** Abbildungen haben keinen Rahmen. Balkendiagramme verwenden Farben oder Muster statt Graustufen. Farbe ist online und im Druck kostenfrei.
- **Nummerierung:** Fortlaufend mit arabischen Ziffern ab 1. Teilabbildungen werden mit Großbuchstaben beschriftet (Figure 1A, B).
- **Legenden und Bildunterschriften:** Symbolerklärungen stehen nach Möglichkeit in der Abbildung. Die Bildunterschrift steht nicht in der Abbildung.

Festlegungen für dieses Projekt:

- Alle Grafiken verwenden dasselbe ggplot2-Theme mit Arial, das einmal zentral in `auswertung.R` definiert ist.
- Beschriftungen sind auf Englisch, weil CCLM nur englischsprachige Artikel veröffentlicht.

```r
ggsave(
  "res/figure_1.png", plot = fig_1,
  device = "png", width = 170, height = 120, units = "mm",
  dpi = 600
)
```

## Tabellen und Zusatzmaterial für CCLM

- **Tabellen:** Sie werden in einem bearbeitbaren Format (Word oder LaTeX) eingereicht, nicht als Bild oder Excel-Datei. Farben, Schattierungen, vertikale Linien und andere Zellrahmen sind zu vermeiden.
- **Zusatzmaterial:** Der Fragebogen kann als Supplementary Material eingereicht werden, ebenso Code, Datensätze und zusätzliche Abbildungen. Jede Datei darf höchstens 10 MB groß sein.

Das Skript schreibt jede Tabelle als eigene HTML-Datei nach `res/`:

```r
tab_1 |>
  gt() |>
  gtsave("res/table_1.html")
```

So kommt eine Tabelle ins Manuskript:

1. `res/table_1.html` im Browser öffnen.
2. Die Tabelle markieren und kopieren.
3. Im Google Doc einfügen.

Für die Einreichung wird das Google Doc als Word-Datei heruntergeladen. Damit liegen die Tabellen im geforderten bearbeitbaren Format vor.

Vor der Einreichung sind diese Vorgaben mit der aktuellen Fassung der Richtlinien abzugleichen.
