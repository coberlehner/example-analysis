library(dplyr)
library(stringr)
library(ggplot2)

# Schritt 1
data <- readLines("input_data.txt")

# Schritt 2
# Trennung der Statements
split_data <- unlist(strsplit(paste(data, collapse = "\n"), "_________+"))
print(split_data)

x <- split(data, cumsum(data == ""))[[1]]

# Erstellung einer Tabelle
extract_data <- function(statement) {
  datum <- gsub(".*Datum:\\s*([0-9.]+).*", "\\1", statement)
  kategorie <- gsub(".*Kategorie:\\s*([A-Za-z]+).*", "\\1", statement)
  value <- as.numeric(gsub(".*Value:\\s*([0-9]+).*", "\\1", statement))
  confidental <- gsub(".*Confidental:\\s*(YES|NO).*", "\\1", statement)
  outcome <- as.numeric(gsub(".*Outcome:\\s*([0-9]+).*", "\\1", statement))
  titel <- gsub(".*Titel:\\s*(.*?)\\n.*", "\\1", statement)
  text <- gsub("(?s).*?Titel:.*?\\n(.*?)(\\nStatement-ID:|$)", "\\1", statement, perl = TRUE)
  statement_id <- gsub(".*Statement-ID:\\s*([0-9]+).*", "\\1", statement)
  return(data.frame(
    Datum = datum,
    Kategorie = kategorie,
    Value = value,
    Confidental = confidental,
    Outcome = outcome,
    Titel = titel,
    Text = text,
    Statement_ID = statement_id,
    stringsAsFactors = FALSE
  ))
}
table <- do.call(rbind, lapply(statements, extract_data))
print(table)

# Schritt 3 und 4
# Hinzufügen der Variablen "positive_sentiment" und "negative_sentiment"
pos <- c("positive", "growth", "prosperity","increase", "strong", 
         "low inflation", "upturn", "more vacancies")
neg <- c("negative", "contraction", "decrease", "weak",
         "high inflation", "few vacancies")

table <- table %>%
  rowwise() %>%
  mutate(
    positive_sentiment = sum(str_count(Text, paste(pos, collapse = "|"))),
    negative_sentiment = sum(str_count(Text, paste(neg, collapse = "|")))
  ) %>%
print(table)


# Schritt 5
# Explorative Datenanalyse

table <- table %>%
  mutate(Confidental = if_else(Confidental == "YES", 1, 0))

# Tabelle deskriptiver Statistiken
table1 <- table %>%
  group_by(Kategorie) %>%
  summarise(
    avg_val = mean(Value),
    avg_conf = mean(Confidental),
    avg_out = mean(Outcome),
    avg_pos = mean(positive_sentiment),
    count_pos = sum(positive_sentiment > 0),
    avg_neg = mean(negative_sentiment),
    count_neg = sum(negative_sentiment > 0)
  )

knitr::kable(table1, digits = 2)

# Korrelationen
cor(table$Value, table$positive_sentiment, use = "complete.obs")
cor(table$Value, table$negative_sentiment, use = "complete.obs")

# Zeitliche Trends
table$Datum <- as.Date(table$Datum, format = "%d.%m.%Y")

table %>%
  mutate(Monat = format(Datum, "%Y-%m")) %>%
  group_by(Monat) %>%
  summarise(avg_value = mean(Value)) %>%
  ggplot(aes(x = Monat, y = avg_value)) +
  geom_point(size = 2) +
  labs(title = "Durchschnittlicher Wert pro Monat", x = "Monat", y = "Durchschnittlicher Wert") +
  theme_minimal()

table %>%
  mutate(Monat = format(Datum, "%Y-%m")) %>%
  group_by(Monat) %>%
  summarise(avg_pos = mean(positive_sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = Monat, y = avg_pos)) +
  geom_point(size = 2) +
  labs(title = "Durchschnittliche Anzahl positiv konnotierter Wörter pro Monat", x = "Monat", y = "Positiv konnotierte Wörter") +
  theme_minimal()

table %>%
  mutate(Monat = format(Datum, "%Y-%m")) %>%
  group_by(Monat) %>%
  summarise(avg_neg = mean(negative_sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = Monat, y = avg_neg)) +
  geom_point(size = 2) +
  labs(title = "Durchschnittliche Anzahl negativ konnotierter Wörter pro Monat", x = "Monat", y = "Negativ konnotierte Wörter") +
  theme_minimal()


