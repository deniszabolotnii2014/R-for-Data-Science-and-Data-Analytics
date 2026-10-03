# ============================================================
# PROIECT FINAL: R ca revanșă – Netflix case
# Întrebarea aleasă: Care factori prezic cel mai bine scorul IMDb
#                   – genul, durata, anul, țara de producție?
# ============================================================

# 0. Pachete ----------------------------------------------------
# Dacă nu sunt instalate:
# install.packages(c("tidyverse"))

library(tidyverse)

# 1. Importul datelor -------------------------------------------
# Pune cele 5 CSV-uri în același folder cu acest script.
titles <- read_csv("titles.csv", show_col_types = FALSE)
genres <- read_csv("genres.csv", show_col_types = FALSE)
countries <- read_csv("countries.csv", show_col_types = FALSE)
ratings <- read_csv("ratings.csv", show_col_types = FALSE)
certifications <- read_csv("certifications.csv", show_col_types = FALSE)

# Verificarea structurii
glimpse(titles)
glimpse(genres)
glimpse(countries)
glimpse(ratings)
glimpse(certifications)

# 2. Pregătirea tabelelor ---------------------------------------
# genres.csv și countries.csv au mai multe rânduri pentru același titlu.
# Pentru model păstrăm o singură observație/titlu și alegem genul/țara
# care apare cel mai frecvent pentru acel titlu (mode).

get_mode <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

genre_main <- genres %>%
  group_by(id) %>%
  summarise(main_genre = get_mode(genre), .groups = "drop")

country_main <- countries %>%
  group_by(id) %>%
  summarise(main_country = get_mode(production_country), .groups = "drop")

# 3. Combinarea tabelelor --------------------------------------
# Sunt folosite mai multe operații left_join(), conform cerinței.
netflix <- titles %>%
  select(id, title, type, release_year, runtime) %>%
  left_join(ratings %>% select(id, imdb_score, imdb_votes,
                               tmdb_score, tmdb_popularity),
            by = "id") %>%
  left_join(genre_main, by = "id") %>%
  left_join(country_main, by = "id") %>%
  left_join(certifications %>% select(id, age_certification),
            by = "id")

# Verificare
glimpse(netflix)
summary(netflix)

# 4. Filtrare și selecție ---------------------------------------
# Pentru întrebarea aleasă avem nevoie de scor IMDb, durată, an,
# gen și țară. Eliminăm observațiile fără aceste valori.
model_data <- netflix %>%
  filter(
    !is.na(imdb_score),
    !is.na(runtime),
    !is.na(release_year),
    !is.na(main_genre),
    !is.na(main_country)
  ) %>%
  select(id, title, type, release_year, runtime,
         main_genre, main_country, imdb_score) %>%
  mutate(
    main_genre = as.factor(main_genre),
    main_country = as.factor(main_country)
  )

cat("Număr observații folosite în model:", nrow(model_data), "\n")

# 5. Explorarea datelor – tendință ------------------------------
# Tendința scorului IMDb în funcție de anul lansării.
p_trend <- model_data %>%
  group_by(release_year) %>%
  summarise(
    mean_imdb = mean(imdb_score),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = release_year, y = mean_imdb)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  labs(
    title = "Tendința scorului IMDb mediu în timp",
    x = "Anul lansării",
    y = "Scor IMDb mediu"
  ) +
  theme_minimal()

print(p_trend)
ggsave("01_trend_imdb.png", p_trend, width = 9, height = 5, dpi = 150)

# 6. Explorarea datelor – comparație ----------------------------
# Comparație între cele mai frecvente genuri.
top_genres <- model_data %>%
  count(main_genre, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(main_genre)

p_compare <- model_data %>%
  filter(main_genre %in% top_genres) %>%
  ggplot(aes(x = reorder(main_genre, imdb_score, FUN = median),
             y = imdb_score)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Comparația scorurilor IMDb între principalele genuri",
    x = "Gen",
    y = "Scor IMDb"
  ) +
  theme_minimal()

print(p_compare)
ggsave("02_genre_comparison.png", p_compare, width = 9, height = 5, dpi = 150)

# 7. Împărțire train/test ---------------------------------------
set.seed(42)

train_index <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.80 * nrow(model_data))
)

train <- model_data[train_index, ]
test  <- model_data[-train_index, ]

# 8. Model predictiv: regresie liniară multiplă -----------------
# Variabila țintă: imdb_score (numerică).
# Predictori: gen, durată, an și țara de producție.
model <- lm(
  imdb_score ~ main_genre + runtime + release_year + main_country,
  data = train
)

summary(model)

# 9. Evaluarea modelului pe date de test ------------------------
test$predicted_imdb <- predict(model, newdata = test)
test$residual <- test$imdb_score - test$predicted_imdb

MAE <- mean(abs(test$residual), na.rm = TRUE)
RMSE <- sqrt(mean(test$residual^2, na.rm = TRUE))
R2_test <- 1 - sum(test$residual^2, na.rm = TRUE) /
  sum((test$imdb_score - mean(test$imdb_score, na.rm = TRUE))^2, na.rm = TRUE)

cat("\n================ EVALUARE MODEL ================\n")
cat("MAE  =", round(MAE, 3), "\n")
cat("RMSE =", round(RMSE, 3), "\n")
cat("R2 test =", round(R2_test, 3), "\n")
cat("R2 train =", round(summary(model)$r.squared, 3), "\n")

# 10. Vizualizarea erorilor -------------------------------------
p_residuals <- ggplot(test, aes(x = predicted_imdb, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Reziduurile modelului pe setul de test",
    x = "Scor IMDb prezis",
    y = "Rezidual"
  ) +
  theme_minimal()

print(p_residuals)
ggsave("03_residuals.png", p_residuals, width = 9, height = 5, dpi = 150)

# 11. Identificarea predictorilor --------------------------------
coef_table <- summary(model)$coefficients
print(coef_table)

# 12. Predicție pe date noi – obligatoriu -----------------------
# Construim scenarii ipotetice pentru conținut nou.
new_content <- tibble(
  scenario = c(
    "Documentar scurt – SUA",
    "Dramă medie – Marea Britanie",
    "Acțiune lungă – SUA",
    "Comedie medie – Coreea de Sud"
  ),
  main_genre = factor(
    c("documentation", "drama", "action", "comedy"),
    levels = levels(train$main_genre)
  ),
  runtime = c(70, 105, 125, 95),
  release_year = c(2026, 2026, 2026, 2026),
  main_country = factor(
    c("US", "GB", "US", "KR"),
    levels = levels(train$main_country)
  )
)

# Pentru categorii care nu există în training, înlocuim cu NA și
# predict() va putea evidenția problema. În acest set, scenariile
# sunt alese dintre categorii existente.
new_content$predicted_imdb <- predict(model, newdata = new_content)

print(new_content)

p_new <- ggplot(new_content,
                aes(x = reorder(scenario, predicted_imdb),
                    y = predicted_imdb)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(predicted_imdb, 2)),
            hjust = -0.1) +
  labs(
    title = "Predicții IMDb pentru scenarii noi",
    x = "Scenariu",
    y = "Scor IMDb prezis"
  ) +
  ylim(0, max(new_content$predicted_imdb) + 1) +
  theme_minimal()

print(p_new)
ggsave("04_new_predictions.png", p_new, width = 10, height = 5, dpi = 150)

# 13. Concluzie pentru management -------------------------------
# Rezultatele numerice trebuie preluate din consola R după rulare.
cat("\n============================================================\n")
cat("RECOMANDARE PENTRU MANAGEMENT\n")
cat("Modelul arată că genul, durata, anul lansării și țara de\n")
cat("producție pot fi folosite pentru estimarea scorului IMDb,\n")
cat("dar puterea explicativă a modelului trebuie interpretată\n")
cat("împreună cu MAE, RMSE și R2 pe setul de test.\n")
cat("Predicțiile pe conținut nou trebuie tratate ca estimări,\n")
cat("nu ca garanții ale succesului.\n")
cat("============================================================\n")
