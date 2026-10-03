
# Instalare, dacă este necesar:
# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)

# 1. Încărcarea datelor

users_raw <- read_csv(
  "users.csv",
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

rentals_raw <- read_csv(
  "rentals.csv",
  na = c("", "NA", "N/A"),
  show_col_types = FALSE
)

books_raw <- read_csv(
  "books_20.csv",
  na = c("", "NA", "N/A"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# Verificarea inițială a structurii
glimpse(users_raw)
glimpse(rentals_raw)
glimpse(books_raw)

summary(users_raw)
summary(rentals_raw)
summary(books_raw)


# 2. Verificarea valorilor duplicate


# Utilizatori cu ID-uri duplicate
duplicated_users <- users_raw %>%
  count(user_id) %>%
  filter(n > 1)

print(duplicated_users)

# Cărți cu ID-uri duplicate
duplicated_books <- books_raw %>%
  count(id) %>%
  filter(n > 1)

print(duplicated_books)


# 3. Curățarea tabelului users

# member_since este transformat în tip Date.
# Pentru ID-urile duplicate păstrăm prima apariție,
# deoarece nu există un câmp care să indice care versiune este mai nouă.

users_clean <- users_raw %>%
  mutate(
    member_since = dmy(member_since),
    age = as.numeric(age),
    books_read_yearly = as.numeric(books_read_yearly)
  ) %>%
  arrange(user_id) %>%
  distinct(user_id, .keep_all = TRUE)


# 4. Curățarea tabelului books

# Coloana ??? nu este relevantă pentru analiză.
# Păstrăm doar informațiile necesare.

books_clean <- books_raw %>%
  transmute(
    book_id = as.integer(id),
    title = title,
    author = na_if(author, "N/A"),
    published = published,
    genre = na_if(genre, "N/A")
  ) %>%
  # Pentru ID-urile duplicate păstrăm prima înregistrare.
  distinct(book_id, .keep_all = TRUE)


# 5. Transformarea rentals din format wide în format tidy

# Inițial, un rând poate conține două împrumuturi:
# book_id_1 și book_id_2.
#
# pivot_longer() transformă aceste coloane astfel încât
# fiecare rând să reprezinte un singur împrumut.

rentals_tidy <- rentals_raw %>%
  pivot_longer(
    cols = matches("^(book_id|duration_days|return_date|returned_late)_"),
    names_to = c(".value", "loan_number"),
    names_pattern = "^(book_id|duration_days|return_date|returned_late)_(\\d+)$"
  ) %>%
  mutate(
    loan_number = as.integer(loan_number),
    book_id = as.integer(book_id),
    duration_days = as.numeric(duration_days),
    return_date = dmy(return_date),
    returned_late = as.logical(returned_late)
  ) %>%
  filter(!is.na(book_id)) %>%
  select(
    user_id,
    loan_number,
    book_id,
    duration_days,
    return_date,
    returned_late
  )

# Numărul de rânduri înainte și după pivotare
cat("Număr rânduri inițiale în rentals:",
    nrow(rentals_raw), "\n")

cat("Număr de împrumuturi după pivotare:",
    nrow(rentals_tidy), "\n")


# 6. Legarea împrumuturilor de utilizatori

rentals_users_df <- rentals_tidy %>%
  left_join(users_clean, by = "user_id")

cat(
  "Număr de observații în rentals_users_df:",
  nrow(rentals_users_df), "\n"
)

# Utilizatorii din rentals care nu există în users
users_missing_from_users_file <- rentals_tidy %>%
  distinct(user_id) %>%
  anti_join(users_clean, by = "user_id")

print(users_missing_from_users_file)

# Verificăm dacă există utilizatori fără date asociate
rentals_users_df %>%
  filter(is.na(age)) %>%
  distinct(user_id) %>%
  print()


# 7. Legarea datelor de cărți

full_data_df <- rentals_users_df %>%
  left_join(books_clean, by = "book_id")

cat(
  "Număr de observații în full_data_df:",
  nrow(full_data_df), "\n"
)

cat(
  "Număr de observații în rentals_users_df:",
  nrow(rentals_users_df), "\n"
)

# Cărți împrumutate care nu există în books_2.0.csv
books_missing_from_books_file <- rentals_tidy %>%
  distinct(book_id) %>%
  anti_join(books_clean, by = "book_id")

print(books_missing_from_books_file)


# 8. Întrebarea 1:
# Care sunt cele mai populare genuri și pentru cine?
# Numărul de împrumuturi pentru fiecare combinație utilizator-gen

loans_by_user_genre <- full_data_df %>%
  filter(!is.na(genre)) %>%
  count(user_id, genre, name = "number_of_loans") %>%
  arrange(desc(number_of_loans), user_id)

print(loans_by_user_genre)

# Popularitatea totală a genurilor
genre_popularity <- full_data_df %>%
  filter(!is.na(genre)) %>%
  count(genre, name = "number_of_loans") %>%
  arrange(desc(number_of_loans))

print(genre_popularity)

# Utilizatorul sau utilizatorii care au împrumutat cel mai mult
# din fiecare gen

genre_fans <- loans_by_user_genre %>%
  group_by(genre) %>%
  filter(number_of_loans == max(number_of_loans)) %>%
  ungroup() %>%
  arrange(genre)

print(genre_fans)


# 9. Întrebarea 2:
# Cum variază numărul de cărți returnate de la o lună la alta?

monthly_returns <- rentals_tidy %>%
  filter(!is.na(return_date)) %>%
  mutate(
    month_number = month(return_date),
    month = factor(
      month(
        return_date,
        label = TRUE,
        abbr = FALSE
      ),
      levels = month(
        seq.Date(
          from = as.Date("2023-01-01"),
          by = "month",
          length.out = 12
        ),
        label = TRUE,
        abbr = FALSE
      )
    )
  ) %>%
  count(month_number, month, name = "number_of_returns") %>%
  arrange(month_number) %>%
  select(month, number_of_returns)

print(monthly_returns)

# 10. Întrebarea 3:
# Avem utilizatori care revin după cărți similare?

# Un utilizator este considerat fidel unui gen dacă
# are mai mult de un împrumut din acel gen.

returning_by_genre <- full_data_df %>%
  filter(!is.na(genre)) %>%
  count(user_id, genre, name = "number_of_loans") %>%
  filter(number_of_loans > 1) %>%
  arrange(user_id, desc(number_of_loans))

print(returning_by_genre)

# Numărul utilizatorilor care revin pentru fiecare gen
returning_users_summary <- returning_by_genre %>%
  count(genre, name = "number_of_returning_users") %>%
  arrange(desc(number_of_returning_users))

print(returning_users_summary)


# 11. Întrebarea 4:
# Care sunt utilizatorii care vin o singură dată?
# Numărăm împrumuturile fiecărui utilizator.
# Sunt considerați utilizatori fantomă cei cu un singur împrumut.

ghost_users <- rentals_tidy %>%
  count(user_id, name = "number_of_loans") %>%
  filter(number_of_loans == 1) %>%
  arrange(user_id)

print(ghost_users)

# Dacă dorim și datele demografice ale acestor utilizatori:

ghost_users_details <- ghost_users %>%
  left_join(users_clean, by = "user_id") %>%
  arrange(user_id)

print(ghost_users_details)


# 12. Întrebarea 5:
# Din ce țări provin cei mai mulți utilizatori cu împrumuturi?

# Numărăm utilizatorii distincți care au cel puțin un împrumut
# și pentru care țara este cunoscută.

users_by_country <- rentals_users_df %>%
  filter(!is.na(country)) %>%
  distinct(user_id, country) %>%
  count(country, name = "number_of_users") %>%
  arrange(desc(number_of_users))

print(users_by_country)

# Țara sau țările cu cei mai mulți utilizatori activi

countries_with_most_users <- users_by_country %>%
  filter(number_of_users == max(number_of_users))

print(countries_with_most_users)

# Împrumuturi provenite de la utilizatori care nu apar în users.csv
unknown_country_loans <- rentals_users_df %>%
  filter(is.na(country)) %>%
  count(user_id, name = "number_of_loans") %>%
  arrange(desc(number_of_loans))

print(unknown_country_loans)


# 13. Întrebarea 6:
# Tabel cu un rând per utilizator și o coloană per gen

user_genre_table <- full_data_df %>%
  filter(!is.na(genre)) %>%
  count(user_id, genre, name = "number_of_loans") %>%
  pivot_wider(
    names_from = genre,
    values_from = number_of_loans,
    values_fill = 0
  ) %>%
  arrange(user_id)

print(user_genre_table)

# 14. Verificări finale
# Numărul total de împrumuturi
total_loans <- rentals_tidy %>%
  summarise(total = n())

print(total_loans)

# Numărul de utilizatori distincți cu împrumuturi
total_active_users <- rentals_tidy %>%
  summarise(total = n_distinct(user_id))

print(total_active_users)

# Numărul de cărți cu gen cunoscut și necunoscut
book_genre_check <- full_data_df %>%
  summarise(
    loans_with_known_genre = sum(!is.na(genre)),
    loans_with_unknown_genre = sum(is.na(genre))
  )

print(book_genre_check)

# Procentul de împrumuturi returnate târziu
late_returns <- rentals_tidy %>%
  summarise(
    total_returns = sum(!is.na(returned_late)),
    late_returns = sum(returned_late == TRUE, na.rm = TRUE),
    late_return_percentage =
      100 * late_returns / total_returns
  )

print(late_returns)


# 15. Salvarea rezultatelor
write_csv(genre_popularity, "rezultat_popularitate_genuri.csv")
write_csv(loans_by_user_genre, "rezultat_genuri_utilizatori.csv")
write_csv(monthly_returns, "rezultat_returnari_lunare.csv")
write_csv(returning_by_genre, "rezultat_utilizatori_recurenti.csv")
write_csv(ghost_users_details, "rezultat_utilizatori_fantoma.csv")
write_csv(users_by_country, "rezultat_utilizatori_tari.csv")
write_csv(user_genre_table, "rezultat_tabel_utilizatori_genuri.csv")