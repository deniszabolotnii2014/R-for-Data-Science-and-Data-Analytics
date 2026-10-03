# Task 3 - Integrarea surselor CSV, Excel si MySQL
# 1. PACHETE 
# Instalare (o singura data, daca este necesar):
# install.packages(c("tidyverse", "readxl", "DBI", "RMySQL"))

library(tidyverse)
library(readxl)
library(DBI)
library(RMySQL)

# 2. DIRECTOR SI FISIERE 
# Pune cele 3 fisiere de date in acelasi director cu acest script.
# Daca folosesti RStudio, poti seta manual directorul de lucru:
# setwd("C:/cale/catre/folder")

csv_file  <- "RDSDA4_Task_03-books_3.csv"
xlsx_file <- "RDSDA4_Task_03-user_rentals_3.xlsx"

# 3. IMPORT CSV 
# Prima linie este un titlu suplimentar, iar a doua linie contine antetul.
# "N/A" si celulele goale sunt tratate ca valori lipsa (NA).
books_df <- read_csv(
  csv_file,
  skip = 1,
  na = c("", "N/A"),
  show_col_types = FALSE
)

# Verificari cerute de tema
glimpse(books_df)
spec(books_df)

# Standardizam numele si tipurile pentru analiza.
books_df <- books_df %>%
  rename(publication_year = published) %>%
  mutate(publication_year = as.integer(publication_year))

# 4. IMPORT EXCEL 
# Coloanele sunt setate explicit pentru a evita interpretarea gresita
# a ID-urilor si a datelor.
rentals_df <- read_excel(
  xlsx_file,
  sheet = "users_and_rentals",
  na = c("", "N/A"),
  col_types = c(
    "numeric",   # id
    "text",      # firstname
    "text",      # lastname
    "text",      # address
    "text",      # city
    "numeric",   # rental_book_id
    "date"       # rental_date
  )
)

rentals_df <- rentals_df %>%
  mutate(
    id = as.integer(id),
    rental_book_id = as.integer(rental_book_id),
    rental_date = as.Date(rental_date)
  )

glimpse(rentals_df)

# 5. CONECTARE LA MYSQL 
# IMPORTANT:
# Inainte de executarea acestei sectiuni:
# 1) importa RDSDA4_Task_03-library_3.sql in MySQL;
# 2) verifica faptul ca baza se numeste "library";
# 3) completeaza user/password de mai jos cu datele tale.
#
# Exemplu:
# con <- dbConnect(
#   RMySQL::MySQL(),
#   dbname = "library",
#   host = "localhost",
#   port = 3306,
#   user = "root",
#   password = "23121990"
# )

con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "library",
  host = "localhost",
  port = 3306,
  user = "root",
  password = "23121990"
)

# Verificam tabelele existente.
dbListTables(con)

# Preluam tabelele relevante.
books_db <- dbReadTable(con, "book")
users_db <- dbReadTable(con, "user")
rentals_db <- dbReadTable(con, "rental")

# Inchidem conexiunea dupa preluarea datelor.
dbDisconnect(con)

# 6. NORMALIZAREA DATELOR DIN MYSQL
books_db <- books_db %>%
  mutate(
    book_id = as.integer(book_id),
    published = as.integer(published)
  )

users_db <- users_db %>%
  mutate(
    user_id = as.integer(user_id),
    city_id = as.integer(city_id)
  )

rentals_db <- rentals_db %>%
  mutate(
    rental_id = as.integer(rental_id),
    rental_date = as.Date(rental_date),
    user_id = as.integer(user_id),
    book_id = as.integer(book_id)
  )

# 7. ANALIZA CERUTA 
nr_books_csv <- nrow(books_df)
nr_books_db <- nrow(books_db)

earliest_rental_xlsx <- min(rentals_df$rental_date, na.rm = TRUE)
latest_rental_xlsx   <- max(rentals_df$rental_date, na.rm = TRUE)

nr_rental_records_xlsx <- nrow(rentals_df)

distinct_books_xlsx <- rentals_df %>%
  filter(!is.na(rental_book_id)) %>%
  summarise(n = n_distinct(rental_book_id)) %>%
  pull(n)

distinct_books_db <- rentals_db %>%
  summarise(n = n_distinct(book_id)) %>%
  pull(n)

distinct_users_xlsx <- n_distinct(rentals_df$id)

# Pentru "East Albert si fara carte imprumutata in prezent",
# folosim informatia din Excel: un utilizator fara rental_book_id
# nu are o carte asociata inregistrarii curente.
east_albert_no_rental_xlsx <- rentals_df %>%
  filter(city == "East Albert", is.na(rental_book_id)) %>%
  summarise(n = n_distinct(id)) %>%
  pull(n)

# Verificare suplimentara direct in baza de date:
# utilizatorii din East Albert care nu apar deloc in rental.
east_albert_no_rental_db <- users_db %>%
  inner_join(
    dbReadTable(
      dbConnect(
        RMySQL::MySQL(),
        dbname = "library",
        host = "localhost",
        port = 3306,
        user = "root",
        password = "23121990"
      ),
      "city"
    ),
    by = "city_id"
  ) %>%
  filter(name == "East Albert") %>%
  anti_join(rentals_db, by = "user_id") %>%
  summarise(n = n_distinct(user_id)) %>%
  pull(n)

# 8. DISCREPANTE / CONSISTENTA 
books_difference <- nr_books_csv - nr_books_db
books_rental_difference <- distinct_books_xlsx - distinct_books_db

# 9. REZULTATE FINALE
rezultate <- tibble(
  indicator = c(
    "Numar carti - CSV books_df",
    "Numar carti - MySQL books_db",
    "Diferenta numar carti CSV vs MySQL",
    "Cea mai veche data de imprumut din Excel",
    "Cea mai recenta data de imprumut din Excel",
    "Numar total inregistrari in rentals_df",
    "Numar carti distincte imprumutate - Excel",
    "Numar carti distincte imprumutate - MySQL",
    "Diferenta carti distincte imprumutate Excel vs MySQL",
    "Numar utilizatori distincti in Excel",
    "Utilizatori East Albert fara carte - Excel",
    "Utilizatori East Albert fara rental - MySQL"
  ),
  valoare = c(
    nr_books_csv,
    nr_books_db,
    books_difference,
    as.character(earliest_rental_xlsx),
    as.character(latest_rental_xlsx),
    nr_rental_records_xlsx,
    distinct_books_xlsx,
    distinct_books_db,
    books_rental_difference,
    distinct_users_xlsx,
    east_albert_no_rental_xlsx,
    east_albert_no_rental_db
  )
)

print(rezultate)

# 10. EXPORT CSV 
write_csv(
  rezultate,
  "Denis_Zabolotnii_Rezultate_Task3.csv"
)

# Optional: export separat al cartilor lipsa/diferentei dintre surse.
xlsx_book_ids <- rentals_df %>%
  filter(!is.na(rental_book_id)) %>%
  distinct(rental_book_id) %>%
  pull(rental_book_id)

db_book_ids <- rentals_db %>%
  distinct(book_id) %>%
  pull(book_id)

discrepante_carti <- tibble(
  book_id = sort(unique(c(setdiff(xlsx_book_ids, db_book_ids),
                          setdiff(db_book_ids, xlsx_book_ids)))),
  sursa = case_when(
    book_id %in% setdiff(xlsx_book_ids, db_book_ids) ~ "Excel",
    book_id %in% setdiff(db_book_ids, xlsx_book_ids) ~ "MySQL",
    TRUE ~ "Ambele"
  )
)

write_csv(
  discrepante_carti,
  "Denis_Zabolotnii_Discrepante_Carti_Task3.csv"
)

