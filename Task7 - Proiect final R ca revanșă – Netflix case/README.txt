NETFLIX CASE – SOLUȚIE FINALĂ

Fișiere:
- netflix_modeling.R – scriptul complet R
- concluzie.txt – executive summary
- cele 5 CSV-uri originale trebuie păstrate în același folder când rulezi scriptul

Pachete R:
install.packages("tidyverse")
library(tidyverse)

Rulare:
1. Deschide netflix_modeling.R în RStudio.
2. Pune titles.csv, genres.csv, countries.csv, ratings.csv și certifications.csv în același folder.
3. Rulează scriptul de la început până la sfârșit.
4. Scriptul generează:
   01_trend_imdb.png
   02_genre_comparison.png
   03_residuals.png
   04_new_predictions.png

Întrebarea aleasă este întrebarea 2 din brief.
Modelul este o regresie liniară multiplă deoarece variabila țintă, imdb_score, este numerică.
