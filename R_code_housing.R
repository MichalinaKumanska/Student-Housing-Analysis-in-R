install.packages("ggplot2")
library(ggplot2)

#wczytywanie danych

data = read.table("R_2.csv", header = TRUE, sep = ";")
head(data)

#porządkowanie kolejności

data$ocena_teraz <- factor(data$ocena_teraz, levels = c("Bardzo źle", "Źle", "Ani dobrze, ani źle", "Dobrze", "Bardzo dobrze"))
data$ocena_wstecz <- factor(data$ocena_wstecz, levels = c("Bardzo źle", "Źle", "Ani dobrze, ani źle", "Dobrze", "Bardzo dobrze"))
data$czynsz_teraz <- factor(data$czynsz_teraz, levels = c("od 0 do 500zł", "od 500zł do 1000zł", "od 1000zł do 1500zł", "od 1500zł do 2000zł", "od 2000 do 2500zł", "2500zł lub więcej"))
data$czynsz_wstecz <- factor(data$czynsz_wstecz, levels = c("od 0 do 500zł", "od 500zł do 1000zł", "od 1000zł do 1500zł", "od 1500zł do 2000zł", "od 2000 do 2500zł", "2500zł lub więcej"))
data$sytuacja_teraz <- factor(data$sytuacja_teraz, levels = c("Mieszkam w akademiku", "Wynajmuję lokal z czterema lub więcej osobami", "Wynajmuję lokal z trzema osobami", "Wynajmuję lokal z dwiema osobami","Wynajmuję lokal z jedną osobą", "Wynajmuję lokal sam"))
data$sytuacja_wstecz <- factor(data$sytuacja_wstecz, levels = c("Mieszkałem/am w akademiku", "Wynajmowałem/am lokal z czterema lub więcej osobami", "Wynajmowałem/am lokal z trzema osobami", "Wynajmowałem/am lokal z dwiema osobami","Wynajmowałem/am lokal z jedną osobą", "Wynajmowałem/am lokal sam"))

#sprawdzenie danych

names(data)
summary(data)
column_names <- names(data)
print(names(data))
print(data$ocena_teraz)
print(data$ocena_wstecz)
print(data$czynsz_teraz)
print(data$czynsz_wstecz)
print(data$sytuacja_teraz)
print(data$sytuacja_wstecz)

#wykresy

#wykres słupkowy porównujący sytuację mieszkaniową studentów w tym roku z wysokością płaconego czynszu (również w tym roku)

ggplot(data, aes(x = sytuacja_teraz, fill = czynsz_teraz)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Wykres słupkowy porównujący sytuację mieszkaniową studentów w tym roku z wysokością płaconego czynszu (również w tym roku)",
    x = "Sytuacja mieszkaniowa studentów w tym roku",
    y = "Ilość osób",
    fill = "Wysokość czynszu w tym roku"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Obrót etykiet na osi X
  )

#wykres słupkowy porównujący sytuację mieszkaniową studentów w roku poprzednim z wysokością płaconego czynszu (również w roku poprzednim)

ggplot(data, aes(x = sytuacja_wstecz, fill = czynsz_wstecz)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Wykres słupkowy porównujący sytuację mieszkaniową studentów w roku poprzednim z wysokością płaconego czynszu (również w roku poprzednim)",
    x = "Sytuacja mieszkaniowa studentów w roku poprzednim",
    y = "Ilość osób",
    fill = "Wysokość czynszu w roku poprzednim"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#wykres kołowy dla sytuacji mieszkaniowej w tym roku


ggplot(data, aes(x = "", fill = sytuacja_teraz)) +
  geom_bar(width = 1, color = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5), size = 3) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Sytuacja mieszkniowa w tym roku",
    fill = "Sytuacja mieszkniowa w tym roku",
    y = "Liczba odpowiedzi"
  )

#wykres kołowy dla sytuacji mieszkaniowej w roku poprzednim


ggplot(data, aes(x = "", fill = sytuacja_wstecz)) +
  geom_bar(width = 1, color = "black") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5), size = 3) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Sytuacja mieszkniowa w roku poprzednim",
    fill = "Sytuacja mieszkniowa w roku poprzednim",
    y = "Liczba odpowiedzi"
  )

#wykres punktowty porównujący wysokość czynszu w tym roku do wysokości czynszu w roku poprzednim

ggplot(data, aes(x = czynsz_teraz, y = czynsz_wstecz)) +
  geom_jitter(color = "blue", alpha = 0.5, width = 0.2, height = 0.2) +
  theme_minimal() +
  labs(title = "Zmiana czynszu: Teraz vs Rok poprzedni", x = "Czynsz obecnie", y = "Czynsz rok temu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

#histogram dla czynsz teraz


ggplot(data, aes(x = czynsz_teraz)) +
geom_bar(fill = "skyblue", color = "black") +  
theme_minimal() +  
labs(
title = "Wysokość czynszu w tym roku",
    x = "Wysokość czynszu",
    y = "Liczba osób"
)

#histogram rok poprzedni

ggplot(data, aes(x = czynsz_wstecz)) +
geom_bar(fill = "lightpink", color = "black") +  
theme_minimal() +  
labs(
title = "Wysokość czynszu w roku poprzednim",
    x = "Wysokość czynszu",
    y = "Liczba osób"
)

