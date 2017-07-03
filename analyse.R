setwd("~/RProjects/JKRowling")

library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)

rm(list=ls())
theme_set(theme_minimal())


#### Wczytanie danych ####
# lista plików
files <- list.files("books/")

# atrubuty z nazwy pliku do tabelki
df <- data_frame(file_name = files)
df <- strsplit(gsub(".txt", "", df$file_name, fixed = TRUE), " - ", fixed = TRUE) %>%
  sapply(function(x) x) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  cbind(df)

colnames(df) <- c("year", "author", "title", "file_name")
df$year <- as.integer(df$year)

# wycztanie tresci ksiązek
books <- data_frame()
for(i in 1:nrow(df)) {
  book <- read_lines(paste0("books/", df[i, "file_name"]))

  # usunięcie pustych linii
  book <- book[nchar(book) != 0]

  # cały tekst do jednego ciągu
  book <- paste(book, collapse = " ")

  # podział na zdania
  phrase <- strsplit(book, ".", fixed = TRUE)[[1]]
  # usunięcie zbędnych spacji
  phrase <- trimws(phrase)

  # konwersja znaków specjalnych
  phrase <- gsub("—", "-", phrase, fixed = TRUE)
  phrase <- gsub("”", "\"", phrase, fixed = TRUE)
  phrase <- gsub("“", "\"", phrase, fixed = TRUE)
  phrase <- gsub("’", "'", phrase, fixed = TRUE)
  phrase <- gsub("ö", "o", phrase, fixed = TRUE)
  phrase <- gsub("Ö", "O", phrase, fixed = TRUE)

  # usunięcie pustych linii (wynik wielokropków)
  phrase <- phrase[nchar(phrase) != 0]

  # zbicie zdań w ramkę danych - zbudowanie ramki dla książki
  book_df <- data_frame(text = phrase,
                        year =  df[i, "year"],
                        author = df[i, "author"],
                        title = df[i, "title"])
  # numery kolejnych zdań
  book_df$n_phrase <- 1:nrow(book_df)
  # procent książki (jako procent zdań)
  book_df$p_phrase <- round(100 * 1:nrow(book_df)/nrow(book_df))

  # dodanie do paczki ze wszystkimi książkami
  books <- bind_rows(books, book_df)
}

# które książki to Harry Potter?
books$HP <- substr(books$title, 1, 12) == "Harry Potter"

# usuwamy zbędne dane
rm(book, book_df, i, df, files, phrase)


#### poszczególne słowa ####
# podział na słowa
## wszystkie słowa
words_all <- books %>%
  unnest_tokens(word, text, token="words") %>%
  select(year, HP, title, word, n_phrase, p_phrase)

## bez stop words
words <- filter(words_all, !word %in% stop_words$word)

# liczba słów
words_count <- count(words, year, HP, title, word) %>% ungroup()

# kolejność książek wg lat
books_order <- words_count %>%
  select(year, title) %>%
  distinct() %>%
  arrange(year)

words_count$title <- factor(words_count$title, levels = books_order$title)

# chmurki słów dla kolejnych książek
by(words_count, words_count$title,
   function(x) {
     wordcloud(x$word, x$n,
               max.words = 100,
               scale=c(2.8, 0.7),
               colors = RColorBrewer::brewer.pal(9, "Greens")[4:9])
     text(0.05, 0.95,
          unique(paste0(x$title, " (", x$year, ")")),
          col = "red", adj = c(0,0))
   }
)



#### Liczba zdań w każdej z książek ####
books %>%
  group_by(year, title, HP) %>%
  summarise(phrases = max(n_phrase)) %>%
  ungroup() %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_bar(aes(title, phrases, fill=HP), stat="identity",
           color="darkgreen", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0)) +
  labs(x="", y="Liczba zdań") +
  scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "lightgreen"))


#### Liczba słów w każdej z książek ####
words_all %>%
  group_by(year, title, HP) %>%
  summarise(words = n()/1000) %>%
  ungroup() %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_bar(aes(title, words, fill=HP), stat="identity",
           color="darkgreen", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0)) +
  labs(x="", y="Liczba słów (w tysiącach)") +
  scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "lightgreen"))


##### liczba słów w zdaniu w każdej z książek ####
## rozkład
books %>%
  # liczba słów w zdaniu
  mutate(nwords = str_count(text, "\\S+")) %>%
  # kolejność wg lat
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_histogram(aes(nwords), binwidth = 1) +
  geom_vline(aes(xintercept = median(nwords)), color = "red") +
  geom_vline(aes(xintercept = mean(nwords)), color = "green") +
  xlim(0, 75) +
  facet_wrap(~title) +
  labs(x="Liczba słów w zdaniu", y="Liczba zdań")

## dla całej książki
books %>%
  mutate(nwords = str_count(text, "\\S+")) %>%
  group_by(year, title) %>%
  summarise(srednia = round(mean(nwords), 1),
            mediana = median(nwords)) %>%
  ungroup() %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_line(aes(title, srednia, group=1), color = "green") +
  geom_line(aes(title, mediana, group=1), color = "red") +
  ylim(0, 20) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0)) +
  geom_text(x=11, y=12, label = "mediana", color="red") +
  geom_text(x=11, y=17.5, label = "średnia", color = "green") +
  labs(x="", y="Liczba słów w zdaniu")



#### Imiona w HP ####
# lista imion
HP_persons <- tolower(c("Harry", "Ron", "Hermione", "Hagrid", "Malfoy",
                        "Snape", "Dumbledore", "Voldemort",
                        "Vernon", "McGonagall", "Trelawney",
                        "Umbridge", "Petunia", "Lupin"))

# kiedy pojawiają się imiona?
words %>%
  filter(HP) %>%
  filter(word %in% HP_persons) %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_jitter(aes(p_phrase, word, color=word),
              height = 0.25, alpha = 0.1, show.legend = FALSE) +
  labs(x = "Procent książki", y="") +
  facet_wrap(~title, ncol = 7) +
  theme(panel.border = element_rect(color = "black", fill = NA))

# jak często pojawiają się imiona
words %>%
  filter(HP) %>%
  filter(word %in% HP_persons[1:8]) %>%
  count(title, word) %>%
  ungroup() %>%
  group_by(title) %>%
  mutate(n = 100*n/sum(n)) %>%
  ungroup() %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_line(aes(title, n, color = word, group = word), size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0)) +
  labs(x="", y="Procent wzmianek (wśród badanych postaci)", color="Postać")


#### Miejsca - kiedy się pojawiają, jak często? ####
# lista miejsc: privet drive, diagon alley, hogwarts, azkaban, ministry of magic, grimmauld place

# kiedy pojawiają się miejsca
books %>%
  filter(HP) %>%
  mutate(text = tolower(text)) %>%
  mutate(`privet drive` = grepl("privet drive", text, fixed = TRUE),
         `diagon alley` = grepl("diagon alley", text, fixed = TRUE),
         `hogwarts` = grepl("hogwarts", text, fixed = TRUE),
         `azkaban` = grepl("azkaban", text, fixed = TRUE),
         `ministry of magic` = grepl("ministry of magic", text, fixed = TRUE),
         `grimmauld place` = grepl("grimmauld place", text, fixed = TRUE)) %>%
  select(-year, -author, -text, -n_phrase, -HP) %>%
  gather(key = "key", value = "val", -title, -p_phrase) %>%
  filter(val) %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_jitter(aes(p_phrase, key, color=key),
              height = 0.25, show.legend = FALSE) +
  labs(x = "Procent książki", y="") +
  facet_wrap(~title, ncol=7) +
  theme(panel.border = element_rect(color = "black", fill = NA))

#### Zaklęcia - kiedy się pojawiają, jak często? ####
# lista zaklęć: expecto patronum, accio, wingardium leviosa, expelliarmus,
# stupefy, lumos, alohomora, imperio, crucio, avada kedavra

# kiedy pojawiają się zaklęcia
books %>%
  filter(HP) %>%
  mutate(text = tolower(text)) %>%
  mutate(`expecto patronum` = grepl("expecto patronum", text, fixed = TRUE),
         `accio` = grepl("accio", text, fixed = TRUE),
         `wingardium leviosa` = grepl("wingardium leviosa", text, fixed = TRUE),
         `expelliarmus` = grepl("expelliarmus", text, fixed = TRUE),
         `stupefy` = grepl("stupefy", text, fixed = TRUE),
         `alohomora` = grepl("alohomora", text, fixed = TRUE),
         `imperio` = grepl("imperio", text, fixed = TRUE),
         `crucio` = grepl("crucio", text, fixed = TRUE),
         `avada kedavra` = grepl("avada kedavra", text, fixed = TRUE),
         `lumos` = grepl("lumos", text, fixed = TRUE)) %>%
  select(-year, -author, -text, -n_phrase, -HP) %>%
  gather(key = "key", value = "val", -title, -p_phrase) %>%
  filter(val) %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_jitter(aes(p_phrase, key, color=key),
              height = 0.25, show.legend = FALSE) +
  labs(x = "Procent książki", y="") +
  facet_wrap(~title, ncol=7) +
  theme(panel.border = element_rect(color = "black", fill = NA))




#### Sentiment ####

# w ramach poszczególnych książek, w ramach całej twórczości

senti_words <- left_join(words,
                         filter(sentiments, lexicon == "AFINN") %>% select(word, score),
                         by = "word") %>%
  filter(!is.na(score)) %>%
  group_by(year, title, p_phrase) %>%
  summarise(senti = mean(score)) %>%
  ungroup()

# sentyment po książce
senti_words %>%
  mutate(p_phrase = round(p_phrase)) %>%
  group_by(title, p_phrase) %>%
  summarise(senti = mean(senti)) %>%
  ungroup() %>%
  mutate(title = factor(title, levels = books_order$title)) %>%
  ggplot() +
  geom_line(aes(p_phrase, senti)) +
  geom_hline(yintercept = 0, color="red") +
  facet_wrap(~title) +
  labs(x="Procent książki", y="Sentiment")


# sentyment po roku
senti_words %>%
  mutate(p_phrase = round(p_phrase)) %>%
  group_by(year) %>%
  summarise(senti = mean(senti)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(year, senti)) +
  geom_hline(yintercept = 0, color="red") +
  labs(x="Rok", y="Sentiment")


#### Czy autorstwo jest takie same ####
# ("słownik autora") - po pojedynczych słowach i po bigramach

# częstość użycia danego słowa w książkach o HP
words_count_HP <- words_count %>%
  filter(HP) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(n = n/sum(n))

words_count_nonHP <- words_count %>%
  filter(!HP) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(n = n/sum(n))

# 10% słów z większego zbioru
max_words <- round(0.1 * max(nrow(words_count_HP), nrow(words_count_nonHP)))

# złączenie po 10% najpopularniejszych słow w obu zbiorach (HP i nonHP)
word_count_comp <- inner_join(words_count_HP %>% top_n(max_words, wt = n),
                              words_count_nonHP %>% top_n(max_words, wt = n),
                              by="word")

# podobieństwo jako współczynnik korelacji pomiędzy częstością użycia słów
cor(word_count_comp$n.x, word_count_comp$n.y)

# odległość między częstościami użycia słów
word_count_comp$diff <- abs(word_count_comp$n.x - word_count_comp$n.y)

word_count_comp %>%
  ggplot() +
  geom_histogram(aes(diff), bins = 50) +
  scale_x_log10()

# przynależność do grup - k-means
word_count_comp$clusters <- kmeans(word_count_comp[, 2:4], 5)$cluster

# popularność słów
word_count_comp %>%
  ggplot() +
  geom_point(aes(n.x, n.y, color=as.factor(clusters))) +
  scale_y_log10() +
  scale_x_log10() +
  labs(x="Udział procentowy słowa w ksiazkach z Harrym Potterem",
       y="Udział procentowy słowa w ksiazkach bez Harrego Pottera",
       color = "Grupa słów")

# która grupa jest najbardziej u góry? (największe częstości słów)
clu <- word_count_comp %>% filter(n.y == max(n.y)) %>% .$clusters

# jakie to słowa?
word_count_comp %>% filter(clusters == clu) %>% .$word

# jakie słowa występują równie często w obu grupach (przekątna wykresu)?
word_count_comp %>% filter(diff <= quantile(diff, 0.02)) %>% .$word


#### bigramy ####

biwords <- books %>%
  unnest_tokens(word, text, token = "ngrams", n=2) %>%
  separate(word, c("word1", "word2")) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(word, word1, word2, sep = " ") %>%
  select(year, HP, title, word, n_phrase, p_phrase)

biwords_count <- count(biwords, year, HP, title, word) %>% ungroup()

by(biwords_count, biwords_count$title,
   function(x) {
     wordcloud(x$word, x$n,
               max.words = 100,
               scale=c(2.8, 0.7),
               colors = RColorBrewer::brewer.pal(9, "Greens")[4:9])
     text(0.05, 0.95,
          unique(paste0(x$title, " (", x$year, ")")),
          col = "red", adj = c(0,0))
   })


biwords_count_HP <- biwords_count %>%
  filter(HP) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(n = n/sum(n))

biwords_count_nonHP <- biwords_count %>%
  filter(!HP) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(n = n/sum(n))

biwords_count_comp <- inner_join(biwords_count_HP,
                                 biwords_count_nonHP,
                                 by="word")

biwords_count_comp %>%
  ggplot() +
  geom_point(aes(n.x, n.y)) +
  scale_y_log10() +
  scale_x_log10() +
  labs(x="Udział procentowy bigramu w ksiazkach z Harrym Potterem",
       y="Udział procentowy bigramu w ksiazkach bez Harrego Pottera")


cor(biwords_count_comp$n.x, biwords_count_comp$n.y)

