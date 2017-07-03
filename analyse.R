setwd("~/RProjects/JKRowling")

library(tidyverse)
library(tidytext)
library(wordcloud)

rm(list=ls())
theme_set(theme_minimal())

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

	book <- book[nchar(book) != 0]
	book <- paste(book, collapse = " ")

	phrase <- strsplit(book, ".", fixed = TRUE)[[1]]
	phrase <- trimws(phrase)
	phrase <- phrase[nchar(phrase) != 0]

	book_df <- data_frame(text = phrase,
								 year =  df[i, "year"],
								 author = df[i, "author"],
								 title = df[i, "title"])
	book_df$n_phrase <- 1:nrow(book_df)
	book_df$p_phrase <- 100 * 1:nrow(book_df)/nrow(book_df)

	books <- bind_rows(books, book_df)
}

books$HP <- substr(books$title, 1, 12) == "Harry Potter"

rm(book, book_df, i, df, files, phrase)


words <- books %>%
	unnest_tokens(word, text, token="words") %>%
	filter(!word %in% stop_words$word) %>%
	select(year, HP, title, word, n_phrase, p_phrase)

words_count <- count(words, year, HP, title, word) %>% ungroup()

by(words_count, words_count$title,
	function(x) {
		wordcloud(x$word, x$n,
					 max.words = 100,
					 scale=c(2.0, 0.5))
		text(0.95, 0.95, unique(x$title))
	})


# sentiments
senti_words <- left_join(words,
								 filter(sentiments, lexicon == "AFINN") %>% select(word, score),
								 by = "word") %>%
	filter(!is.na(score)) %>%
	group_by(year, HP, title, p_phrase) %>%
	summarise(senti = mean(score)) %>%
	ungroup()


senti_words %>%
	mutate(p_phrase = round(p_phrase)) %>%
	group_by(year, HP, title, p_phrase) %>%
	summarise(senti = mean(senti)) %>%
	ungroup() %>%
	ggplot() +
	geom_line(aes(p_phrase, senti)) +
	geom_hline(yintercept = 0, color="red") +
	facet_wrap(~title)


senti_words %>%
	mutate(p_phrase = round(p_phrase)) %>%
	group_by(year) %>%
	summarise(senti = mean(senti)) %>%
	ungroup() %>%
	ggplot() +
	geom_line(aes(year, senti)) +
	geom_hline(yintercept = 0, color="red")


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

max_words <- round(0.1 * max(nrow(words_count_HP), nrow(words_count_nonHP)))


word_count_comp <- inner_join(words_count_HP %>% top_n(max_words, wt = n),
										words_count_nonHP %>% top_n(1000, wt = n),
										by="word")

word_count_comp$diff <- abs(word_count_comp$n.x - word_count_comp$n.y)

cor(word_count_comp$n.x, word_count_comp$n.y)

word_count_comp %>%
	ggplot() +
	geom_point(aes(n.x, n.y)) +
	scale_y_log10() +
	scale_x_log10()

word_count_comp %>%
	ggplot() +
	geom_histogram(aes(diff), bins = 50) +
	scale_x_log10()


word_count_comp <- inner_join(words_count_HP,
										words_count_nonHP,
										by="word")

word_count_comp %>%
	ggplot() +
	geom_point(aes(n.x, n.y)) +
	scale_y_log10() +
	scale_x_log10()

word_count_comp$diff <- abs(word_count_comp$n.x - word_count_comp$n.y)

cor(word_count_comp$n.x, word_count_comp$n.y)

word_count_comp %>%
	ggplot() +
	geom_density(aes(diff)) +
	scale_x_log10()


HP_persons <- tolower(c("Harry", "Ron", "Hermione", "Hagrid", "Malfoy", "Snape", "Dumbledore", "Voldemort"))

words %>%
	filter(HP) %>%
	filter(word %in% HP_persons) %>%
	arrange(year) %>%
	mutate(title = factor(title, levels = unique(title))) %>%
	ggplot() +
	geom_jitter(aes(p_phrase, word, color=word),
					height = 0.25, alpha = 0.1, show.legend = FALSE) +
	facet_wrap(~title)



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
					 scale=c(2.0, 0.5))
		text(0.95, 0.95, unique(x$title))
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
	scale_x_log10()

cor(word_count_comp$n.x, word_count_comp$n.y)

