#SECTION 1

## Counting words

library(janeaustenr)
library(stringr)
library(purrr)

extract_words <- function(book_name) {
     # extract the text of the book
     text <- subset(austen_books(), book == book_name)$text
     # extract words from the text and convert to lowercase
     str_extract_all(text, boundary("word")) %>% unlist %>% tolower
}

janeausten_words <- function() {
     # Names of the six books contained in janeaustenr
     books <- austen_books()$book %>% unique %>% as.character
     # Vector of words from all six books
     words <- sapply(books, extract_words) %>% unlist
     return(words)
}

words <- janeausten_words()

select_words <- function(letter, words, min_length = 1) {
     min_length_words <- words[nchar(words) >= min_length]
     grep(paste0("^", letter), min_length_words, value = TRUE)
}
     
max_frequency <- function(letter, words, min_length = 1) {
     w <- select_words(letter, words = words, min_length = min_length)
     frequency <- table(w)    
     frequency[which.max(frequency)]
}

max_frequency(letter = "a", words = words, min_length = 5)

map(letters, .f=max_frequency, words=words, min_length = 5)
lapply(letters, max_frequency, words=words, min_length = 5)


##Producing random normal distributions

mean_of_norm <- function(n){
     random_numbers <- rnorm(n)
     mu <- mean(random_numbers)
     return(mu)
}

mean_of_norm(10000)
n_replicates <- 50

result <- vector("numeric", n_replicates)
for (i in 1:n_replicates){
     result[i] <- mean_of_norm(1000)
}
mean(result)

map_dbl(.x = rep(1000,n_replicates), ~mean_of_norm(.x))












              