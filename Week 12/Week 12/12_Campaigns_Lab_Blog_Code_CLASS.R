#' @title GOV 1347: Week 12 (Campaigns) Laboratory Session
#' @author Matthew E. Dardet
#' @date December 6, 2024

####----------------------------------------------------------#
#### Preamble.
####----------------------------------------------------------#

# Load libraries.
library(httr)
library(jsonlite)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(stm)
library(text2vec)
library(tidyverse)
library(tm)
library(wordcloud)

# Key packages: 
# Quanteda (general text pre-processing): http://quanteda.io
# stm (structural topic modelling): https://cran.r-project.org/web/packages/stm/index.html
# tm (text mining): https://cran.r-project.org/web/packages/tm/index.html
# text2vec (general NLP/word embeddings): https://text2vec.org

####-------------------------------------------------------------------------#
#### Read data from pre-processed and unprocessed singular speeches. 
####-------------------------------------------------------------------------#

# Read processed speeches from 10/28. 
harris_1028_enos <- readtext("enos_processed_speeches/Harris_10_28.txt")
trump_1028_enos <- readtext("enos_processed_speeches/Trump_10_28.txt")

# Read unprocessed speeches from 10/28. 
harris_1028_raw <- readLines("harris_raw_speeches/Harris_10_28")
trump_1028_raw <- readLines("trump_raw_speeches/Trump_10_28")

####-------------------------------------------------------------------------#
#### Create document-feature matrices.
####-------------------------------------------------------------------------#

# Create corpus from Professor Enos' pre-processed data. 
harris_1028_tokens <- tokens(harris_1028_enos$text)
names(harris_1028_tokens) <- harris_1028_enos$doc_id
trump_1028_tokens <- tokens(trump_1028_enos$text)
names(trump_1028_tokens) <- trump_1028_enos$doc_id

# Further refine tokens by removing numbers, punctuation, stopwords, etc. 
harris_1028_tokens_processed <- tokens(harris_1028_enos$text, 
                                       remove_symbols = TRUE,
                                       remove_numbers = TRUE, 
                                       remove_punct = TRUE, 
                                       remove_separators = TRUE) |> 
  tokens_tolower() |> 
  tokens_remove(pattern = c("joe", "biden", "donald", "trump", "president", "kamala", "harris")) |> 
  tokens_remove(pattern = stopwords("en")) |> 
  tokens_select(min_nchar = 3)
names(harris_1028_tokens_processed) <- harris_1028_enos$doc_id

trump_1028_tokens_processed <- tokens(trump_1028_enos$text, 
                                      remove_symbols = TRUE,
                                      remove_numbers = TRUE, 
                                      remove_punct = TRUE, 
                                      remove_separators = TRUE) |> 
  tokens_tolower() |> 
  tokens_remove(pattern = c("joe", "biden", "donald", "trump", "president", "kamala", "harris")) |> 
  tokens_remove(pattern = stopwords("en")) |> 
  tokens_select(min_nchar = 3)
names(trump_1028_tokens_processed) <- trump_1028_enos$doc_id

# Keywords-in-context function to see whether/how often speech contains "economy", "foreign", other issues. 
kwic(harris_1028_tokens, pattern = "economy")
kwic(trump_1028_tokens, pattern = "economy")

kwic(harris_1028_tokens, pattern = "foreign")
kwic(trump_1028_tokens, pattern = "foreign")

kwic(harris_1028_tokens, pattern = "immigration")
kwic(harris_1028_tokens, pattern = "illegal")

kwic(trump_1028_tokens, pattern = "immigration")
kwic(trump_1028_tokens, pattern = "illegal")

kwic(harris_1028_tokens, pattern = "defense")
kwic(trump_1028_tokens, pattern = "defense")

kwic(harris_1028_tokens, pattern = "policy")
kwic(trump_1028_tokens, pattern = "policy")

####-------------------------------------------------------------------------#
#### Get topics/words for individual speeches from Harris and Trump.
####-------------------------------------------------------------------------#

# Get topics for individual speeches.
harris_1028_dfm <- dfm(harris_1028_tokens_processed)
trump_1028_dfm <- dfm(trump_1028_tokens_processed)

# Make feature co-occurrence matrix via word embeddings model. 
harris_1028_fcm <- fcm(harris_1028_dfm)
(topfeat <- names(topfeatures(harris_1028_fcm, 50)))
harris_1028_fcm_select <- fcm_select(harris_1028_fcm, pattern = topfeat, selection = "keep")
size <- log(colSums(harris_1028_fcm_select))
set.seed(02138)
textplot_network(harris_1028_fcm_select, min_freq = 0.8, vertex_size = size/max(size)*3)

trump_1028 <- fcm(trump_1028_dfm)
(topfeat <- names(topfeatures(trump_1028, 50)))
trump_1028_select <- fcm_select(trump_1028, pattern = topfeat, selection = "keep")
size <- log(colSums(trump_1028_select))
set.seed(02138)
textplot_network(trump_1028_select, min_freq = 0.8, vertex_size = size/max(size)*3)

####-------------------------------------------------------------------------#
#### Combine all speeches in document-feature matrices.
####-------------------------------------------------------------------------#

# Read raw files. 
harris_raw_files <- paste0("harris_raw_speeches/", list.files("harris_raw_speeches"))
harris_speeches_raw <- readtext(harris_raw_files)

trump_raw_files <- paste0("trump_raw_speeches/", list.files("trump_raw_speeches"))
trump_speeches_raw <- readtext(trump_raw_files)

# Get processed word tokens from speeches for each candidate. 
harris_tokens_processed <- tokens(harris_speeches_raw$text, 
                                  remove_symbols = TRUE,
                                  remove_numbers = TRUE, 
                                  remove_punct = TRUE, 
                                  remove_separators = TRUE) |> 
  tokens_tolower() |> 
  tokens_remove(pattern = c("joe", "biden", "donald", "trump", "president", "kamala", "harris")) |> 
  tokens_remove(pattern = stopwords("en")) |> 
  tokens_select(min_nchar = 6)

names(harris_tokens_processed) <- harris_speeches_raw$doc_id

trump_tokens_processed <- tokens(trump_speeches_raw$text, 
                                 remove_symbols = TRUE,
                                 remove_numbers = TRUE, 
                                 remove_punct = TRUE, 
                                 remove_separators = TRUE) |> 
  tokens_tolower() |> 
  tokens_remove(pattern = c("joe", "biden", "donald", "trump", "president", "kamala", "harris")) |> 
  tokens_remove(pattern = stopwords("en")) |> 
  tokens_select(min_nchar = 6)

names(trump_tokens_processed) <- trump_speeches_raw$doc_id

# Make document-feature matrices for each candidate. 
harris_dfm <- dfm(harris_tokens_processed)
trump_dfm <- dfm(trump_tokens_processed)

# Summarize word frequncies. 
freq_harris_dfm <- textstat_frequency(harris_dfm)
head(freq_harris_dfm, 10)
textplot_wordcloud(harris_dfm)

freq_trump_dfm <- textstat_frequency(trump_dfm)
head(freq_trump_dfm, 10)
textplot_wordcloud(trump_dfm)

# Visualize word "keyness for specific group of documents.
harris_keyness <- textstat_keyness(harris_dfm)
textplot_keyness(harris_keyness)

trump_keyness <- textstat_keyness(trump_dfm)
textplot_keyness(trump_keyness)













####-------------------------------------------------------------------------#
#### Structural topic modelling on speech corpi from Harris and Trump.
####-------------------------------------------------------------------------#

# Process documents internally to STM. 
harris_text_processed <- textProcessor(documents = harris_speeches_raw$text, 
                                       language = "en", 
                                       lowercase = TRUE, 
                                       removestopwords = TRUE, 
                                       removenumbers = TRUE, 
                                       removepunctuation = TRUE, 
                                       ucp = TRUE,
                                       wordLengths = c(4, Inf),
                                       customstopwords = c("joe", "biden", "donald", "trump", "president", "kamala", "harris"),
                                       stem = TRUE)
harris_out <- prepDocuments(harris_text_processed$documents, 
                            harris_text_processed$vocab, 
                            harris_text_processed$meta)
set.seed(02138)
harris_stm <- stm(documents = harris_out$documents, 
                  vocab = harris_out$vocab, 
                  K = 20, 
                  data = harris_out$meta, 
                  init.type = "Spectral")
labelTopics(harris_stm)
#' TODO: What manual labels would you give to these topics?

plot(harris_stm, type = "summary")
plot(harris_stm, type = "perspectives", topics = c(1, 19))
plot(harris_stm, type = "perspectives", topics = c(16, 8))
plot(harris_stm, type = "perspectives", topics = c(7, 6))
cloud(harris_stm, topic = 7)
cloud(harris_stm, topic = 10)

trump_text_processed <- textProcessor(documents = trump_speeches_raw$text, 
                                      language = "en", 
                                      lowercase = TRUE, 
                                      removestopwords = TRUE, 
                                      removenumbers = TRUE, 
                                      removepunctuation = TRUE, 
                                      ucp = TRUE,
                                      wordLengths = c(4, Inf),
                                      customstopwords = c("joe", "biden", "donald", "trump", "president", "kamala", "harris"),
                                      stem = TRUE)
trump_out <- prepDocuments(trump_text_processed$documents,
                           trump_text_processed$vocab,
                           trump_text_processed$meta)
set.seed(02138)
trump_stm <- stm(documents = trump_out$documents, 
                 vocab = trump_out$vocab, 
                 K = 20, 
                 data = trump_out$meta, 
                 init.type = "Spectral")
labelTopics(trump_stm)
#' TODO: What manual labels would you give to these topics?
#' 
plot(trump_stm, type = "summary")
plot(trump_stm, type = "perspectives", topics = c(14, 20))
plot(trump_stm, type = "perspectives", topics = c(8, 10))
plot(trump_stm, type = "perspectives", topics = c(12, 14))
cloud(trump_stm, topic = 1)
cloud(trump_stm, topic = 7)











####-------------------------------------------------------------------------#
#### LLM-based classification of speeches using Google Gemini.
####-------------------------------------------------------------------------#

# Step 1. If you are interested, get free trial of Google Gemini Advanced or any LLM where you can get an API key. 
#         Unfortunately, I am not sure if Harvard has any free ones. 
gemini_api_key <- "" # TODO: Put your API key here. 

# Step 2. Define a function that will call the Gemini API. Based on code from here: https://www.listendata.com/2023/12/google-gemini-r.html.
callGemini <- function(prompt, 
                       temperature = 0.5, 
                       max_output_tokens = 1024, 
                       api_key = gemini_api_key, 
                       model = "gemini-1.5-flash-latest") {
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
                   query = list(key = api_key),
                   content_type_json(),
                   encode = "json",
                   body = list(
                     contents = list(
                       parts = list(
                         list(text = prompt)
                       )),
                     generationConfig = list(
                       temperature = temperature,
                       maxOutputTokens = max_output_tokens
                     )
                   )
                 )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- httr::content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
}

# Step 3. Define a prompt that asks Gemini API to classify the main topics being mentioned in the processed 10/28 Harris/Trump speeches. 
harris_1028_prompt <- paste("This is a speech from a presidential candidate who ran in the 2024 U.S. Presidential Election. 
                             Please extract the topics being mentioned in the speech and order them in a numbered list: ", harris_1028_enos$text)

harris_1028_output <- callGemini(harris_1028_prompt)
cat(harris_1028_output)

trump_1028_prompt <- paste("This is a speech from a presidential candidate who ran in the 2024 U.S. Presidential Election. 
                            Please extract the topics being mentioned in the speech and order them in a numbered list: ", trump_1028_enos$text)

trump_1028_output <- callGemini(trump_1028_prompt)
cat(trump_1028_output)

# Step 4. See if LLMs can do what Vavreck (2009) did!
vavreck_style_prompt_harris <- paste("This is a speech from a presidential candidate who ran in the 2024 U.S. Presidential Election. 
                                     Please classify the speech by whether it is mostly about (1.) the candidates' personal traits; (2.) the economy; (3.) domestic policy; (4.) defense; or (5.) foreign policy. 
                                     Please just output which category the speech mostly corresponds to as well as a brief summary of why you classified the speech that way: ", harris_1028_enos$text)

harris_1028_vavreck_style_output <- callGemini(vavreck_style_prompt_harris)
cat(harris_1028_vavreck_style_output)


vavreck_style_prompt_trump <- paste("This is a speech from a presidential candidate who ran in the 2024 U.S. Presidential Election. 
                                    Please classify the speech by whether it is mostly about (1.) the candidates' personal traits; (2.) the economy; (3.) domestic policy; (4.) defense; or (5.) foreign policy. 
                                    Please just output which category the speech mostly corresponds to as well as a brief summary of why you classified the speech that way: ", trump_1028_enos$text)

trump_1028_vavreck_style_output <- callGemini(vavreck_style_prompt_trump)
cat(trump_1028_vavreck_style_output)

# Step 5. Make and run loop to classify each speech by Harris and Trump in the style of Vavreck (2009).
gemini_harris_vavreck_classifications <- rep(NA, length = length(harris_speeches_raw$text))

for (i in 1:length(harris_speeches_raw$text)) { 
  sample_prompt <- paste("This is a speech from a presidential candidate who ran in the 2024 U.S. Presidential Election. 
                  Please classify the speech by whether it is mostly about the candidates' personal traits; the economy; domestic policy; defense; or foreign policy. 
                  Please just output which category the speech mostly corresponds: ", harris_speeches_raw$text[i])
  output <- NA 
  while(!is.character(output)) {
    output <- callGemini(sample_prompt)  
  }
  # sum(!is.na(gemini_harris_vavreck_classifications)) # If loop fails; restart at new i
  
  gemini_harris_vavreck_classifications[i] <- output 
}

gemini_trump_vavreck_classifications <- rep(NA, length = length(trump_speeches_raw$text))
for (i in 55:length(trump_speeches_raw$text)) {
  sample_prompt <- paste("This is a speech from a presidential candidate who ran in the 2024 U.S. Presidential Election. 
                  Please classify the speech by whether it is mostly about the candidates' personal traits; the economy; domestic policy; defense; or foreign policy. 
                  Please just output which category the speech mostly corresponds: ", trump_speeches_raw$text[i])
  output <- NA 
  while(!is.character(output)) {
    output <- callGemini(sample_prompt)  
  }
  # sum(!is.na(gemini_trump_vavreck_classifications)) # If loop fails; restart at new i
  
  gemini_trump_vavreck_classifications[i] <- output 
}

# Remove "\n" demarcations from Gemini output/classifications.
harris_gemini_class_clean <- str_remove(gemini_harris_vavreck_classifications, "\n")
trump_gemini_class_clean <- str_remove(gemini_trump_vavreck_classifications, "\n")
trump_gemini_class_clean <- trump_gemini_class_clean[-13]
trump_gemini_class_clean <- trump_gemini_class_clean[!is.na(trump_gemini_class_clean)]

# Summarize results. 
harris_gemini_class_clean |> table()
harris_gemini_class_clean |> table() |> prop.table()*100

trump_gemini_class_clean |> table()
trump_gemini_class_clean |> table() |> prop.table()*100

# Visualize results. 
# TODO: 









