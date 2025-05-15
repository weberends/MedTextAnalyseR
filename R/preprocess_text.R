#' Preprocess text column
#'
#' Preprocesses text by optionally lowercasing and removing stopwords, punctuation, and/or digits.
#'
#' @param df A dataframe with `id` and `text` columns.
#' @param stopword_list A character vector of stopwords (default = dutch_stopwords).
#' @param remove_stopwords Logical, whether to remove stopwords. Default TRUE.
#' @param remove_punctuation Logical, whether to remove standalone punctuation. Default TRUE.
#' @param remove_digits Logical, whether to remove standalone digits. Default TRUE.
#' @param tolower Logical, whether to force lowercase before tokenisation. Default TRUE.
#' @param output Character, "tokenised" to return a tidy df with separate word counts (i.e., tokenised), "preprocessed" to return df with SOEP_clean column (i.e., the complete SOEP text, but cleaned). Default is "preprocessd".
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' preprocess_text(example_soep_texts)
preprocess_text <- function(df,
                            stopword_list = dutch_stopwords,
                            remove_stopwords = TRUE,
                            remove_punctuation = TRUE,
                            remove_digits = TRUE,
                            tolower = TRUE,
                            output = c("preprocessed", "tokenised")) {
  library(dplyr)
  library(tidytext)
  library(stringr)

  output <- match.arg(output)

  if (!all(c("id", "text") %in% colnames(df))) {
    stop("Dataframe must contain columns named 'id' and 'text'.")
  }

  # Inform user if default stopword list is being used
  if (missing(stopword_list)) {
    message("Using default stopword list: 'dutch_stopwords'. To include custom stopwords, provide 'dutch_stopwords_extra' as the stopword_list argument.")
  }

  stopwords_tbl <- tibble(word = stopword_list)

  df_tokenised <- df

  # Make lowercase if requested
  if (tolower) {
    df_tokenised <- df_tokenised %>%
      mutate(text = str_to_lower(text))
  }

  # Tokenise
  df_tokenised <- df_tokenised %>%
    unnest_tokens(word, text)

  # Optional: remove stopwords, numbers, and punctuation
  if (remove_stopwords) {
    df_tokenised <- df_tokenised %>%
      anti_join(stopwords_tbl, by = "word")
  }

  if (remove_digits) {
    df_tokenised <- df_tokenised %>%
      filter(!str_detect(word, "^[0-9]+$"))
  }

  if (remove_punctuation) {
    df_tokenised <- df_tokenised %>%
      filter(!str_detect(word, "^[[:punct:]]+$"))
  }

  # Make df_preprocessed
  df_preprocessed <- df_tokenised %>%
    group_by(id) %>%
    summarise(text_clean = paste(word, collapse = " "), .groups = "drop") %>%
    right_join(df, by = "id")

  # Return dependent on user input
  if (output == "tokenised") {
    return(df_tokenised)
  } else if (output == "preprocessed") {
    return(df_preprocessed)
  }
  else {
    stop("Invalid output type. Choose 'tokenised' or 'preprocessed'.")
  }
}
