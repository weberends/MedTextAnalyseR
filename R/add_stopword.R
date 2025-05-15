#' Add new stopwords to dutch_stopwords_extra
#'
#' This function adds one or more new stopwords to the existing list of Dutch stopwords.
#' If a word already exists, a message will notify the user.
#'
#' @param ... One or more words to add as stopwords.
#'
#' @return A character vector with updated stopwords (`dutch_stopwords_extra`).
#' @export
#'
#' @examples
#' add_stopword("er", "bij", "over")
add_stopword <- function(...) {
  # Check if dutch_stopwords exists
  if (!exists("dutch_stopwords")) {
    stop("The object 'dutch_stopwords' must exist in your environment.")
  }

  # Collect new words
  new_words <- c(...)

  # Change to lowercase for consistency
  new_words <- tolower(new_words)

  # Existing stopwords
  all_current_stopwords <- dutch_stopwords

  if (exists("dutch_stopwords_extra")) {
    all_current_stopwords <- unique(c(dutch_stopwords, dutch_stopwords_extra))
  }

  # Check which words already exist
  already_exists <- new_words %in% all_current_stopwords
  new_unique_words <- new_words[!already_exists]

  # Provide message if a stopword already exists
  if (any(already_exists)) {
    message("The following stopwords were already registered and not added again: ",
            paste(new_words[already_exists], collapse = ", "))
  }

  # If there are new stopwords added: add
  if (length(new_unique_words) > 0) {
    if (exists("dutch_stopwords_extra")) {
      updated_stopwords <- unique(c(dutch_stopwords_extra, new_unique_words))
    } else {
      updated_stopwords <- unique(c(dutch_stopwords, new_unique_words))
    }

    assign("dutch_stopwords_extra", updated_stopwords, envir = .GlobalEnv)

    message(length(new_unique_words), " new stopword(s) added. Total stopwords: ", length(updated_stopwords))
  } else {
    message("No new stopwords added.")
  }

  return(invisible(dutch_stopwords_extra))
}
