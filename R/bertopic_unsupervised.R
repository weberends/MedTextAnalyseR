#' Run Unsupervised BERTopic model via Python
#'
#' @param texts Character vector of documents
#' @param nr_topics How many topics to extract (default "auto")
#' @param language Language for BERTopic ("multilingual", "english", etc.)
#' @param embedding_model_name Sentence transformer model (e.g. "all-MiniLM-L6-v2")
#' @param top_n_words Number of words per topic
#'
#' @return A list with topics, probabilities, and topic/document info
#' @importFrom reticulate py
#' @export
bertopic_unsupervised <- function(df,
                                  text_column = "text_clean",
                                  nr_topics = "auto",
                                  language = "multilingual",
                                  embedding_model_name = "all-MiniLM-L6-v2",
                                  top_n_words = 10) {

  stopifnot(is.data.frame(df))
  stopifnot(text_column %in% names(df))

  texts <- df[[text_column]]
  meta <- df %>%
    select(-all_of(text_column))
  n_docs <- length(texts)

  start_time <- Sys.time()
  message("Running BERTopic on ", n_docs, " documents... this may take several minutes depending on the size of your data and embedding model.")

  # Assign in Python
  py$texts <- texts
  py$meta <- meta

  script_path <- system.file("python", "bertopic_unsupervised.py", package = "MedTextAnalyseR")
  reticulate::source_python(script_path)

  end_time <- Sys.time()
  message("BERTopic completed in ", round(difftime(end_time, start_time, units = "mins"), 2), " minutes.")

  pyresult <- py$bertopic_unsupervised(
    docs = py$texts,
    meta = py$meta,
    nr_topics = nr_topics,
    language = language,
    embedding_model_name = embedding_model_name,
    top_n_words = as.integer(top_n_words)
  )

  return(pyresult)
}

