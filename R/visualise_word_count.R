#' Visualise word counts
#'
#' This function visualises word counts from a dataframe, either directly from raw text (SOEP_clean) or from already tokenised data (word counts).
#' Optionally, words can be grouped and faceted by a category such as gender, age group, etc.
#' The plot uses a viridis color palette for better readability.
#'
#' @param df A dataframe with either a `SOEP_clean` column (raw cleaned text) or `word` and `n` columns (already tokenised data).
#' @param top_n Integer. The number of top words to display per group. Default is 10.
#' @param title Character. The title of the plot. Default is "Top Words".
#' @param x_label Character. The label for the x-axis. Default is "Word".
#' @param y_label Character. The label for the y-axis. Default is "Count".
#' @param palette Character. The name of the viridis color palette to use ("viridis", "magma", "plasma", "inferno", "cividis", etc.). Default is "viridis".
#' @param by_category Optional. Name of a column (character) to group and facet by. Default is NULL (no grouping).
#'
#' @return A ggplot2 object showing the word counts.
#'
#' @details
#' If a `SOEP_clean` column is found, the function tokenises the text into individual words.
#' If a `word` column is found instead, it assumes the dataframe is already tokenised.
#' Words are counted, the top `n` words are selected, and optionally grouped and faceted by a category.
#'
#' @examples
#' # Without preprocessing (directly from SOEP_clean):
#' visualise_word_count(example_soep_texts)
#'
#' # With preprocessing:
#' result <- preprocess_text(example_soep_texts)
#' visualise_word_count(result)
#'
#' # Grouped and faceted by gender, with magma color palette:
#' visualise_word_count(result, by_category = "gender", palette = "magma")
#'
#' @export
visualise_word_count <- function(df,
                                 top_n = 10,
                                 title = "Top Words",
                                 x_label = "Word",
                                 y_label = "Count",
                                 palette = "viridis",
                                 by_category = NULL) {
  library(dplyr)
  library(ggplot2)
  library(tidytext)
  library(forcats)
  library(viridis)
  library(rlang)

  # Smart detection: if text_clean exists -> tokenise
  if ("text_clean" %in% colnames(df)) {
    tidy_df <- df %>%
      unnest_tokens(word, text_clean)

    if (!is.null(by_category)) {
      if (!(by_category %in% colnames(df))) {
        stop(paste0("Column ", by_category, " not found in dataframe."))
      }

       # Count words by category
      tidy_df <- tidy_df %>%
        count(.data[[by_category]], word, name = "n")
    } else {
      tidy_df <- tidy_df %>%
        count(word, name = "n")
    }

  } else if ("word" %in% colnames(df)) {
    # If already tokenised
    if (!is.null(by_category)) {
      if (!(by_category %in% colnames(df))) {
        stop(paste0("Column ", by_category, " not found in dataframe."))
      }
      tidy_df <- df %>%
        count(.data[[by_category]], word, name = "n")
    } else {
      tidy_df <- df %>%
        count(word, name = "n")
    }
  } else {
    stop("Input dataframe must contain either 'text_clean' or 'word' column.")
  }

  # Select top words
  if (!is.null(by_category)) {
    tidy_df <- tidy_df %>%
      group_by(.data[[by_category]]) %>%
      slice_max(n = top_n, order_by = n) %>%
      ungroup()
  } else {
    tidy_df <- tidy_df %>%
      slice_max(n = top_n, order_by = n)
  }

  # Sort words
  tidy_df <- tidy_df %>%
    mutate(word = fct_reorder(word, n))

  # Develop plot
  p <- ggplot(tidy_df, aes(x = word, y = n)) +
    geom_col(aes(fill = if (!is.null(by_category)) .data[[by_category]] else "1"), show.legend = FALSE) +
    coord_flip() +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal()

  if (!is.null(by_category)) {
    p <- p +
      facet_wrap(as.formula(paste0("~", by_category)), scales = "free_y") +
      scale_fill_viridis_d(option = palette)
  } else {
    p <- p +
      scale_fill_viridis_d(option = palette, guide = "none")
  }

  return(p)
}
