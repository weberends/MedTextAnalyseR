print("Importing BERTopic...")
from bertopic import BERTopic
print("Importing KeyBERTInspired...")
from bertopic.representation import KeyBERTInspired
print("Importing CountVectorizer...")
from sklearn.feature_extraction.text import CountVectorizer


def bertopic_unsupervised(docs, meta,
                          nr_topics="auto",
                          language="multilingual",
                          embedding_model_name="all-MiniLM-L6-v2",
                          top_n_words=10):
    """
    Unsupervised BERTopic pipeline using user-defined parameters
    """

    # Initieel model
    topic_model = BERTopic(
        nr_topics=nr_topics,
        language=language,
        embedding_model=embedding_model_name,
        top_n_words=top_n_words
    )

    # Fit model
    topics, probs = topic_model.fit_transform(docs)
    topic_info = topic_model.get_topic_info()
    document_info = topic_model.get_document_info(docs)
    document_info_with_meta = document_info.join(meta)

    # Alternative representation (KeyBERT)
    representation_model = KeyBERTInspired()
    topic_model_keybert = BERTopic(
        embedding_model=representation_model,
        language=language,
        top_n_words=top_n_words
    )
    topics_keybert, probs_keybert = topic_model_keybert.fit_transform(docs)
    document_info_keybert = topic_model_keybert.get_document_info(docs)
    topic_info_keybert = topic_model_keybert.get_topic_info()

    return {
        "topics": topics,
        "probs": probs,
        "document_info": document_info_with_meta.to_dict("records"),
        "topic_info": topic_info.to_dict("records"),
        "topics_keybert": topics_keybert,
        "probs_keybert": probs_keybert,
        "document_info_keybert": document_info_keybert.to_dict("records"),
        "topic_info_keybert": topic_info_keybert.to_dict("records")
    }
