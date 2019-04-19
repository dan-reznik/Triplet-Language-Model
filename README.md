Triplet Language Model
================

Source code for the triplet language model available as a shiny app [here](https://dreznik.shinyapps.io/triplet/).

<img src="cervantes.jpg" width="33%" style="display: block; margin: auto;" />

The algo, implemented in `source/language_model.R`, is based on [3-grams](https://en.wikipedia.org/wiki/N-gram), and described as follows:

-   Departing from a corpus of text, build a table of unique 3-token sequences (w1,w2,w3), annotating each with the probability of w3, given (w1,w2)
-   The novel utterance is a simple Markov-walk on this table, i.e., start with an initial (w1,w2).
-   For all possible w3’s that may follow, select one via an unfair RNG, weighted by the w3’s probs.
-   Repeat the process starting with (w2,w3), to produce a w4, etc.
-   Notes:
    1.  within a novel utterance, all sequential triples (w1,w2,w3) are guaranteed to have been penned by the original author, whereas a given quadruple (w1,w2,w3,w4) may not.
    2.  a novel utterance may contain less than max words when the Markov-walk hits a dead-end, i.e., for some (w1,w2) there is no w3 on the table of triples. This happens when a previous (w0,w1) produced a w2 such that the pair (w1,w2) comes from the end of a chapter.
-   Future work: upgrade triplet-based language model to RNN or LSTM
