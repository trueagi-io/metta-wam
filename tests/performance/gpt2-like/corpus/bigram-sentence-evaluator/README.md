## Bigram Model Sentence Evaluator
**Problem:** Prolog project to evaluate the correctness of english sentence using bigram model

**Approach:**
The project constructs a Prolog bigram language model using small [DA_Corpus.text](DA_Corpus.txt) corpus.

Steps taken ([bigram_model.pl](bigram_model.pl)):

1. The [DA_Corpus.text](DA_Corpus.txt) corpus is normalized using [unix](unix_commands.txt) commands.
2. Created a prolog readable [unigram.pl](unigrams.pl) and [bigram.pl](bigrams.pl) database from normalized corpus.
3. In the final step, implemented [bigram_model.pl](bigram_model.pl) which computes the probability of any word sequence, of any size, via a predicate called **calc_prob/2**. The predicate calc_prob/2 works in log space and applies laplace smoothing on fly to compute the probability of given sentence.

**Sample outputs:** 
As shown in the output below, sentence like "the book fell" will have better value than "i fell on the book"

![output1](output/output1.png)

Similarly the sentence like "the book that he wanted fell on my feet" will have better value than "book the that he wanted fell on my feet"

![output2](output/output2.png)



##
## Sentence Tagger

**Problem:** Identify all possible tags for given sentence with there correctness probability.

**Approach:** The project makes use of Viterbi algorithm to compute all the possible tag list with probability for given sentence.
[tagger.pl](hmm_tagger.pl)

**Sample outputs:**

![output1](sentence-tagging/output.PNG)



##
## Word Similarity
**Problem:** Prolog project for finding the cosine similarity between two given words and finding most similar words of a given word

**Approach:**
  The project applies cosine distance rule to find the probability of two words similarity. This is then extended to identify and rank all the similar words for given word.
  

