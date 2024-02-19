# The Self-dialogue Corpus

NEW CODE FOR USAGE @

https://gitlab.logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/tree/master/packs_xtra/logicmoo_chat


This is an early release of the Self-dialogue Corpus containing 24,165 conversations, or 3,653,313 words, across 23 topics. For more information on the data, please see [our corpus paper](https://arxiv.org/pdf/1809.06641.pdf) or [our submission to the Alexa Prize](http://alexaprize.s3.amazonaws.com/2017/technical-article/edina.pdf).

### Statistics


| Category | Count     |
---------- | -----------
| Topics   | 23        |
| Conversations | 24,165 |
| Words    | 3,653,313 |
| Turns    | 141,945   |
| Unique users | 2,717 |
| Conversations per user | ~9 |
| Unique tokens | 117,068 |

Topics include movies, music, sports, and subtopics within these.

### Using the data
* `corpus` contains the raw CSVs from Amazon Mechanical Turk, sorted by individual tasks (topics);
* `blocked_workers.txt` lists workers who did not comply with the requirements of the tasks, these are omitted by default;
* `get_data.py` is a preprocessing script which will format the CSVs into text (by default saved to `dialogues`), along with various options (see below).

#### `get_data.py`
Example usage: `python get_data.py`. This will by default read from `corpus` and write to `dialogues`. 

Optional arguments:
* `--inDir` Directory to read corpus from
* `--outDir` Directory to write processed files 
* `--output-naming` whether to name output files with integers (`integer`) or by assignment_id (`assignment_id`);
* `--remove-punctuation` removes punctuation from the output;
* `--set-case` sets case of output to `original`, `upper` or `lower`;
* `--exclude-topic` excludes any of the topics (or subdirectories of `corpus`), e.g. `--exclude-topic music`;
* `--include-only` includes only the given topics, e.g. `--include-only music`.

### Citation
For research using this data, please cite:
```
@article{fainberg2018talking,
  title={Talking to myself: self-dialogues as data for conversational agents},
  author={Fainberg, Joachim and Krause, Ben and Dobre, Mihai and Damonte, Marco and Kahembwe, Emmanuel and Duma, Daniel and Webber, Bonnie and Fancellu, Federico},
  journal={arXiv preprint arXiv:1809.06641},
  year={2018}
}
@article{krause2017edina,
  title={Edina: Building an Open Domain Socialbot with Self-dialogues},
  author={Krause, Ben and Damonte, Marco and Dobre, Mihai and Duma, Daniel and Fainberg, Joachim and Fancellu, Federico and Kahembwe, Emmanuel and Cheng, Jianpeng and Webber, Bonnie},
  journal={Alexa Prize Proceedings},
  year={2017}
}
```

```bash

function export-topic {
  python get_data.py --include-only $1 --outDir $1
  (cd $1 
    for each in *; do cat $each; echo "XXXXXXXXXXX"; done > ../train_from_topic_$1.txt 
  )
}

export-topic lady_gaga
export-topic rap_hiphop
export-topic disney
export-topic baseball
export-topic comedy
export-topic transition_music_movies
export-topic basketball
export-topic music
export-topic beatles
export-topic movies
export-topic pop
export-topic harry_potter
export-topic thriller
export-topic action
export-topic fast_furious
export-topic superhero
export-topic nfl_football
export-topic rock
export-topic star_wars
export-topic fashion
export-topic music_and_movies
export-topic icehockey

```

