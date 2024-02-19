
```
SOC_crawler.perl
csplit -k --elide-empty-files  soc_corpus.txt '/TLINK/' {*} --prefix="train_on_so_corpus"  --suffix-format="%08d.txt"
```
