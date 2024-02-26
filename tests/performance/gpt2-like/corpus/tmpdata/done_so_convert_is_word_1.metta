;                                           (convert_to_metta_file  done_so_convert_is_word_1 $_371146 tests/performance/gpt2-like/corpus/tmpdata/done_so_convert_is_word_1.pl tests/performance/gpt2-like/corpus/tmpdata/done_so_convert_is_word_1.metta)
  !(encoding iso-latin-1)

  (= (with_self  (pllm *) (is-word $_))  
    (dumpST)
    (break))


  !(dynamic (with_self  (so-convert *) (/ is-word 1)))
  !(multifile (with_self  (so-convert *) (/ is-word 1)))
  !(module-transparent (with_self  (so-convert *) (/ is-word 1)))



