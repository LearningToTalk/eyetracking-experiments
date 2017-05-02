Stimulus Timings
================
TJ Mahr
May 2, 2017

Over the course of the longitudinal study, we re-recorded the stimuli used in our word recognition experiments. I would like double-check when the stimuli were changed in each experiment. This scripts iterates over the wav files in each year of study x experiment x dialect version and computes the durations of the files.

Helpers
-------

``` r
library(purrr)
library(dplyr, warn.conflicts = FALSE)

# Measure duration of a wav file in ms
get_wave_duration <- function(filepath) {
  info <- tuneR::readWave(filepath, header = TRUE)
  # milliseconds = (1000 ms per second / samples per second) * num samples
  (1000 / info$sample.rate) * info$samples
}

find_waves <- function(dirpath) {
  list.files(dirpath, pattern = ".wav|.WAV", 
             recursive = TRUE, full.names = TRUE)
}
```

List all the wav files in each of the longitudinal study folders and compute the duration.

``` r
wav_files <- list(tp1 = "tp1", tp2 = "tp2", tp3 = "tp3") %>% 
  # find the files in each study, creating a data-frame for each study
  map(find_waves) %>% 
  map(~ data_frame(file = .x)) %>% 
  # collapse to a single data-frame
  bind_rows(.id = "study") %>% 
  mutate(
    token = basename(file),
    duration = map_dbl(file, get_wave_duration)) %>% 
  select(study, token, duration, file)
wav_files
#> # A tibble: 1,668 × 4
#>    study               token duration
#>    <chr>               <chr>    <dbl>
#> 1    tp1  AAE_cake_811_8.wav 712.4036
#> 2    tp1 AAE_cake_812_20.wav 712.4036
#> 3    tp1   AAE_check1_16.wav 913.8549
#> 4    tp1    AAE_check1_4.wav 913.8549
#> 5    tp1    AAE_check1_8.wav 913.8549
#> 6    tp1   AAE_check2_10.wav 913.8549
#> 7    tp1   AAE_Cim_331_6.wav 712.4036
#> 8    tp1  AAE_Cim_332_32.wav 712.4036
#> 9    tp1  AAE_dog_311_12.wav 712.4036
#> 10   tp1  AAE_dog_311_34.wav 712.4036
#> # ... with 1,658 more rows, and 1 more variables: file <chr>
```

Deduce dialect and experiment name.

``` r
wav_files <- wav_files %>% 
  mutate(
    task = file %>% stringr::str_extract("MP|RWL"),
    dialect = file %>% stringr::str_extract("AAE|SAE")) %>% 
  select(study, task, dialect, token, duration, file)
wav_files
#> # A tibble: 1,668 × 6
#>    study  task dialect               token duration
#>    <chr> <chr>   <chr>               <chr>    <dbl>
#> 1    tp1    MP     AAE  AAE_cake_811_8.wav 712.4036
#> 2    tp1    MP     AAE AAE_cake_812_20.wav 712.4036
#> 3    tp1    MP     AAE   AAE_check1_16.wav 913.8549
#> 4    tp1    MP     AAE    AAE_check1_4.wav 913.8549
#> 5    tp1    MP     AAE    AAE_check1_8.wav 913.8549
#> 6    tp1    MP     AAE   AAE_check2_10.wav 913.8549
#> 7    tp1    MP     AAE   AAE_Cim_331_6.wav 712.4036
#> 8    tp1    MP     AAE  AAE_Cim_332_32.wav 712.4036
#> 9    tp1    MP     AAE  AAE_dog_311_12.wav 712.4036
#> 10   tp1    MP     AAE  AAE_dog_311_34.wav 712.4036
#> # ... with 1,658 more rows, and 1 more variables: file <chr>
```

For each experiment, count the number of token *durations*. This count will reveal whether the tokens in an experiment were normalized to have the same duration.

``` r
wav_files %>% 
  group_by(study, task, dialect) %>% 
  summarise(num_durations = n_distinct(duration)) 
#> Source: local data frame [12 x 4]
#> Groups: study, task [?]
#> 
#>    study  task dialect num_durations
#>    <chr> <chr>   <chr>         <int>
#> 1    tp1    MP     AAE             3
#> 2    tp1    MP     SAE             3
#> 3    tp1   RWL     AAE             3
#> 4    tp1   RWL     SAE             3
#> 5    tp2    MP     AAE             3
#> 6    tp2    MP     SAE             3
#> 7    tp2   RWL     AAE            50
#> 8    tp2   RWL     SAE            50
#> 9    tp3    MP     AAE             3
#> 10   tp3    MP     SAE             3
#> 11   tp3   RWL     AAE            50
#> 12   tp3   RWL     SAE            50
```

It looks like the RWL experiments did *not* have normalized tokens.

Now, I would like to exclude the files that were not the main nouns used in the experiments. Our trials involved three parts: a carrier phrase, a noun, and an attention-getter phrase. For example, *find the* \[carrier phrase\] *dog* \[noun\] ... *check it out* \[attention getter\]. I'm going to clean up the token filenames and figure out a pattern that will filter out nouns versus non-nouns.

``` r
# Strip off junk from file name to see the main content of it
words <- wav_files %>% 
  getElement("token") %>% 
  stringr::str_replace_all("AAE|SAE", "") %>% 
  stringr::str_replace_all("\\d", "") %>% 
  stringr::str_replace_all("_[A-Z]_", "") %>% 
  stringr::str_replace_all("_", "") %>% 
  stringr::str_replace_all(".wav", "") %>% 
  unique() %>% 
  sort()
words
#>  [1] "bear"   "bee"    "bell"   "bep"    "bread"  "cake"   "check" 
#>  [8] "Check"  "cheese" "Cim"    "dog"    "dress"  "dRl"    "drum"  
#> [15] "duck"   "Fin"    "flag"   "fly"    "Fun"    "gek"    "gift"  
#> [22] "girl"   "giv"    "guk"    "heart"  "horse"  "kite"   "look"  
#> [29] "Look"   "neJ"    "pan"    "pear"   "pen"    "pum"    "rice"  
#> [36] "ring"   "San"    "See"    "shirt"  "shoes"  "soup"   "spoon" 
#> [43] "Sup"    "suz"    "swan"   "swing"  "sword"  "tag"    "this"  
#> [50] "van"    "vase"   "vef"    "wais"

non_noun <- "check|Check|Fin|Fun|look|Look|See|this"

noun_files <- wav_files %>%
  filter(!stringr::str_detect(token, non_noun))
```

Now, there are fewer file durations per experiment.

``` r
noun_files %>% 
  group_by(study, task, dialect) %>% 
  summarise(num_durations = n_distinct(duration)) 
#> Source: local data frame [12 x 4]
#> Groups: study, task [?]
#> 
#>    study  task dialect num_durations
#>    <chr> <chr>   <chr>         <int>
#> 1    tp1    MP     AAE             1
#> 2    tp1    MP     SAE             1
#> 3    tp1   RWL     AAE             1
#> 4    tp1   RWL     SAE             1
#> 5    tp2    MP     AAE             1
#> 6    tp2    MP     SAE             1
#> 7    tp2   RWL     AAE            48
#> 8    tp2   RWL     SAE            48
#> 9    tp3    MP     AAE             1
#> 10   tp3    MP     SAE             1
#> 11   tp3   RWL     AAE            48
#> 12   tp3   RWL     SAE            48
```

And I can get what I would like, which is the duration of the noun tokens in each experiment.

``` r
noun_files %>% 
  group_by(study, task, dialect) %>% 
  summarise(
    n_durations = n_distinct(duration),
    min_dur = min(duration) %>% round(1),
    med_dur = median(duration) %>% round(1),
    max_dur = max(duration) %>% round(1)) %>% 
  ungroup() %>% 
  knitr::kable()
```

| study | task | dialect |  n\_durations|  min\_dur|  med\_dur|  max\_dur|
|:------|:-----|:--------|-------------:|---------:|---------:|---------:|
| tp1   | MP   | AAE     |             1|     712.4|     712.4|     712.4|
| tp1   | MP   | SAE     |             1|     766.7|     766.7|     766.7|
| tp1   | RWL  | AAE     |             1|     566.7|     566.7|     566.7|
| tp1   | RWL  | SAE     |             1|     815.2|     815.2|     815.2|
| tp2   | MP   | AAE     |             1|     737.0|     737.0|     737.0|
| tp2   | MP   | SAE     |             1|     779.1|     779.1|     779.1|
| tp2   | RWL  | AAE     |            48|     344.5|     572.1|     819.5|
| tp2   | RWL  | SAE     |            48|     457.5|     545.7|     725.9|
| tp3   | MP   | AAE     |             1|     737.0|     737.0|     737.0|
| tp3   | MP   | SAE     |             1|     779.1|     779.1|     779.1|
| tp3   | RWL  | AAE     |            48|     344.5|     572.1|     819.5|
| tp3   | RWL  | SAE     |            48|     457.5|     545.7|     725.9|

Okay, that confirms what I thought. The stimuli were re-recorded after TP1, so the same files are used in TP2 and TP3.
