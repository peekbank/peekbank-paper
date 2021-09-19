
## Links to parent report vocabulary data

```{r target_word_data}
target_words_large <- c("book","dog","frog","apple", "car",           
                        "shoe","kitty","cookie","carrot","birdie","banana","ball")

target_word_data_large <- aoi_data_joined %>%
  filter(english_stimulus_label %in% target_words_large) %>%
  group_by(t_norm, administration_id, age_group, english_stimulus_label) %>% 
  summarise(correct = mean(aoi == "target") / 
              mean(aoi %in% c("target","distractor"), na.rm=TRUE)) %>%
  group_by(t_norm,english_stimulus_label) %>% 
  summarise(correct = mean(correct, na.rm=TRUE), 
            n = n()) 
```

```{r get_aoas, eval=FALSE}
items <- wordbankr::get_item_data(language = "English (American)") %>%
  filter(definition %in% target_words_large)
ws_data <- wordbankr::get_instrument_data(language = "English (American)", 
                                          form = "WS", 
                                          administrations = TRUE, 
                                          items = items$item_id[items$form == "WS"]) %>%
  right_join(items)
wg_data <- wordbankr::get_instrument_data(language = "English (American)", 
                                          form = "WG", 
                                          administrations = TRUE, 
                                          items = items$item_id[items$form == "WG"]) %>%
  right_join(items)

wordbank_data <- bind_rows(ws_data, wg_data) %>%
  mutate(produces = value == "produces", 
         form = "both", 
         num_item_id = definition, # stupid stuff to make fit_aoa work on joint data
         item_id = definition) 

aoas <- wordbankr::fit_aoa(wordbank_data, 
                           measure = "produces", 
                           method = "glmrob",
                           age_min = 8, 
                           age_max = 36)
saveRDS(aoas, here("brm","data","aoas.rds"))
```

```{r aoas}
aoas <- readRDS(here("brm","data","aoas.rds"))

wb_pb <- target_word_data %>%
  filter(t_norm > 300, t_norm < 3000, n > 20) %>%
  group_by(age_group, english_stimulus_label) %>%
  summarise(accuracy = mean(correct)) %>%
  left_join(select(aoas, definition, aoa) %>% 
              rename(english_stimulus_label = definition))

ggplot(wb_pb, 
       aes(x = aoa, y = accuracy, col = english_stimulus_label))+
  geom_point() + 
  facet_wrap(~age_group)+
  geom_smooth(aes(group = 1), method = "lm")
```