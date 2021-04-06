load(file = "data/aoi_data_joined.Rds")

foo <- select(aoi_data_joined, administration_id, trial_id, t_norm, aoi) %>%
  arrange(administration_id, trial_id)

rled <- foo %>%
  group_by(administration_id, trial_id) %>%
  summarise(lengths = rle(aoi)$lengths, 
            values = rle(aoi)$values, 
            t_norm_min = min(t_norm)) 

profvis::profvis({
  
unrled <- rled %>%
  group_by(administration_id, trial_id, t_norm_min) %>%
  nest() %>%
  mutate(rle_vector = map(data,
                           ~ `class<-`(list(lengths = .$lengths, 
                                            values = .$values), "rle")), 
            aoi = map(rle_vector, inverse.rle)) %>%
  select(-data, -rle_vector) %>%
  unnest(aoi) %>%
  group_by(administration_id, trial_id) %>%
  mutate(t_norm = seq(t_norm_min[1], t_norm_min[1] + (n()-1) * 25, 25)) %>%
  select(administration_id, trial_id, t_norm, aoi) %>%
  arrange(administration_id, trial_id)
})
