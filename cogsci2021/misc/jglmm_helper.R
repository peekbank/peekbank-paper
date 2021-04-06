jglmm_predict_fixed <- function(coefs, formula, newdata) {
  fixed_effects <- coefs %>% dplyr::select(term, fixed_effect = estimate)
  # TODO: get_all_vars only for x terms (then don't need deselect)
  newdata_vals <- get_all_vars(formula, newdata) %>% as_tibble() %>%
    dplyr::select(-elogit) %>%
    distinct() %>%
    mutate(id = 1:n())
  newdata_terms <- colnames(newdata_vals) %>% discard(~. == "id")
  # handle polynomial terms
  poly_terms <- coefs$term %>% discard(~str_detect(., "&")) %>%
    keep(~str_detect(., "\\^")) %>% str_split(" \\^ ")
  for (term in poly_terms) {
    term_name <- sym(term[1])
    term_val <- as.numeric(term[2])
    poly_term <- paste(term_name, "^", term_val)
    newdata_vals %<>% mutate(!!poly_term := `^`(!!term_name, term_val))
  }
  # handle interactions
  int_terms <- coefs$term %>% keep(~str_detect(., "&")) %>% str_split(" & ")
  for (term in int_terms) {
    term_1 <- sym(term[1])
    term_2 <- sym(term[2])
    int_term <- paste(term_1, "&", term_2)
    newdata_vals %<>% mutate(!!int_term := !!term_1 * !!term_2)
  }
  fits <- newdata_vals %>%
    mutate(`(Intercept)` = 1) %>%
    pivot_longer(cols = -id, names_to = "term", values_to = "value") %>%
    left_join(fixed_effects, by = "term") %>%
    mutate_at(vars(matches("effect")), ~replace(., is.na(.), 0)) %>%
    mutate(effect = fixed_effect * value) %>%
    group_by(id) %>%
    summarise(.fitted = sum(effect)) %>%
    left_join(newdata_vals, by = "id")
  newdata %>% left_join(fits, by = newdata_terms) %>%
    dplyr::select(!!colnames(newdata), .fitted)
}
