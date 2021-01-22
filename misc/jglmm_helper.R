# looks like model.LMM doesn't hvae ranefs any more, it's just model
# but then model ranefs don't have names attribute either...
jglmm_ranef <- function(x, group) {
  julia_assign("model", x$model)
  julia_command(glue("model_ranef = ranef(model.LMM, named=true);"))
  ranef_terms <- julia_eval("names(model_ranef[1])[1]")
  ranef_groups <- julia_eval("names(model_ranef[1])[2]")
  ranefs <- julia_eval("model_ranef[1]")
  ranef_df <- ranefs %>% t() %>% as_tibble() %>% set_names(ranef_terms)
  ranef_df[group] <- ranef_groups
  ranef_df
}

# supports intercepts, slopes, polynomial terms, interactions
# doesn't support categorical predictors
jglmm_predict <- function(coefs, ranefs, formula, newdata, group) {
  
  fixed_effects <- coefs %>% dplyr::select(term, fixed_effect = estimate)
  random_effects <- ranefs %>% gather(term, random_effect, -!!group)
  
  newdata_vals <- get_all_vars(formula, newdata) %>% as_tibble() %>%
    dplyr::select(-value) %>% distinct() %>%
    mutate(id = 1:n(), stem = as.character(stem))
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
    gather(term, value, -id, -!!group) %>%
    left_join(fixed_effects, by = "term") %>%
    left_join(random_effects, by = c(group, "term")) %>%
    mutate_at(vars(matches("effect")), ~replace(., is.na(.), 0)) %>%
    mutate(effect = fixed_effect * value + random_effect * value) %>%
    group_by(id) %>%
    summarise(.fitted = sum(effect)) %>%
    left_join(newdata_vals, by = "id")
  
  newdata %>% left_join(fits, by = newdata_terms) %>%
    dplyr::select(!!colnames(newdata), .fitted)
}