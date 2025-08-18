

psd_coffee <- readRDS("/Users/sebastianchacon/Desktop/sc_tesis/bld/out/data/psd_data.rds")

table1 <- psd_coffee %>%
  filter(id == "090") %>%                             
  group_by(country) %>%
  summarise(total_pais = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    total_mundial = sum(total_pais),
    share = round(total_pais / total_mundial, 4)
  ) %>%
  select(country, share) %>%
  arrange(desc(share)) %>%
  mutate(cum_share = cumsum(share))

table2 <- psd_coffee %>%
  filter(id == "058") %>%                             
  group_by(country) %>%
  summarise(total_pais = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    total_mundial = sum(total_pais),
    share = round(total_pais / total_mundial, 4)
  ) %>%
  select(country, share) %>%
  arrange(desc(share)) %>%
  mutate(cum_share = cumsum(share)) 