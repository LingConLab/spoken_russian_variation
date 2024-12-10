library(tidyverse)

database <- readxl::read_xlsx("data/data.xlsx")

numerals <- readxl::read_xlsx("/home/agricolamz/work/articles/2025_dialect_numerals_Sveta_Chiara/numerals_10.09.xlsx")

numerals |> 
  mutate(non_standard = if_else(non_standard == "n_stnd_genpl", "non_standard", "standard")) |> 
  count(corpus, tier_name, non_standard, lat, lon) |> 
  mutate(feature_title = "Numeral construction",
         contributor = "Chiara Naccarato, Svetlana Zemicheva",
         url = case_when(corpus == "data_dialect_keba" ~ "https://lingconlab.ru/keba/",
                         corpus == "data_dialect_khislavichi" ~ "https://lingconlab.ru/khislavichi/",
                         corpus == "data_dialect_kostroma" ~ "https://lingconlab.ru/manturovo/",
                         corpus == "data_dialect_lukh_and_teza" ~ "https://lingconlab.ru/lukhteza/",
                         corpus == "data_dialect_luzhnikovo" ~ "https://lingconlab.ru/luzhnikovo/",
                         corpus == "data_dialect_makeevo" ~ "https://lingconlab.ru/shetnevo/",
                         corpus == "data_dialect_malinino" ~ "https://lingconlab.ru/malinino/",
                         corpus == "data_dialect_nekhochi" ~ "https://lingconlab.ru/nekhochi/",
                         corpus == "data_dialect_opochka" ~ "https://lingconlab.ru/opochka/",
                         corpus == "data_dialect_rogovatka" ~ "https://lingconlab.ru/rogovatka/",
                         corpus == "data_dialect_spiridonova_buda" ~ "https://lingconlab.ru/SpiridonovaBuda/",
                         corpus == "data_dialect_tserkovnoe" ~ "https://lingconlab.ru/tserkovnoe/",
                         corpus == "data_dialect_upper_pinega_and_vyya" ~ "https://lingconlab.ru/vaduga/",
                         corpus == "data_dialect_ustya" ~ "http://www.parasolcorpus.org/Pushkino/",
                         corpus == "dialect_dvina" ~ "https://lingconlab.ru/dvina/",
                         corpus == "dialect_middle_pinega" ~ "https://lingconlab.ru/pinega/",
                         corpus == "dialect_mikhaylov" ~ "https://lingconlab.ru/mikhaylov/",
                         corpus == "dialect_novgorod" ~ "https://lingconlab.ru/novgorod/",
                         corpus == "dialect_pyoza" ~ "https://lingconlab.ru/pyoza/",
                         corpus == "dialect_veegora" ~ "https://lingconlab.ru/veegora/",
                         corpus == "dialect_yelets" ~ "https://lingconlab.ru/yelets/"),
         latitude = lat,
         longitude = lon,
         corpus = case_when(corpus == "data_dialect_keba" ~ "Corpus of the Russian dialect spoken in Keba",
                            corpus == "data_dialect_khislavichi" ~ "Corpus of the Russian dialect spoken in Khislavichi district",
                            corpus == "data_dialect_kostroma" ~ "Corpus of the Russian dialect spoken in Manturovo",
                            corpus == "data_dialect_lukh_and_teza" ~ "Corpus of Lukh and Teza river basins dialects",
                            corpus == "data_dialect_luzhnikovo" ~ "Luzhnikovo Corpus",
                            corpus == "data_dialect_makeevo" ~ "Corpus of Shetnevo and Makeevo dialect",
                            corpus == "data_dialect_malinino" ~ "Corpus of the Russian dialect spoken in the village Malinino",
                            corpus == "data_dialect_nekhochi" ~ "Corpus of the Russian dialect spoken in Nekhochi",
                            corpus == "data_dialect_opochka" ~ "Corpus of Opochetsky dialects",
                            corpus == "data_dialect_rogovatka" ~ "Corpus of Rogovatka dialect",
                            corpus == "data_dialect_spiridonova_buda" ~ "Corpus of Spiridonova Buda dialect",
                            corpus == "data_dialect_tserkovnoe" ~ "Corpus of the Russian dialect spoken in Tserkovnoe",
                            corpus == "data_dialect_upper_pinega_and_vyya" ~ "Upper Pinega and Vyya Corpus",
                            corpus == "data_dialect_ustya" ~ "Ustja River Basin Corpus",
                            corpus == "dialect_dvina" ~ "Corpus of the Russian dialect spoken in the villages of the Middle Northern Dvina",
                            corpus == "dialect_middle_pinega" ~ "Corpus of the Russian dialect spoken in the villages of the Middle Pinega",
                            corpus == "dialect_mikhaylov" ~ "Corpus of the Russian dialect spoken in the Mikhaylov area",
                            corpus == "dialect_novgorod" ~ "Corpus of the Russian dialect spoken in Ilmen Lake district",
                            corpus == "dialect_pyoza" ~ "Corpus of the Russian dialect spoken in the villages of the Middle Pyoza",
                            corpus == "dialect_veegora" ~ "Corpus of the Russian dialect spoken in the village Veegora",
                            corpus == "dialect_yelets" ~ "Svishni and Trostnoe Corpus"),
         latitude = as.double(latitude),
         longitude = as.double(longitude),
         speaker = tier_name,
         feature_text = "Here will be short text",
         update_year = 2024,
         update_month = 12,
         update_day = 1)  |> 
  pivot_wider(names_from = non_standard, values_from = n, values_fill = 0) |> 
  select(colnames(database)) |> 
  bind_rows(database) |> 
  writexl::write_xlsx("data/data.xlsx")

readxl::read_xlsx("data/data.xlsx") |> 
  mutate(feature_id = fct_inorder(feature_title) |> as.double()) |> 
  relocate(feature_id, .before = "corpus") |> 
  writexl::write_xlsx("data/data.xlsx")
