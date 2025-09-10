# install packages ---------------------------------------------------------
packages <- c("tidyverse", "quarto", "lingtypology", "DT", "knitr", "ymlthis", 
              "lubridate", "stringr")

to_install <- packages[!(packages %in% installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install, dependencies = TRUE)
}

rm(packages, to_install)

# GENERATE QMD ------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))

readxl::read_xlsx("data/data.xlsx") |> 
  mutate(filename = str_replace_all(feature_title, "[\\s:\\./]", "_"),
         filename = str_replace_all(filename, "č", "ch"),
         filename = str_replace_all(filename, "š", "sh"),
         filename = str_replace_all(filename, "ž", "zh"),
         filename = str_remove_all(filename, "\\*"),
         filename = str_to_lower(filename),
         filename = str_c(sprintf(str_c("%0", nchar(max(feature_id)), "d_"), feature_id), 
                          filename, ".qmd")) ->
  db

c(list.files("source/en/", pattern = ".qmd", full.names = TRUE),
  list.files("source/en/", pattern = ".html", full.names = TRUE),
  list.files("source/ru/", pattern = ".qmd", full.names = TRUE),
  list.files("source/ru/", pattern = ".html", full.names = TRUE),
  list.files(".", pattern = ".html", full.names = TRUE)) |> 
  file.remove()

options(ymlthis.rmd_body = "
```{r, include=FALSE}
library(tidyverse)
readxl::read_xlsx('../../data/data.xlsx') |> 
  filter(feature_id == PUT_FEATURE_ID_HERE) ->
  db
```

PUT_FEATURE_DESCRIPTION_HERE

##

::: {.panel-tabset}

### Вариативные носители

```{r}
library(waffle)
db |> 
  mutate(standardness = if_else(non_standard == 0, 'невариативные носители', 'вариативные носители')) |> 
  count(corpus_ru, standardness) |> 
  ggplot(aes(fill = standardness, values = n))+
  geom_waffle(colour = 'white', size = 0.5)+
  facet_wrap(~corpus_ru, ncol = 2) +
  scale_fill_manual(name = NULL, values = c('#c68958', '#9AC0CD')) +
  theme_enhance_waffle()+
  theme_void()+
  theme(legend.position = 'top', text = element_text(size = 15))
```

### Карта

```{r}
db |> 
  mutate(standardness = if_else(non_standard == 0, 'невариативные носители', 'вариативные носители')) |> 
  count(corpus_ru, standardness, latitude, longitude, url) |> 
  mutate(url = str_glue('<a href={url}>{corpus_ru}</a>')) |> 
  pivot_wider(names_from = standardness, values_from = n) ->
  for_map
  
if(!('невариативные носители' %in% colnames(for_map))) {
  for_map |> 
    mutate(`невариативные носители` = 0) ->
    for_map
}

library(leaflet)
library(leaflet.minicharts)

leaflet() |> 
  addTiles() |> 
  addMinicharts(lng = for_map$longitude, 
                lat = for_map$latitude,
                type = 'pie',
                chartdata = for_map[, c('невариативные носители', 'вариативные носители')],
                showLabels = TRUE,
                popup = leaflet.minicharts::popupArgs(html = for_map$url))
```

### Вариативность

```{r}
library(tidytext)
db |> 
  filter(non_standard > 0) |> 
  mutate(total = non_standard + standard,
         ratio = non_standard/total,
         speaker = reorder_within(speaker, by = ratio, within = corpus)) |> 
  ggplot(aes(ratio, speaker))+
  geom_point()+
  facet_wrap(~corpus_ru, scales = 'free_y', ncol = 2)+
  scale_y_reordered()+
  scale_x_continuous(labels = scales::percent)+
  labs(x = NULL, y = NULL)+
  theme_minimal()+
  theme(axis.text.y = element_blank(), text = element_text(size = 15))
```

### Данные

```{r}
db |> 
  select(corpus_ru, speaker, non_standard, standard) |> 
  DT::datatable(class = 'cell-border stripe', 
    rownames = FALSE, 
    filter = 'top', 
    extensions = 'Buttons',
    options = list(pageLength = 42, 
                   autoWidth = TRUE, 
                   info = FALSE,
                   dom = 'fBltp',
                   buttons = list(list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = '<i class=\"fas fa-download\"></i>')),
                   paginate = TRUE))
```
:::

")

library(ymlthis)

db |>
  select(feature_id, feature_title_ru, feature_text_ru, contributor_ru, update_year, update_month, update_day, filename) |>
  rename(contributor = contributor_ru,
         feature_title = feature_title_ru,
         feature_text = feature_text_ru) |>
  mutate(filename = str_c("source/ru/", filename)) |> 
  distinct() ->
  qmd_creation

walk(qmd_creation$feature_id, function(i){
  yml_empty() |> 
    yml_title(qmd_creation$feature_title[i]) |> 
    yml_author(qmd_creation$contributor[i]) |> 
    yml_date(str_c('Last update: ', 
                   '`r lubridate::make_datetime(year = ',
                   qmd_creation$update_year[i],
                   ', month = ',
                   qmd_creation$update_month[i],
                   ', day = ',
                   qmd_creation$update_day[i],
                   ')`')) |> 
    yml_output(html_document(number_sections = TRUE,
                             anchor_sections = TRUE,
                             pandoc_args = "--shift-heading-level-by=-1")) |> 
    use_rmarkdown(path = qmd_creation$filename[i], 
                  open_doc = FALSE, 
                  quiet = TRUE,
                  include_body = FALSE,
                  body = NULL)
  
  t <- read_lines(qmd_creation$filename[i])
  # change id
  t[str_which(t, "PUT_FEATURE_ID_HERE")] <- 
    str_replace(t[str_which(t, "PUT_FEATURE_ID_HERE")], 
                "PUT_FEATURE_ID_HERE", 
                as.character(qmd_creation$feature_id[i]))
  # change text
  t[str_which(t, "PUT_FEATURE_DESCRIPTION_HERE")] <- 
    str_replace(t[str_which(t, "PUT_FEATURE_DESCRIPTION_HERE")], 
                "PUT_FEATURE_DESCRIPTION_HERE", 
                as.character(qmd_creation$feature_text[i]))
  
  write_lines(t, qmd_creation$filename[i])
})

options(ymlthis.rmd_body = "
```{r, include=FALSE}
library(tidyverse)
readxl::read_xlsx('../../data/data.xlsx') |> 
  filter(feature_id == PUT_FEATURE_ID_HERE) ->
  db
```

PUT_FEATURE_DESCRIPTION_HERE

##

::: {.panel-tabset}

### Variational speakers

```{r}
library(waffle)
db |> 
  mutate(standardness = if_else(non_standard == 0, 'non-variational speakers', 'variational speakers')) |> 
  count(corpus, standardness) |> 
  ggplot(aes(fill = standardness, values = n))+
  geom_waffle(colour = 'white', size = 0.5)+
  facet_wrap(~corpus, ncol = 2) +
  scale_fill_manual(name = NULL, values = c('#c68958', '#9AC0CD')) +
  theme_enhance_waffle()+
  theme_void()+
  theme(legend.position = 'top', text = element_text(size = 15))
```

### Map

```{r}
db |> 
  mutate(standardness = if_else(non_standard == 0, 'non-variational speakers', 'variational speakers')) |> 
  count(corpus, standardness, latitude, longitude, url) |> 
  mutate(url = str_glue('<a href={url}>{corpus}</a>')) |> 
  pivot_wider(names_from = standardness, values_from = n) ->
  for_map
  
if(!('non-variational speakers' %in% colnames(for_map))) {
  for_map |> 
    mutate(`non-variational speakers` = 0) ->
    for_map
}

library(leaflet)
library(leaflet.minicharts)

leaflet() |> 
  addTiles() |> 
  addMinicharts(lng = for_map$longitude, 
                lat = for_map$latitude,
                type = 'pie',
                chartdata = for_map[, c('non-variational speakers', 'variational speakers')],
                showLabels = TRUE,
                popup = leaflet.minicharts::popupArgs(html = for_map$url))
```

### Variation

```{r}
library(tidytext)
db |> 
  filter(non_standard > 0) |> 
  mutate(total = non_standard + standard,
         ratio = non_standard/total,
         speaker = reorder_within(speaker, by = ratio, within = corpus)) |> 
  ggplot(aes(ratio, speaker))+
  geom_point()+
  facet_wrap(~corpus, scales = 'free_y', ncol = 2)+
  scale_y_reordered()+
  scale_x_continuous(labels = scales::percent)+
  labs(x = NULL, y = NULL)+
  theme_minimal()+
  theme(axis.text.y = element_blank(), text = element_text(size = 15))
```

### Raw data

```{r}
db |> 
  select(corpus, speaker, non_standard, standard) |> 
  DT::datatable(class = 'cell-border stripe', 
    rownames = FALSE, 
    filter = 'top', 
    extensions = 'Buttons',
    options = list(pageLength = 42, 
                   autoWidth = TRUE, 
                   info = FALSE,
                   dom = 'fBltp',
                   buttons = list(list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = '<i class=\"fas fa-download\"></i>')),
                   paginate = TRUE))
```
:::

")

db |>
  select(feature_id, feature_title, feature_text, contributor, update_year, update_month, update_day, filename) |>
  mutate(filename = str_c("source/en/", filename)) |> 
  distinct() ->
  qmd_creation

walk(qmd_creation$feature_id, function(i){
  yml_empty() |> 
    yml_title(qmd_creation$feature_title[i]) |> 
    yml_author(qmd_creation$contributor[i]) |> 
    yml_date(str_c('Last update: ', 
                   '`r lubridate::make_datetime(year = ',
                   qmd_creation$update_year[i],
                   ', month = ',
                   qmd_creation$update_month[i],
                   ', day = ',
                   qmd_creation$update_day[i],
                   ')`')) |> 
    yml_output(html_document(number_sections = TRUE,
                             anchor_sections = TRUE,
                             pandoc_args = "--shift-heading-level-by=-1")) |> 
    use_rmarkdown(path = qmd_creation$filename[i], 
                  open_doc = FALSE, 
                  quiet = TRUE,
                  include_body = FALSE,
                  body = NULL)
  
  t <- read_lines(qmd_creation$filename[i])
  # change id
  t[str_which(t, "PUT_FEATURE_ID_HERE")] <-
    str_replace(t[str_which(t, "PUT_FEATURE_ID_HERE")],
                "PUT_FEATURE_ID_HERE", 
                as.character(qmd_creation$feature_id[i]))
  # change text
  t[str_which(t, "PUT_FEATURE_DESCRIPTION_HERE")] <-
    str_replace(t[str_which(t, "PUT_FEATURE_DESCRIPTION_HERE")],
                "PUT_FEATURE_DESCRIPTION_HERE",
                as.character(qmd_creation$feature_text[i]))
  
  write_lines(t, qmd_creation$filename[i])
})


# RENDER ------------------------------------------------------------------

library(quarto)
quarto_render(profile = "russian")
quarto_render(profile = "english")
