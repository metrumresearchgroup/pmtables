
library(tidyverse)
data <- read_csv("inst/data/analysis1.csv", na = '.')
data <- select(data, -LDOS,-TAFD,-TAD,-MDV)
data <- rename(data, BQL = BLQ)
set.seed(112233)
id <- data %>% filter(SEQ==0) %>% distinct(ID,STUDY)
id <- mutate(
  id,
  SEX = case_when(
    STUDY <=2 ~ rbinom(n(),1, 0.6),
    STUDY > 2 ~ rbinom(n(),1,0.45)
  ),
  RACE = sample(1:3, n(), prob = c(0.5, 0.4, 0.1),replace = TRUE),
  STUDY = NULL,
  FORM = sample(c("tablet", "capsule", "troche"), n(), prob = c(0.8, 0.1, 0.1), replace = TRUE)
)

data <- left_join(data,id)

# pd endpoint
end1 <- filter(data,SEQ==1,STUDY>2) %>% group_by(ID) %>% slice(1) %>% ungroup()
end1 <- mutate(end1,SEQ=2)

set.seed(1009)
end2 <- filter(data,SEQ==1,STUDY > 2) %>% group_by(ID) %>% slice(sample(seq(n()),3)) %>% ungroup()
end2 <- mutate(end2,SEQ=3)

data <- bind_rows(data,end1,end2) %>% arrange(ID,TIME,SEQ)
data <- mutate(data, STUDYN = STUDY)
spec <- yspec::ys_load("inst/data/analysis1.yml")
data <- select(data, names(spec))
data <- yspec::yspec_add_factors(data,spec,RF,SEX,CP,SEQ,STUDY,FORM,.suffix="f")


saveRDS(file = "inst/data/all.RDS",data)
id <- distinct(data,ID,.keep_all=TRUE)
saveRDS(file = "inst/data/id.RDS",id)
obs <- filter(data,SEQ > 0)
saveRDS(file = "inst/data/obs.RDS",obs)



