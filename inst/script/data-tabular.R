library(tidyverse)

foo <- mtcars[1:7,]
nc <- ncol(foo)
nr <- nrow(foo)
row.names(foo) <- NULL
names(foo) <- toupper(names(foo))
foo$N <- paste0(c("1221", "1221", "1232", "1232", "1232", "2255", "2255"))
foo <- select(foo, N, everything())
wraptable <- function(x) {
  writeLines("\\begin{table}[h]")
  writeLines("\\centering")
  writeLines(x)
  writeLines("\\end{table}")
}

ptable <- tibble(
  symbol = "CL (L)",
  descr = "Metabolic clearance in adults who graduated high school before 1973 and live in Muncie.",
  estimate = 100,
  standard.error = 200,
  rse = 2
)


saveRDS(file = "inst/datasets/ptable.RDS", ptable)

blah <- tibble(
  study = c("A", "B", "C", "D"),
  result1 = 100,
  result2 = 200,
  result3 = 300
)

summ <- filter(blah, study=="D")
summ <- mutate(summ,
               study = "X",
               result1 = sum(blah$result1),
               result2 = sum(blah$result2),
               result3 = sum(blah$result3))

blah <- bind_rows(blah,summ)
studies <- c("DEMO-111-01", "DEMO-111-02")
runs <- c(1,2,3)
doses <- c(5,10)
data <- tibble(WT = 50, ALB = 4.1, CRCL = 100, this = 5, that = 1002, theother = "ABCDEFG")
dd <- map_dfr(studies, function(study) {
  map_dfr(doses, function(dose) {
    map_dfr(runs, function(run) {
      mutate(data, dose = paste0(dose," mg"), study = study, run = run)
    })
  })
}) %>% select(study,dose,run,everything())



data <- pmtables:::data("all")
summ <- group_by(data,ID) %>% mutate(DOSE = first(AMT[!is.na(AMT)])) %>% ungroup

summ <- mutate(summ, DOSEf = factor(DOSE, unique(DOSE), paste0(unique(DOSE), " mg")))
summ <- filter(summ, DOSE >= 50)
summ <- group_by(summ, STUDYf,DOSEf,FORMf)
summ  <- summarise(summ, WT = mean(WT,na.rm=TRUE), CRCL = mean(CRCL,na.rm=TRUE), AGE = mean(AGE,na.rm=TRUE),
                   N = n(), ALB = mean(ALB,na.rm=TRUE),
                   SCR = mean(SCR,na.rm=TRUE), .groups="drop")
summ_numeric <- summ

summ <- mutate(summ,across(c(WT,CRCL,AGE,ALB,SCR), ~ pmtables:::sig(.x)))
pmdata <- modify(summ,as.character) %>% ungroup()
pmdata <- arrange(pmdata, STUDYf,DOSEf,FORMf)

saveRDS(file = "inst/datasets/pmdata.RDS", pmdata)


dotdata <- select(
  summ_numeric,
  STUDY = STUDYf,
  Normal.WT = WT,
  Normal.CRCL = CRCL,
  Normal.ALB = ALB,
  ESRD.WT = WT,
  ESRD.CRCL = CRCL,
  ESRD.ALB = ALB
) %>% mutate(ESRD.WT = ESRD.WT * 1.1, ESRD.CRCL = ESRD.CRCL/4, ESRD.ALB = ESRD.ALB/2)

dotdata <- mutate(dotdata,across(Normal.WT:ESRD.ALB, ~ pmtables:::sig(.x)))
dotdata <- modify(dotdata,as.character) %>% ungroup()
dotdata <- arrange(dotdata, STUDY)


saveRDS(file = "inst/datasets/with-dots.RDS", dotdata)


df <- group_by(summ_numeric, STUDY = STUDYf) %>% summarise(WT = mean(WT), AGE = mean(AGE), CRCL = mean(CRCL),
                                                           ALB = mean(ALB), SCR = mean(SCR))

over <- summarise(df, WT = mean(WT), AGE = mean(AGE), CRCL = mean(CRCL), ALB = mean(ALB), SCR = mean(SCR))
over <- mutate(over, STUDY = "all")

df.total <- bind_rows(df,over)
df.total <- mutate(df.total,across(WT:SCR, ~ pmtables:::sig(.x)))
df.total <- modify(df.total,as.character) %>% ungroup()

saveRDS(file = "inst/datasets/with-total.RDS", df.total)

ptab <- tribble(
  ~.type,~Parameter, ~Math, ~Symbol, ~Estimate, ~SE,
  "Fixed-effects","Clearance", "$\\exp(\\theta_1)$", "CL (L/hr)", 1.22, 0.4,
  "Fixed-effects","Volume of distribution", "$\\exp(\\theta_2)$", "V2 (L)", 5.87, 0.89,
  "Fixed-effects","Absorption rate constant","$\\theta_3$", "KA (1/hr)", 1.23, 0.1,
  "Unexplained variability","Additive_error", "$\\sigma_1$", "RUV", 0.02, 0.01
) %>% select(.type,Parameter, Symbol, Math, Estimate, SE)

saveRDS(file = "inst/datasets/ptab.RDS", ptab)
