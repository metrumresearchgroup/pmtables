
# shared -----

data <- pmplots::pmplots_data_obs()
id <- pmplots::pmplots_data_id()
spec <- yspec::ys_help$spec()
ys_get_unit <- function(spec) purrr::map(spec, ~ yspec:::unit(.x))
ys_get_short <- function(spec) purrr::map(spec, "short")


# re-decode -----
re <- yaml::yaml.load_file("inst/recode.yml")

redecode_impl <- function(col,table) {
  if(is.list(table)) table <- unlist(table)
  hit <- match(names(table),col$decode)
  col$decode[hit] <- table[col$decode[hit]]
  col
}

redecode <- function(spec,re,cols) {
  #cols <- cvec_cs(cols)
  exist <- all(cols %in% names(re))
  stopifnot(exist)
  for(col in cols) {
    spec[[col]] <- redecode_impl(spec[[col]],re[[col]])
  }
  spec
}

# short and units
spec2 <- redecode(spec,re,c("CP", "RF"))

short <- ys_get_short(spec2)
unit <- ys_get_unit(spec2)



# continuous data ------

id <- dplyr::mutate(
  id,
  STUDY = dplyr::case_when(
    STUDYc=="SAD" ~ "12-XYZ9981-001",
    STUDYc=="MAD" ~ "11-XYZ9981-008",
    STUDYc=="Renal" ~ "09-XYZ9981-003",
    STUDYc=="Hepatic" ~ "15-XYZ9981-001",
  )
)

data.cont <- id



# categorical data -------

mf <- c("male", "female")
id <- dplyr::mutate(id, SEX = mf[(ID%% 2)+1])
cols <- c("CPc","RF","SEX","STUDYc")
id <- yspec::yspec_add_factors(id,spec2,CP,RF)
id <- dplyr::mutate(id, PEDIATRIC = factor(AGE > 30, labels=c("pediatric", "adult")))

short$CP_f <- "Child-Pugh Score"
short$SEX <- "Sex"
short$STUDY <- "Study"
short$RF_f <- "Renal Function"
short$PEDIATRIC <- "Age Group"
data.cat <- dplyr::select(id, ID,STUDY,SEX,RF_f,CP_f,PEDIATRIC)

