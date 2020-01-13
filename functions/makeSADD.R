# This function makes the SADD categories when passed sex c(male, female, unknown) and age in years. 
# The function may not be needed in all reports.

makeSADD <- function(sex, age) {
  if (is.character(sex) && is.numeric(age)) {
    ADD <- "unknown"
    ADD <- ifelse(age >= 0  & age <6,  "0_5",   ADD)
    ADD <- ifelse(age >= 6  & age <10, "6_9",   ADD)
    ADD <- ifelse(age >= 10 & age <15, "10_14", ADD)
    ADD <- ifelse(age >= 15 & age <20, "15_19", ADD)
    ADD <- ifelse(age >= 20 & age <25, "20_24", ADD)
    ADD <- ifelse(age >= 25 & age <35, "25_34", ADD)
    ADD <- ifelse(age >= 35 & age <50, "35_49", ADD)
    ADD <- ifelse(age >= 50,           "50_",   ADD)
    
    SADD <- ifelse(sex == "male"   | sex == "Male"   | sex == "m" | sex == "M", 
                   paste0("M",ADD), 
                   ifelse(sex == "female" | sex == "Female" | sex == "f" | sex == "F", 
                          paste0("F",ADD), 
                          paste0("U", ADD))
    )
    SADD
  } else {
    stop("At least one of the inputs is not correct, please check that the age variable is numeric and the sex variable is character.")
  }
}
