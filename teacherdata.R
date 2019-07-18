## Load in data.
    # The source is https://publicstaffreports.dpi.wi.gov/PubStaffReport/Public/PublicReport/AllStaffReport with the following filters:
      # Year: 2018 - 2019
      # Hiring Agency: 2849 - La Crosse School District
      # Assignment Position: 53 - Teacher
    # Download the generated data in .csv format and place in working directory.

data <- read.csv("./data/LaCrosseTeachers.csv", stringsAsFactors = FALSE, skip = 1) #skip first row containing filter criteria

## Clean data.
teach <- data %>%
  select(3:8, 10, 13:14) %>% # select variables of interest
  distinct() %>% # retain distinct rows (eliminate identical rows... these differ in some of the dropped columns but are just redundant here)
  drop_na() # drop rows with NA values

teach$Age <- 2018 - teach$Birth.Year # approximate age at time of contract

teach <- teach %>%
  select(4:5, 7:10) %>% # reducing variables further
  mutate(Gender = recode(Gender, # recode some stuff for clarity
                         F = "Female",
                         M = "Male")) %>%
  mutate(RaceEthnicity = recode(RaceEthnicity,
                                "W - White" = "White",
                                "A - Asian" = "Asian",
                                "H - Hispanic/Latino" = "Hispanic/Latino",
                                "T - Two or More Races" = "Two or More Races",
                                "P - Native Hawaiian or Other Pacific Islander" = "Native Hawaiian or Other Pacific Islander",
                                "B - Black or African American" = "Black or African American",
                                "I - American Indian or Alaska Native" = "American Indian or Alaska Native"
                                )) %>%
  mutate(Contract.High.Degree = recode(Contract.High.Degree,
                                       "5 - Master's degree" = "Masters",
                                       "4 - Bachelor's degree" = "Bachelors",
                                       "7 - Doctorate" = "Doctoral",
                                       "8 - Other" = "Other"))

teach <- teach %>% # rename variables for clarity
  rename("Race/Ethnicity" = RaceEthnicity,
         "Degree" = Contract.High.Degree,
         "Experience" = Contract.Total.Experience,
         "Salary" = Total.Salary)

# convert to appropriate data type
teach$Salary <- as.numeric(gsub("[$,]", "", teach$Salary)) # remove dollar sign and comma from salary variable; also convert to numeric
teach$Gender <- as.factor(teach$Gender)
teach$`Race/Ethnicity` <- as.factor(teach$`Race/Ethnicity`)
teach$Degree <- as.factor(teach$Degree)

# remove extra data object
rm(data)

