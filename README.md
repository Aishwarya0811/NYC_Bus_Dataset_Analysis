 # New York city school bus Dataset 
 
 
1. 268018 observations 21 variables 

2. Highly categorical dataset with multiple levels present for different attributes.

# Dataset Characteristics: 

Multivariate

# Attribute Characteristics: 

Categorical, Integer

# Missing Values: 

Yes

# Data Cleaning 

- Divided the data into two groups with how_long_delayed as NULL and one with NOT NULL
- Cant use how_long_delayed as NULL in test/train, but can be used as final test dataset
- Converted the Boro column to character
- Converted the route number to character and levelling them as per description of details about this dataset
- Removed Route_Number column as its levelled
- Filtered out schools with 0 and ` values
- Levelled the schools serviced based on the length and number of alphabets in schools serviced column
- Got the time component(with AM/PM) of bus breakdown occurence time
- Took only the last part of bus companyname to easily dummify the data later

# Exploratory Analysis


# Algorithim 
