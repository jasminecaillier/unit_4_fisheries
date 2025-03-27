#2025-03-20

###################
#UNIT 4 Fisheries
###################

### Joining data from different tables
#Mutating joins- add new variables to one table from matching observations in another table, adding more columns

library(tidyverse)
data1 = data.frame(ID = 1:2,
                   X1 = c("a1", "a2"))
data2 = data.frame(ID = 2:3,
                   X2 = c("b1", "b2"))

#left joins- joins all data from your first data to your second data that has data that is common, if not common it will not bring that data over
data12_left = left_join(data1, data2)

data12_left = data1 %>%
  left_join(data2)
data12_left

data12_left = data1 %>%
  left_join(data2, by = "ID")

#right join
data12_right = right_join( data1, data2)
data12_right
dim(data12_right)

#inner join

data12_inner = inner_join(data1, data2, by = "ID")
data12_inner

#full join

data12_full = data1 %>%
  full_join(data2)
data12_full

#FILTERING JOINS
#semi joins - reduced data 1 to only the rows that have a match in data 2
data12_semi = data1 %>%
  semi_join(data2)
data12_semi

#anti join - it returns all rows from data 1 where there are not matching values in data 2
data12_anti = anti_join(data1, data2, by = "ID")
data12_anti

#EXERCISE 1.1
#use left join, and join by common name. If different citations it could add more fish than you might actually have
#use distinct function and then 

#PIVOT- Transforms rows to columns or columns to rows

survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle = c(2, 11, 8, 27),
                    chiton = c(1, 0, 0, 2),
                    mussel = c(0, 1, 1, 4))
survey

#turn rows to columns using pivot_long
long_survey= survey %>%
  pivot_longer(cols = c("barnacle", "chiton", "mussel"),
               names_to = "beastie", values_to = "counts")
long_survey


wide_survey = long_survey %>%
  pivot_wider(names_from = beastie, values_from = counts)
wide_survey

#Exercise 1.2

ggplot(data = survey) +
  geom_point(aes(x = quadrat_id, y = chiton), color= "red") +
  geom_point(aes(x = quadrat_id, y = mussel), color= "green") +
  geom_point(aes(x = quadrat_id, y = barnacle), color= "blue") 

#long format works better for making plots, but wide format works better with wide format only!
ggplot(data = long_survey) +
  geom_point(aes(x= quadrat_id, y= counts, color= beastie))
