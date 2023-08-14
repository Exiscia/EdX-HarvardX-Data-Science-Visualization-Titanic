options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train |>
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) |>
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


# Question 1: Variable Types
str(titanic_train) #Shows the structure of the titanic_train data set

# Survived:                       non-ordinal categorical
# PClass (Passenger Class):       ordinal categorical
# Sex :                           non-ordinal categorical
# SibSp (Sibling/Spouse Aboard):  discrete
# Parch: (Parent/Child Aboard):   discrete
# Fare:                           continuous

# Question 2: Demographics of Titanic Passengers
titanic |> ggplot(aes(Age, fill=Sex)) + geom_density(aes(alpha=0.2))
titanic |> ggplot(aes(Age, fill=Sex)) + geom_density(aes(y=..count.., alpha=0.2))

# Question 3: QQ-plot of Age Distribution
params <- titanic |>
  filter(!is.na(Age)) |> #remove lines with NA in Age
  summarize(mean = mean(Age), sd = sd(Age))

titanic |>
  filter(!is.na(Age)) |> #remove lines with NA in Age
    ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline()

# Question 4: Survival by Sex
titanic |> ggplot(aes(Survived, fill=Sex)) + geom_bar()
titanic |> ggplot(aes(Survived, fill=Sex)) + geom_bar(position = position_dodge())

# Question 5: Survival by Age
titanic |> ggplot(aes(Age, fill=Survived)) + geom_density(aes(y=..count.., alpha=0.2))
titanic |> ggplot(aes(Age, fill=Survived)) + geom_density(aes(alpha=0.2))

# Question 6: Survival by Fare
titanic |> filter(Fare != 0) |> 
  ggplot(aes(Survived, Fare)) + geom_boxplot() + scale_y_continuous(trans="log2") + geom_jitter(width=0.2, alpha=0.2)

# Question 7: Survival by Passenger Class
ggplot(titanic, aes(Pclass, fill=Survived)) + geom_bar()
ggplot(titanic, aes(Pclass, fill=Survived)) + geom_bar(position = position_fill())
ggplot(titanic, aes(Survived, fill=Pclass)) + geom_bar(position = position_fill())

# Question 8: Survival by Age, Sex, and Passenger Class
titanic |> 
  ggplot(aes(Age, fill=Survived)) + geom_density(aes(y=..count.., alpha=0.2)) + facet_grid(Sex~Pclass)
