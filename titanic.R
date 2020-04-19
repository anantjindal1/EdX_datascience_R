options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

data("titanic_train")

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


p <- titanic   %>% ggplot(aes(Age,..count..,fill=Survived))
p + geom_density(alpha = 0.2) 

t1 <- titanic %>% filter(Age==80)
max(t1$Age)
t1

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p <- titanic %>% filter(!is.na(Age))   %>% ggplot(aes(sample=Age))
p + geom_abline()+geom_qq(dparams = params) 

titanic %>% filter(!is.na(Survived)) %>% ggplot(aes(Survived,fill=Sex)) + geom_bar(position = position_dodge())



 titanic %>% filter(Fare != 0) %>% ggplot(aes(Survived,Fare, color=Survived)) +scale_y_continuous(trans = "log2")+ geom_boxplot()+geom_jitter(width = 0.1, alpha = 0.2) 

 
 
 titanic %>%  ggplot(aes(Pclass, color=Survived)) + geom_bar()
 
 titanic %>%  ggplot(aes(Survived, color=Pclass)) + geom_bar(position = position_fill())
 
 titanic %>%  ggplot(aes(Pclass, color=Survived)) + geom_bar(position = position_fill())
 
 
 p <- titanic   %>% ggplot(aes(Age,..count..,fill=Survived))
 p + geom_density(alpha = 0.2) + facet_grid(Sex ~ Pclass)
 