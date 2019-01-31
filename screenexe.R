

library(tidyverse)

list.files()

df=read_csv("screening_exercise_orders_v201810.csv")
colnames(df)
head(df)

# A) Assemble a dataframe with one row per customer and the following columns:----
df2 = df %>% arrange(customer_id, date)
count= df2 %>% 
  group_by(customer_id)%>%
  summarize(count=n())
df3= df2[row.names(unique(df2[,c("customer_id")])),]
head(df3)
colnames(df3)
colnames(df3)[3]="most_recent_order_date"
df4= df3 %>% 
  select("customer_id","gender","most_recent_order_date") %>%
  cbind(count[,2])

colnames(df4)[4]="order_count"
head(df4, n=10)

# B) Plot the count of orders per week.----
library(lubridate)
dfw=df %>%
  mutate(weekth=week(ymd_hms(date)))  %>% 
  group_by(weekth) %>%
  summarise(count=n())
plot(dfw$weekth,dfw$count)
p = dfw %>% 
  ggplot(aes(x=weekth, y=count)) +
  geom_point()+
  geom_line() +
  theme_bw()
p
ggsave("weeks_count_plot.png")                

# 
# C) Compute the mean order value for gender 0 and for gender 1. Do you think the difference is significant?----
dfg = df[,c("gender","value")]
mean_0 = mean(dfg[dfg$gender==0,]$value)
mean_1 = mean(dfg[dfg$gender==1,]$value)
#t test to examine the significance between the two populations
t.test(dfg[dfg$gender==0,]$value,dfg[dfg$gender==1,]$value)
t.test(dfg[dfg$gender==0,]$value,dfg[dfg$gender==1,]$value,var.equal = TRUE)
#p-value = 0.0482, if we set the threshold as 0.05, then it's statistically significant.
# The assumptions made in t-tests are: data are continuous scale, random selected,  normal distributed, large sample size, and equal variance. Although the default t test in R does not assume equal variance.

#   
# D) Assuming a single gender prediction was made for each customer, generate a confusion matrix for predicted gender. What does the confusion matrix tell you about the quality of the predictions?----
dfgp=df[,c("gender","predicted_gender")]
table(dfgp)
library(gmodels)
CrossTable(dfgp$gender,dfgp$predicted_gender,chisq=TRUE)


#   
# E) Describe one of your favorite tools or techniques and give a small example of how it's helped you solve a problem. Limit your answer to one paragraph.----







#
df2=aggregate(df, by = list(df$customer_id), FUN = sum)
library(reshape)
df2=as.data.frame(df) #reshape only works for data.frame, not tibble
df2 = melt(df2, id=c("customer_id", "gender" ,"date" ,"value" ))
df2 = cast(df, id~variable, mean)


head(melt(tips))
names(airquality) <- tolower(names(airquality))
melt(airquality, id=c("month", "day"))



