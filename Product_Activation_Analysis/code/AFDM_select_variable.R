source("code/data_cleaning.R")

library(FactoMineR)


char<-complete_df%>%
  select_if(~is.character(.x))

num<-complete_df%>%
  select_if(~is.numeric(.x))


char_num<-bind_cols(c(char,num))
char_num_selected <-char_num[1:75]



res.MFA <-MFA(char_num_selected, group = c(5,5,6,30,29 ), type = c(rep("n",3), rep("s",2)),
              ncp=5, name.group=c("Cater1","cater2","cater3","num1","num2"))
quality<-res.MFA$quali.var$contrib
quality%>%View()



quantity<-res.MFA$quanti.var$contrib
quantity%>%abs()%>%View()
