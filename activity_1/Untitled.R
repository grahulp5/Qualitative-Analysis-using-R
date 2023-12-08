prob_1<-pnorm(57,48.5,8.5,lower.tail = TRUE)
prob_1

prob_2_1<-pnorm(60,48.5,8.5,lower.tail = TRUE)
prob_2_1

prob_2_2<-pnorm(40,48.5,8.5,lower.tail = TRUE)
prob_2_2

prob_2<-prob_2_1-prob_2_2
prob_2

prob_3<-pnorm(57,48.5,8.5,lower.tail = TRUE)
1-prob_3

prob_4<-qnorm(0.05,48.5,8.5,lower.tail = TRUE)
prob_4

prob_5<-qnorm(0.1,48.5,8.5,lower.tail = FALSE)
prob_5
