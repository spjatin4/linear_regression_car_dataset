
library(neuralnet)
library(MASS)
mynn <- Boston

str(mynn)

summary(mynn)


min(mynn$medv)

#hist(n_df$medv)

head(Boston)

maxvalue <- apply(mynn, 2,max)
minvalue <- apply(mynn, 2,min)

n_df <- as.data.frame(scale(mynn,center = minvalue,scale = maxvalue - minvalue))

#Lets create a Data partition

nn_train <- sample(2, nrow(n_df),replace = TRUE, prob=c(0.7,0.3))
nn_trainset <- n_df[nn_train==1,]
nn_testset <- n_df[nn_train==2,]

nnvars <- colnames(n_df)
p_vars <- nnvars[!nnvars%in%"medv"]
p_vars1 <- paste(p_vars,collapse = "+")
n_form <- as.formula(paste("medv~",p_vars1,collapse = "+"))
neural_model <- neuralnet(formula = n_form,data = nn_trainset,
                          hidden = c(4,2),linear.output = T)

plot(neural_model)                

neural_model1 <- neuralnet(formula = n_form,data = nn_trainset,
                           hidden = c(1,2),linear.output = T)
plot(neural_model1)


