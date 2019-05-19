library(ranger)
library(ggplot2)
library(dplyr)
rm(list=ls())

setwd("C:/Users/-Bl4z3_P1x3l4-/Documents/Kaggle/House Prices")
raw_data <- read.csv("Data/train.csv")
data <- raw_data

numeric_vars <- names(sapply(data, class)[sapply(data, class) == "integer"])
categorical_vars <- names(sapply(data, class)[sapply(data, class) == "factor"])

val_prop <- 0.8

set.seed(123)

# Quick EDA
dir.create("Plots")
data_for_plot <- data
data_for_plot$Index <- seq(1:nrow(data_for_plot))

# for (var in numeric_vars){
#     print(var)
#     if(var != "SalePrice"){
#         summary <- summary(lm(paste0("SalePrice ~ ", var), data=data_for_plot))
#         r_square <- round(summary$r.squared, 3)
#         f <- summary$fstatistic
#         p_value <- round(pf(f[1],f[2],f[3],lower.tail=F), 3)
#         attributes(p_value) <- NULL
#     }
#     else{
#     p_value = 0
#     r_square = 1
#     }
#     p <- ggplot(data_for_plot, aes_string(x=var, y="SalePrice")) + geom_point() +
#         geom_text(aes(label=Index),hjust=0, vjust=0) + geom_smooth(method="lm") +
#         ggtitle(paste0(var, expression("    :    R^2 = "), r_square, " , p = ", p_value)) +
#     theme(plot.title = element_text(hjust = 0.5, size=18))
#     ggsave(paste0("Plots/", var, ".png"))
# }
# 
# for (var in categorical_vars){
#     p <- ggplot(data_for_plot, aes_string(x=var, y="SalePrice", color=var)) + geom_boxplot() +
#         ggtitle(var) + theme(plot.title = element_text(hjust = 0.5, size=18))
#     ggsave(paste0("Plots/", var, ".png"))
# }


a <- which(data$GrLivArea>4500)
b <- which(data$OverallQual==10 & data$SalePrice < 200000)

drop_outlier_indices <- unique(c(a, b))
data <- data[-drop_outlier_indices,]


train_index <- sample(1:nrow(data), as.integer(nrow(data)*val_prop))
val_index <- (1:nrow(data))[-train_index]

missings <- data.frame(var = colnames(data), 
                       missing = 
                           sapply(data, function(x) 
                               sum(is.na(x)))/nrow(data))

missings <- missings[order(-missings$missing) & missings$missing > 0,]



give_missing_level <- c("Alley","BsmtQual", "BsmtCond", "BsmtExposure", 
                        "BsmtFinType1", "BsmtFinType2", "FireplaceQu", 
                        "GarageType", "GarageFinish", "GarageQual", 
                        "GarageCond", "PoolQC", "Fence", 
                        "MiscFeature")
# GarageYrBuilt is also missing when there's no garage, but it's numeric so leave for now

missings <- missings[!missings$var %in% give_missing_level,]

numeric_missings <- as.character(missings[missings$var %in% numeric_vars,]$var)
categorical_missings <- as.character(missings[missings$var %in% categorical_vars,]$var)

impute_missings <- function(dataset, vars, var_type){
    getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    if (var_type == "numeric"){
        for (var in vars){
            dataset[is.na(dataset[[var]]), var] <- mean(dataset[train_index, var], na.rm = TRUE)
            # dataset[dataset[[var]] == "<NA>"] <- mean(dataset[train_index, var], na.rm = TRUE)
        }
    } else if (var_type == "categorical"){
        for (var in vars){
            dataset[is.na(dataset[[var]]), var] <- as.character(getmode(dataset[train_index, var]))
            # dataset[dataset[[var]] == "<NA>"] <- as.numeric(getmode(dataset[train_index, var]))
        }    
    }
    return (dataset)
}

data <- impute_missings(data, numeric_missings, "numeric")
data <- impute_missings(data, categorical_missings, "categorical")

# Give some <NA> a factor level
for(column in give_missing_level){
    data[[column]] <- addNA(data[[column]])    
}


# Features

features <- data.frame(matrix(NA, nrow=nrow(data)))
features$f_TotalSF <- data$TotalBsmtSF + data$X1stFlrSF + data$X2ndFlrSF
features$f_TotalBathrooms <- data$FullBath + data$HalfBath + data$BsmtFullBath + data$BsmtHalfBath
features$f_TotalPorchSF <- data$OpenPorchSF + data$X3SsnPorch + data$EnclosedPorch + data$ScreenPorch + data$WoodDeckSF
features$f_HasPool <- ifelse(data$PoolArea > 0, 1, 0)
features$f_Has2ndFloor <- ifelse(data$X2ndFlrSF > 0, 1, 0)
features$f_HasGarage <- ifelse(data$GarageArea > 0, 1, 0)
features$f_HasBsmt <- ifelse(data$TotalBsmtSF > 0, 1, 0)
features$f_HasFireplace <- ifelse(data$Fireplaces > 0, 1, 0)
# features$f_remodelled <- ifelse(data$YearBuilt != data$YearRemodAdd, 1, 0)

features <- features[,2:ncol(features)]

data <- cbind(data, features)


to_one_hot = function(dataset, varlist){
    for (factor in varlist){
        levels = rownames(table(dataset[[factor]]))
        # Give NA level a proper character level
        levels[is.na(levels)] <- "NA"
        for (i in 1:length(levels)){
            dataset[paste0(factor,".",levels[i])] = ifelse(dataset[[factor]] == levels[i],1,0)
        }
        dataset = dataset[!colnames(dataset) %in% factor]
    }
    return(dataset)
}

data <- to_one_hot(data, categorical_vars)


# Remove brackets from column names
colnames(data) <- colnames(data) %>% gsub("\\((.*)\\)","\\1",.)

# Remove &
colnames(data) <- colnames(data) %>% gsub("&","",.)

# Remove spaces
colnames(data) <- colnames(data) %>% gsub(" ","",.)

train <- data[train_index,]
val <- data[val_index,]

rf <- ranger(SalePrice ~ ., as.data.frame(train), num.trees=1000, importance="permutation") 


predictions_rf <- predict(rf, val)$predictions


RMSLE_rf <- (sum((log(val[["SalePrice"]]) - log(predictions_rf))^2)^0.5)

print(round(RMSLE_rf, 3))

# 1000 trees, no features : 2.773
# 1000 trees, dropping outliers : 2.274
# 1000 trees, features : 2.156


df <- as.data.frame(rf$variable.importance)
df$var <- rownames(df)
colnames(df) <- c("imp", "var")
df$var <- factor(df$var, levels = df$var[order(df$imp)])
df <- df[order(-df$imp),]
p <- ggplot(df[1:20,], aes(x=var, y=imp)) + geom_bar(stat='identity') + coord_flip()
p
