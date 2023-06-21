library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(reshape2)
library(ggpubr)
library(rlang)
library(caret)
library(ggcorrplot)

setwd("/home/cam/Documents/STA 138/Project")
bys_data <- read.csv("Byssinosis.csv")


# create data that is easier for ggplot2 to plot
plot_data <- bys_data %<>% pivot_longer(cols = c(Byssinosis,Non.Byssinosis)) %>% filter(value != 0) 

# Seperate categories and remove rows with no individuals
plot_data_bys <- plot_data %>% filter(name == "Byssinosis", value != 0)
plot_data_no_bys <- plot_data %>% filter(name == "Non.Byssinosis", value != 0)

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.95) + 1.5 * IQR(x))
}

plot_data_bys %<>% mutate(outlier = ifelse(is_outlier(value), rownames(plot_data_bys), as.numeric(NA)))
plot_data_no_bys %<>% mutate(outlier = ifelse(is_outlier(value), rownames(plot_data_no_bys), as.numeric(NA)))


plot_list_bys <- colnames(plot_data_bys)[1:5] %>% 
  
  map(~ ggplot(data = plot_data_bys,  aes(x = factor(!!sym(.x)), y = value,fill = factor(!!sym(.x))))+ 
        geom_label(aes(label = outlier),
                   na.rm = TRUE,
                   hjust = -0.3)+
        geom_boxplot()+
        xlab(.x)+
        ylab("Count")+
        labs(fill = .x))

plot_list_no_bys <- colnames(plot_data_no_bys)[1:5] %>% 
  
  map(~ ggplot(data = plot_data_no_bys,  aes(x = factor(!!sym(.x)), y = value,fill = factor(!!sym(.x))))+ 
        geom_label(aes(label = outlier),
                   na.rm = TRUE,
                   hjust = -0.3)+
        geom_boxplot()+
        xlab(.x)+
        ylab("Count")+
        labs(fill = .x))




  

### Plotting
bys_plots    <- ggarrange(plotlist = plot_list_bys,ncol = 5) %>% annotate_figure(top = text_grob("Byssinosis Present", face = "bold", size = 14))
no_bys_plots <- ggarrange(plotlist = plot_list_no_bys,ncol = 5) %>% annotate_figure(top = text_grob("Byssinosis Not Present", face = "bold", size = 14))
                                                  
ggarrange(bys_plots,no_bys_plots,nrow = 2) 
  ggsave("/home/cam/Documents/STA 138/Project/boxplots.png")



ggplot(data = plot_data_bys,aes(x = factor(Workspace), value, fill = factor(Workspace)))+geom_boxplot()+ geom_label(aes(label = outlier),
                                                                                                      na.rm = TRUE,
                                                                                                      hjust = -0.3)


ggplot(data = plot_data_bys,aes(x = factor(Workspace),y = value, fill = factor(Sex)))+geom_boxplot()

ggplot(data = plot_data_bys,aes(x = factor(Workspace), fill = factor(Race)))+geom_bar()


workspace_plot_list_bys <- colnames(plot_data_bys)[1:4] %>% map(~ ggplot(data = plot_data_bys,  aes(x = factor(Workspace), y = value,fill = factor(!!sym(.x))))+ 
        geom_boxplot()+
        xlab(.x)+
        ylab("Count")+
        labs(fill = .x))

workspace_plot_list_no_bys <- colnames(plot_data_no_bys)[1:4] %>% map(~ ggplot(data = plot_data_no_bys,  aes(x = factor(Workspace), y = value,fill = factor(!!sym(.x))))+ 
        geom_boxplot()+
        xlab(.x)+
        ylab("Count")+
        labs(fill = .x))

workspace_plot_list <- colnames(plot_data)[1:4] %>% map(~ ggplot(data = plot_data,  aes(x = factor(Workspace), y = value,fill = factor(!!sym(.x))))+ 
        geom_boxplot()+
        xlab(.x)+
        ylab("Count")+
        labs(fill = .x))


workspace_plots_bys <- ggarrange(plotlist = workspace_plot_list_bys,ncol = 4) %>% annotate_figure(top = text_grob("Byssinosis Present", face = "bold", size = 14)) 
workspace_plots_no_bys <- ggarrange(plotlist = workspace_plot_list_no_bys,ncol = 4) %>% annotate_figure(top = text_grob("Byssinosis Not Present", face = "bold", size = 14))
workspace_plots <- ggarrange(plotlist = workspace_plot_list,ncol = 4) 

ggarrange(workspace_plots_bys,workspace_plots_no_bys,nrow = 2) 



### Modeling
model_data <- data.frame()

for(row_num in 1:(plot_data %>% nrow)){
  
  row <- plot_data[row_num,]
  
  name  <- row$name 
  value <- row$value 
  
  if(name == "Byssinosis"){
    
  new_row <- row %>% select(-name) %>% mutate(value = 1)
  
  }else{
    
  new_row <- row %>% select(-name) %>% mutate(value = 0)
    
  }
  
  for(value in 1:row$value){
    
   model_data %<>% rbind(new_row)
    
  }
  
}


model_data %<>%  lapply(factor) %>% as.data.frame  

model_data %<>% mutate("Byssinosis" = value) %>% select(-value) 

model_data_bys <- model_data %>% filter(Byssinosis == 1)
model_data_no_bys <- model_data %>% filter(Byssinosis == 0)


model.matrix(~0+., data=model_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


set.seed(42)

### No Category Balancing

train_ind <- sample(model_data %>% nrow, (model_data %>% nrow) * 0.9) 

train_set <- model_data[train_ind,]
test_set <- model_data[-train_ind,]

explore_vars <- c("Byssinosis","Workspace","Smoking")

model_formulas <- map(explore_vars, ~  step(glm(paste0(.x,"~","1") %>% as.formula(),family = "binomial",data = train_set),
                   scope = paste0("~", c("Employment","Smoking","Sex","Race","Workspace")[c("Employment","Smoking","Sex","Race","Workspace") != .x]
                                  %>% paste0(collapse = "*")) %>% as.formula, 
                   direction = "forward",
                   k = log(4877),
                   trace = 0)$formula
                      ) %>% setNames(explore_vars)



model_summaries <- model_formulas %>% map(~ glm(.x,family = "binomial",data = test_set) %>%  summary ) %>% setNames(explore_vars)




                          

best_model_train <- step(glm(Byssinosis~1,family = "binomial",data = train_set),
                   scope = ~Employment*Smoking*Sex*Race*Workspace,
                   direction = "forward",
                   k = log(4877),
                   trace = 0)

best_model_workspace <- step(glm(factor(Workspace)~1,family = "binomial",data = train_set),
                   scope = ~Employment*Smoking*Sex*Race,
                   direction = "forward",
                   k = log(4877),
                   trace = 0)

best_model_smoke <- step(glm(factor(Smoking)~1,family = "binomial",data = train_set),
                   scope = ~Employment*Employment*Sex*Race,
                   direction = "forward",
                   k = log(4877),
                   trace = 0)




print(best_model_train$formula)

print(best_model_workspace$formula)

print(best_model_smoke$formula)

glm(Byssinosis ~ Workspace + Smoking, family = "binomial",data = test_set) %>% summary 

glm(Byssinosis ~ Workspace*Sex + Workspace*Race, family = "binomial",data = test_set) %>% summary 

glm(factor(Workspace) ~ Sex + Race, family = "binomial",data = test_set) %>% summary 

glm(factor(Smoking) ~ Sex + Employment + Sex*Employment, family = "binomial",data = test_set) %>% summary 


### Evenly Bys Category

train_ind_bys <- sample(model_data_bys %>% nrow, (model_data_bys %>% nrow) * 0.8) 

train_bys <- model_data_bys[train_ind_bys,]
test_bys <- model_data_bys[-train_ind_bys,]


train_ind_no_bys <- sample(model_data_no_bys %>% nrow, (model_data_no_bys %>% nrow) * 0.8) 

train_no_bys <- model_data_no_bys[train_ind_no_bys,]
test_no_bys <- model_data_no_bys[-train_ind_no_bys,]


train_set <- train_bys %>% rbind(train_no_bys)
test_set <- test_bys %>% rbind(test_no_bys)


best_model_train <- step(glm(Byssinosis~1,family = "binomial",data = train_set),
                   scope = ~Employment*Smoking*Sex*Race*Workspace,
                   direction = "forward",
                   k = log(4843),
                   trace = 0)


print(best_model_train$formula)

glm(Byssinosis ~ Workspace, family = "binomial",data = test_set) %>% summary 





# 165 Byssinosis 5254 Non.Byssinosis
# Thinking of downsampling to balance categories
# if not make sure train test split has 50% of each category







# Upsample

model_data_upsample <- upSample(model_data[,1:5], factor(model_data$Byssinosis), list = FALSE)

train_set_ind <- sample(model_data_upsample %>% nrow, (model_data_upsample %>% nrow) * 0.9) 

train_set <- model_data_upsample[train_set_ind,] 
test_set <- model_data_upsample[-train_set_ind,] 


best_model_train <- step(glm(Class~1,family = "binomial",data = train_set),
                   scope = ~Employment*Smoking*Sex*Race*Workspace,
                   direction = "forward",
                   k = log(9457),
                   trace = 0)


print(best_model_train$formula)


glm(Class ~ Workspace + Smoking + Employment + Race + Smoking:Employment + 
      Workspace:Employment + Workspace:Race + Employment:Race + 
      Workspace:Smoking + Smoking:Race + Workspace:Employment:Race, family = "binomial",data = test_set) %>% summary 





# Downsample

model_data_downsample <- downSample(model_data %>% select(-Byssinosis), factor(model_data$Byssinosis), list = FALSE)
model_data_downsample %<>% mutate(Byssinosis = Class) %>% select(-Class) 

train_set_ind <- sample(model_data_downsample %>% nrow, (model_data_downsample%>% nrow) * 0.8) 

train_set <- model_data_downsample[train_set_ind,] 
test_set <- model_data_downsample[-train_set_ind,] 


model_formulas <- map(explore_vars, ~  step(glm(paste0(.x,"~","1") %>% as.formula(),family = "binomial",data = train_set),
                   scope = paste0("~", c("Employment","Smoking","Sex","Race","Workspace")[c("Employment","Smoking","Sex","Race","Workspace") != .x]
                                  %>% paste0(collapse = "*")) %>% as.formula, 
                   direction = "forward",
                   k = log(264),
                   trace = 0)$formula
                      ) %>% setNames(explore_vars)



model_summaries <- model_formulas %>% map(~ glm(.x,family = "binomial",data = test_set) %>%  summary ) %>% setNames(explore_vars)



best_model_train <- step(glm(Class~1,family = "binomial",data = train_set),
                   scope = ~Employment*Smoking*Sex*Race*Workspace,
                   direction = "forward",
                   k = log(230),
                   trace = 0)


print(best_model_train$formula)


glm(Class ~ Workspace, family = "binomial",data = test_set) %>% summary 





n <- nrow(bys_data)
countColumns <- c(which(names(bys_data) == "Byssinosis"),
                  which(names(bys_data) == "Non.Byssinosis"))
longByss <- rbind(
  cbind(Byss=1,
        bys_data[rep(1:n, bys_data[,"Byssinosis"]), -countColumns]),
  cbind(Byss=0,
        bys_data[rep(1:n, bys_data[,"Non.Byssinosis"]), -countColumns])
)
row.names(longByss) <- 1:nrow(longByss)

