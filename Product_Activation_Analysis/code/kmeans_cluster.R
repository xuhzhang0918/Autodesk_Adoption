source("code/PCA.R")
#kmeans-------------
final <- one_hot %>% 
    select_at(vars(one_of(most_important_variable)))

library(factoextra)
library(NbClust)
elbow_graph <- fviz_nbclust(final, kmeans, method = "wss")
#elbow graph suggest 4 clusters
elbow_graph #+  
  #geom_vline(xintercept = 3, linetype = 2)+
  #labs(subtitle = "Elbow method")

# silhouette <- fviz_nbclust(final, kmeans, method = "silhouette") +
#   labs(subtitle = "Silhouette method")
# silhouette

#nb <- NbClust(final, distance = "euclidean", min.nc = 2,
#              max.nc = 5, method = "kmeans")
#fviz_nbclust(nb)
#Error: division by zero!Error in solve.default(W) : 
#Lapack routine dgesv: system is exactly singular: U[77,77] = 0
kmeans_model <- kmeans(final,centers = 4,nstart =25)

cluster_group <- kmeans_model$cluster

complete_df_group <- complete_df %>%
    bind_cols(tibble(group_num = cluster_group))

# get index back ----------
group_num_full <- post_feat_eng %>%
    left_join(complete_df_group,by = "index") %>%
    pull(group_num)

# add group_num to original dataset -------------
data_selected_grouped <- dataset_selected %>%x
    bind_cols(tibble(group_num=group_num_full)) %>%
    dplyr::filter(!is.na(group_num))


numeric_summary <- data_selected_grouped %>%
    group_by(group_num) %>%
    summarise_if(~is.numeric(.x),~mean(.x,na.rm=T)) 

# add group_num to featur engineered dataset -------------
data_selected_grouped_en <- post_feat_eng %>%
    bind_cols(tibble(group_num=group_num_full)) %>%
    dplyr::filter(!is.na(group_num))


numeric_summary_en <- data_selected_grouped_en %>%
    group_by(group_num) %>%
    dplyr::filter(!is.infinite(discount_rate_country_code)) %>%
    summarise_if(~is.numeric(.x),~mean(.x,na.rm=T)) 

data_selected_grouped_en %>% 
    dplyr::filter(is.infinite(discount_rate_country_code))
