pacman::p_load(googlesheets, dplyr, stringr)
Q = gs_title("2019BAR期初問卷 (回應)") %>% gs_read() %>% setNames(c(
  'time','name','id','dept','tel','status','gsuite','g_datacamp','g_github',
  'prob','stat','econ','r','rStudio','dplyr','rmarkdown','github','python',
  'DataCamp','edX','Coursera','morning','afternoon','evening'))
sapply(Q[22:24], function(v){strsplit(v, ', ') %>% unlist %>% table}) %>% 
  {.[c(1:3,7,4:6),]} 

# check distribution
substr(Q$dept,1,2) %>% table %>% sort %>% data.frame
substr(Q$id,1,1) %>% table %>% sort %>% data.frame
table(Q$status)
table(Q$rStudio)

# random blocking
g = replicate(8, sample(1:10)) %>% as.vector
Q = Q %>% arrange(desc(r), desc(id))
Q$group = g[1:nrow(Q)]

# check group balance
table(Q$group,Q$r)
table(Q$group,substr(Q$id,1,1))

