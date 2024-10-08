seq_gaus_3d <- function(x, y, range, nugget, var_nm = colnames(x), top_n){
  ncol_x = ncol(x)
  ind_pair = expand.grid(1:ncol_x, 1:ncol_x, 1:ncol_x)
  ind_pair = ind_pair[ind_pair[,1]>ind_pair[,2] & ind_pair[,2]>ind_pair[,3],]
  row.names(ind_pair) = NULL
  
  one_var_res = apply(data.frame(dims = 1:ncol_x), MARGIN = 1, 
                      FUN = auto_gaus_sel_ind, y = y, range = range[1], nugget = nugget, x_mat = x)
  
  start.time <- Sys.time()
  no_cores = detectCores() - 1
  cl = makeCluster(no_cores)
  clusterEvalQ(cl, library(RobustGaSP))
  clusterExport(cl, c("auto_gaus_sel_ind", "ind_pair", "y", "range", "nugget", "x"), envir = environment())
  #print(clusterEvalQ(cl, y))
  pair_var_res = parApply(cl, ind_pair, MARGIN = 1, FUN = auto_gaus_sel_ind, y = y, range = range, nugget = nugget, x_mat = x)
  end.time <- Sys.time()
  par_time = end.time - start.time
  stopCluster(cl)
  
  ind_pair$mu = sd(y) - pair_var_res - (sd(y) * 3 -  one_var_res[ind_pair[,1]] - one_var_res[ind_pair[,2]] - one_var_res[ind_pair[,3]]) 
  
  ind_pair_selected = ind_pair[order(ind_pair$mu, decreasing = TRUE)[1:top_n],]
  
  output_meta = cbind(ind_pair_selected[,1:3], 
                      rep(range[1], top_n), rep(range[2], top_n), rep(range[3], top_n),
                      rep(nugget, top_n))
  
  colnames(output_meta) = NULL
  rownames(output_meta) = NULL
  print(ind_pair_selected[1:30,])
  return(list(output_meta, ind_pair_selected, ind_pair))
}
