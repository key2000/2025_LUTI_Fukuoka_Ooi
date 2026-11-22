src <- readLines("project_log/base-year-revision/07_center-assignment/voronoi_vs_time.R")
src <- src[1:(grep("^res <- list", src)-1)]
eval(parse(text = src))
for (E in c(Inf, 10000)) {
  cen <- union(wz17, d$KEY_CODE[d$emp >= E]); cen_nodes <- paste0("mc_", cen)
  tm <- get_distance_matrix(g, from = ok_nodes, to = cen_nodes, algorithm = "mch")
  a <- d %>% left_join(vor_assign(cen), by = "KEY_CODE") %>% filter(paste0("mc_",KEY_CODE) %in% ok_nodes, !is.na(center))
  a$center_t <- sub("mc_","",colnames(tm))[apply(tm[paste0("mc_",a$KEY_CODE),],1,function(x) if(all(is.na(x))) NA_integer_ else which.min(x))]
  a$t_min <- apply(tm[paste0("mc_",a$KEY_CODE),],1,function(x) suppressWarnings(min(x,na.rm=TRUE)))
  a$t_vor <- mapply(function(k,c) tm[paste0("mc_",k),paste0("mc_",c)], a$KEY_CODE, a$center)
  a <- a[!is.na(a$center_t),]; a$x <- a$t_vor - a$t_min; ch <- a$center != a$center_t
  cat("E=",E," 変わったメッシュ",sum(ch)," うち差5分以下",sum(a$x[ch]<=5)," 5〜10分",sum(a$x[ch]>5 & a$x[ch]<=10)," 10分超",sum(a$x[ch]>10),
      " | 10分超の従業者",sum(a$emp[ch & a$x>10]),"\n")
  print(head(a[ch,] %>% arrange(desc(x)) %>% select(KEY_CODE,emp,center,center_t,t_vor,t_min,x), 6))
  if (E==Inf) { print(a[a$center=="50303302" & a$center_t=="50302390",c("KEY_CODE","emp","t_vor","t_min")]); cat("天神→六本松に移るメッシュ数・従業者:", sum(a$center=="50303302" & a$center_t=="50302390"), sum(a$emp[a$center=="50303302" & a$center_t=="50302390"]),"\n")
    cat("天神メッシュ自身の行き先:", a$center_t[a$KEY_CODE=="50303302"], " 天神→天神 t=", tm["mc_50303302","mc_50303302"], " 天神→六本松 t=", tm["mc_50303302","mc_50302390"], "\n") }
}
cen <- union(wz17, d$KEY_CODE[d$emp >= 10000]); cat("E=1万で足すメッシュ:\n"); print(d %>% filter(KEY_CODE %in% setdiff(cen, wz17)) %>% arrange(desc(emp)))
