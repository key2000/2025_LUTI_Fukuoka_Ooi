suppressMessages({library(sf);library(dplyr);library(tidyr)})
load("data/key_code_sf.xdr"); load("data/work_zone.xdr")
rd <- function(p,col) read.csv(p,fileEncoding="CP932",stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
  filter(!is.na(KEY_CODE)) %>% transmute(KEY_CODE=as.character(KEY_CODE), emp=replace_na(suppressWarnings(as.numeric(.data[[col]])),0))
e09 <- rd("data/raw/経済センサス2009/tblT000616S5030.csv","T000616002")
e14 <- rd("data/raw/経済センサス従業員数1kmメッシュ/tblT000841S5030.csv","T000841002")
e21 <- rd("data/raw/経済センサス2021/tblT001146S5030.csv","T001146022")
e21noS <- rd("data/raw/経済センサス2021/tblT001146S5030.csv","T001146023")
ks <- key_code_sf %>% mutate(KEY_CODE=as.character(KEY_CODE))
mp <- st_centroid(ks)
d <- st_drop_geometry(mp) %>% select(KEY_CODE) %>%
  left_join(e09%>%rename(y09=emp),by="KEY_CODE") %>% left_join(e14%>%rename(y14=emp),by="KEY_CODE") %>%
  left_join(e21%>%rename(y21=emp),by="KEY_CODE") %>% left_join(e21noS%>%rename(y21S=emp),by="KEY_CODE") %>%
  mutate(across(starts_with("y"),~replace_na(.,0)))
di <- function(a,b) 100*0.5*sum(abs(a/sum(a)-b/sum(b)))
vor_assign <- function(centers){
  cp <- st_centroid(ks %>% filter(KEY_CODE %in% centers))
  vor <- cp %>% st_geometry() %>% st_union() %>% st_voronoi() %>% st_collection_extract("POLYGON") %>% st_sf()
  vor <- st_join(vor, cp %>% select(center=KEY_CODE), join=st_intersects)
  st_join(mp %>% select(KEY_CODE), vor, join=st_within) %>% st_drop_geometry()
}
wz <- as.character(work_zone)
va <- vor_assign(wz); dd <- d %>% left_join(va,by="KEY_CODE") %>% filter(center!="50302329")
cat("総数（対象範囲, 下大利領域除く）: 2009",sum(dd$y09)," 2014",sum(dd$y14)," 2021",sum(dd$y21)," 2021(公務除く)",sum(dd$y21S),"\n")
cat("2021 公務(S)の従業者:", sum(dd$y21)-sum(dd$y21S), "\n\n")
z <- dd %>% group_by(center) %>% summarise(across(starts_with("y"),sum))
cat("== 相違指数（%） ==\n")
pairs <- list(c("y09","y14"),c("y14","y21"),c("y09","y21"),c("y14","y21S"),c("y09","y21S"))
for(p in pairs) cat(sprintf("%s-%s  メッシュ %.2f  17ゾーン %.2f\n",p[1],p[2],di(dd[[p[1]]],dd[[p[2]]]),di(z[[p[1]]],z[[p[2]]])))
cat("\n== 17ゾーンのシェア(%) ==\n")
print(as.data.frame(z %>% mutate(across(starts_with("y"),~round(100*./sum(.),2))) %>% arrange(desc(y14))))
saveRDS(list(d=d,dd=dd,z=z),"/private/tmp/claude-501/-Users-natsumioi-2025-LUTI-Fukuoka-Ooi/df327c61-809d-47fc-94e5-df6a73435d00/scratchpad/three_points.rds")

# t22 事前チェック：中心地を増やしたとき2009→2021の移動が何%残るか
cat("\n== 中心地を増やした場合（現行17 + 単独でE人以上のメッシュ） ==\n")
inarea <- dd$KEY_CODE
for(base in c("y14","y21")) for(E in c(30000,20000,10000,5000,3000)){
  add <- dd$KEY_CODE[dd[[base]]>=E]
  cen <- union(setdiff(wz,"50302329"), add)
  va2 <- vor_assign(cen); d2 <- d %>% filter(KEY_CODE %in% inarea) %>% left_join(va2,by="KEY_CODE")
  z2 <- d2 %>% group_by(center) %>% summarise(across(starts_with("y"),sum))
  cat(sprintf("基準%s E>=%5d  K=%3d  09→21 %.2f%%  09→14 %.2f%%  14→21 %.2f%%\n",base,E,length(cen),
              di(z2$y09,z2$y21),di(z2$y09,z2$y14),di(z2$y14,z2$y21)))
}
