#####
rm(list=ls())
gc();gc();

# Install required packages
#install.packages("sf")
#install.packages("dplyr")
#install.packages("raster")
#install.packages("terra")
#install.packages("rgdal")
# install.packages("janitor")
# install.packages("ggplot2")
library(tidyr)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(rgdal)
library(janitor)
library(ggplot2)

#メートル座標系、JGD2011 2011 福岡2系
target_crs <- 6670
# crs_info <- st_crs(UEA_Fukuoka)
# print(crs_info)

# 福岡の都市雇用圏
MEA <- st_read("data/raw/UEA") %>%
  st_transform(crs = target_crs)
MEA_selected <- MEA %>% dplyr::select(mea_mea, geometry)
UEA_Fukuoka <- MEA_selected %>%
  filter(mea_mea == "40130") %>%
  dplyr::select(mea_mea, geometry)
UEA_Fukuoka <- st_make_valid(UEA_Fukuoka)
plot(UEA_Fukuoka)

#バッファ作成
UEA_Fukuoka_buf=st_buffer(UEA_Fukuoka,100)
plot(UEA_Fukuoka_buf$geometry,add=T,border="red",col=NA,lwd=2)

# dsn_folder <- "data/processed/UEAバッファ"
# layer_name <- "UEA_Fukuoka_buf"
# st_write(UEA_Fukuoka_buf,
#          dsn = dsn_folder,
#          layer = layer_name,
#          driver = "ESRI Shapefile",
#          delete_layer = TRUE)

#1kmメッシュ福岡境界データ
bnd_mesh <- st_read("data/raw/1kmメッシュ福岡境界データ") %>%
  st_transform(crs = target_crs) %>%
  dplyr::select(-c(MESH1_ID, MESH2_ID, MESH3_ID, OBJ_ID))
tI2=st_intersects(UEA_Fukuoka_buf,bnd_mesh) %>% unlist()
bnd_mesh_crop=bnd_mesh[tI2,]
plot(bnd_mesh_crop$geometry,col="green")
plot(UEA_Fukuoka$geometry,add=T,border="blue",col=NA,lwd=2)

crs_info <- st_crs(bnd_mesh)
print(crs_info)

#ゾーン数カウント
nrow(bnd_mesh_crop)


#福岡土地利用細分メッシュデータ100mメッシュ
NLNI_Fukuoka <- st_read("data/raw/NLNI_Fukuoka",options = "ENCODING=CP932") %>%
  st_transform(crs = target_crs)
# 1kmメッシュコード（8桁）を作成
NLNI_Fukuoka$mesh_1km <- substr(NLNI_Fukuoka$メッシュ, 1, 8)
# 1kmメッシュごと、土地利用種ごとのカウント
landuse_count <- NLNI_Fukuoka %>%
  st_drop_geometry() %>%  # geometryは不要なので削除
  group_by(mesh_1km, 土地利用種) %>%
  summarise(
    count = n(),  # 出現回数をカウント
    .groups = 'drop'
  )
# 土地利用種の種類数の確認
n_distinct(NLNI_Fukuoka$土地利用種)
# landuse_count を横展開して、1kmメッシュごとに各土地利用種のカウント列を作る
landuse_wide <- landuse_count %>%
  tidyr::pivot_wider(
    names_from = 土地利用種,
    values_from = count,
    values_fill = 0
  )
# bnd_mesh_crop に結合
bnd_mesh_with_landuse <- bnd_mesh_crop %>%
  left_join(landuse_wide, by = c("KEY_CODE" = "mesh_1km"))



# 非可住土地利用種コード（森林、荒地、河川地及び湖沼、海水域、海浜、その他用地）
target_codes <- c("0500","0600","1100","1500","1400","1000")
# 全ての土地利用種コードを取得
all_landuse_codes <- names(landuse_wide)[!names(landuse_wide) %in% c("mesh_1km")]
# 可住土地利用種コード（target_codes以外）を取得
other_codes <- setdiff(all_landuse_codes, target_codes)

# 可住土地利用種の合計を追加
bnd_mesh_with_landuse <- bnd_mesh_with_landuse %>%
  mutate(across(all_of(other_codes), ~as.numeric(.x))) %>%
  mutate(
    habit_sum = rowSums(across(all_of(other_codes), ~coalesce(.x, 0)), na.rm = TRUE),#可住土地利用種の合計
    habit_category = ifelse(habit_sum <10, "less10", "over10")#10%以上で分類
  )

#plot
colors <- c("less10" = "red", "over10" = "lightblue")
plot(st_geometry(bnd_mesh_with_landuse),
     col = colors[bnd_mesh_with_landuse$habit_category],
     main = "可住土地利用種の割合")
legend("topleft", 
       legend = c("less10", "over10"),
       fill = colors,
       title = "メッシュ内の割合(％)",
       bg = "white")
plot(st_geometry(UEA_Fukuoka), 
     add = TRUE,                                       
     border = "blue", 
     lwd = 2)

save(bnd_mesh_with_landuse,file="data/bnd_mesh_with_landuse.xdr")

# over10のゾーン数をカウント
mesh_over10_count <- sum(bnd_mesh_with_landuse$habit_category == "over10", na.rm = TRUE)
print(paste0("over10のメッシュ数: ", mesh_over10_count))
print(paste0("総メッシュ数: ", nrow(bnd_mesh_with_landuse)))

bnd_mesh_wL10 <- bnd_mesh_with_landuse %>%
  filter(habit_category == "over10") %>%
  dplyr::select(KEY_CODE, geometry)

dsn_folder <- "data/processed/bnd_mesh_wL10"
layer_name <- "bnd_mesh_wL10"
st_write(bnd_mesh_wL10,
         dsn = dsn_folder,
         layer = layer_name,
         driver = "ESRI Shapefile",
         delete_layer = TRUE)



# #飛び地メッシュを除く、by労働力人口
# csv_path <- "data/raw/国勢調査労働力人口等1kmメッシュ/tblT001189S5030.csv" 
# laborpop <- read.csv(csv_path, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA")) 
# 
# laborpop <- laborpop %>%
#   dplyr::select(KEY_CODE, T001189001)%>%
#   mutate(across(everything(), ~{
#     numeric_value <- as.numeric(.)
#     ifelse(is.na(numeric_value), 0, numeric_value)
#   }))%>%
#   mutate(KEY_CODE = as.character(KEY_CODE))
# 
# bnd_mesh_wL10_wLP <- left_join(bnd_mesh_wL10, laborpop, by = "KEY_CODE") %>%
#   mutate(T001189001=replace_na(T001189001, 0))
# 
# # bnd_mesh_wL10_wLP <- bnd_mesh_wL10_wE %>%
# #   mutate(
# #     emp_category = ifelse(T001189001 <10, "less10", "over10") #10人で分類
# #   )
# 
# dsn_folder <- "data/processed/労働力人口"
# layer_name <- "bnd_mesh_wL10_wLP"
# st_write(bnd_mesh_wL10_wLP,
#          dsn = dsn_folder,
#          layer = layer_name,
#          driver = "ESRI Shapefile",
#          delete_layer = TRUE)


#従業者数
# 経済センサス従業員数1kmメッシュ
csv_path2 <- "data/raw/経済センサス従業員数1kmメッシュ/tblT000841S5030.csv" 
emp <- read.csv(csv_path2, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA")) 

emp <- emp %>%
  dplyr::select(KEY_CODE, T000841002)%>%
  mutate(across(everything(), ~{
    numeric_value <- as.numeric(.)
    ifelse(is.na(numeric_value), 0, numeric_value)
  }))%>%
  mutate(KEY_CODE = as.character(KEY_CODE))

save(emp,file="data/emp.xdr")

bnd_mesh_wL10_wE <- left_join(bnd_mesh_wL10, emp, by = "KEY_CODE") %>%
  mutate(T000841002=replace_na(T000841002, 0))

dsn_folder <- "data/processed/bnd_mesh_wL10_wE"
layer_name <- "bnd_mesh_wL10_wE"
st_write(bnd_mesh_wL10_wE,
         dsn = dsn_folder,
         layer = layer_name,
         driver = "ESRI Shapefile",
         delete_layer = TRUE)


# #focal
# emp_vect <- terra::vect(bnd_mesh_wL10_wE)
# emp_template <- terra::rast(emp_vect, resolution = 1000)
# emp_rst <- terra::rasterize(emp_vect, emp_template, field = "T000841002")
# 
# kernel_5X5 <- matrix(1, nrow = 5, ncol = 5)
# 
# emp_focal_sum <- terra::focal(emp_rst, w = kernel_5X5, fun ="sum", na.rm = TRUE)
# 
# output_folder <- "data/processed/平均化フィルター"
# output_filename <- "emp_focal_sum.tif" 
# terra::writeRaster(
#   emp_focal_sum,
#   filename = file.path(output_folder, output_filename),
#   filetype = "GTiff",  
#   overwrite = TRUE     
# )
# 
# plot(emp_focal_sum, main ="5X5 Focal mean (Employment)")


#離島など不要メッシュを手動で除く
bnd_mesh_wL10_filterd <- bnd_mesh_wL10_wE %>%
  filter(!KEY_CODE %in%  c(50301053,50306454,50306393,50306383,50306373,50306384,50306374,50306385,50303254,50303244,50303234))
nrow(bnd_mesh_wL10_filterd)

dsn_folder <- "data/processed/bnd_mesh_wL10_filterd"
layer_name <- "bnd_mesh_wL10_filterd"
st_write(bnd_mesh_wL10_filterd,
         dsn = dsn_folder,
         layer = layer_name,
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

key_code_sf<-bnd_mesh_wL10_filterd%>%
  dplyr::select(KEY_CODE,geometry)
save(key_code_sf,file="data/key_code_sf.xdr")

#####


#####

rm(list=ls())
gc();gc();

# Install required packages
# install.packages("sf")
# install.packages("dplyr")
# install.packages("raster")
# install.packages("terra")
# install.packages("rgdal")
# install.packages("nleqslv")
library(tidyr)
library(sf)
library(dplyr)
library(raster)
library(terra)
# library(rgdal)
library(tidyr)
library(cppRouting)
library(igraph)
library(nleqslv)
library(ggplot2)

#メートル座標系、JGD2011 2011 福岡2系
target_crs <- 6670

if(F){
  # source("tntp.R")
  load("data/osm/link10.xdr") # link10
  load("data/osm/node10.xdr") # node10
  
  # st_write(link10,dsn="data/osm/link10.shp")
  # which(is.na(link10$maxspeed))
  
  # capacity, alpha, beta, free.flow.time
  
  # capacity: pcu/hour
  # https://www.nilim.go.jp/lab/bcg/siryou/tnn/tnn0317pdf/ks0317005.pdf
  cap.df=data.frame(hwy=unique(link10$highway) %>% sort(),
                    cap=c(10^4,2500,1700,2500,1700,1500,1500,1250,1250,2500,1700))
  # 1500pcu/lane/hour
  # 日換算係数の考え方
  # https://www.nilim.go.jp/lab/bcg/siryou/tnn/tnn0317pdf/ks0317006.pdf
  KK=0.1
  # capacity pcu/day
  # cap.df$cap/KK
  
  # http://library.jsce.or.jp/jsce/open/00037/2002/695-0091.pdf
  # alpha=0.15;beta=4 #BPR
  # alpha=0.96;beta=1.2 # mizokami/matsui, 1989
  
  # https://bin.t.u-tokyo.ac.jp/kaken/pdf/traffic_assign%20tutrial%20for%20C.pdf
  # alpha=0.48;beta=2.82
  
  #https://www.jstage.jst.go.jp/article/thagis/17/2/17_203/_pdf
  alpha=0.48;beta=2.89 # yamada/matsui (1998)
  
  
  link10=link10 %>% mutate(FFtime=linklen/maxspeed*60/10^3) %>%  # fftime (minutes)
    merge(cap.df, by.x = "highway", by.y = "hwy", all.x = TRUE)
  
  
  nodes=data.frame(node10$ID,st_coordinates(node10))
  names(nodes)=c("Node","X","Y")
  
  # E:/WorkDir01/prog/R/2024/2024_TAP/cppRouting01.R
  
  
  tI=grep("mc_",node10$ID)
  # tM=expand.grid(node10$ID[tI],node10$ID[tI])
  # # head(tM)
  # trips=data.frame(from=tM[,1],to=tM[,2],demand=5)
  # dim(trips)
  zones=node10$ID[tI]
  # city certers
  work_zone<-c(50303302,50303206,50303395,50303208,50302364,50302356,50303344,50303393,50302390,50302290,50302347,50302329,50305475,50305318,50304377,50302422,50301491,50302176)
  tI=which(node10$ID%in%paste0("mc_",work_zone))
  centers=node10$ID[tI]
  
  save(work_zone,file="data/work_zone.xdr")
  
  sgr <- makegraph(df = link10[,c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(), 
                   directed = TRUE,
                   # capacity = 10^4,
                   capacity = link10$cap/KK,
                   alpha = alpha,
                   beta = beta,
                   coords = nodes)
  
  
  # estimate OD travel time under user zero traffic : minutes
  system.time({ # 0.13 
    dists0<-get_distance_matrix(sgr,
                                from=zones,
                                to=centers,
                                algorithm = "mch") # because of the rectangular shape of the matrix
  })
  
  save(dists0,file="data/dists0.xdr")
  
}
load("data/dists0.xdr") # dists0

## codes for traffic assignment
if(F){
  tM=expand.grid(zones,centers)
  trips=data.frame(from=tM[,1],to=tM[,2],demand=10)
  
  # traffic assignment
  system.time({ # 6.64 
    traffic01 <- assign_traffic(Graph = sgr,  from = trips$from, to = trips$to, demand = trips$demand, 
                                max_gap = 1e-6, algorithm = "bfw", verbose = FALSE)
  })
  save(traffic01,file="data/traffic_01.xdr")
  
  # create graph with speed under assigned traffic
  sgr2 <- makegraph(df = traffic01$data[,c("from","to","cost")], 
                    directed = TRUE,
                    capacity = 10^4,
                    alpha = alpha,
                    beta = beta,
                    coords = nodes)
  
  # estimate OD travel time under user equilibrium traffic assignment
  system.time({ # 0.13 
    dists<-get_distance_matrix(sgr2,
                               from=zones,
                               to=centers,
                               algorithm = "mch") # because of the rectangular shape of the matrix
  })
  
  head(dists)
  head(dists0)
  
  # # estimated link trafic 
  # estM.df=traffic$data %>% left_join(link10 %>% 
  #                                      st_drop_geometry() %>% 
  #                                      dplyr::select(highway,linkID,ID1,ID2,maxspeed,linklen),
  #                                    by=c("from"="ID1","to"="ID2"))
  # 
  # # merge(cap.df, by.x = "highway", by.y = "hwy", all.x = TRUE)
  # dim(estM.df)
}


#####
load("data/dists0.xdr") # dists0

# 経済センサス従業員数1kmメッシュ
load("data/emp.xdr") # emp

# 土地利用：可住地数
load("data/bnd_mesh_with_landuse.xdr") # bnd_mesh_with_landuse
# max(bnd_mesh_with_landuse$habit_sum)

load("data/key_code_sf.xdr")

load("data/work_zone.xdr")

#従業地賃金omega_j
csv.emp_by_ind<-"data/raw/経済センサス　1kmメッシュ　産業別従業者数/tblT001157S5030.csv"
emp_by_ind <- read.csv(csv.emp_by_ind, fileEncoding = "CP932", stringsAsFactors = FALSE, na.strings = c("", "NA"))

emp_by_ind <- emp_by_ind %>%
  dplyr::select(KEY_CODE,T001157023,T001157025,T001157026,T001157027,T001157029,T001157030,T001157031,T001157032,T001157033,T001157034,T001157035,T001157036,T001157037,T001157038,T001157039,T001157040,T001157041)%>%
  filter(KEY_CODE%in%work_zone)%>%
  mutate(across(everything(), ~{
    numeric_value <- as.numeric(.)
    ifelse(is.na(numeric_value), 0, numeric_value)
  }))%>%
  mutate(KEY_CODE = as.character(KEY_CODE))
colnames(emp_by_ind)<-c("KEY_CODE","all","Ind_C","Ind_D","Ind_E","Ind_F","Ind_G","Ind_H","Ind_I","Ind_J","Ind_K","Ind_L","Ind_M","Ind_N","Ind_O","Ind_P","Ind_Q","Ind_R")

M<-emp_by_ind%>%
  dplyr::select(starts_with("Ind_"))%>%
  as.matrix()
A<-emp_by_ind%>%
  dplyr::select("all")

csv.wage_by_ind<-"data/raw/賃金構造基本統計調査　福岡　産業別/Fukuoka_avg_wage.csv"
wage_by_ind<-read.csv(csv.wage_by_ind,stringsAsFactors=FALSE,na.strings=c("","NA"))#単位：千円
wage_by_ind<-wage_by_ind%>%
  # mutate(avg_wage=avg_wage*1000)
  mutate(avg_wage=avg_wage) # 千円単位にする
W<-pull(wage_by_ind,avg_wage)

estimated_avg_wage <-(M%*%W)/A$all
estimated_avg_wage<-floor(estimated_avg_wage)
omega_j <- emp_by_ind%>%
  dplyr::select(KEY_CODE)%>%
  dplyr::bind_cols(
    as.data.frame(estimated_avg_wage)%>%
      dplyr::rename("omega_j" = "V1")
  ) %>%
  dplyr::select(KEY_CODE, omega_j)

#従業世帯数　ボロノイ分割
emp_sf<-left_join(key_code_sf,emp,by="KEY_CODE")
center_mesh_sf<-emp_sf%>%
  dplyr::filter(KEY_CODE %in% work_zone)

centers_points<-st_centroid(center_mesh_sf)
meshes_points<-st_centroid(emp_sf)

voronoi_polys<-centers_points%>%
  st_geometry()%>%
  st_union()%>%
  st_voronoi()%>%
  st_collection_extract("POLYGON")%>%
  st_sf()
voronoi_polys<-st_join(voronoi_polys,centers_points,join=st_intersects)

# boundary<-st_union(emp_sf)
# voronoi_clipped<-st_intersection(voronoi_polys,boundary)

voronoi_points<-st_join(meshes_points,voronoi_polys,join = st_within)
# KEY_CODE.y がボロノイ領域のID (中心地のID)
# KEY_CODE.x が個々のメッシュID
L_j_hat<-voronoi_points%>%
  group_by(KEY_CODE.y)%>%
  summarise(
    L_j_hat=sum(T000841002.x, na.rm=TRUE)
  )%>%
  st_drop_geometry()%>%
  tibble::column_to_rownames(var="KEY_CODE.y")
sum(L_j_hat)  


#dist0のNAメッシュを除く
# 列 (従業地 j) の選別
valid_res_rows <- apply(dists0, 1, function(x) sum(!is.na(x)) > 1)
dists0 <- dists0[valid_res_rows, ]
# 行 (居住地 i) の選別
valid_work_cols <- apply(dists0, 2, function(x) sum(!is.na(x)) > 1)
dists0 <- dists0[, valid_work_cols]

print(paste("削除された孤立居住地数:", sum(!valid_res_rows)))
print(paste("削除された孤立従業地数:", sum(!valid_work_cols)))

# 関連データの同期 (work_zone, omega_j, L_j_hat を修正) 
# 残った従業地のIDリストを取得
remaining_work_ids <- colnames(dists0) %>% gsub("mc_", "", .)

# work_zone の更新
work_zone <- work_zone[work_zone %in% remaining_work_ids]

# B. omega_j (賃金データ) の更新
omega_j <- omega_j %>%
  dplyr::filter(KEY_CODE %in% remaining_work_ids)

# C. L_j_hat (目標人口データ) の更新
L_j_hat <- L_j_hat %>%
  dplyr::filter(rownames(.) %in% remaining_work_ids)

remaining_res_ids <- rownames(dists0) %>% gsub("mc_", "", .)
key_code_sf <- key_code_sf %>%
  dplyr::filter(KEY_CODE %in% remaining_res_ids)



#parameter
alpha_a = 0.3  
alpha_z = 0.7  #  (α_z + α_a = 1 と仮定)
p = 1          # 財価格  p=1 と仮定
alpha_0 = (alpha_z^alpha_z) * (alpha_a^alpha_a) 

theta_H = 1.0  
# gamma_0 = 10.0
gamma_0 = 1*10^-1
# gamma_1 = 0.8  
gamma_1 = 0.5

#grobal variable
nz_res <- nrow(dists0)
nz_work <- ncol(dists0)

#従業地の給料
omega_j_rownames<-omega_j%>%
  tibble::column_to_rownames(var = "KEY_CODE")
omega_j_t<-t(omega_j_rownames)
omega_j_vector<-as.vector(omega_j_t)
omega_j_matrix<-matrix(
  rep(omega_j_vector,times=nz_res),
  nrow=nz_res,
  ncol=ncol(omega_j_t),
  byrow=TRUE
)
colnames(omega_j_matrix)<-colnames(omega_j_t)


# c_ij <- dists0*175 #時間費用掛け算
# c_ij <- dists0*1600 #時間費用掛け算
c_ij <- dists0*1.600 #時間費用掛け算：千円単位

disposable_income_ij = pmax(-c_ij+omega_j_matrix, 0)

k_i = rep(0.1, nz_res)

# habitable area
library(units)
habd=bnd_mesh_with_landuse %>% mutate(area=st_area(.) %>% drop_units()) %>% 
  dplyr::select(KEY_CODE,habit_sum,area) %>% mutate(hab_a=area*habit_sum/100) %>% 
  st_drop_geometry() %>% dplyr::select(KEY_CODE,hab_a)
# typeof(habd$KEY_CODE)
habz=data.frame(zones=rownames(dists0) %>% gsub("mc_","",.)) %>% left_join(habd,by=c("zones"="KEY_CODE"))

G0_i=habz$hab_a # habitable area, sqm
phi_pub=0.5 # public space

# G_i = rep(150000, nz_res) # sqm: land
rr_a=0.1 # agriculture land rent (000 yen/m2/month)
theta_L=2
phi=0.6 # building coverage ratio

theta_H=0.1
# theta_H=1


#均衡状態計算
caluculate_model_state<-function(v_j_vec){ # v_j_vec=exp(v_j_vec2); v_j_vec=exp(v_start)
  v_j_matrix = matrix(v_j_vec, nrow = nz_res, ncol = nz_work, byrow = TRUE)
  
  #eq.11
  r_ij_H=(alpha_0*disposable_income_ij/v_j_matrix)^(1/alpha_a)
  r_ij_H[is.nan(r_ij_H)]<-0
  
  #eq.12
  exp_r=exp(theta_H*r_ij_H)
  exp_r[which(r_ij_H==0)]=0 # disposable_income_ij=0の時，立地確率を強制的にゼロにする．
  sum_exp_r_per_i=rowSums(exp_r,na.rm=T) # na.rm=T added @251204
  P_j_given_i=exp_r/pmax(sum_exp_r_per_i, 1e-9)#0割防止
  
  #eq.13
  r_bar_i=rowSums(r_ij_H*P_j_given_i,na.rm=TRUE)
  # as.numeric(r_bar_i)
  
  # eq.15(new)
  power_gamma=gamma_1/(1-gamma_1)
  rg_i=phi*(1-gamma_1)*(r_bar_i*gamma_0)^(1/(1-gamma_1))*(gamma_1/k_i)^power_gamma
  # as.numeric(rg_i)
  G_i=G0_i*exp(theta_L*(rg_i-rr_a))/(1+exp(theta_L*(rg_i-rr_a)))*phi_pub*phi
  
  #eq.14
  # power_gamma=gamma_1/(1-gamma_1)
  A_Fi_S=gamma_0*((r_bar_i*gamma_0*gamma_1)/k_i)^power_gamma*G_i
  A_Fi_S[is.nan(A_Fi_S)]<-0
  # building height
  FF=A_Fi_S/G_i
  # as.numeric(FF)
  
  #eq.15
  A_fij_S=A_Fi_S*P_j_given_i
  
  #eq.9
  r_bar_i_matrix=matrix(r_bar_i,nrow=nz_res,ncol=nz_work)
  # a_fij_H=alpha_a*disposable_income_ij/pmax(r_bar_i_matrix, 1e-9) # in case of disposable_income==0, population should be zero
  a_fij_H=alpha_a*disposable_income_ij/pmax(r_ij_H, 1e-9) # in case of disposable_income==0, population should be zero
  
  # r_bar_i_matrix[,"mc_50305475"]
  # a_fij_H[,"mc_50305475"]
  
  #eq.16
  l_i_j=A_fij_S/pmax(a_fij_H, 1e-9) 
  l_i_j[is.nan(l_i_j)] <- 0
  l_i_j[which(a_fij_H==0)]=0
  
  # sum(l_i_j[,"mc_50305475"],na.rm=T)
  
  #eq.17
  L_j_tilde=colSums(l_i_j,na.rm = TRUE)
  
  #
  return(list(
    L_j_tilde = L_j_tilde, 
    r_bar_i = r_bar_i,     
    l_i_j = l_i_j,       
    r_ij_H = r_ij_H,     
    a_fij_H = a_fij_H    
  ))
}


# 均衡条件のベクトルを返す
gapf_vj<-function(v_j_vec2){ # v_j_vec2=v_start
  # v_j_vec=abs(v_j_vec) # これはおかしいkii@251203: 正であることを保証するなら，v_j_vec2=exp(v_j_vec)などとすべき
  v_j_vec=exp(v_j_vec2)
  
  model_state=caluculate_model_state(v_j_vec)
  L_j_tilde=model_state$L_j_tilde
  
  # gap_vector=(L_j_tilde-L_j_hat)/L_j_hat　#人口差が巨大だったから割合にした
  gap_vector=(L_j_tilde-L_j_hat$L_j_hat) #%>% 
    # as.numeric()
    # gap_vector$L_j_hat
  # length(L_j_tilde)
  # length(L_j_hat)
  return(gap_vector)
}


#初期値の設定(vを逆算)
# assumed_r_mean=1500 #これちゃんと調べる！！
# mean_dispossable_income_j=colMeans(disposable_income_ij,na.rm = TRUE)
# v_start=alpha_0*mean_dispossable_income_j/(assumed_r_mean^alpha_a)
# v_start = v_start * 20　# 初期値を意図的に大きくして、地代を下げ、供給過多を抑える

# 初期効用の推計: 床地代は2000円/m2と仮定
# rr0=0. # 000yen/sqm/month
# v_j_mat0=alpha_0*disposable_income_ij/rr0^alpha_a
# v_j_vec0=colMeans(v_j_mat0,na.rm = TRUE)
v_j_vec0=rep(50,nz_work)
v_start=log(v_j_vec0)
v_j_vec2=v_start
v_j_vec=exp(v_start)

# v_start=rep(5*10^3,ncol(disposable_income_ij))
# v_start=log(v_start)
# names(v_start) = colnames(omega_j_matrix)
# View(v_start)
gapf_vj(v_start)

#均衡解の推定
result_nleqslv <- nleqslv(
  x = v_start,
  fn = gapf_vj,
  global = "dbldog",
  control = list(ftol=1e-8, xtol=1e-8, maxit=200)
)

result_nleqslv$x
gapf_vj(result_nleqslv$x)

print(result_nleqslv$termcd) # 1なら成功
print(result_nleqslv$x)      # 均衡効用



#最終状態カクニン
v_equilibrium <- result_nleqslv$x %>% exp()
final_state <- caluculate_model_state(v_equilibrium)

final_state$r_bar_i
final_state$r_ij_H

final_state$L_j_tilde
L_j_hat



# 居住・従業地別 世帯数 (l_i_j) の確認
final_L_i_j=final_state$l_i_j

# 計算した：メッシュごと世帯数
zone_population <- rowSums(final_state$l_i_j,na.rm=TRUE)
zone_population<-tibble(
  KEY_CODE=rownames(final_state$l_i_j),
  total_population=zone_population
)%>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))

zone_population<-left_join(key_code_sf,zone_population,by="KEY_CODE")

dsn_folder <- "data/processed/final_zone_population"
layer_name <- "final_zone_population"
st_write(zone_population,
         dsn = dsn_folder,
         layer = layer_name,
         driver = "ESRI Shapefile",
         delete_layer = TRUE)
# プロット
# きりの良い数字にするなら（例: 30000など）、手動で設定してもOKです
max_pop_val <- 25000
choropleth_map <- zone_population %>%
  ggplot() +
  geom_sf(aes(fill = total_population), 
          color = "gray50",  # メッシュ境界線の色
          linewidth = 0.1 # メッシ
          ) + # メッシュ境界線の太さ

  scale_fill_viridis_c(
    option = "magma", # 'viridis', 'plasma', 'cividis', 'magma' などから選択
    name = "モデル：居住世帯数",
    direction = -1,
    limits = c(0, max_pop_val), # ★ここで範囲を固定！
    labels = scales::label_comma()
  ) +
  labs(
    title = "モデル：居住世帯数",
    caption = "データソース: zone_population"
  ) +
  theme_minimal() +
  coord_sf(datum = NA) # datum = NA で緯度経度グリッドを非表示
print(choropleth_map)


# # 人口上位5ゾーンを表示
# print(head(sort(zone_population, decreasing = TRUE), 5))


#実データ：メッシュごと世帯数
csv.household<-"data/raw/国勢調査_人口及び世帯数_1kmメッシュ/tblT001100S5030.csv"
household<-read.csv(csv.household,fileEncoding = "CP932",stringsAsFactors=FALSE,na.strings= c("", "NA"))%>%
  dplyr::select(KEY_CODE,T001100035)%>%
  dplyr::rename(household="T001100035")%>%
  dplyr::mutate(KEY_CODE=as.character(KEY_CODE),household=as.numeric(household))
household<-household[-1,]
household<-left_join(key_code_sf,household,by="KEY_CODE") %>% 
  dplyr::mutate(
    household = tidyr::replace_na(household, 0)
  )
  
dsn_folder <- "data/processed/実データ：メッシュごと世帯数"
layer_name <- "household"
st_write(household,
         dsn = dsn_folder,
         layer = layer_name,
         driver = "ESRI Shapefile",
         delete_layer = TRUE)

#プロット
choropleth_map_1<- household %>%
  ggplot() +
  geom_sf(aes(fill = household), 
          color = "gray50",  # メッシュ境界線の色
          linewidth = 0.1) + # メッシュ境界線の太さ
  scale_fill_viridis_c(
    option = "magma", # 'viridis', 'plasma', 'cividis', 'magma' などから選択
    name = "実データ：居住世帯数",
    direction = -1,
    limits = c(0, max_pop_val), # ★実データと同じ範囲を指定！
    labels = scales::label_comma()
  ) +
  labs(
    title = "実データ：居住世帯数",
    caption = "データソース: household"
  ) +
  theme_minimal() +
  
  coord_sf(datum = NA) # datum = NA で緯度経度グリッドを非表示

# プロットを表示
print(choropleth_map_1)





key_code_sf_w <- key_code_sf %>%
  dplyr::mutate(
    is_work_zone = ifelse(KEY_CODE %in% work_zone, 1, 0)
  )
dsn_folder <- "data/processed/key_code_sf_w"
layer_name <- "key_code_sf_w"
st_write(key_code_sf_w,
         dsn = dsn_folder,
         layer = layer_name,
         driver = "ESRI Shapefile",
         delete_layer = TRUE)
#####

#家賃の比較
#家賃の確認
final_r_bar_i=final_state$r_bar_i
final_r_ij_H=final_state$r_ij_H

rent_df <- data.frame(
  KEY_CODE = names(final_state$r_bar_i), 
  market_rent = as.numeric(final_state$r_bar_i) # 数値化
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))

rent_map_data <- left_join(key_code_sf, rent_df, by = "KEY_CODE") 
print(summary(rent_map_data$market_rent))

# 家賃マップのプロット 
rent_plot <- ggplot(rent_map_data) +
  geom_sf(aes(fill = market_rent), 
          color = "gray50",  # メッシュ境界線の色
          linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",         # 家賃は "plasma" (青→赤→黄) が見やすいことが多いです
    name = "平均付値地代\n(千円/m2)", # 単位に合わせて修正してください
    direction = -1             # 色の反転（高い方を明るく/濃くするかはお好みで）
  ) +
  labs(
    title = "モデル：平均付値地代 (r_bar_i) "
  ) +
  theme_void()

print(rent_plot)




#アットホームデータの集計
athome <- st_read("data/raw/athome_date") %>% 
  st_transform(crs = target_crs)
athome_mesh <- st_join(athome,key_code_sf, join=st_intersects)
athome_crop <- athome_mesh %>% 
  filter(!is.na(KEY_CODE))  #251224データ重たいからいったんここでdataに保存した方がいいかも
save(athome_crop,file="data/athome_crop.xdr")
load("data/athome_crop.xdr")

athome_crop <- athome_crop %>% 
  dplyr::select(rent,room_ar,KEY_CODE) %>% 
  mutate(rent_par_ar=rent/room_ar)
athome_crop<-athome_crop %>% 
  st_drop_geometry() %>% 
  group_by(KEY_CODE) %>% 
  summarise(
    avg_rent=mean(rent_par_ar, na.rm=TRUE)
  ) 
athome_crop<-key_code_sf %>% 
  left_join(athome_crop, by="KEY_CODE")


#プロット
rent_real_plot <- ggplot(athome_crop) +
  geom_sf(aes(fill = avg_rent), 
          color = "gray50",  # メッシュ境界線の色
          linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",         # 家賃は "plasma" (青→赤→黄) が見やすいことが多いです
    name = "平均家賃\n(千円/m2)", # 単位に合わせて修正してください
    direction = -1             # 色の反転（高い方を明るく/濃くするかはお好みで）
  ) +
  labs(
    title = "実データ：平均家賃 (r_bar_i) "
  ) +
  theme_void()

print(rent_real_plot)


