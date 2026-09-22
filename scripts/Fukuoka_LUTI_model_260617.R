#init####
rm(list=ls())
gc();gc();

# Install required packages
# install.packages("sf")
# install.packages("dplyr")
# install.packages("raster")
# install.packages("terra")
# install.packages("rgdal")
# install.packages("janitor")
# install.packages("ggplot2")
library(tidyr)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(janitor)
library(ggplot2)

#メートル座標系、JGD2011 2011 福岡2系
target_crs <- 6670
# crs_info <- st_crs(UEA_Fukuoka)
# print(crs_info)

# 福岡の都市雇用圏
MEA <- st_read("data/UEA") %>%
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
     main = "Percentage of Habitable Land Use Types")
legend("topleft",
       legend = c("less10", "over10"),
       fill = colors,
       title = "Percentage within Mesh (%)",
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

bnd_mesh_wL10_wE=st_read("data/processed/bnd_mesh_wL10_wE")


#離島など不要メッシュを手動で除く
bnd_mesh_wL10_filterd <- bnd_mesh_wL10_wE %>%
  filter(!KEY_CODE %in%  c(50301053,50306454,50306393,50306383,50306373,50306384,50306374,50306385,50303254,50303244,50303234,50300426)) # 260525 50300426を除外
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

plot(bnd_mesh_crop$geometry,col="gray")
plot(UEA_Fukuoka$geometry,add=T,border="blue",col=NA,lwd=2)
plot(key_code_sf$geometry,add=T,col="green" )
save(bnd_mesh_crop,file="data/bnd_mesh_crop.xdr")
save(UEA_Fukuoka,file="data/UEA_Fukuoka.xdr")


#####


#ここから####

rm(list=ls())
gc();gc();

# install.packages("ggplot2")
# install.packages("units")
# install.packages("stringr")
# install.packages("cppRouting")
# install.packages("igraph")
# install.packages("nleqslv")
# install.packages("units")
# install.packages("stringr")
# install.packages("tidyr")


library(tidyr)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(tidyr)
library(cppRouting)
library(igraph)
library(nleqslv)
library(ggplot2)
library(units)
library(stringr)

#メートル座標系、JGD2011 2011 福岡2系
target_crs <- 6670


load("data/dists0.xdr") # dists0
# which(is.na(dists0))
# 経済センサス従業員数1kmメッシュ
load("data/emp.xdr") # emp

# 土地利用：可住地数
load("data/bnd_mesh_with_landuse.xdr") # bnd_mesh_with_landuse
load("data/key_code_sf.xdr")
load("data/work_zone.xdr") # work_zone


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

#従業世帯数　L_j_hat ボロノイ分割
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
# sum(L_j_hat)  


#dist0のNAメッシュを除く####
# 列 (従業地 j) の選別
valid_res_rows <- apply(dists0, 1, function(x) sum(!is.na(x)) > 1)
 # mc_50300426を除外対象に追加
valid_res_rows["mc_50300426"] <- FALSE
dists0 <- dists0[valid_res_rows, ]
# 行 (居住地 i) の選別
valid_work_cols <- apply(dists0, 2, function(x) sum(!is.na(x)) > 1)
dists0 <- dists0[, valid_work_cols]

"mc_50300426" %in% rownames(dists0)


# dists00=dists0
# save(dists0,file="dists00.xdr")
# dim(dists0)
# which(is.na(dists0))

print(paste("削除された孤立居住地数:", sum(!valid_res_rows)))
print(paste("削除された孤立従業地数:", sum(!valid_work_cols)))
dim(dists0)

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


# save(omega_j,file="data/omega_j.xdr")
# save(L_j_hat,file="data/L_j_hat.xdr")

remaining_res_ids <- rownames(dists0) %>% gsub("mc_", "", .)
key_code_sf <- key_code_sf %>%
  dplyr::filter(KEY_CODE %in% remaining_res_ids)



#parameter####
alpha_a = 0.3  #
alpha_z = 0.7 #  (α_z + α_a = 1 と仮定)
p = 1          # 財価格  p=1 と仮定
alpha_0 = (alpha_z^alpha_z) * (alpha_a^alpha_a) 

# gamma_0 = 10.0
gamma_0 = 0.2
# gamma_1 = 0.8  
gamma_1 = 0.6

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

# save(omega_j_matrix,file="data/omega_j_matrix.xdr")
c_ij <- dists0*1.600 #時間費用掛け算：千円単位1.600から変更：：distは分単位の所要時間．月の時間費用（千円/月）であってますか？
# which(is.na(c_ij))
disposable_income_ij = pmax(-c_ij+omega_j_matrix, 0)
k_i = rep(0.1, nz_res)

# habitable area
habd=bnd_mesh_with_landuse %>% mutate(area=st_area(.) %>% drop_units()) %>% 
  dplyr::select(KEY_CODE,habit_sum,area) %>% mutate(hab_a=area*habit_sum/100) %>% 
  st_drop_geometry() %>% dplyr::select(KEY_CODE,hab_a)
# typeof(habd$KEY_CODE)
habz=data.frame(zones=rownames(dists0) %>% gsub("mc_","",.)) %>% left_join(habd,by=c("zones"="KEY_CODE"))

G0_i=habz$hab_a # habitable area, sqm
# save(G0_i,file="data/G0_i.xdr")

phi_pub=0.6 # public space
# G_i = rep(150000, nz_res) # sqm: land
rr_a=0.1 # agriculture land rent (000 yen/m2/month)
theta_L=2
phi=0.5 # building coverage ratio
floor_height_m=3 # 1階あたりの階高（m）：階数→建物高さ換算用の仮定値
theta_H=0.1 #260106 theta_H=0.1から0.2に調整


# mode choice####
# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_modeChoide_260309.R
load("data/result.mc02.xdr") # result: mode choice parameters
# dists00 %>% dim()
# travel time
load("data/dists0.bike.xdr") # dists0.bike
# dim(dists0.bike)
dists0.bike <- dists0.bike[valid_res_rows, ]
dists0.bike <- dists0.bike[, valid_work_cols]
# dim(dists0)
# dim(dists0.bike)
# which(rownames(dists0.bike)!=rownames(dists0))
# which(colnames(dists0.bike)!=colnames(dists00))

# load("data/dists0.xdr") # dists0 , road time distance under free flow, minutes
dists0.road=dists0
# dists0.road <- dists0.road[valid_res_rows, ]
# dists0.road <- dists0.road[, valid_work_cols]
# which(rownames(dists0.road)!=rownames(dists00))
# which(colnames(dists0.road)!=colnames(dists00))

# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_OSM_03.R
load("data/railway_dist.xdr") # dists0, minutes per one way: do not depend on traffic　ここでdists0filter前に戻ってる
dists0.rail=dists0
dists0 <- dists0.road 
# 260824: dists0.railはkey_code_sf由来のゾーン集合で再構築しており、valid_res_rows/valid_work_cols
# （道路dists0の元の行数・順序が前提）と行数が一致しない場合があるため、位置ベースではなく
# 名前ベースで絞り込む（mc_50300426のようにdists0.rail側に元々存在しないゾーンがあっても安全）。
dists0.rail <- dists0.rail[rownames(dists0.rail) %in% names(valid_res_rows)[valid_res_rows], ]
dists0.rail <- dists0.rail[, colnames(dists0.rail) %in% names(valid_work_cols)[valid_work_cols]]
# which(rownames(dists0.rail)!=rownames(dists0.road))
# which(colnames(dists0.rail)!=colnames(dists00))

zones=dists0.rail %>% rownames()
centers=dists0.rail %>% colnames()
# dim(dists0.bike)
# dim(dists0.road)
# dim(dists0.rail)

# need land rent for parking cost estimation
# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_Rosenka01.R
load("data/LP_mean_2025.xdr") # mean_by_poly
tI=which(is.nan(mean_by_poly$mean_value))
# mean_by_poly[640:670,]
mean_by_poly$mean_value[tI]=mean_by_poly$mean_value[tI-1] # heuristic: need to modify at interpolation of land price

# head(mean_by_poly)
oz=rownames(dists0.bike) %>% str_extract("\\d+$") |> as.character()
dz=colnames(dists0.bike) %>% str_extract("\\d+$") |> as.character()
LR.o=data.frame(oz=oz) %>% left_join(mean_by_poly,by=c("oz"="ID"))
LR.d=data.frame(dz=dz) %>% left_join(mean_by_poly,by=c("dz"="ID"))
# assume 3% of land price for annual land rent, 12.5 sqm for parking lot: yen/day
parkPrice=outer(LR.o$mean_value*0.03/365*12.5,LR.d$mean_value*0.03/365*12.5,FUN="+")



# initial utilty vector
v_j_vec0=rep(270,nz_work) #50→500→100→80→60
v_start=log(v_j_vec0)

# functions for LU model####
#土地利用モデル：与えられた効用水準下での立地，消費量
caluculate_model_state<-function(v_j_vec){ # v_j_vec=exp(v_j_vec2); v_j_vec=exp(v_start), v_j_vec=v_equilibrium
  v_j_matrix = matrix(v_j_vec, nrow = nz_res, ncol = nz_work, byrow = TRUE)
  # v_j_matrix = matrix(v_equilibrium, nrow = nz_res, ncol = nz_work, byrow = TRUE)
  
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
  # building height: FF=容積率（延床面積/宅地面積）、stories_i=推定階数（容積率/建蔽率）、height_i=推定建物高さ
  FF=A_Fi_S/G_i
  FF[is.nan(FF)]<-0
  FF[is.infinite(FF)]<-0
  stories_i=FF/phi
  height_i=stories_i*floor_height_m
  # as.numeric(FF)

  #eq.17
  A_fij_S=A_Fi_S*P_j_given_i
  
  #eq.9
  r_bar_i_matrix=matrix(r_bar_i,nrow=nz_res,ncol=nz_work)
  # a_fij_H=alpha_a*disposable_income_ij/pmax(r_bar_i_matrix, 1e-9) # in case of disposable_income==0, population should be zero
  a_fij_H=alpha_a*disposable_income_ij/pmax(r_ij_H, 1e-9) # in case of disposable_income==0, population should be zero
  
  #eq.18
  l_i_j=A_fij_S/pmax(a_fij_H, 1e-9) 
  l_i_j[is.nan(l_i_j)] <- 0
  l_i_j[which(a_fij_H==0)]=0
  
  #eq.19
  L_j_tilde=colSums(l_i_j,na.rm = TRUE)
  
  #return
  return(list(
    L_j_tilde = L_j_tilde, 
    r_bar_i = r_bar_i,     
    l_i_j = l_i_j,       # residence i, work place j
    r_ij_H = r_ij_H,     
    a_fij_H = a_fij_H, 
    rg_i = rg_i,
    G_i = G_i,
    FF = FF,
    stories_i = stories_i,
    height_i = height_i
  ))
}

# 均衡条件のベクトルを返す
gapf_vj<-function(v_j_vec2){ # v_j_vec2=v_start, 
  # v_j_vec=abs(v_j_vec) # これはおかしいkii@251203: 正であることを保証するなら，v_j_vec2=exp(v_j_vec)などとすべき
  v_j_vec=exp(v_j_vec2) # 消費モデルの効用水準
  
  model_state=caluculate_model_state(v_j_vec)
  L_j_tilde=model_state$L_j_tilde
  
  gap_vector=(L_j_tilde-L_j_hat$L_j_hat) 
  return(gap_vector)
}


# road network####
# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_OSM_03.R
load("data/osm/link10.xdr") # link10
load("data/osm/node10.xdr") # node10
# capacity: pcu/hour
# https://www.nilim.go.jp/lab/bcg/siryou/tnn/tnn0317pdf/ks0317005.pdf
cap.df=data.frame(hwy=unique(link10$highway) %>% sort(),
                  cap=c(10^4,2500,1700,2500,1700,1500,1500,1250,1250,2500,1700))
# 1500pcu/lane/hour
# 日換算係数の考え方
# https://www.nilim.go.jp/lab/bcg/siryou/tnn/tnn0317pdf/ks0317006.pdf
KK=0.1
#https://www.jstage.jst.go.jp/article/thagis/17/2/17_203/_pdf
alpha=0.48;beta=2.89 # yamada/matsui (1998)

link10=link10 %>% mutate(FFtime=linklen/maxspeed*60/10^3) %>%  # fftime (minutes)
  merge(cap.df, by.x = "highway", by.y = "hwy", all.x = TRUE)
nodes=data.frame(node10$ID,st_coordinates(node10))
names(nodes)=c("Node","X","Y")

sgr <- makegraph(df = link10[,c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(), 
                 directed = TRUE,
                 # capacity = 10^4,
                 capacity = link10$cap/KK,
                 alpha = alpha,
                 beta = beta,
                 coords = nodes)

# 鉄道網（リンクレベル）####
# scripts/Fukuoka_OSM_03_copied.R の「rail network」節で構築（要事前実行、data/はGit管理外）。
# 鉄道は混雑関数を持たない（速度低下を考慮しない）ため、capacityは大容量プレースホルダ。
# 道路側の link10/nodes/sgr とは別名にする（run_scenario内で道路側と同時に使うため）。
load("data/osm/rail_network.xdr")        # rail_network
load("data/osm/rail_network.nodes.xdr")  # rail_network.nodes

lkspd_rail <- data.frame(type = c("rail", "subway", "access"), speed = c(60, 40, 5))

link10_rail <- rbind(rail_network, rail_network %>% rename(ID1 = ID2, ID2 = ID1)) %>%
  left_join(lkspd_rail, by = "type") %>%
  mutate(FFtime = length / speed * 60 / 10^3, cap = 1e5)  # FFtime（分）

nodes_rail <- data.frame(rail_network.nodes$nodeID, st_coordinates(rail_network.nodes))
names(nodes_rail) <- c("Node", "X", "Y")

sgr_rail <- makegraph(df = link10_rail[, c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(),
                       directed = TRUE,
                       capacity = 1e5,
                       alpha = 0.1,
                       beta = 1,
                       coords = nodes_rail)

# fixed variables
# mode choice utilities
para=result$estimate
V.bike=para[1]*dists0.bike
V.rail=para[1]*dists0.rail+para[3]

# which(is.na(dists0.rail))

# 260820: ゾーンメッシュの塗り分け地図を作る共通関数。スクリプト後半の各セクション（モデル適合度検証、
# シナリオ比較など）で使われるため、if(FALSE)ブロック等に紛れないよう早い段階で定義しておく。
make_map <- function(data, fill_var, title, option, limits) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = option, direction = -1,
                         limits = limits, oob = scales::squish,
                         labels = scales::label_comma()) +
    labs(title = title) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

# 差分（シナリオ間比較など）を発散配色で塗り分ける共通関数。make_map同様、早い段階で定義しておく。
make_diff_map2 <- function(data, fill_vars, titles, unit_label = "変化量") {
  # 共通スケール（全変数の最大絶対値）
  lim <- sapply(fill_vars, function(v) max(abs(data[[v]]), na.rm = TRUE)) %>% max()

  plots <- Map(function(var, title) {
    ggplot(data) +
      geom_sf(aes(fill = .data[[var]]), color = "gray70", linewidth = 0.1) +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", midpoint = 0,
        limits = c(-lim, lim),
        labels = scales::label_comma(),
        name = unit_label
      ) +
      labs(title = title) +
      theme_void() +
      theme(
        plot.title      = element_text(size = 10, hjust = 0.5),
        legend.title    = element_text(size = 8),
        legend.text     = element_text(size = 7),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "gray80", color = NA)
      )
  }, fill_vars, titles)

  Reduce(`+`, plots)  # patchwork で結合
}




# LUTI ####
if(F){
flag01=0
alpha01=0.5
ii=1
tt0=proc.time()
while(flag01==0){
  # for(ii in 1:30){ # ii=1

  V.car=para[1]*dists0.road+para[2]+para[4]*parkPrice/2 # todo: 地価の変動を駐車場価格に反映（未対応＠260316）

  den=exp(V.bike)+exp(V.car)+exp(V.rail)
  P.bike=exp(V.bike)/den
  P.car=exp(V.car)/den
  P.rail=exp(V.rail)/den

  # average generalized converted cost (minutes/trip), assume average
  agcc=(P.bike*V.bike+P.car*V.car+P.rail*V.rail)/para[1]
  # which(is.na(V.rail))
  c_ij <- agcc*1.600 #時間費用掛け算：千円単位1.600から変更：：distは分単位の所要時間．月の時間費用（千円/月）であってますか？ OK
  disposable_income_ij = pmax(-c_ij+omega_j_matrix, 0)

  #均衡解の推定
  result_nleqslv <- nleqslv(
    x = v_start,
    fn = gapf_vj,
    global = "dbldog",
    control = list(ftol=1e-8, xtol=1e-8, maxit=200)
  )

  #最終状態カクニン
  v_equilibrium <- result_nleqslv$x %>% exp()
  final_state <- caluculate_model_state(v_equilibrium)
  # aa=final_state$l_i_j
  # dim(aa)
  # dim(dists0.road)

  # car OD demand
  # which(is.na(final_state$l_i_j))
  ODD.car=final_state$l_i_j*P.car
  trips <- as.data.frame.table(ODD.car, responseName = "demand")
  names(trips) <- c("from", "to", "demand")
  # which(is.na(trips$demand))

  traffic01 <- assign_traffic(Graph = sgr,  from = trips$from, to = trips$to, demand = trips$demand,
                              max_gap = 1e-2, algorithm = "bfw", verbose = FALSE)

  sgr2 <- makegraph(df = traffic01$data[,c("from","to","cost")],
                    directed = TRUE,
                    capacity = 10^4,
                    alpha = alpha,
                    beta = beta,
                    coords = nodes)


  # estimate OD travel time under user equilibrium traffic assignment
  dists0.road.b=dists0.road
  system.time({ # 0.13
    dists1.road<-get_distance_matrix(sgr2,
                                     from=zones,
                                     to=centers,
                                     algorithm = "mch") # because of the rectangular shape of the matrix
  })

  dists0.road=alpha01*dists1.road+(1-alpha01)*dists0.road.b
  # cat("ii=",ii,", diff=",sum((dists0.road.b-dists0.road)^2),"\n")
  diff01=sum((dists0.road.b-dists0.road)^2)
  cat("ii=",ii,", diff=",diff01,"\n")
  ii=ii+1
  if(diff01<100 | ii>10^3){
    flag01=1
  }

}
proc.time()-tt0
}



# GA較正（住宅・土地統計調査ベース、852次元＝5変数+ゾーン別k_i）####
# project_log/calibration/07_ga-housing-survey-target/ に対応。
library(GA)
library(parallel)
library(doParallel)

# 住宅・土地統計調査の年次系列（本体L.1137でも同じものを定義している）
yyv1 <- c(1993, 2003, 2008, 2013, 2018, 2023)

# ---- 世帯数（国勢調査メッシュ。世帯数の目標値とメッシュ→市区町村集計の重みに使う）----
csv.household <- "data/raw/国勢調査_人口及び世帯数_1kmメッシュ/tblT001100S5030.csv"
household <- read.csv(csv.household, fileEncoding = "CP932", stringsAsFactors = FALSE,
                      na.strings = c("", "NA")) %>%
  dplyr::select(KEY_CODE, T001100035) %>%
  dplyr::rename(obs_hh = "T001100035") %>%
  dplyr::mutate(KEY_CODE = as.character(KEY_CODE), obs_hh = as.numeric(obs_hh))
household <- household[-1, ]

# ---- 市区町村単位の実データ準備（住宅・土地統計調査、2023年）----
load("data/area.df.xdr")   # area.df, dwelling size (m2)
load("data/hhn.df.xdr")    # hhn.df, household size (member)
load("data/rent.df.xdr")   # rent.df, 1か月当たり家賃（円/月、２人世帯・建て方総数・家賃０円を含まない。
                           # 借家のみ。data/raw/住宅土地統計調査_家賃/README.md参照。
                           # scripts/build_rent_df_from_estat.R で生成）
target_year <- 2023  # モデルの推定時点に最も近い年（06トライアルと同じ方針）

# extract Fukuoka-ken
tI01 <- grep("^40", area.df$mun)
tI02 <- grep("^40", hhn.df$mun)

# Area per person
ApP <- (area.df[tI01, -1] %>% mutate(across(everything(), as.numeric))) /
  (hhn.df[tI02, -1]  %>% mutate(across(everything(), as.numeric)))
area_2p <- ApP * 2  # 2人世帯換算 床面積(m2)。06トライアルと同じ変換
colnames(area_2p) <- yyv1
area_target_mun <- tibble(mun = area.df$mun[tI01], area_2p) %>%
  dplyr::transmute(mun, area_target_mun = .data[[as.character(target_year)]])

# rent.dfは build_rent_df_from_estat.R の時点で既に「２人」世帯人員でフィルタ済み
# （家賃は世帯人員別の統計表がそのまま存在するため、床面積のような2人世帯換算は不要）。
# 単位は円/月なので、同じ２人世帯換算の床面積（m2）で割って円/m2/月にする
# （モデル側のr_bar_i（千円/m2/月）×1000＝円/m2/月と比べるため）。
# ★注意（07は承知の上で実行）：家賃は借家のみ、area_target_munは総数（持家込みと推定）なので
# 分母が借家の実際の床面積より大きくなり、円/m2は低めに出る。
# project_log/calibration/07_*/meta.yml のcaveats参照。
rent_target_mun <- rent.df %>%
  dplyr::filter(grepl("^40", mun)) %>%
  dplyr::transmute(mun, rent_month = .data[[paste0("rent_", target_year)]]) %>%
  dplyr::left_join(area_target_mun, by = "mun") %>%
  dplyr::transmute(mun, rent_target_mun = rent_month / area_target_mun)  # 円/m2/月

# ---- メッシュ(KEY_CODE)→市区町村の対応付け（06_housing-survey-comparisonと同じ）----
# 市区町村境界の作成（国土数値情報N03、2023年）。詳細は data/raw/住宅土地統計調査_市区町村境界/README.md 参照。
if(F){
  city_raw <- st_read("data/raw/住宅土地統計調査_市区町村境界/N03-20230101_40_GML/N03-23_40_230101.shp",
                       options = "ENCODING=CP932") %>%
    st_transform(crs = target_crs) %>%
    dplyr::rename(mun = N03_007, CITY_NAME = N03_004)
  city_sf <- city_raw %>%
    group_by(mun, CITY_NAME) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_make_valid()
  save(city_sf, file = "data/city_sf.xdr")
}
load("data/city_sf.xdr")

key_code_mun <- key_code_sf %>%
  st_centroid() %>%
  st_join(city_sf, join = st_within) %>%
  st_drop_geometry() %>%
  dplyr::select(KEY_CODE, mun)

# 市区町村単位の重み（国勢調査メッシュ世帯数householdを市区町村に集計）
household_mun <- household %>%
  dplyr::left_join(key_code_mun, by = "KEY_CODE") %>%
  dplyr::filter(!is.na(mun)) %>%
  dplyr::group_by(mun) %>%
  dplyr::summarise(obs_hh_mun = sum(obs_hh, na.rm = TRUE), .groups = "drop")

# 比較に使う市区町村（study area内 かつ 2023年データあり）
comparison_mun_ga <- Reduce(intersect, list(
  unique(na.omit(key_code_mun$mun)),
  area_target_mun$mun[!is.na(area_target_mun$area_target_mun)],
  rent_target_mun$mun[!is.na(rent_target_mun$rent_target_mun)]
))
cat("[GA住宅土地統計調査版] 比較に使う市区町村数:", length(comparison_mun_ga), "\n")

# k_iの探索範囲（05_ki-relaxationと同じ）
k_lower <- 0.01
k_upper <- 0.4


# ---- fitness関数 ----
# LUTIループ本体は01のGA前検証用fitnessと同一。異なるのは「観測値との誤差計算」で
# athome_df/athome_arの代わりに市区町村集計した住宅・土地統計調査の値を使う点、
# およびk_iをxの6番目以降（GAの探索対象）から取る点。
# x = c(alpha_a, gamma_0, gamma_1, theta_H, theta_L, k_i[1:nz_res])（長さ 5 + nz_res）
calibration_fitness_housing_survey <- function(x) {

  alpha_a_c <- x[1]
  alpha_z_c <- 1 - alpha_a_c
  alpha_0_c <- (alpha_z_c ^ alpha_z_c) * (alpha_a_c ^ alpha_a_c)
  gamma_0_c <- x[2]
  gamma_1_c <- x[3]
  theta_H_c <- x[4]
  theta_L_c <- x[5]
  k_i_c     <- x[6:(5 + nz_res)]        # ゾーン別k_i（GAの探索対象、長さnz_res）

  tryCatch({

    # ---- LUTIループ ----
    dists_road_local <- dists0.road * 1
    flag_local <- 0
    ii_local   <- 1

    while (flag_local == 0) {

      V.car_l  <- para[1] * dists_road_local + para[2] + para[4] * parkPrice / 2
      den_l    <- exp(V.bike) + exp(V.car_l) + exp(V.rail)
      P.bike_l <- exp(V.bike)  / den_l
      P.car_l  <- exp(V.car_l) / den_l
      P.rail_l <- exp(V.rail)  / den_l
      agcc_l   <- (P.bike_l * V.bike + P.car_l * V.car_l + P.rail_l * V.rail) / para[1]
      c_ij_l   <- agcc_l * 1.600
      disp_inc_l <- pmax(-c_ij_l + omega_j_matrix, 0)

      calc_state_local <- function(v_j_vec) {
        v_j_mat  <- matrix(v_j_vec, nrow = nz_res, ncol = nz_work, byrow = TRUE)
        r_ij_H   <- (alpha_0_c * disp_inc_l / v_j_mat) ^ (1 / alpha_a_c)
        r_ij_H[is.nan(r_ij_H)] <- 0
        exp_r    <- exp(theta_H_c * r_ij_H)
        exp_r[r_ij_H == 0] <- 0
        P_j_i    <- exp_r / pmax(rowSums(exp_r, na.rm = TRUE), 1e-9)
        r_bar_i  <- rowSums(r_ij_H * P_j_i, na.rm = TRUE)
        pg       <- gamma_1_c / (1 - gamma_1_c)
        rg_i     <- phi * (1 - gamma_1_c) *
          (r_bar_i * gamma_0_c) ^ (1 / (1 - gamma_1_c)) *
          (gamma_1_c / k_i_c) ^ pg
        G_i_l    <- G0_i * exp(theta_L_c * (rg_i - rr_a)) /
          (1 + exp(theta_L_c * (rg_i - rr_a))) * phi_pub * phi
        A_Fi_S   <- gamma_0_c * ((r_bar_i * gamma_0_c * gamma_1_c) / k_i_c) ^ pg * G_i_l
        A_Fi_S[is.nan(A_Fi_S)] <- 0
        A_fij_S  <- A_Fi_S * P_j_i
        a_fij_H  <- alpha_a_c * disp_inc_l / pmax(r_ij_H, 1e-9)
        l_i_j    <- A_fij_S / pmax(a_fij_H, 1e-9)
        l_i_j[is.nan(l_i_j)] <- 0
        l_i_j[a_fij_H == 0]  <- 0
        list(l_i_j     = l_i_j,
             r_bar_i   = r_bar_i,
             a_fij_H   = a_fij_H,
             L_j_tilde = colSums(l_i_j, na.rm = TRUE))
      }

      gapf_local <- function(v_j_vec2) {
        s <- calc_state_local(exp(v_j_vec2))
        s$L_j_tilde - L_j_hat$L_j_hat
      }

      res_lu <- nleqslv(
        x       = v_start,
        fn      = gapf_local,
        global  = "dbldog",
        control = list(ftol = 1e-6, xtol = 1e-6, maxit = 100)
      )
      if (!(res_lu$termcd %in% c(1, 2))) return(-200)

      state_l <- calc_state_local(exp(res_lu$x))

      ODD.car_l <- state_l$l_i_j * P.car_l
      trips_l   <- as.data.frame.table(ODD.car_l, responseName = "demand")
      names(trips_l) <- c("from", "to", "demand")

      traffic_l <- assign_traffic(
        Graph     = sgr,
        from      = trips_l$from,
        to        = trips_l$to,
        demand    = trips_l$demand,
        max_gap   = 1e-2,
        algorithm = "bfw",
        verbose   = FALSE
      )
      sgr2_l <- makegraph(
        df       = traffic_l$data[, c("from", "to", "cost")],
        directed = TRUE,
        capacity = 1e4,
        alpha    = alpha,
        beta     = beta,
        coords   = nodes
      )
      dists_road_prev  <- dists_road_local
      dists_road_new   <- get_distance_matrix(sgr2_l,
                                              from      = zones,
                                              to        = centers,
                                              algorithm = "mch")
      dists_road_local <- 0.5 * dists_road_new + 0.5 * dists_road_prev

      diff_local <- sum((dists_road_prev - dists_road_local) ^ 2)
      ii_local   <- ii_local + 1

      # GA評価中は反復回数を制限（本番LUTIは ii>1000 まで）
      if (diff_local < 1000 | ii_local > 20) {
        flag_local <- 1
      }
    }
    # ---- LUTIループここまで ----


    # ---- 観測値との誤差計算（世帯数はメッシュ、家賃・床面積は市区町村集計）----

    # 世帯数（メッシュ単位、国勢調査。athome由来ではないため変更しない）
    est_hh_df <- tibble(
      KEY_CODE = rownames(state_l$l_i_j) %>% gsub("^mc_", "", .),
      est_hh   = rowSums(state_l$l_i_j, na.rm = TRUE)
    )
    comp_hh <- household %>%
      left_join(est_hh_df, by = "KEY_CODE") %>%
      filter(!is.na(obs_hh), !is.na(est_hh),
             obs_hh > 0, est_hh > 0)
    w_hh_i <- comp_hh$obs_hh / sum(comp_hh$obs_hh)
    f_hh <- sum(w_hh_i * (log(comp_hh$est_hh) - log(comp_hh$obs_hh)) ^ 2)

    # 家賃・床面積：メッシュ→市区町村に世帯数加重平均で集計してから住宅・土地統計調査と比較
    est_rent_df <- tibble(
      KEY_CODE = names(state_l$r_bar_i) %>% gsub("^mc_", "", .),
      est_rent = as.numeric(state_l$r_bar_i) * 1000  # 千円/m2 → 円/m2
    )
    est_ar_vec <- rowSums(state_l$a_fij_H * state_l$l_i_j, na.rm = TRUE) /
      rowSums(state_l$l_i_j, na.rm = TRUE)
    est_ar_df <- tibble(
      KEY_CODE = rownames(state_l$a_fij_H) %>% gsub("^mc_", "", .),
      est_ar   = est_ar_vec
    )

    est_mun <- est_rent_df %>%
      left_join(est_ar_df, by = "KEY_CODE") %>%
      left_join(key_code_mun, by = "KEY_CODE") %>%
      left_join(household, by = "KEY_CODE") %>%
      filter(!is.na(mun), !is.na(obs_hh), obs_hh > 0,
             !is.na(est_rent), !is.na(est_ar)) %>%
      group_by(mun) %>%
      summarise(
        est_rent_mun = weighted.mean(est_rent, w = obs_hh, na.rm = TRUE),
        est_ar_mun   = weighted.mean(est_ar,   w = obs_hh, na.rm = TRUE),
        .groups = "drop"
      )

    comp_rent <- rent_target_mun %>%
      inner_join(est_mun, by = "mun") %>%
      left_join(household_mun, by = "mun") %>%
      filter(!is.na(rent_target_mun), !is.na(est_rent_mun),
             rent_target_mun > 0, est_rent_mun > 0,
             !is.na(obs_hh_mun), obs_hh_mun > 0)
    if (nrow(comp_rent) < 5) return(-200)  # 比較できる市区町村が少なすぎる場合は打ち切り
    w_rent_mun <- comp_rent$obs_hh_mun / sum(comp_rent$obs_hh_mun)
    f_rent <- sum(w_rent_mun * (log(comp_rent$est_rent_mun) - log(comp_rent$rent_target_mun)) ^ 2)

    comp_ar <- area_target_mun %>%
      inner_join(est_mun, by = "mun") %>%
      left_join(household_mun, by = "mun") %>%
      filter(!is.na(area_target_mun), !is.na(est_ar_mun),
             area_target_mun > 0, est_ar_mun > 0,
             !is.na(obs_hh_mun), obs_hh_mun > 0)
    if (nrow(comp_ar) < 5) return(-200)
    w_ar_mun <- comp_ar$obs_hh_mun / sum(comp_ar$obs_hh_mun)
    f_ar <- sum(w_ar_mun * (log(comp_ar$est_ar_mun) - log(comp_ar$area_target_mun)) ^ 2)

    # ---- ペナルティ項 ----
    # 注: xは長さ5+nz_res、x0は長さ5のため、(x - x0)がリサイクルされる
    # （02と同条件のまま。元コード旧L.2355-2357の挙動をそのまま維持している）。
    x0      <- c(0.3, 0.2, 0.6, 0.1, 2.0)
    lambda  <- 0.05
    penalty <- lambda * sum(((x - x0) / x0) ^ 2)

    # ---- 重みつき合計Loss ----
    w_hh   <- 1.0
    w_rent <- 0.5
    w_ar   <- 0.3

    total_loss <- w_hh * f_hh + w_rent * f_rent + w_ar * f_ar + penalty

    return(-total_loss)  # GAは最大化 → 符号反転

  }, error = function(e) {
    return(-200)
  })
}


# ---- 単発テスト（852次元。k_iは基準値0.1で初期化）----
x0_test <- c(0.3, 0.2, 0.6, 0.1, 2.0, rep(0.1, nz_res))
test_result_hs <- calibration_fitness_housing_survey(x0_test)
cat("[住宅土地統計調査版] 単発テスト fitness値:", test_result_hs, "\n")
# -200が返る場合はcomp_rent/comp_arの行数不足やLUTIループの非収束が原因。
# 2026-09-19の単発テストでは -20.199（比較市区町村29）だった（07のmeta.yml参照）。
# 正常な値が出ることを確認してから下のGA本番実行に進むこと。

# 852次元でも動くか、ランダムなk_iを含めて確認
set.seed(1)
test_vals_hs <- sapply(1:10, function(i) {
  x_random <- c(0.3, 0.2, 0.6, 0.1, 2.0, runif(nz_res, k_lower, k_upper))
  calibration_fitness_housing_survey(x_random)
})
print(round(test_vals_hs, 2))


# ---- GA本番（852次元：5変数 + ゾーン別k_i。02_ga-800dimと同じ探索範囲・条件）----
# 重いので、実行するときだけ TRUE にする。結果は ga_result.RData に保存され、
# active_calibration <- "07_ga-housing-survey-target" で下の較正結果読込に反映できる。
run_ga_852dim <- FALSE
if (run_ga_852dim) {
  popSize_852 <- 50
  maxiter_852 <- 100
  lower_vec <- c(0.1,  0.05, 0.3,  0.02, 0.5,  rep(k_lower, nz_res))
  upper_vec <- c(0.5,  0.8,  0.9,  0.5,  5.0,  rep(k_upper, nz_res))

  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, varlist = c(
    "calibration_fitness_housing_survey",
    "key_code_mun", "household_mun",
    "area_target_mun", "rent_target_mun",
    "dists0.road", "sgr", "nodes", "zones", "centers",
    "alpha", "beta",
    "V.bike", "V.rail", "para", "parkPrice",
    "omega_j_matrix", "L_j_hat",
    "G0_i", "phi", "phi_pub", "rr_a",
    "nz_res", "nz_work", "v_start",
    "k_lower", "k_upper",
    "household",
    "assign_traffic", "makegraph", "get_distance_matrix"
  ), envir = .GlobalEnv)
  clusterEvalQ(cl, {
    library(nleqslv)
    library(cppRouting)
    library(dplyr)
    library(tibble)
  })

  cat("[住宅土地統計調査版] GA開始:", format(Sys.time(), "%H:%M:%S"), "\n")
  system.time({
    ga_result_housing_survey <- ga(
      type    = "real-valued",
      fitness = calibration_fitness_housing_survey,
      lower   = lower_vec,
      upper   = upper_vec,
      popSize = popSize_852,
      maxiter = maxiter_852,
      parallel = cl,
      monitor  = TRUE
    )
  })
  cat("[住宅土地統計調査版] GA終了:", format(Sys.time(), "%H:%M:%S"), "\n")

  stopCluster(cl)  # closeAllConnections()は削除：stopCluster()より先に呼ぶとワーカーとの接続を潰してしまい、
                   # stopCluster()内のsummary.connection()が「コネクションが不正です」で失敗する（260922発覚）
  gc(); gc()

  cat("最良fitness（-Loss）:", ga_result_housing_survey@fitnessValue, "\n")
  print(ga_result_housing_survey@solution)

  # ---- 較正結果読込（load_calibration_result）が読む形式に正規化して保存 ----
  # calib_result$params（5変数named vector）と calib_result$k_i（長さnz_res）の共通形式。
  best_x <- ga_result_housing_survey@solution[1, ]
  params_852 <- best_x[1:5]
  names(params_852) <- c("alpha_a", "gamma_0", "gamma_1", "theta_H", "theta_L")

  calib_result <- list(
    params    = params_852,
    k_i       = best_x[6:(5 + nz_res)],
    fitness   = ga_result_housing_survey@fitnessValue,
    ga_result = ga_result_housing_survey
  )
  save(calib_result,
       file = "project_log/calibration/07_ga-housing-survey-target/ga_result.RData")
}



# 較正まわりの各パターンは scripts/calibration/ に分割済み（scripts/calibration/README.md参照。
# 07はこの上のGAセクションに戻してあり、このディレクトリには無い）。
# ここでは「どの較正結果を使うか」を1行で選び、対応するキャッシュ済みga_result.RDataを
# 読み込んでグローバルパラメータ・k_iに反映する。GAは再実行しない
# （再実行したい場合は scripts/calibration/ga_baseline_5var.R 等の calibrate_*() を
# 手動で実行し、project_log/calibration/<試行>/ga_result.RData を更新すること）。

# 選べる値:
#   "01_baseline-5param-ga"       — 5変数GA、k_i=0.1固定。ga_result.RDataあり、すぐ使える。
#   "05_ki-relaxation"            — k_iは03-05の解析的推定（scripts/calibration/ki_estimation.R
#                                    のcalibrate_ki_relaxation()）、5変数は01を暫定的に借用。
#                                    使うには一度 calibrate_ki_relaxation() を実行してキャッシュを
#                                    作る必要がある（未実行ならga_result.RDataが無くエラーになる）。
#   "07_ga-housing-survey-target" — 852次元GA（5変数+ゾーン別k_i）、住宅・土地統計調査ベース。
#                                    上の「GA較正（住宅・土地統計調査ベース…）」セクションで
#                                    run_ga_852dim <- TRUE にして実行し、ga_result.RData を作る必要がある
#                                    （未実行ならga_result.RDataが無くエラーになる）。
active_calibration <- "07_ga-housing-survey-target"

# ---- 選んだ較正パターンのキャッシュ（ga_result.RData）を読み込む ----
# scripts/calibration/*.R の calibrate_*() はどれも calib_result$params（5変数named vector）
# と calib_result$k_i（長さnz_resのベクトル）を持つ同じ形式で保存するので、
# ここではパターンごとの分岐は不要（新しい較正パターンを追加しても、この関数は変更不要）。
load_calibration_result <- function(name) {
  rdata_path <- file.path("project_log", "calibration", name, "ga_result.RData")
  if (!file.exists(rdata_path)) {
    stop(
      "較正パターン '", name, "' のキャッシュ（", rdata_path, "）が見つかりません。\n",
      "scripts/calibration/README.md を参照し、対応する calibrate_*() を先に実行して\n",
      "結果を保存してください（07はdata/rent.df.xdrが揃うまで実行できません）。"
    )
  }
  e <- new.env()
  load(rdata_path, envir = e)
  if (is.null(e$calib_result) || is.null(e$calib_result$params) || is.null(e$calib_result$k_i)) {
    stop(
      "キャッシュの形式が不正です（calib_result$params / $k_i が必要）: ", rdata_path, "\n",
      "scripts/calibration/*.R の calibrate_*() で作り直してください。"
    )
  }
  e$calib_result
}

calib_result      <- load_calibration_result(active_calibration)
best_params_global <- calib_result$params
cat("\n使用するグローバルパラメータ（", active_calibration, " の記録値）:\n", sep = "")
print(best_params_global)


# ---- グローバル変数に反映 ----
alpha_a <- best_params_global["alpha_a"]
alpha_z <- 1 - alpha_a
alpha_0 <- (alpha_z ^ alpha_z) * (alpha_a ^ alpha_a)
gamma_0 <- best_params_global["gamma_0"]
gamma_1 <- best_params_global["gamma_1"]

theta_H <- best_params_global["theta_H"]
theta_L <- best_params_global["theta_L"]

k_i <- calib_result$k_i
# k_iが正しく代入されているか確認
length(k_i)
range(k_i, na.rm = TRUE)
sum(is.na(k_i))
nz_res

# ---- LUTIループを最適パラメータで再実行 ----
# 元コードL.882〜954と同じ処理を最適パラメータで実行する
flag01 <- 0
alpha01 <- 0.5
ii <- 1
tt0 <- proc.time()
while (flag01 == 0) {
  V.car <- para[1] * dists0.road + para[2] + para[4] * parkPrice / 2
  den   <- exp(V.bike) + exp(V.car) + exp(V.rail)
  P.bike <- exp(V.bike) / den
  P.car  <- exp(V.car) / den
  P.rail <- exp(V.rail) / den
  agcc   <- (P.bike * V.bike + P.car * V.car + P.rail * V.rail) / para[1]
  c_ij   <- agcc * 1.600
  disposable_income_ij <- pmax(-c_ij + omega_j_matrix, 0)
  
  result_nleqslv <- nleqslv(
    x = v_start, fn = gapf_vj,
    global = "dbldog",
    control = list(ftol = 1e-8, xtol = 1e-8, maxit = 200)
  )
  v_equilibrium <- result_nleqslv$x %>% exp()
  final_state   <- caluculate_model_state(v_equilibrium)

  ODD.car <- final_state$l_i_j * P.car
  trips   <- as.data.frame.table(ODD.car, responseName = "demand")
  names(trips) <- c("from", "to", "demand")

  cat("[debug] termcd=", result_nleqslv$termcd,
      " NA(v_equilibrium)=", sum(is.na(v_equilibrium)),
      " NA(l_i_j)=", sum(is.na(final_state$l_i_j)),
      " NA(P.car)=", sum(is.na(P.car)),
      " NA(disposable_income_ij)=", sum(is.na(disposable_income_ij)),
      " NA(ODD.car)=", sum(is.na(ODD.car)),
      " NA(demand)=", sum(is.na(trips$demand)),
      " range(demand)=", paste(range(trips$demand, na.rm = TRUE), collapse=","),
      "\n")

  traffic01 <- assign_traffic(
    Graph     = sgr,
    from      = trips$from,
    to        = trips$to,
    demand    = trips$demand,
    max_gap   = 1e-2,
    algorithm = "bfw",
    verbose   = FALSE
  )

  cat("[debug2] gap=", traffic01$gap, " iteration=", traffic01$iteration,
      " NA(cost)=", sum(is.na(traffic01$data$cost)),
      " Inf(cost)=", sum(is.infinite(traffic01$data$cost)),
      " NA(flow)=", sum(is.na(traffic01$data$flow)),
      " neg(flow)=", sum(traffic01$data$flow < 0, na.rm = TRUE),
      " NA(capacity)=", sum(is.na(traffic01$data$capacity)),
      " zero(capacity)=", sum(traffic01$data$capacity == 0, na.rm = TRUE),
      "\n")
  if (sum(is.na(traffic01$data$cost)) > 0) {
    bad <- traffic01$data[is.na(traffic01$data$cost), ][1:min(5, sum(is.na(traffic01$data$cost))), ]
    cat("[debug2] sample rows with NA cost:\n")
    print(bad)
  }

  sgr2 <- makegraph(
    df       = traffic01$data[, c("from", "to", "cost")],
    directed = TRUE,
    capacity = 1e4,
    alpha    = alpha,
    beta     = beta,
    coords   = nodes
  )
  dists0.road.b <- dists0.road
  dists1.road   <- get_distance_matrix(sgr2,
                                       from      = zones,
                                       to        = centers,
                                       algorithm = "mch")
  dists0.road <- alpha01 * dists1.road + (1 - alpha01) * dists0.road.b
  diff01 <- sum((dists0.road.b - dists0.road) ^ 2)
  cat("ii=", ii, ", diff=", diff01, "\n")
  ii <- ii + 1
  if (diff01 < 100 | ii > 1e3) flag01 <- 1
}
proc.time() - tt0






# 260808方針: 下のモデル適合度検証セクション（実データとの比較）は今回スキップするが、
# シナリオ比較セクションが参照する est_household / rent_df / est_ar / Gi_df は
# final_state（モデル出力）のみから作れるので、ここで先に用意しておく。
# （make_map()自体は260820よりL.575付近で定義済み）
est_household <- rowSums(final_state$l_i_j, na.rm = TRUE)
est_household <- tibble(
  KEY_CODE = rownames(final_state$l_i_j),
  est_household = est_household
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))

rent_df <- tibble(
  KEY_CODE = names(final_state$r_bar_i) %>% gsub("^mc_", "", .),
  est_rent = as.numeric(final_state$r_bar_i) * 1000
)

avg_floor_i <- rowSums(final_state$a_fij_H * final_state$l_i_j, na.rm = TRUE) / rowSums(final_state$l_i_j, na.rm = TRUE)
est_ar <- tibble(
  KEY_CODE = rownames(final_state$a_fij_H),
  est_ar = avg_floor_i
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))

Gi_df <- data.frame(
  KEY_CODE = names(final_state$G_i),
  habit_ar = as.numeric(final_state$G_i)
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))


if(T){ # モデル適合度検証セクション（アットホームデータとの比較）
#実データとモデル比較#####
# install.packages("patchwork")
library(patchwork)
# install.packages("wCorr")
library(wCorr)

# 地図プロットを作る関数を1つ定義する
make_map <- function(data, fill_var, title, option, limits) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = option, direction = -1,
                         limits = limits, oob = scales::squish,
                         labels = scales::label_comma()) +
    labs(title = title) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
}


## 居住・従業地別 世帯数 (l_i_j) 

# モデル：メッシュごと世帯数
est_household=rowSums(final_state$l_i_j,na.rm=TRUE)
est_household<-tibble(
  KEY_CODE=rownames(final_state$l_i_j),
  est_household=est_household
)%>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))

#実データ：メッシュごと世帯数
csv.household<-"data/raw/国勢調査_人口及び世帯数_1kmメッシュ/tblT001100S5030.csv"
obs_household<-read.csv(csv.household,fileEncoding = "CP932",stringsAsFactors=FALSE,na.strings= c("", "NA"))%>%
  dplyr::select(KEY_CODE,T001100035)%>%
  dplyr::rename(obs_household="T001100035")%>%
  dplyr::mutate(KEY_CODE=as.character(KEY_CODE),obs_household=as.numeric(obs_household))
obs_household<-obs_household[-1,]

#hh比較
hhC=left_join(key_code_sf,est_household,by="KEY_CODE") %>% 
  left_join(obs_household,by="KEY_CODE") %>% 
  dplyr::mutate(
    obs_household = tidyr::replace_na(obs_household, 0))

plot(hhC$obs_household,hhC$est_household)
abline(0,1,col="red")
sum(hhC$obs_household)
sum(hhC$est_household)
(cor(hhC$obs_household,hhC$est_household, use="complete.obs"))^2
r_hh_weighted <- weightedCorr(
  x = hhC$obs_household,
  y = hhC$est_household,
  weights = hhC$obs_household,
  method = "Pearson"
)
cat("従業世帯数 重みつきR²:", r_hh_weighted^2, "\n")

#居住プロット 
p_hh_obs   <- make_map(hhC, "obs_household",       "Observed: Number of Households", "magma", c(0, 25000))
p_hh_model <- make_map(hhC, "est_household", "Model: Number of Households",   "magma", c(0, 25000))
print(p_hh_obs + p_hh_model)  # patchwork


##　家賃の比較
#モデル：家賃
final_r_bar_i=final_state$r_bar_i
final_r_ij_H=final_state$r_ij_H
rent_df <- tibble(
  KEY_CODE = names(final_state$r_bar_i)%>%gsub("^mc_", "", .), 
  est_rent = as.numeric(final_state$r_bar_i)*1000
) 

#実データ：家賃アットホームデータ
if(F){
  athome <- st_read("data/raw/athome_date") %>% 
    st_transform(crs = target_crs)
  athome_mesh <- st_join(athome,key_code_sf, join=st_intersects)
  athome_crop <- athome_mesh %>% 
    filter(!is.na(KEY_CODE))
  save(athome_crop,file="data/athome_crop.xdr")
}
load("data/athome_crop.xdr")
athome_df <- athome_crop %>%
  dplyr::select(rent, room_ar, KEY_CODE) %>%
  mutate(rent_par_ar = rent / room_ar) %>%
  st_drop_geometry() %>%
  group_by(KEY_CODE) %>%
  summarise(obs_rent = mean(rent_par_ar, na.rm = TRUE))

athome_sf<-key_code_sf %>% 
  left_join(athome_df, by="KEY_CODE")


#rent比較
rentC <- athome_sf %>% 
  left_join(rent_df, by="KEY_CODE")%>%
  left_join(obs_household,by="KEY_CODE") 

rentC_flt <- rentC |> 
  filter(!is.na(obs_rent), !is.na(est_rent), !is.na(obs_household),
  est_rent>0, obs_rent>0,obs_household>0)
  
plot(rentC_flt$obs_rent,rentC_flt$est_rent)
abline(0,1,col="red")
(cor(rentC_flt$obs_rent, rentC_flt$est_rent))^2

r_rent_weighted <- weightedCorr(
  x = rentC_flt$obs_rent,
  y = rentC_flt$est_rent,
  weights = rentC_flt$obs_household,
  method = "Pearson"
)
cat("家賃 重みつきR²:", r_rent_weighted^2, "\n")


#家賃プロット　  
p_rent_obs   <- make_map(rentC, "obs_rent",       "Observed: Rent", "plasma", c(0, 3200))
p_rent_model <- make_map(rentC, "est_rent", "Model: Rent",   "plasma", c(0, 3200))
print(p_rent_obs + p_rent_model)  # patchwork

# diff_rent
rentC_flt_diff <- rentC_flt %>% 
  mutate(
    diff_rent = est_rent - obs_rent
  )

p_rentC_diff <- make_diff_map2(rentC_flt_diff, "diff_rent", 
                             "diff_rent", "(Yen/Month)")
plot(p_rentC_diff)

##　床面積の比較
#モデル：a_fij_H
final_a_fij_H=final_state$a_fij_H
avg_floor_i <- rowSums(final_a_fij_H * final_state$l_i_j, na.rm=TRUE) / rowSums(final_state$l_i_j, na.rm=TRUE)
est_ar <- tibble(
  KEY_CODE=rownames(final_a_fij_H),
  est_ar=avg_floor_i
) %>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))


#実データ：アットホームデータの面積
load("data/athome_crop.xdr")
athome_ar <- athome_crop %>% 
  dplyr::select(KEY_CODE,room_ar) %>% 
  st_drop_geometry() %>% 
  group_by(KEY_CODE)%>% 
  summarise(obs_ar=mean(room_ar, na.rm=TRUE)) 
obs_ar<-key_code_sf %>% 
  left_join(athome_ar, by="KEY_CODE")

#床面積比較プロット　 
arC <- obs_ar %>% 
  left_join(est_ar, by="KEY_CODE")%>%
  left_join(obs_household,by="KEY_CODE") 

arC_flt <- arC |> 
  filter(!is.na(est_ar), !is.na(obs_ar), !is.na(obs_household),
  est_ar>0, obs_ar>0,obs_household>0)
  
plot(arC_flt$obs_ar,arC_flt$est_ar)
abline(0,1,col="red")
(cor(arC_flt$obs_ar, arC_flt$est_ar))^2

r_ar_weighted <- weightedCorr(
  x = arC_flt$obs_ar,
  y = arC_flt$est_ar,
  weights = arC_flt$obs_household,
  method = "Pearson"
)
cat("床面積 重みつきR²:", r_ar_weighted^2, "\n")

p_ar_obs   <- make_map(arC, "obs_ar",       "Observed: Floor Area", "viridis", c(0, 200))
p_ar_model <- make_map(arC, "est_ar", "Model: Floor Area",   "viridis", c(0, 200))
print(p_ar_obs + p_ar_model)  # patchwork

# diff_ar
arC_flt_diff <- arC_flt %>% 
  mutate(
    diff_ar = est_ar - obs_ar
  )

p_arC_diff <- make_diff_map2(arC_flt_diff, "diff_ar", 
               "diff_ar", "(m^2)")
plot(p_arC_diff)


#宅地面積
final_Gi <- final_state$G_i
Gi_df <- data.frame(
  KEY_CODE = names(final_state$G_i), 
  habit_ar = as.numeric(final_state$G_i) ) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))
Gi_map_data <- left_join(key_code_sf, Gi_df, by = "KEY_CODE")
Gi_map_data <- Gi_map_data %>%
  mutate(habit_ar=habit_ar/10000)

hist(Gi_map_data$habit_ar)

#宅地割合プロット

#建物高さ（推定）：容積率(FF)/建蔽率(phi)=階数、階数×階高(floor_height_m)=高さ(m)
Hi_df <- data.frame(
  KEY_CODE = names(final_state$height_i),
  est_stories = as.numeric(final_state$stories_i),
  est_height = as.numeric(final_state$height_i)
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))
} # end モデル適合度検証セクション


## 床面積の比較（住宅・土地統計調査、市区町村・2人世帯換算） ----
# project_log/calibration/06_housing-survey-comparison/ 参照。
# 既存のアットホームデータとの比較（メッシュ単位、上のarC/rentC）はサンプル数が少なく
# 信頼性に課題があるため、住宅・土地統計調査（市区町村単位、公的統計）を別の実データとして
# 使い、モデルの床面積出力と整合しているか確認する。

# --- (a) 住宅・土地統計調査：福岡県分の抽出と2人世帯換算 ---
load("data/area.df.xdr") # area.df, dwelling size (m2)
load("data/hhn.df.xdr")  # hhn.df, household size (member)

yyv1 <- c(1993, 2003, 2008, 2013, 2018, 2023)

tI01 <- grep("^40", area.df$mun)
tI02 <- grep("^40", hhn.df$mun)

ApP <- (area.df[tI01, -1] %>% mutate(across(everything(), as.numeric))) /
  (hhn.df[tI02, -1]  %>% mutate(across(everything(), as.numeric)))
# 2人世帯換算 床面積(m2)：1人あたり床面積 × 2
area_2p <- ApP * 2
colnames(area_2p) <- yyv1

housing_survey_fukuoka <- tibble(
  mun = area.df$mun[tI01],
  area_2p
) %>%
  pivot_longer(cols = -mun, names_to = "year", values_to = "area_2p_m2") %>%
  mutate(year = as.integer(year))

# 年次推移（福岡県内、市区町村ごと）
ggplot(housing_survey_fukuoka, aes(x = year, y = area_2p_m2, group = mun)) +
  geom_line(color = "gray60", alpha = 0.6) +
  labs(
    title = "住宅・土地統計調査：2人世帯換算 平均床面積の推移（福岡県）",
    x = "調査年", y = "2人世帯換算 床面積 (m2)"
  ) +
  theme_minimal()
ggsave("project_log/calibration/06_housing-survey-comparison/plots/housing_survey_area2p_trend.png",
       width = 6, height = 5, dpi = 150)

# --- (b) メッシュ(KEY_CODE)→市区町村の対応付け（CITY_NAME付き。city_sf自体は上の方で読込済み）---
key_code_mun <- key_code_sf %>%
  st_centroid() %>%
  st_join(city_sf, join = st_within) %>%
  st_drop_geometry() %>%
  dplyr::select(KEY_CODE, mun, CITY_NAME)

# 分析範囲の可視化：study area内の市区町村のうち、2023年データがあり実際に比較へ使えたもの
study_area_mun <- unique(na.omit(key_code_mun$mun))
mun_2023 <- housing_survey_fukuoka %>% filter(year == 2023, !is.na(area_2p_m2)) %>% pull(mun)
comparison_mun <- intersect(study_area_mun, mun_2023)

city_scope <- city_sf %>%
  mutate(status = case_when(
    mun %in% comparison_mun ~ "比較に使用（2023年データあり）",
    mun %in% study_area_mun ~ "study area内だが2023年データなし",
    TRUE ~ "study area外"
  ))
label_pts <- city_scope %>% filter(status == "比較に使用（2023年データあり）") %>% st_centroid()

p_scope <- ggplot() +
  geom_sf(data = city_scope, aes(fill = status), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "比較に使用（2023年データあり）" = "#4472C4",
    "study area内だが2023年データなし" = "#A9C4EB",
    "study area外" = "gray90"
  )) +
  geom_sf_text(data = label_pts, aes(label = CITY_NAME), size = 2.2, color = "black") +
  labs(
    title = "住宅・土地統計調査による床面積比較の対象市区町村（福岡県）",
    subtitle = "濃青：2023年比較に使用した市区町村",
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )
ggsave("project_log/calibration/06_housing-survey-comparison/plots/study_area_municipality_map.png",
       plot = p_scope, width = 7, height = 6.5, dpi = 150)

# --- (c) モデル出力（est_ar）の市区町村集計と比較 ---
# est_ar：ゾーンiごとの従業地方向世帯数加重平均床面積（m2/世帯、全世帯平均）
# L.2233-2238で既に計算済みのest_arをそのまま使う（再計算しない）
# 市区町村集計：国勢調査メッシュ世帯数(obs_household)で加重平均
est_ar_mun <- est_ar %>%
  left_join(key_code_mun, by = "KEY_CODE") %>%
  left_join(obs_household, by = "KEY_CODE") %>%
  filter(!is.na(mun), !is.na(obs_household), obs_household > 0) %>%
  group_by(mun, CITY_NAME) %>%
  summarise(
    est_ar_mun = weighted.mean(est_ar, w = obs_household, na.rm = TRUE),
    .groups = "drop"
  )

# 最新調査年（2023年）の2人世帯換算値と結合して比較
comparison_2023 <- est_ar_mun %>%
  inner_join(
    housing_survey_fukuoka %>% filter(year == 2023),
    by = "mun"
  )

plot(comparison_2023$area_2p_m2, comparison_2023$est_ar_mun,
     xlab = "実データ：2人世帯換算 床面積(m2) [住宅・土地統計調査2023]",
     ylab = "モデル：市区町村平均床面積(m2) [est_ar_mun]",
     main = "床面積の比較（市区町村単位、2023年）")
abline(0, 1, col = "red")
r_ar_mun <- cor(comparison_2023$area_2p_m2, comparison_2023$est_ar_mun, use = "complete.obs")
cat("床面積（市区町村・住宅土地統計調査） R²:", r_ar_mun^2, "\n")

png("project_log/calibration/06_housing-survey-comparison/plots/floor_area_mun_comparison_2023.png",
    width = 6, height = 5.5, units = "in", res = 150)
plot(comparison_2023$area_2p_m2, comparison_2023$est_ar_mun,
     xlab = "実データ：2人世帯換算 床面積(m2) [住宅・土地統計調査2023]",
     ylab = "モデル：市区町村平均床面積(m2) [est_ar_mun]",
     main = "床面積の比較（市区町村単位、2023年）")
abline(0, 1, col = "red")
dev.off()


#シナリオ分析 ####

# === Base Scenario (scn0) Setup ===
scn0_v <- result_nleqslv$x  
scn0_state <- caluculate_model_state(exp(scn0_v))

# Save scn0 mode shares
V.car_scn0  <- para[1] * dists0.road + para[2] + para[4] * parkPrice / 2
den_scn0    <- exp(V.bike) + exp(V.car_scn0) + exp(V.rail)
P.car_scn0  <- exp(V.car_scn0) / den_scn0
P.rail_scn0 <- exp(V.rail) / den_scn0
P.bike_scn0 <- exp(V.bike) / den_scn0

# Save base scenario employment and wages
scn0_L_j_hat <- L_j_hat
scn0_omega_j <- omega_j
scn0_omega_j_matrix <- omega_j_matrix
scn0_disposable_income_ij <- disposable_income_ij


# === Scenario 1: Uniform Employment Redistribution ===
target_zone_id <- "50303344"  # Kyushu University redevelopment site
diff_Lj_target <- 15000       # Employment increase (+15,000 jobs)

all_zones <- rownames(scn0_L_j_hat)
other_zones <- setdiff(all_zones, target_zone_id)

scn1_Lj_target <- scn0_L_j_hat[target_zone_id, "L_j_hat"] + diff_Lj_target

# Maintain total employment by uniform reduction in other zones
total_L <- sum(scn0_L_j_hat$L_j_hat, na.rm = TRUE)
reduction_ratio <- (total_L - scn0_L_j_hat[target_zone_id, "L_j_hat"] - diff_Lj_target) / 
                   (total_L - scn0_L_j_hat[target_zone_id, "L_j_hat"])

scn1_L_j_hat <- scn0_L_j_hat
scn1_L_j_hat[other_zones, "L_j_hat"] <- round(scn0_L_j_hat[other_zones, "L_j_hat"] * reduction_ratio)
scn1_L_j_hat[target_zone_id, "L_j_hat"] <- scn1_Lj_target
total_L_scn1 <- sum(scn1_L_j_hat$L_j_hat, na.rm = TRUE)


# Adjust wages to maintain total payroll (payroll-neutral)
k01 <- sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE) / 
       sum(omega_j$omega_j * scn1_L_j_hat$L_j_hat, na.rm = TRUE)

scn1_omega_j <- omega_j %>% 
  mutate(scn1_omega_j = omega_j * k01) %>% 
  dplyr::select(KEY_CODE, scn1_omega_j)

sum(scn0_omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE)  # 確認：総給与は同じはず
sum(scn1_omega_j$scn1_omega_j * scn1_L_j_hat$L_j_hat, na.rm = TRUE)  # 確認：総給与は同じはず

scn1_omega_j_matrix <- matrix(as.numeric(scn1_omega_j$scn1_omega_j), 
                              nrow = nz_res, 
                              ncol = nrow(scn1_omega_j), 
                              byrow = TRUE)
colnames(scn1_omega_j_matrix) <- scn1_omega_j$KEY_CODE 



# === Scenario 2: CBD-Distance Weighted Employment Redistribution ===
# Employment redistributed inversely to CBD distance × current employment

scn2_L_j_hat <- scn0_L_j_hat
scn2_Lj_target <- scn0_L_j_hat[target_zone_id, "L_j_hat"] + diff_Lj_target

# Ensure dists0 has zone names without "mc_" prefix
colnames(dists0) <- gsub("^mc_", "", colnames(dists0))
rownames(dists0) <- gsub("^mc_", "", rownames(dists0))

cbd_code <- "50303302"
dist_from_cbd <- dists0[other_zones, cbd_code] # dists0.roadで距離を取るべきか、、、no!

# Weight by distance and inverse employment
reduction_weight <- dist_from_cbd / (scn0_L_j_hat[other_zones, "L_j_hat"] + 1)
reduction_amount <- diff_Lj_target * (reduction_weight / sum(reduction_weight))

scn2_L_j_hat[other_zones, "L_j_hat"] <- round(scn0_L_j_hat[other_zones, "L_j_hat"] - reduction_amount)
scn2_L_j_hat[target_zone_id, "L_j_hat"] <- scn2_Lj_target
sum(scn2_L_j_hat$L_j_hat, na.rm = TRUE)  # 確認：総雇用は同じはず

# Apply same wage adjustment as scn1 (payroll-neutral)
k02 <- sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE) / 
       sum(omega_j$omega_j * scn2_L_j_hat$L_j_hat, na.rm = TRUE)

scn2_omega_j <- omega_j %>% 
  mutate(scn2_omega_j = omega_j * k02) %>% 
  dplyr::select(KEY_CODE, scn2_omega_j)

sum(scn0_omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE)  # 確認：総給与は同じはず
sum(scn2_omega_j$scn2_omega_j * scn2_L_j_hat$L_j_hat, na.rm = TRUE)  # 確認：総給与は同じはず

scn2_omega_j_matrix <- matrix(as.numeric(scn2_omega_j$scn2_omega_j), 
                              nrow = nz_res, 
                              ncol = nrow(scn2_omega_j), 
                              byrow = TRUE)
colnames(scn2_omega_j_matrix) <- scn2_omega_j$KEY_CODE


# === Consolidate All Scenario Definitions ===
scenarios <- list(
  scn0 = list(
    name = "Base Scenario (Current Conditions)",
    L_j_hat = scn0_L_j_hat,
    omega_j_matrix = scn0_omega_j_matrix,
    description = "Calibrated baseline equilibrium"
  ),
  scn1 = list(
    name = "Scenario 1: Uniform Distribution",
    L_j_hat = scn1_L_j_hat,
    omega_j_matrix = scn1_omega_j_matrix,
    description = paste0("15,000 jobs to ", target_zone_id, ", uniform reduction elsewhere")
  ),
  scn2 = list(
    name = "Scenario 2: CBD-Distance Weighted",
    L_j_hat = scn2_L_j_hat,
    omega_j_matrix = scn2_omega_j_matrix,
    description = paste0("15,000 jobs to ", target_zone_id, ", distance-weighted reduction")
  )
)

cat("=== Scenario Definitions ===\n")
for (scn in names(scenarios)) {
  cat(scn, ":", scenarios[[scn]]$name, "\n")
  cat("  Total employment:", sum(scenarios[[scn]]$L_j_hat$L_j_hat, na.rm = TRUE), "\n")
}


# シナリオ実行関数 ####
# Converted to while loop; passes parameters explicitly
# Returns comprehensive result set including P.bike
run_scenario <- function(scenario_name, L_j_hat_scn, omega_j_matrix_scn, 
                        v_init, max_iter = 50, msa = 0.5, tol = 100) {
  
  # Initialize loop control
  flag <- 0
  it <- 1
  
  # Local copies to avoid reference issues
  dists_road <- dists0.road * 1  # local copy
  v_l <- v_init
  
  while (flag == 0) {
    # Mode choice: recalculate for each updated dists_road
    V.car_l <- para[1] * dists_road + para[2] + para[4] * parkPrice / 2
    den_l   <- exp(V.bike) + exp(V.car_l) + exp(V.rail)
    P.car_l <- exp(V.car_l) / den_l
    P.rail_l <- exp(V.rail) / den_l
    P.bike_l <- exp(V.bike) / den_l
    
    # Calculate accessibility-adjusted generalized cost
    agcc_l <- (P.bike_l * V.bike + P.car_l * V.car_l + P.rail_l * V.rail) / para[1]
    c_ij_local <- agcc_l * 1.600
    disposable_income_ij_local <- pmax(-c_ij_local + omega_j_matrix_scn, 0)
    
    # Solve land use equilibrium
    # Update global environment for gapf_vj to access parameters
    assign("L_j_hat", L_j_hat_scn, envir = .GlobalEnv)
    assign("c_ij", c_ij_local, envir = .GlobalEnv)
    assign("disposable_income_ij", disposable_income_ij_local, envir = .GlobalEnv)
    
    sol <- nleqslv(x = v_l, fn = gapf_vj, 
                   global = "dbldog",
                   control = list(ftol = 1e-8, xtol = 1e-8, maxit = 200, allowSingular = TRUE))
    v_l <- sol$x
    state <- caluculate_model_state(exp(v_l))
    
    # Traffic assignment
    ODD.car <- state$l_i_j * P.car_l
    trips <- as.data.frame.table(ODD.car, responseName = "demand")
    names(trips) <- c("from", "to", "demand")
    
    ta <- assign_traffic(Graph = sgr, from = trips$from, to = trips$to, 
                         demand = trips$demand, max_gap = 1e-2, 
                         algorithm = "bfw", verbose = FALSE)
    
    sgr2 <- makegraph(df = ta$data[, c("from", "to", "cost")], 
                      directed = TRUE, capacity = 1e4, 
                      alpha = alpha, beta = beta, coords = nodes)
    
    dists_road_new <- get_distance_matrix(sgr2, from = zones, to = centers, algorithm = "mch")
    
    # MSA averaging for convergence
    diff <- sum((dists_road_new - dists_road)^2)
    dists_road <- msa * dists_road_new + (1 - msa) * dists_road
    
    cat("Scenario:", scenario_name, " | Iter:", it, " | diff:", 
        formatC(diff, format = "e", digits = 2), "\n")
    
    # Check stopping conditions
    it <- it + 1
    if (diff < tol | it > max_iter) flag <- 1
  }
  
  # 鉄道リンク配分（均衡収束後の最終stateに対して一度だけ実行、混雑は考慮しない）
  ODD.rail <- state$l_i_j * P.rail_l
  trips_rail <- as.data.frame.table(ODD.rail, responseName = "demand")
  names(trips_rail) <- c("from", "to", "demand")

  ta_rail <- assign_traffic(Graph = sgr_rail, from = trips_rail$from, to = trips_rail$to,
                             demand = trips_rail$demand, max_gap = 1e-2,
                             algorithm = "bfw", verbose = FALSE)

  # Return comprehensive result set
  list(
    scenario = scenario_name,
    v = v_l,
    state = state,
    dists_road = dists_road,
    disposable_income_ij = disposable_income_ij_local,
    P.car = P.car_l,
    P.rail = P.rail_l,
    P.bike = P.bike_l,
    link_flow = ta$data,  # リンク別交通量・所要時間（太さ=交通量／色=速度マップ用）
    rail_link_flow = ta_rail$data,  # 鉄道リンク別利用世帯数（太さ=色=利用世帯数マップ用）
    iterations = it - 1,
    converged = (diff < tol),
    final_diff = diff
  )
}

# === Execute All Scenarios ===
# Initialize results list
results <- list()

# Use base scenario utilities as starting point for all scenarios
v_init_all <- scn0_v

# Run each scenario
for (scn_name in names(scenarios)) {
  cat("\n=== Running:", scenarios[[scn_name]]$name, "===\n")
  
  results[[scn_name]] <- run_scenario(
    scenario_name = scn_name,
    L_j_hat_scn = scenarios[[scn_name]]$L_j_hat,
    omega_j_matrix_scn = scenarios[[scn_name]]$omega_j_matrix,
    v_init = v_init_all,
    max_iter = 50,
    msa = 0.5,
    tol = 100
  )
  
  cat("Converged:", results[[scn_name]]$converged, 
      " in ", results[[scn_name]]$iterations, " iterations\n\n")
}

# === Extract Results for Comparison ===
scn0_v <- results$scn0$v
scn1_v <- results$scn1$v
scn2_v <- results$scn2$v

scn0_state <- results$scn0$state
scn1_state <- results$scn1$state
scn2_state <- results$scn2$state

# Mode shares
P.car_scn1 <- results$scn1$P.car
P.rail_scn1 <- results$scn1$P.rail
P.bike_scn1 <- results$scn1$P.bike

P.car_scn2 <- results$scn2$P.car
P.rail_scn2 <- results$scn2$P.rail
P.bike_scn2 <- results$scn2$P.bike


scn0_disposable_income_ij <- results$scn0$disposable_income_ij
scn1_disposable_income_ij <- results$scn1$disposable_income_ij
scn2_disposable_income_ij <- results$scn2$disposable_income_ij

# # forループ後に復元
# disposable_income_ij <- results$scn0$disposable_income_ij
# L_j_hat <- scenarios$scn0$L_j_hat

#シナリオ別 交通量・速度マップ####
# 太さ = リンク別交通量(flow, pcu/日相当)、色 = リンク別速度(km/h)
# ※ 容量は link10$cap（pcu/hour）を KK=0.1（K値）で除して日単位相当にスケールしたもの。
#   需要側（l_i_j×P.car）も日単位の通勤トリップ想定のため、flowは pcu/hour ではなく pcu/日相当。
# results$scn0（現状）/ scn1（シナリオB）/ scn2（シナリオC）の link_flow(from, to, ftt, cost, flow, capacity)を
# link10（道路網ジオメトリ）に結合してマップ化する

dir.create("project_log/scenario-analysis/plots", recursive = TRUE, showWarnings = FALSE)

scn_titles <- c(
  scn0 = "現状シナリオ",
  scn1 = "シナリオB: 雇用一律再配置",
  scn2 = "シナリオC: CBD距離加重再配置"
)

build_scn_link_map_data <- function(scn_result) {
  link10 %>%
    mutate(ID1 = as.character(ID1), ID2 = as.character(ID2)) %>%
    left_join(
      scn_result$link_flow %>%
        mutate(from = as.character(from), to = as.character(to)),
      by = c("ID1" = "from", "ID2" = "to")
    ) %>%
    mutate(
      flow = tidyr::replace_na(flow, 0),
      speed_kmh = dplyr::if_else(
        !is.na(cost) & cost > 0,
        (linklen / 1000) / (cost / 60),
        maxspeed
      )
    )
}

scn_link_maps <- lapply(results, build_scn_link_map_data)

# 3シナリオ間で比較しやすいよう、太さ・色のスケールを共通の範囲に固定する
flow_range  <- range(unlist(lapply(scn_link_maps, function(d) d$flow)), na.rm = TRUE)
speed_range <- range(unlist(lapply(scn_link_maps, function(d) d$speed_kmh)), na.rm = TRUE)

make_flow_speed_map <- function(map_data, title) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = flow, color = speed_kmh)) +
    scale_linewidth_continuous(range = c(0.15, 2.5), limits = flow_range,
                                name = "交通量\n(pcu/日)") +
    scale_color_viridis_c(option = "plasma", direction = -1,
                           limits = speed_range, oob = scales::squish,
                           name = "速度\n(km/h)") +
    labs(title = title) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

scn_flow_speed_plots <- Map(make_flow_speed_map, scn_link_maps, scn_titles[names(scn_link_maps)])

library(patchwork)
combined_flow_speed_map <- scn_flow_speed_plots$scn0 + scn_flow_speed_plots$scn1 + scn_flow_speed_plots$scn2 +
  plot_layout(ncol = 3, guides = "collect")
print(combined_flow_speed_map)
ggsave("project_log/scenario-analysis/plots/road/scenario_flow_speed_maps.png",
       plot = combined_flow_speed_map, width = 18, height = 6, dpi = 150)

for (scn_name in names(scn_flow_speed_plots)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_flow_speed_map.png", scn_name),
         plot = scn_flow_speed_plots[[scn_name]], width = 7, height = 6, dpi = 150)
}


#シナリオ別 交通量・速度 差分マップ（シナリオB/C − 現状）####
# 太さ = 変化量の絶対値、色 = 変化の符号つき大きさ（青=減少、赤=増加）
# scn1（シナリオB）・scn2（シナリオC）それぞれについて、リンクごとにscn0（現状）との差を取る

build_scn_diff_map_data <- function(scn_map, base_map) {
  base_flow_speed <- base_map %>%
    st_drop_geometry() %>%
    dplyr::select(ID1, ID2, flow_base = flow, speed_base = speed_kmh)

  scn_map %>%
    left_join(base_flow_speed, by = c("ID1", "ID2")) %>%
    mutate(
      flow_diff  = flow - flow_base,
      speed_diff = speed_kmh - speed_base
    )
}

scn_diff_maps <- list(
  scn1 = build_scn_diff_map_data(scn_link_maps$scn1, scn_link_maps$scn0),
  scn2 = build_scn_diff_map_data(scn_link_maps$scn2, scn_link_maps$scn0)
)

diff_titles <- c(
  scn1 = "シナリオB − 現状",
  scn2 = "シナリオC − 現状"
)

# scn1・scn2で共通の対称スケールにする（0を中心に、絶対値の最大で揃える）
flow_diff_lim  <- max(abs(unlist(lapply(scn_diff_maps, function(d) d$flow_diff))), na.rm = TRUE)
speed_diff_lim <- max(abs(unlist(lapply(scn_diff_maps, function(d) d$speed_diff))), na.rm = TRUE)

make_flow_diff_map <- function(map_data, title) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = abs(flow_diff), color = flow_diff)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, flow_diff_lim),
                                transform = "sqrt",
                                name = "交通量の変化\n(|Δ pcu/日|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-flow_diff_lim, flow_diff_lim), oob = scales::squish,
                           name = "交通量の変化\n(Δ pcu/日)") +
    labs(title = paste0(title, "：交通量差分")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

make_speed_diff_map <- function(map_data, title) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = abs(speed_diff), color = speed_diff)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, speed_diff_lim),
                                transform = "sqrt",
                                name = "速度の変化\n(|Δ km/h|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-speed_diff_lim, speed_diff_lim), oob = scales::squish,
                           name = "速度の変化\n(Δ km/h)") +
    labs(title = paste0(title, "：速度差分")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

flow_diff_plots  <- Map(make_flow_diff_map,  scn_diff_maps, diff_titles[names(scn_diff_maps)])
speed_diff_plots <- Map(make_speed_diff_map, scn_diff_maps, diff_titles[names(scn_diff_maps)])

combined_flow_diff_map <- flow_diff_plots$scn1 + flow_diff_plots$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_flow_diff_map)
ggsave("project_log/scenario-analysis/plots/road/scenario_flow_diff_maps.png",
       plot = combined_flow_diff_map, width = 12, height = 6, dpi = 150)

combined_speed_diff_map <- speed_diff_plots$scn1 + speed_diff_plots$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_speed_diff_map)
ggsave("project_log/scenario-analysis/plots/road/scenario_speed_diff_maps.png",
       plot = combined_speed_diff_map, width = 12, height = 6, dpi = 150)

for (scn_name in names(scn_diff_maps)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_vs_scn0_flow_diff_map.png", scn_name),
         plot = flow_diff_plots[[scn_name]], width = 7, height = 6, dpi = 150)
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_vs_scn0_speed_diff_map.png", scn_name),
         plot = speed_diff_plots[[scn_name]], width = 7, height = 6, dpi = 150)
}


#シナリオ別 交通量・速度 差分マップ（主要道路のみ・試作）####
# 上のマップは全リンク（各メッシュ重心から道路網に接続する合成リンク highway="access" を含む）を
# 描画しており、特に都心部でリンクが密集して見にくい。ここでは highway が
# primary（及びそれ以上の幹線）のリンクだけに絞った版を試作として別ファイルで出力する。
# 上のマップ（scenario_flow_diff_maps.png 等）はそのまま残し、ファイル名に "_major" を付けて別出力する。

major_highway_classes <- c("motorway", "motorway_link", "trunk", "trunk_link", "primary", "primary_link")

scn_diff_maps_major <- lapply(scn_diff_maps, function(d) {
  d %>% dplyr::filter(highway %in% major_highway_classes)
})

# 主要道路のみに絞った上で、スケール上下限もその範囲内の最大絶対値に合わせ直す
flow_diff_lim_major  <- max(abs(unlist(lapply(scn_diff_maps_major, function(d) d$flow_diff))), na.rm = TRUE)
speed_diff_lim_major <- max(abs(unlist(lapply(scn_diff_maps_major, function(d) d$speed_diff))), na.rm = TRUE)

make_flow_diff_map_major <- function(map_data, title, lim) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = abs(flow_diff), color = flow_diff)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, lim),
                                transform = "sqrt",
                                name = "交通量の変化\n(|Δ pcu/日|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-lim, lim), oob = scales::squish,
                           name = "交通量の変化\n(Δ pcu/日)") +
    labs(title = paste0(title, "：交通量差分（主要道路のみ）")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

make_speed_diff_map_major <- function(map_data, title, lim) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = abs(speed_diff), color = speed_diff)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, lim),
                                transform = "sqrt",
                                name = "速度の変化\n(|Δ km/h|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-lim, lim), oob = scales::squish,
                           name = "速度の変化\n(Δ km/h)") +
    labs(title = paste0(title, "：速度差分（主要道路のみ）")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

flow_diff_plots_major <- Map(function(d, t) make_flow_diff_map_major(d, t, flow_diff_lim_major),
                              scn_diff_maps_major, diff_titles[names(scn_diff_maps_major)])
speed_diff_plots_major <- Map(function(d, t) make_speed_diff_map_major(d, t, speed_diff_lim_major),
                               scn_diff_maps_major, diff_titles[names(scn_diff_maps_major)])

combined_flow_diff_map_major <- flow_diff_plots_major$scn1 + flow_diff_plots_major$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_flow_diff_map_major)
ggsave("project_log/scenario-analysis/plots/road/scenario_flow_diff_maps_major.png",
       plot = combined_flow_diff_map_major, width = 12, height = 6, dpi = 150)

combined_speed_diff_map_major <- speed_diff_plots_major$scn1 + speed_diff_plots_major$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_speed_diff_map_major)
ggsave("project_log/scenario-analysis/plots/road/scenario_speed_diff_maps_major.png",
       plot = combined_speed_diff_map_major, width = 12, height = 6, dpi = 150)

for (scn_name in names(scn_diff_maps_major)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_vs_scn0_flow_diff_map_major.png", scn_name),
         plot = flow_diff_plots_major[[scn_name]], width = 7, height = 6, dpi = 150)
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_vs_scn0_speed_diff_map_major.png", scn_name),
         plot = speed_diff_plots_major[[scn_name]], width = 7, height = 6, dpi = 150)
}


#シナリオ別 交通量・速度 差分マップ（主要道路のみ・1kmメッシュ背景・試作）####
# 上の「主要道路のみ」版に、1kmメッシュ（key_code_sf）の境界線を背景に重ねた版。
# 既存の全リンク版・主要道路版（メッシュなし）はそのまま残し、ファイル名に "_major_mesh" を付けて別出力する。

make_flow_diff_map_major_mesh <- function(map_data, title, lim) {
  ggplot(map_data) +
    geom_sf(data = key_code_sf, fill = NA, color = "gray60", linewidth = 0.15, inherit.aes = FALSE) +
    geom_sf(aes(linewidth = abs(flow_diff), color = flow_diff)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, lim),
                                transform = "sqrt",
                                name = "交通量の変化\n(|Δ pcu/日|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-lim, lim), oob = scales::squish,
                           name = "交通量の変化\n(Δ pcu/日)") +
    labs(title = paste0(title, "：交通量差分（主要道路のみ・1kmメッシュ）")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

make_speed_diff_map_major_mesh <- function(map_data, title, lim) {
  ggplot(map_data) +
    geom_sf(data = key_code_sf, fill = NA, color = "gray60", linewidth = 0.15, inherit.aes = FALSE) +
    geom_sf(aes(linewidth = abs(speed_diff), color = speed_diff)) +
    scale_linewidth_continuous(range = c(0.4, 2.8), limits = c(0, lim),
                                transform = "sqrt",
                                name = "速度の変化\n(|Δ km/h|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-lim, lim), oob = scales::squish,
                           name = "速度の変化\n(Δ km/h)") +
    labs(title = paste0(title, "：速度差分（主要道路のみ・1kmメッシュ）")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

flow_diff_plots_major_mesh <- Map(function(d, t) make_flow_diff_map_major_mesh(d, t, flow_diff_lim_major),
                                   scn_diff_maps_major, diff_titles[names(scn_diff_maps_major)])
speed_diff_plots_major_mesh <- Map(function(d, t) make_speed_diff_map_major_mesh(d, t, speed_diff_lim_major),
                                    scn_diff_maps_major, diff_titles[names(scn_diff_maps_major)])

combined_flow_diff_map_major_mesh <- flow_diff_plots_major_mesh$scn1 + flow_diff_plots_major_mesh$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_flow_diff_map_major_mesh)
ggsave("project_log/scenario-analysis/plots/road/scenario_flow_diff_maps_major_mesh.png",
       plot = combined_flow_diff_map_major_mesh, width = 12, height = 6, dpi = 150)

combined_speed_diff_map_major_mesh <- speed_diff_plots_major_mesh$scn1 + speed_diff_plots_major_mesh$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_speed_diff_map_major_mesh)
ggsave("project_log/scenario-analysis/plots/road/scenario_speed_diff_maps_major_mesh.png",
       plot = combined_speed_diff_map_major_mesh, width = 12, height = 6, dpi = 150)

for (scn_name in names(scn_diff_maps_major)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_vs_scn0_flow_diff_map_major_mesh.png", scn_name),
         plot = flow_diff_plots_major_mesh[[scn_name]], width = 7, height = 6, dpi = 150)
  ggsave(sprintf("project_log/scenario-analysis/plots/road/%s_vs_scn0_speed_diff_map_major_mesh.png", scn_name),
         plot = speed_diff_plots_major_mesh[[scn_name]], width = 7, height = 6, dpi = 150)
}


#シナリオ比較####
exp(scn0_v)
exp(scn1_v)
exp(scn2_v)

scn0_state$l_i_j
scn1_state$l_i_j
scn2_state$l_i_j

# シナリオ比較関数
make_diff_map2 <- function(data, fill_vars, titles, unit_label = "変化量") {
  # 共通スケール（全変数の最大絶対値）
  lim <- sapply(fill_vars, function(v) max(abs(data[[v]]), na.rm = TRUE)) %>% max()
  
  plots <- Map(function(var, title) {
    ggplot(data) +
      geom_sf(aes(fill = .data[[var]]), color = "gray70", linewidth = 0.1) +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", midpoint = 0,
        limits = c(-lim, lim),
        labels = scales::label_comma(),
        name = unit_label
      ) +
      labs(title = title) +
      theme_void() +
      theme(
        plot.title      = element_text(size = 10, hjust = 0.5),
        legend.title    = element_text(size = 8),
        legend.text     = element_text(size = 7),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "gray80", color = NA)
      )
  }, fill_vars, titles)
  
  Reduce(`+`, plots)  # patchwork で結合
}

# 差分プロットのスケール上下限：固定値ではなく、実際の差分値の絶対値の最大値を採用する
# （キリのいい固定値だと実データの分布に対して広すぎ／狭すぎで発色が薄くなり視認性が落ちるため）
sym_limits <- function(data, vars) {
  lim <- max(abs(unlist(sf::st_drop_geometry(data)[vars])), na.rm = TRUE)
  c(-lim, lim)
}


 #比較　世帯数
scn0_household <- key_code_sf |> 
  left_join(est_household, by ="KEY_CODE") |> 
  rename(scn0_household=est_household)
scn1_household <- rowSums(scn1_state$l_i_j,na.rm=TRUE)
scn1_household<-tibble(
  KEY_CODE=rownames(scn1_state$l_i_j),
  scn1_household=scn1_household
)%>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))
scn2_household <- rowSums(scn2_state$l_i_j,na.rm=TRUE)
scn2_household<-tibble(
  KEY_CODE=rownames(scn2_state$l_i_j),
  scn2_household=scn2_household
)%>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))

diff_household <- scn0_household %>% 
  left_join(scn1_household,by="KEY_CODE") %>% 
  left_join(scn2_household,by="KEY_CODE") %>%
  mutate(diff_household_01=scn1_household-scn0_household) %>% 
  mutate(diff_household_02=scn2_household-scn0_household) %>% 
  dplyr::select(KEY_CODE,diff_household_01,diff_household_02,geometry)

#plot
hh_lim <- sym_limits(diff_household, c("diff_household_01", "diff_household_02"))
p_hh_scn1 <- make_map(diff_household, "diff_household_01", "Change in Number of Resident Households", "magma", hh_lim)
p_hh_scn2 <- make_map(diff_household, "diff_household_02", "Change in Number of Resident Households", "magma", hh_lim)
p_hh_combined <- p_hh_scn1 + p_hh_scn2 # patchwork
plot(p_hh_combined)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_household_diff_maps.png",
       plot = p_hh_combined, width = 12, height = 6, dpi = 150)

p_hh_diverging <- make_diff_map2(diff_household, c("diff_household_01", "diff_household_02"),
                        c("ScenarioB", "ScenarioC"), "(Households)")
print(p_hh_diverging)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_household_diff_maps_diverging.png",
       plot = p_hh_diverging, width = 12, height = 6, dpi = 150)

scn1_household<-left_join(key_code_sf,scn1_household,by="KEY_CODE")
scn2_household<-left_join(key_code_sf,scn2_household,by="KEY_CODE")


#比較　家賃
scn0_rent <- key_code_sf %>% 
  left_join(rent_df, by="KEY_CODE") %>% 
  rename(scn0_rent=est_rent) #単位：円
scn1_rent <- data.frame(
  KEY_CODE = names(scn1_state$r_bar_i), 
  scn1_rent = as.numeric(scn1_state$r_bar_i)
) %>%
  mutate(
    KEY_CODE = gsub("^mc_", "", KEY_CODE),
    scn1_rent=scn1_rent*1000)#単位：千円→円
scn2_rent <- data.frame(
  KEY_CODE = names(scn2_state$r_bar_i), 
  scn2_rent = as.numeric(scn2_state$r_bar_i)
) %>%
  mutate(
    KEY_CODE = gsub("^mc_", "", KEY_CODE),
    scn2_rent=scn2_rent*1000)#単位：千円→円
diff_rent <- scn0_rent %>% 
  left_join(scn1_rent,by="KEY_CODE") %>% 
  left_join(scn2_rent,by="KEY_CODE") %>%
  mutate(diff_rent_01=scn1_rent-scn0_rent) %>% 
  mutate(diff_rent_02=scn2_rent-scn0_rent) %>% 
  dplyr::select(KEY_CODE,diff_rent_01,diff_rent_02,geometry)

#plot
rent_lim <- sym_limits(diff_rent, c("diff_rent_01", "diff_rent_02"))
p_rent_scn1 <- make_map(diff_rent, "diff_rent_01", "Change in Average Imputed Land Rent", "plasma", rent_lim)
p_rent_scn2 <- make_map(diff_rent, "diff_rent_02", "Change in Average Imputed Land Rent", "plasma", rent_lim)
p_rent_combined <- p_rent_scn1 + p_rent_scn2 # patchwork
plot(p_rent_combined)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_rent_diff_maps.png",
       plot = p_rent_combined, width = 12, height = 6, dpi = 150)

p_rent_diverging <- make_diff_map2(diff_rent, c("diff_rent_01", "diff_rent_02"),
                        c("ScenarioB", "ScenarioC"), "(Yen/Month)")
print(p_rent_diverging)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_rent_diff_maps_diverging.png",
       plot = p_rent_diverging, width = 12, height = 6, dpi = 150)


scn1_rent <- left_join(key_code_sf, scn1_rent, by = "KEY_CODE")
scn2_rent <- left_join(key_code_sf, scn2_rent, by = "KEY_CODE")



#比較　床面積
scn0_ar <- key_code_sf %>% 
  left_join(est_ar, by="KEY_CODE") %>% 
  rename(scn0_ar=est_ar) #単位：m2
scn1_ar <- rowSums(scn1_state$a_fij_H * scn1_state$l_i_j, na.rm=TRUE) / rowSums(scn1_state$l_i_j, na.rm=TRUE)
scn1_ar <- tibble(
  KEY_CODE=rownames(scn1_state$a_fij_H),
  scn1_ar =scn1_ar
) %>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))
scn2_ar <- rowSums(scn2_state$a_fij_H * scn2_state$l_i_j, na.rm=TRUE) / rowSums(scn2_state$l_i_j, na.rm=TRUE)
scn2_ar <- tibble(
  KEY_CODE=rownames(scn2_state$a_fij_H),
  scn2_ar =scn2_ar
) %>%
  mutate(KEY_CODE=gsub("^mc_","",KEY_CODE))
diff_ar <- scn0_ar %>% 
  left_join(scn1_ar,by="KEY_CODE") %>% 
  left_join(scn2_ar,by="KEY_CODE") %>% 
  mutate(diff_ar_01=scn1_ar-scn0_ar) %>% 
  mutate(diff_ar_02=scn2_ar-scn0_ar) %>% 
  dplyr::select(KEY_CODE,diff_ar_01,diff_ar_02,geometry)

scn1_ar <- left_join(key_code_sf, scn1_ar , by = "KEY_CODE")
scn2_ar <- left_join(key_code_sf, scn2_ar , by = "KEY_CODE")

ar_lim <- sym_limits(diff_ar, c("diff_ar_01", "diff_ar_02"))
p_ar_scn1 <- make_map(diff_ar, "diff_ar_01", "Change in Average Floor Area", "viridis", ar_lim)
p_ar_scn2 <- make_map(diff_ar, "diff_ar_02", "Change in Average Floor Area", "viridis", ar_lim)
p_ar_combined <- p_ar_scn1 + p_ar_scn2 # patchwork
plot(p_ar_combined)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_floor_area_diff_maps.png",
       plot = p_ar_combined, width = 12, height = 6, dpi = 150)

p_ar_diverging <- make_diff_map2(diff_ar, c("diff_ar_01", "diff_ar_02"),
                        c("ScenarioB", "ScenarioC"), "(m^2)")
print(p_ar_diverging)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_floor_area_diff_maps_diverging.png",
       plot = p_ar_diverging, width = 12, height = 6, dpi = 150)

# 比較　宅地割合
scn0_Gi <- key_code_sf %>% 
  left_join(Gi_df, by="KEY_CODE") %>% 
  rename(scn0_Gi=habit_ar) #単位：割合
scn1_Gi <- data.frame(
  KEY_CODE = names(scn1_state$G_i), 
  scn1_Gi = as.numeric(scn1_state$G_i)
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))
scn2_Gi <- data.frame(
  KEY_CODE = names(scn2_state$G_i), 
  scn2_Gi = as.numeric(scn2_state$G_i)
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))

diff_Gi <- scn0_Gi %>% 
  left_join(scn1_Gi,by="KEY_CODE") %>% 
  left_join(scn2_Gi,by="KEY_CODE") %>%
  mutate(diff_Gi_01=scn1_Gi-scn0_Gi) %>% 
  mutate(diff_Gi_02=scn2_Gi-scn0_Gi) %>% 
  dplyr::select(KEY_CODE,diff_Gi_01,diff_Gi_02,geometry)

Gi_lim <- sym_limits(diff_Gi, c("diff_Gi_01", "diff_Gi_02"))
p_Gi_scn1 <- make_map(diff_Gi, "diff_Gi_01", "Change in Residential Land Ratio", "mako", Gi_lim)
p_Gi_scn2 <- make_map(diff_Gi, "diff_Gi_02", "Change in Residential Land Ratio", "mako", Gi_lim)
p_Gi_combined <- p_Gi_scn1 + p_Gi_scn2 # patchwork
plot(p_Gi_combined)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_residential_land_ratio_diff_maps.png",
       plot = p_Gi_combined, width = 12, height = 6, dpi = 150)

scn1_Gi <- left_join(key_code_sf, scn1_Gi , by = "KEY_CODE")
scn2_Gi <- left_join(key_code_sf, scn2_Gi , by = "KEY_CODE")

p_Gi_diverging <- make_diff_map2(diff_Gi, c("diff_Gi_01", "diff_Gi_02"),
                        c("ScenarioB", "ScenarioC"), "(m^2)")
print(p_Gi_diverging)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_residential_land_ratio_diff_maps_diverging.png",
       plot = p_Gi_diverging, width = 12, height = 6, dpi = 150)


# 比較　建物高さ（推定）
scn0_height <- key_code_sf %>%
  left_join(Hi_df, by="KEY_CODE") %>%
  rename(scn0_height=est_height) #単位：m
scn1_height <- data.frame(
  KEY_CODE = names(results$scn1$state$height_i),
  scn1_height = as.numeric(results$scn1$state$height_i)
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))
scn2_height <- data.frame(
  KEY_CODE = names(results$scn2$state$height_i),
  scn2_height = as.numeric(results$scn2$state$height_i)
) %>%
  mutate(KEY_CODE = gsub("^mc_", "", KEY_CODE))

diff_height <- scn0_height %>%
  left_join(scn1_height,by="KEY_CODE") %>%
  left_join(scn2_height,by="KEY_CODE") %>%
  mutate(diff_height_01=scn1_height-scn0_height) %>%
  mutate(diff_height_02=scn2_height-scn0_height) %>%
  dplyr::select(KEY_CODE,diff_height_01,diff_height_02,geometry)

height_lim <- sym_limits(diff_height, c("diff_height_01", "diff_height_02"))
p_height_scn1 <- make_map(diff_height, "diff_height_01", "Change in Estimated Building Height", "inferno", height_lim)
p_height_scn2 <- make_map(diff_height, "diff_height_02", "Change in Estimated Building Height", "inferno", height_lim)
p_height_combined <- p_height_scn1 + p_height_scn2 # patchwork
plot(p_height_combined)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_building_height_diff_maps.png",
       plot = p_height_combined, width = 12, height = 6, dpi = 150)

scn1_height <- left_join(key_code_sf, scn1_height, by = "KEY_CODE")
scn2_height <- left_join(key_code_sf, scn2_height, by = "KEY_CODE")

p_height_diverging <- make_diff_map2(diff_height, c("diff_height_01", "diff_height_02"),
                        c("ScenarioB", "ScenarioC"), "(m)")
print(p_height_diverging)
ggsave("project_log/scenario-analysis/plots/landuse/scenario_building_height_diff_maps_diverging.png",
       plot = p_height_diverging, width = 12, height = 6, dpi = 150)


#welfare
scn0_welfare <- mean(exp(scn0_v)) #260116厚生の計算、調べる!
scn1_welfare <- mean(exp(scn1_v))
scn2_welfare <- mean(exp(scn2_v))
welfare_change_rate1 <- scn1_welfare / scn0_welfare
welfare_change_rate2 <- scn2_welfare / scn0_welfare
print(paste("scn1平均厚生の変化率:", round(welfare_change_rate1, 4)))
print(paste("scn2平均厚生の変化率:", round(welfare_change_rate2, 4)))

# calculated indirect utility
# vij.sc0=alpha_0*scn0_disposable_income_ij/scn0_state$r_ij_H^alpha_a
# vj.sc0=vij.sc0[1,] %>% as.vector()

# vij.sc1=alpha_0*scn1_disposable_income_ij/scn0_state$r_ij_H^alpha_a
# vj.sc1=vij.sc1[1,] %>% as.vector()

# vij.sc2=alpha_0*scn2_disposable_income_ij/scn0_state$r_ij_H^alpha_a
# vj.sc2=vij.sc2[1,] %>% as.vector()

# EV1=(vj.sc1-vj.sc0)/alpha_0*scn0_state$r_ij_H^alpha_a
# EV2=(vj.sc2-vj.sc0)/alpha_0*scn0_state$r_ij_H^alpha_a


# scn0_household
# scn1_household
# scn2_household 

# # 台形公式
# Benefit.0_1=(scn0_household$scn0_household+scn1_household$scn1_household)/2*EV1
# Benefit.1_1=apply(Benefit.0_1,2,sum)
# Benefit.2_1=sum(Benefit.1_1) # 千円/月

# Benefit.0_2=(scn0_household$scn0_household+scn2_household$scn2_household)/2*EV2
# Benefit.1_2=apply(Benefit.0_2,2,sum)
# Benefit.2_2=sum(Benefit.1_2) # 千円/月


# Δv_j = 均衡効用の差（これがシンプルかつ正確）
delta_vj_1 <- exp(scn1_v) - exp(scn0_v)  # 長さnz_workのベクトル
delta_vj_2 <- exp(scn2_v) - exp(scn0_v)

# EV (式23): i×j行列に展開（byrow=TRUEでjがcolumnに対応）
delta_vj_1_mat <- matrix(delta_vj_1, nrow=nz_res, ncol=nz_work, byrow=TRUE)
delta_vj_2_mat <- matrix(delta_vj_2, nrow=nz_res, ncol=nz_work, byrow=TRUE)

EV1_mat <- (1/alpha_0) * scn0_state$r_ij_H^alpha_a * delta_vj_1_mat
EV2_mat <- (1/alpha_0) * scn0_state$r_ij_H^alpha_a * delta_vj_2_mat

# 台形公式 (式24): l_i_j行列をそのまま使う　千円/月
Benefit.2_1 <- sum((scn0_state$l_i_j + scn1_state$l_i_j) / 2 * EV1_mat, na.rm=TRUE)
Benefit.2_2 <- sum((scn0_state$l_i_j + scn2_state$l_i_j) / 2 * EV2_mat, na.rm=TRUE)

# 万円・月　単位に変換
Benefit.2_1 / 10
Benefit.2_2 / 10




# aa=c(1,2)
# bb=matrix(1:4,2,2)
# aa*bb

# # co2 by dists
# scn0_total_dist <- sum(scn0_state$l_i_j * dists0, na.rm=TRUE) # dists0は片道の所要時間?(分）往復？
# scn1_total_dist <- sum(scn1_state$l_i_j * dists0, na.rm=TRUE)

# 時速を仮定して走行距離を推定：今後は交通モデルと連動
# dist.km=dists0.road*40/60 #km/人・片道 40:時速
# dim(scn1_state$l_i_j)
# dim(dist.km)

# scn0_total_dist <- sum(scn0_state$l_i_j * dist.km*2, na.rm=TRUE) # dists0は片道の所要時間(分）
# scn1_total_dist <- sum(scn1_state$l_i_j * dist.km*2, na.rm=TRUE)
# scn1_total_dist-scn0_total_dist # km/日・全員
# (scn1_total_dist-scn0_total_dist)/scn0_total_dist
# # 260127 127gCO2/kmと仮定
# eCO2=127
# eCO2*scn0_total_dist/10^6
# eCO2*scn1_total_dist/10^6
# dCO2=eCO2*(scn1_total_dist-scn0_total_dist)/10^6 #(tCO2/day)：CO2排出は減少
# dCO2/(eCO2*scn0_total_dist/10^6)


eCO2_car  <- 125  # g-CO2/人・km（自動車） 国土交通省：https://www.mlit.go.jp/sogoseisaku/environment/sosei_environment_tk_000007.html
eCO2_rail <- 17   # g-CO2/人・km（鉄道）

# --- 移動距離行列 (km, 片道) ---
dist_car.km  <- dists0.road * 40 / 60   # 道路：時速40km仮定
dist_rail.km <- dists0.rail * 40 / 60   # 鉄道：表定速度40km仮定

# --- モード分担率行列（l_i_jと同次元であること確認）---

# シナリオ0
scn0_lij_car  <- scn0_state$l_i_j * P.car_scn0   # 自動車通勤世帯数
scn0_lij_rail <- scn0_state$l_i_j * P.rail_scn0  # 鉄道通勤世帯数

scn0_CO2_car  <- sum(scn0_lij_car  * dist_car.km  * 2, na.rm=TRUE) * eCO2_car  / 1e6  # tCO2/日
scn0_CO2_rail <- sum(scn0_lij_rail * dist_rail.km * 2, na.rm=TRUE) * eCO2_rail / 1e6  # tCO2/日
scn0_CO2_total <- scn0_CO2_car + scn0_CO2_rail

# シナリオ1（同様）
scn1_lij_car  <- scn1_state$l_i_j * P.car_scn1
scn1_lij_rail <- scn1_state$l_i_j * P.rail_scn1

scn1_CO2_car  <- sum(scn1_lij_car  * dist_car.km  * 2, na.rm=TRUE) * eCO2_car  / 1e6
scn1_CO2_rail <- sum(scn1_lij_rail * dist_rail.km * 2, na.rm=TRUE) * eCO2_rail / 1e6
scn1_CO2_total <- scn1_CO2_car + scn1_CO2_rail

# シナリオ２
scn2_lij_car  <- scn2_state$l_i_j * P.car_scn2
scn2_lij_rail <- scn2_state$l_i_j * P.rail_scn2

scn2_CO2_car  <- sum(scn2_lij_car  * dist_car.km  * 2, na.rm=TRUE) * eCO2_car  / 1e6
scn2_CO2_rail <- sum(scn2_lij_rail * dist_rail.km * 2, na.rm=TRUE) * eCO2_rail / 1e6
scn2_CO2_total <- scn2_CO2_car + scn2_CO2_rail



# --- 比較 ---
print(paste("scn0 自動車CO2:", round(scn0_CO2_car,  1), "tCO2/日"))
print(paste("scn1 自動車CO2:", round(scn1_CO2_car,  1), "tCO2/日"))
print(paste("scn2 自動車CO2:", round(scn2_CO2_car,  1), "tCO2/日"))

print(paste("scn0 鉄道CO2:", round(scn0_CO2_rail, 1), "tCO2/日"))
print(paste("scn1 鉄道CO2:", round(scn1_CO2_rail, 1), "tCO2/日"))
print(paste("scn2 鉄道CO2:", round(scn2_CO2_rail, 1), "tCO2/日"))


#鉄道 ゾーン別利用世帯数マップ####
# 道路のようなリンク単位のネットワーク（BPR型混雑関数など）が鉄道には無い（dists0.railは
# ゾーン間所要時間行列のみ）ため、リンク別flowマップは作れない。代わりに、ゾーン単位
# （居住地側・従業地側）で鉄道通勤世帯数（l_i_j × P.rail）を集計して塗り分け地図にする。

rail_res_household <- function(lij_rail) {
  tibble(
    KEY_CODE = rownames(lij_rail) %>% gsub("^mc_", "", .),
    rail_household = rowSums(lij_rail, na.rm = TRUE)
  )
}
rail_work_household <- function(lij_rail) {
  tibble(
    KEY_CODE = colnames(lij_rail) %>% gsub("^mc_", "", .),
    rail_household = colSums(lij_rail, na.rm = TRUE)
  )
}

rail_res_list <- list(
  scn0 = rail_res_household(scn0_lij_rail),
  scn1 = rail_res_household(scn1_lij_rail),
  scn2 = rail_res_household(scn2_lij_rail)
)
rail_work_list <- list(
  scn0 = rail_work_household(scn0_lij_rail),
  scn1 = rail_work_household(scn1_lij_rail),
  scn2 = rail_work_household(scn2_lij_rail)
)

# 3シナリオ間で比較しやすいよう、色スケールを共通の範囲に固定する
rail_res_lim  <- range(unlist(lapply(rail_res_list,  function(d) d$rail_household)), na.rm = TRUE)
rail_work_lim <- range(unlist(lapply(rail_work_list, function(d) d$rail_household)), na.rm = TRUE)

rail_res_maps <- lapply(names(rail_res_list), function(scn) {
  make_map(
    key_code_sf %>% left_join(rail_res_list[[scn]], by = "KEY_CODE"),
    "rail_household", scn_titles[[scn]], "viridis", rail_res_lim
  )
})
names(rail_res_maps) <- names(rail_res_list)

rail_work_maps <- lapply(names(rail_work_list), function(scn) {
  make_map(
    key_code_sf %>% left_join(rail_work_list[[scn]], by = "KEY_CODE"),
    "rail_household", scn_titles[[scn]], "viridis", rail_work_lim
  )
})
names(rail_work_maps) <- names(rail_work_list)

combined_rail_res_map <- rail_res_maps$scn0 + rail_res_maps$scn1 + rail_res_maps$scn2 +
  plot_layout(ncol = 3, guides = "collect")
print(combined_rail_res_map)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_ridership_residence_maps.png",
       plot = combined_rail_res_map, width = 18, height = 6, dpi = 150)

combined_rail_work_map <- rail_work_maps$scn0 + rail_work_maps$scn1 + rail_work_maps$scn2 +
  plot_layout(ncol = 3, guides = "collect")
print(combined_rail_work_map)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_ridership_workplace_maps.png",
       plot = combined_rail_work_map, width = 18, height = 6, dpi = 150)

for (scn in names(rail_res_maps)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/rail/%s_rail_ridership_residence_map.png", scn),
         plot = rail_res_maps[[scn]], width = 7, height = 6, dpi = 150)
  ggsave(sprintf("project_log/scenario-analysis/plots/rail/%s_rail_ridership_workplace_map.png", scn),
         plot = rail_work_maps[[scn]], width = 7, height = 6, dpi = 150)
}

# 差分（シナリオB/C − 現状）
diff_rail_res <- key_code_sf %>%
  left_join(rail_res_list$scn0 %>% rename(rail_scn0 = rail_household), by = "KEY_CODE") %>%
  left_join(rail_res_list$scn1 %>% rename(rail_scn1 = rail_household), by = "KEY_CODE") %>%
  left_join(rail_res_list$scn2 %>% rename(rail_scn2 = rail_household), by = "KEY_CODE") %>%
  mutate(
    diff_rail_res_01 = rail_scn1 - rail_scn0,
    diff_rail_res_02 = rail_scn2 - rail_scn0
  )

diff_rail_work <- key_code_sf %>%
  left_join(rail_work_list$scn0 %>% rename(rail_scn0 = rail_household), by = "KEY_CODE") %>%
  left_join(rail_work_list$scn1 %>% rename(rail_scn1 = rail_household), by = "KEY_CODE") %>%
  left_join(rail_work_list$scn2 %>% rename(rail_scn2 = rail_household), by = "KEY_CODE") %>%
  mutate(
    diff_rail_work_01 = rail_scn1 - rail_scn0,
    diff_rail_work_02 = rail_scn2 - rail_scn0
  )

p_rail_res_diverging <- make_diff_map2(diff_rail_res, c("diff_rail_res_01", "diff_rail_res_02"),
                        c("ScenarioB", "ScenarioC"), "(Households)")
print(p_rail_res_diverging)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_ridership_residence_diff_maps.png",
       plot = p_rail_res_diverging, width = 12, height = 6, dpi = 150)

p_rail_work_diverging <- make_diff_map2(diff_rail_work, c("diff_rail_work_01", "diff_rail_work_02"),
                        c("ScenarioB", "ScenarioC"), "(Households)")
print(p_rail_work_diverging)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_ridership_workplace_diff_maps.png",
       plot = p_rail_work_diverging, width = 12, height = 6, dpi = 150)


#鉄道 リンク別利用者数マップ####
# 太さ・色 = リンク別利用世帯数（世帯/日）。道路と異なり鉄道配分は混雑関数を持たないため、
# 「太さ=交通量・色=速度」という組合せは作れず、太さ・色とも利用世帯数の単一指標とする。
# results$scn0/scn1/scn2の rail_link_flow(from, to, ftt, cost, flow, capacity) を
# rail_network（鉄道網ジオメトリ、scripts/Fukuoka_OSM_03_copied.R の rail network 節で構築）
# に結合してマップ化する。type=="access"（メッシュ重心↔最寄駅・駅↔線形頂点の合成リンク）は
# 実在の軌道ではないため描画から除外する。
# assign_traffic()の結果はID1→ID2/ID2→ID1の方向別に別レコードを持つため、物理的に同じ区間を
# 1本の線として描くために往復のflowを合算する（道路の交通量マップとの違い）。

build_scn_rail_map_data <- function(scn_result) {
  flow_fwd <- scn_result$rail_link_flow %>%
    dplyr::select(ID1 = from, ID2 = to, flow_fwd = flow) %>%
    mutate(ID1 = as.character(ID1), ID2 = as.character(ID2))
  flow_rev <- scn_result$rail_link_flow %>%
    dplyr::select(ID1 = to, ID2 = from, flow_rev = flow) %>%
    mutate(ID1 = as.character(ID1), ID2 = as.character(ID2))

  rail_network %>%
    mutate(ID1 = as.character(ID1), ID2 = as.character(ID2)) %>%
    left_join(flow_fwd, by = c("ID1", "ID2")) %>%
    left_join(flow_rev, by = c("ID1", "ID2")) %>%
    mutate(
      flow_fwd = tidyr::replace_na(flow_fwd, 0),
      flow_rev = tidyr::replace_na(flow_rev, 0),
      flow = flow_fwd + flow_rev  # 物理セグメント単位の往復合計利用世帯数
    ) %>%
    dplyr::filter(type %in% c("rail", "subway"))
}

scn_rail_maps <- lapply(results, build_scn_rail_map_data)

rail_flow_range <- range(unlist(lapply(scn_rail_maps, function(d) d$flow)), na.rm = TRUE)

make_rail_flow_map <- function(map_data, title) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = flow, color = flow)) +
    scale_linewidth_continuous(range = c(0.4, 3), limits = rail_flow_range,
                                name = "鉄道利用世帯数\n(世帯/日)") +
    scale_color_viridis_c(option = "viridis", limits = rail_flow_range,
                           oob = scales::squish, name = "鉄道利用世帯数\n(世帯/日)") +
    labs(title = title) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

scn_rail_flow_plots <- Map(make_rail_flow_map, scn_rail_maps, scn_titles[names(scn_rail_maps)])

combined_rail_flow_map <- scn_rail_flow_plots$scn0 + scn_rail_flow_plots$scn1 + scn_rail_flow_plots$scn2 +
  plot_layout(ncol = 3, guides = "collect")
print(combined_rail_flow_map)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_flow_maps.png",
       plot = combined_rail_flow_map, width = 18, height = 6, dpi = 150)

for (scn_name in names(scn_rail_flow_plots)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/rail/%s_rail_flow_map.png", scn_name),
         plot = scn_rail_flow_plots[[scn_name]], width = 7, height = 6, dpi = 150)
}


#鉄道 リンク別利用者数 差分マップ（シナリオB/C − 現状）####

build_scn_rail_diff_map_data <- function(scn_map, base_map) {
  base_flow <- base_map %>%
    st_drop_geometry() %>%
    dplyr::select(ID1, ID2, flow_base = flow)

  scn_map %>%
    left_join(base_flow, by = c("ID1", "ID2")) %>%
    mutate(flow_diff = flow - flow_base)
}

scn_rail_diff_maps <- list(
  scn1 = build_scn_rail_diff_map_data(scn_rail_maps$scn1, scn_rail_maps$scn0),
  scn2 = build_scn_rail_diff_map_data(scn_rail_maps$scn2, scn_rail_maps$scn0)
)

diff_titles_rail <- c(
  scn1 = "シナリオB − 現状",
  scn2 = "シナリオC − 現状"
)

# scn1・scn2で共通の対称スケールにする（固定値ではなく、実際の差分値の絶対値の最大値を使う）
rail_flow_diff_lim <- max(abs(unlist(lapply(scn_rail_diff_maps, function(d) d$flow_diff))), na.rm = TRUE)

make_rail_flow_diff_map <- function(map_data, title, lim) {
  ggplot(map_data) +
    geom_sf(aes(linewidth = abs(flow_diff), color = flow_diff)) +
    scale_linewidth_continuous(range = c(0.4, 3), limits = c(0, lim),
                                transform = "sqrt",
                                name = "鉄道利用世帯数の変化\n(|Δ 世帯/日|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-lim, lim), oob = scales::squish,
                           name = "鉄道利用世帯数の変化\n(Δ 世帯/日)") +
    labs(title = paste0(title, "：鉄道利用者数差分")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

rail_flow_diff_plots <- Map(function(d, t) make_rail_flow_diff_map(d, t, rail_flow_diff_lim),
                             scn_rail_diff_maps, diff_titles_rail[names(scn_rail_diff_maps)])

combined_rail_flow_diff_map <- rail_flow_diff_plots$scn1 + rail_flow_diff_plots$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_rail_flow_diff_map)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_flow_diff_maps.png",
       plot = combined_rail_flow_diff_map, width = 12, height = 6, dpi = 150)

for (scn_name in names(scn_rail_diff_maps)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/rail/%s_vs_scn0_rail_flow_diff_map.png", scn_name),
         plot = rail_flow_diff_plots[[scn_name]], width = 7, height = 6, dpi = 150)
}


#鉄道 リンク別利用者数マップ・差分マップ（1kmメッシュ背景・試作）####
# 上のリンク別利用者数マップ・差分マップに、1kmメッシュ（key_code_sf）の境界線を背景に重ねた版。
# 道路の「主要道路のみ・1kmメッシュ背景」版（scenario_flow_diff_maps_major_mesh.png等）と同じ
# パターン。既存の（メッシュなし）版はそのまま残し、ファイル名に "_mesh" を付けて別出力する。

make_rail_flow_map_mesh <- function(map_data, title) {
  ggplot(map_data) +
    geom_sf(data = key_code_sf, fill = NA, color = "gray60", linewidth = 0.15, inherit.aes = FALSE) +
    geom_sf(aes(linewidth = flow, color = flow)) +
    scale_linewidth_continuous(range = c(0.4, 3), limits = rail_flow_range,
                                name = "鉄道利用世帯数\n(世帯/日)") +
    scale_color_viridis_c(option = "viridis", limits = rail_flow_range,
                           oob = scales::squish, name = "鉄道利用世帯数\n(世帯/日)") +
    labs(title = title) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

scn_rail_flow_plots_mesh <- Map(make_rail_flow_map_mesh, scn_rail_maps, scn_titles[names(scn_rail_maps)])

combined_rail_flow_map_mesh <- scn_rail_flow_plots_mesh$scn0 + scn_rail_flow_plots_mesh$scn1 + scn_rail_flow_plots_mesh$scn2 +
  plot_layout(ncol = 3, guides = "collect")
print(combined_rail_flow_map_mesh)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_flow_maps_mesh.png",
       plot = combined_rail_flow_map_mesh, width = 18, height = 6, dpi = 150)

for (scn_name in names(scn_rail_flow_plots_mesh)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/rail/%s_rail_flow_map_mesh.png", scn_name),
         plot = scn_rail_flow_plots_mesh[[scn_name]], width = 7, height = 6, dpi = 150)
}

make_rail_flow_diff_map_mesh <- function(map_data, title, lim) {
  ggplot(map_data) +
    geom_sf(data = key_code_sf, fill = NA, color = "gray60", linewidth = 0.15, inherit.aes = FALSE) +
    geom_sf(aes(linewidth = abs(flow_diff), color = flow_diff)) +
    scale_linewidth_continuous(range = c(0.4, 3), limits = c(0, lim),
                                transform = "sqrt",
                                name = "鉄道利用世帯数の変化\n(|Δ 世帯/日|)") +
    scale_color_gradient2(low = "blue", mid = "gray85", high = "red", midpoint = 0,
                           limits = c(-lim, lim), oob = scales::squish,
                           name = "鉄道利用世帯数の変化\n(Δ 世帯/日)") +
    labs(title = paste0(title, "：鉄道利用者数差分（1kmメッシュ）")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "gray80", color = NA))
}

rail_flow_diff_plots_mesh <- Map(function(d, t) make_rail_flow_diff_map_mesh(d, t, rail_flow_diff_lim),
                                  scn_rail_diff_maps, diff_titles_rail[names(scn_rail_diff_maps)])

combined_rail_flow_diff_map_mesh <- rail_flow_diff_plots_mesh$scn1 + rail_flow_diff_plots_mesh$scn2 +
  plot_layout(ncol = 2, guides = "collect")
print(combined_rail_flow_diff_map_mesh)
ggsave("project_log/scenario-analysis/plots/rail/scenario_rail_flow_diff_maps_mesh.png",
       plot = combined_rail_flow_diff_map_mesh, width = 12, height = 6, dpi = 150)

for (scn_name in names(scn_rail_diff_maps)) {
  ggsave(sprintf("project_log/scenario-analysis/plots/rail/%s_vs_scn0_rail_flow_diff_map_mesh.png", scn_name),
         plot = rail_flow_diff_plots_mesh[[scn_name]], width = 7, height = 6, dpi = 150)
}


#total ar
scn0_total_ar <- rowSums(scn0_state$a_fij_H * scn0_state$l_i_j, na.rm = TRUE)
scn1_total_ar <- rowSums(scn1_state$a_fij_H * scn1_state$l_i_j, na.rm = TRUE)
scn2_total_ar <- rowSums(scn2_state$a_fij_H * scn2_state$l_i_j, na.rm = TRUE)
(sum(scn1_total_ar) - sum(scn0_total_ar))/sum(scn0_total_ar) # 住宅面積は増加
(sum(scn2_total_ar) - sum(scn0_total_ar))/sum(scn0_total_ar) # 住宅面積は減少


#co2 by ar
co20=(sum(scn0_total_ar)*296/3.6/4.17+(1193*1.99))*0.57/1000/365 #tCO2\day
co21=(sum(scn1_total_ar)*296/3.6/4.17+(1193*1.99))*0.57/1000/365 #tCO2\day
co22=(sum(scn2_total_ar)*296/3.6/4.17+(1193*1.99))*0.57/1000/365 #tCO2\day
(co21-co20)/co20
(co22-co20)/co20

# 地代収入
GI0=scn0_state$rg_i*scn0_state$G_i+(G0_i-scn0_state$G_i)*rr_a
GI1=scn1_state$rg_i*scn1_state$G_i+(G0_i-scn1_state$G_i)*rr_a
GI2=scn2_state$rg_i*scn2_state$G_i+(G0_i-scn2_state$G_i)*rr_a
sum(GI0) # 千円/月 億は10^4千円
sum(GI1) # 千円/月
sum(GI2) # 千円/月

(sum(GI1)-sum(GI0))/sum(GI0) # 千円/月
(sum(GI2)-sum(GI0))/sum(GI0) # 千円/月

# 宅地面積変化
sum(scn0_state$G_i) # m2
sum(scn1_state$G_i) # m2
sum(scn2_state$G_i) # m2
(sum(scn1_state$G_i)-sum(scn0_state$G_i))/sum(scn0_state$G_i) # 宅地面積は増加
(sum(scn2_state$G_i)-sum(scn0_state$G_i))/sum(scn0_state$G_i) # 宅地面積は減少



d_rg_1 <- scn1_state$rg_i - scn0_state$rg_i
d_G_1  <- scn1_state$G_i  - scn0_state$G_i

contrib_price_1 <- sum(d_rg_1 * scn0_state$G_i) #地代単価減少効果が大きい
contrib_area_1  <- sum(scn0_state$rg_i * d_G_1)

# GI の変化を単価変化と面積変化に分解（積の近似分解）
d_rg_2 <- scn2_state$rg_i - scn0_state$rg_i
d_G_2  <- scn2_state$G_i  - scn0_state$G_i

contrib_price_2 <- sum(d_rg_2 * scn0_state$G_i)   # 単価効果　
contrib_area_2  <- sum(scn0_state$rg_i * d_G_2)   # 面積効果　宅地面積増加効果がより大きい

# scn2 総床面積減、総地代収入増
# 地代が安い郊外で面積減り、地代が高い中心部で面積増えるので、地代収入増
# 宅地面積が増えたゾーンと減ったゾーンに分けて集計
increased_2 <- d_G_2 > 0
sum(d_G_2[increased_2])   # 増加分の総量
sum(d_G_2[!increased_2])  # 減少分の総量

# 増加ゾーンの平均地代単価（ベースライン）
mean(scn0_state$rg_i[increased_2]) # 増加ゾーンは単価高い
mean(scn0_state$rg_i[!increased_2]) # 減少ゾーンは単価低い

# あんまり意味ないかも
increased_1 <- d_G_1 > 0 
sum(d_G_1[increased_1])   # 増加分の総量
sum(d_G_1[!increased_1])  # 減少分の総量

# 増加ゾーンの平均地代単価（ベースライン）
mean(scn0_state$rg_i[increased_1]) # 増加ゾーンは単価ちょい高い
mean(scn0_state$rg_i[!increased_1]) # 減少ゾーンは単価ちょい低い


#####

#論文用プロット
## 分析対象範囲プロットの作成
library(ggplot2)
library(ggspatial)
load("data/bnd_mesh_crop.xdr")
load("data/UEA_Fukuoka.xdr")
key_code_sf_w <- key_code_sf %>%
  dplyr::mutate(
    is_work_zone = ifelse(KEY_CODE %in% work_zone, 1, 0)
  )
# dsn_folder <- "data/processed/key_code_sf_w"
# layer_name <- "key_code_sf_w"
# st_write(key_code_sf_w,
#          dsn = dsn_folder,
#          layer = layer_name,
#          driver = "ESRI Shapefile",
#          delete_layer = TRUE)
p_study_area <- ggplot() +
  geom_sf(data = bnd_mesh_crop, aes(fill = "excluded"), color = "gray80") +
  geom_sf(data = key_code_sf_w, aes(fill = as.character(is_work_zone)), color = "gray50",linewidth = 0.3) +
  geom_sf(data = UEA_Fukuoka, fill = NA, aes(color = "boundary"), linewidth = 0.5) +
  
  scale_color_manual(
    name = "Boundary",
    values = c("boundary" = "blue"),
    labels = c("boundary" = "Fukuoka Urban Employment Area")
  ) +
  scale_fill_manual(
    name = "Area Classification",
    values = c("excluded" = "gray", "0" = "green", "1"="red"),
    labels = c("excluded" = "Excluded Area","0"="Analysis Target: Residential Zone", "1"="Analysis Target: Employment Zone")
  ) +
  
  
  annotation_scale(location = "bl", width_hint = 0.3) + # bl: 左下 (bottom left)
  annotation_north_arrow(
    location = "tl", which_north = "true", # tl: 左上 (top left)
    style = north_arrow_fancy_orienteering(), # デザインはお好みで
    height = unit(1, "cm"), width = unit(1, "cm")
  ) +
  
  theme_bw() +
  theme(
    axis.title = element_blank(), # 軸ラベル削除
    axis.text = element_blank(),  # 軸メモリ削除
    axis.ticks = element_blank()  # 目盛線削除
  )
print(p_study_area)
ggsave("project_log/scenario-analysis/plots/study_area_map.png",
       plot = p_study_area, width = 8, height = 7, dpi = 150)


##妥当性確認plot
# install.packages("patchwork")
library(patchwork)
library(ggplot2)
library(ggspatial)

lij_plot_M#model
lij_plot_R#real

rent_plot_M
rent_plot_R

ar_plot_M
ar_plot_R

# ※論文用に個別のタイトルは消して、パネル全体のタイトルにする等の調整も可能です
theme(legend.position = "right") #などで凡例位置を揃えておくと綺麗です

combined_plot <- (lij_plot_R + lij_plot_M) / 
  (rent_plot_R + rent_plot_M) / 
  (ar_plot_R + ar_plot_M) + 
  # 全体の設定
  plot_layout(ncol = 1) & # guides="collect"で同じ凡例をまとめることも可能
  # plot_annotation(tag_levels = 'a')& # (a), (b)... の番号を自動で振る
  theme(
    plot.title = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.4,"cm"),
    legend.key.width = unit(0.4,"cm")
    )


map_elements <- list(
  annotation_north_arrow(
    location = "tl",             # tl=Top Left (左上), tr=Top Right (右上)
    which_north = "true",
    pad_x = unit(0.2, "cm"),     # 端からの余白
    pad_y = unit(0.2, "cm"),
    style = north_arrow_fancy_orienteering(), # デザインはお好みで
    height = unit(0.8, "cm"),    # 矢印の大きさ
    width = unit(0.8, "cm")
  ),

  annotation_scale(
    location = "bl",             # br=Bottom Right (右下), bl=Bottom Left (左下)
    width_hint = 0.3,            # バーの幅（地図全体の何割くらいにするか）
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    text_cex = 0.6,              # 文字サイズ
    bar_cols = c("grey40", "white"),
    line_width = 1,
    height = unit(0.15, "cm")    # バーの太さ（細めにする）
  )
)

final_plot_with_map <- combined_plot & map_elements

print(final_plot_with_map)


##居住分散図
library(scales)
cor_val <- cor(zone_pop_sf$household, zone_pop_sf$zone_population, use = "complete.obs")
r_label <- paste("r =", round(cor_val, 3))

ggplot(zone_pop_sf, aes(x = household, y = zone_population)) +
  geom_point(alpha = 0.6, size = 2, color = "navy") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 0.8) +
  
  scale_x_continuous(labels = label_comma()) + 
  scale_y_continuous(labels = label_comma()) +
  
  labs(
    x = "Observed: Number of Households (households)",
    y = "Model Estimate: Number of Households (households)",
    title = "Model Reproducibility Validation (Households)"
  ) +
  annotate("text", x = Inf, y = -Inf, label = r_label,
           hjust = 1.1, vjust = -1, size = 5, fontface = "italic") +
  
  theme_bw() +
  coord_fixed(ratio = 1)


#シナリオ分析のplot
make_diff_map <- function(data, fill_var, title, limits, breaks) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "gray60", linewidth = 0.1) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = "bl", width_hint = 0.3,
                     bar_cols = c("grey40", "white"),
                     text_cex = 0.8, height = unit(0.15, "cm")) +
    scale_fill_steps2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      limits = limits, breaks = breaks,
      labels = label_comma(), show.limits = TRUE
    ) +
    labs(fill = "Change\n(households)") +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray80", color = NA),
      plot.margin      = margin(10, 10, 10, 10),
      legend.key.height = unit(1.5, "cm")
    )
}

# 使い方
p_scn1 <- make_diff_map(diff_hh_scn1, "diff_household", "Scenario 1: Change in Number of Households",
                         limits = c(-210, 210), breaks = c(-100, -30, -5, 5, 30, 100))
p_scn2 <- make_diff_map(diff_hh_scn2, "diff_household", "Scenario 2: Change in Number of Households",
                         limits = c(-210, 210), breaks = c(-100, -30, -5, 30, 100))

print(p_scn1 + p_scn2)
