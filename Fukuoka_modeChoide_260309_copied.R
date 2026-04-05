# mode choice model estimation
#####
rm(list=ls())
gc();gc();

library(sf)
library(dplyr)
library(viridisLite)
library(tidyr)
library(cppRouting)
# library(igraph)
library(nleqslv)
library(ggplot2)
library(stringr)

#メートル座標系、JGD2011 2011 福岡2系
target_crs <- 6670

# 1kmメッシュ 
bnd_mesh_wL10_wE=st_read("data/processed/bnd_mesh_wL10_filterd/bnd_mesh_wL10_filterd.shp")
st_crs(bnd_mesh_wL10_wE)
plot(bnd_mesh_wL10_wE$geometry)

plot_df <- bnd_mesh_wL10_wE %>%
  mutate(value = log10(T000841002 + 1))

ggplot(plot_df) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "指標（対数）") +
  labs(title = "log(employee)") +
  theme_minimal()

tstr="E:/WorkDir01/data/MLIT/都市交通調査プラットフォーム/第5回北部九州圏パーソントリップ調査（平成29年）/H29_Hokubukyusyu_Bzone/H29_Hokubukyusyu_Bzone/H29_hokubukyusyu5PT_Bzone.shp"
PT_bnd=st_read(tstr)
st_crs(PT_bnd)
PT_bnd2=st_transform(PT_bnd,target_crs)

# extract target area
tI=st_intersects(PT_bnd2,bnd_mesh_wL10_wE)
tI1=lapply(tI,function(x)length(x)) %>% unlist() %>% {\(v) v > 0}() %>% which(.>0) # \(v): function(v) のリテラルだそう，()にパイプで渡る値が入る，返り値はTrue/false
plot(PT_bnd2$geometry)
plot(PT_bnd2$geometry[tI1],col="blue",add=T)
# PT_bnd2
PT_bnd3=PT_bnd2[tI1,]
dim(PT_bnd3)
plot(PT_bnd3$geometry)
plot(bnd_mesh_wL10_wE$geometry,add=T,col=NA,border="blue")
save(PT_bnd3,file="data/PT_bnd3.xdr")


tstr="E:/WorkDir01/data/MLIT/都市交通調査プラットフォーム/第5回北部九州圏パーソントリップ調査（平成29年）/t1208_1000003_all_目的代表交通手段別OD交通量.csv"
ptd=read.csv(tstr,header=T,stringsAsFactors = F)
save(ptd,file="data/ptd.xdr")
load("data/ptd.xdr") # ptd
# file.exists("data/ptd.xdr")
dim(ptd)
ptd1=ptd %>% dplyr::select("O"="発ゾーン","D"="着ゾーン","mode"="代表交通手段","purpose"="目的","trips"="トリップ数") %>% 
  filter(purpose=="1_通勤") %>% mutate(trips=as.numeric(trips)) %>% na.omit()
ptd1=ptd1 %>% mutate(O = str_extract(O, "\\d+$") |> as.integer()) %>% # stringr: \\d+$ は「末尾にある連続した数字」（「末尾が数字で終わる」前提）。
  mutate(D = str_extract(D, "\\d+$") |> as.integer())
# aggragte modes
# ?str_extract
# unique(ptd1$mode) %>% sort()
# unique(ptd1$O) %>% sort()
md.cr.tbl=data.frame(name=c("1_徒歩","2_自転車","3_自動二輪車","4_鉄道","5_バス","6_自動車" ),code=c(1,1,2,3,3,2)) # 1:active,2:private,3:public

ptd2=left_join(ptd1,md.cr.tbl,by=c("mode"="name")) %>% dplyr::select(-mode,-purpose) %>% filter(O!=9999, D!=9999) %>% 
  group_by(O,D,code) %>% summarise(trips=sum(trips)) %>% ungroup()
# dim(ptd1)
sum(ptd2$trips)

# PT_bnd3
ptz=PT_bnd3$B5NO %>% as.numeric() # PTzone code for target region

ptd4=ptd2 %>% filter(O%in%ptz) %>% filter(D%in%ptz)
sum(ptd4$trips)

# code, 1:active,2:private,3:public
# ptd2

load("data/L_j_hat.xdr") # L_j_hat
# sum(L_j_hat$L_j_hat)
# sum(ptd1$trips)

# 経済センサス従業員数1kmメッシュ
# load("data/emp.xdr") # emp
# bnd_mesh_wL10_wEE = bnd_mesh_wL10_wE %>% left_join(emp,by="KEY_CODE") # number of househld

load("data/household.xdr") # household
bnd_mesh_wL10_wH = bnd_mesh_wL10_wE %>% left_join(household,by="KEY_CODE") # number of househld

# sum(bnd_mesh_wL10_wH$household,na.rm=T)
# sum(bnd_mesh_wL10_wE$T000841002)

# ゾーン内の最大の世帯数のメッシュを出発地の代表点，最大の従業員数メッシュを到着地の代表点として
# 機関別のOD交通量をアサインし，ネットワーク上の所要時間を求める
tI=st_intersects(PT_bnd3,bnd_mesh_wL10_wH)
tI4=lapply(tI,\(x){x[which.max(bnd_mesh_wL10_wH$household[x])]}) %>% unlist() 
plot(bnd_mesh_wL10_wH$geometry[tI4],add=T,col="red")
# Gh_max=bnd_mesh_wL10_wH[tI4,]
# 出発グリッド
# home_zone = Gh_max$KEY_CODE %>% as.numeric()
hz_gz=data.frame(ptz=PT_bnd3$B5NO %>% as.numeric(),grd=bnd_mesh_wL10_wH$KEY_CODE[tI4]%>% as.numeric())
# plot(PT_bnd3$geometry[which(PT_bnd3$B5NO=="0305")])
# plot(bnd_mesh_wL10_wH$geometry[which(bnd_mesh_wL10_wH$KEY_CODE==50305486)],add=T,col="red")
tI5=lapply(tI,\(x){x[which.max(bnd_mesh_wL10_wE$T000841002[x])]}) %>% unlist() 
plot(bnd_mesh_wL10_wE$geometry[tI5],add=T,col="green")
# Gh_max=bnd_mesh_wL10_wH[tI4,]
# 出発グリッド
# home_zone = Gh_max$KEY_CODE %>% as.numeric()
wz_gz=data.frame(ptz=PT_bnd3$B5NO %>% as.numeric(),grd=bnd_mesh_wL10_wH$KEY_CODE[tI4]%>% as.numeric())

if(F){
  # 目的地グリッド
  load("data/L_j_hat.xdr") # L_j_hat, grid and number of employee 
  
  work_zone=rownames(L_j_hat) %>% as.numeric()
  L_j_hat.dt=L_j_hat %>% as.data.frame() %>% mutate(KEY_CODE=work_zone)
  wz_grid=bnd_mesh_wL10_wH %>% mutate(KEY_CODE=as.numeric(KEY_CODE)) %>% filter(KEY_CODE %in%work_zone) %>% left_join(L_j_hat.dt,by="KEY_CODE")
  plot(wz_grid$geometry,add=T,col="green")
  # PT zones contains work zone grids
  tI=st_intersects(wz_grid,PT_bnd3) %>% unlist() %>% unique() %>% sort()
  plot(PT_bnd3$geometry[tI],add=T,col=NA,border="purple",lwd=2)
  tI2=st_intersects(PT_bnd3[tI,],wz_grid)
  tI3=lapply(tI2,\(x){x[which.max(wz_grid$L_j_hat[x])]}) %>% unlist() 
  
  wz_gz=data.frame(ptz=PT_bnd3$B5NO[tI] %>% as.numeric(),grd=wz_grid$KEY_CODE[tI3])
}

# 1:active,2:private,3:public
ptd3=ptd4 %>% left_join(hz_gz,by=c("O"="ptz")) %>% left_join(wz_gz,by=c("D"="ptz")) %>% na.omit()
# sum(ptd3$trips)

# railway travel time
# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_OSM_03.R
load("sgr.rail.xdr") # sgr, railway network model for assignment
sgr.rail=sgr

od.rail=ptd3 %>% filter(code==3)
sum(od.rail$trips)

# estimate OD travel time under user zero traffic 
zones.o=ptd3$grd.x %>% unique() %>% sort() %>% paste0("mc_",.)
zones.d=ptd3$grd.y %>% unique() %>% sort() %>% paste0("mc_",.)

# assume no congestion
system.time({ # 0.13 
  dists0.rail<-get_distance_matrix(sgr.rail,
                                   from=zones.o,
                                   to=zones.d,
                                   algorithm = "mch") # because of the rectangular shape of the matrix
})


# road travel time
# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_OSM_03.R
load("sgr.road.xdr") # sgr
sgr.road=sgr
ptd3.road=ptd3 %>% filter(code==2)
## codes for traffic assignment
# tM=expand.grid(zones,centers)
trips=data.frame(from=paste0("mc_",ptd3.road$grd.x),to=paste0("mc_",ptd3.road$grd.y),demand=ptd3.road$trips)
sum(trips$demand)

alpha=0.48;beta=2.89 # yamada/matsui (1998)

load("data/osm/link10.xdr") # link10
load("data/osm/node10.xdr") # node10
nodes=data.frame(node10$ID,st_coordinates(node10))
names(nodes)=c("Node","X","Y")

# traffic assignment
system.time({ # 0.68  
  traffic01 <- assign_traffic(Graph = sgr.road,  from = trips$from, to = trips$to, demand = trips$demand, 
                              max_gap = 1e-4, algorithm = "bfw", verbose = FALSE)
})
# save(traffic01,file="data/traffic_01.xdr")

# create graph with speed under assigned traffic
sgr.road2 <- makegraph(df = traffic01$data[,c("from","to","cost")], 
                       directed = TRUE,
                       capacity = 10^4,
                       alpha = alpha,
                       beta = beta,
                       coords = nodes)

# estimate OD travel time under user equilibrium traffic assignment
system.time({ # 0.01  
  dists1.road<-get_distance_matrix(sgr.road2,
                                   from=zones.o,
                                   to=zones.d,
                                   algorithm = "mch") # because of the rectangular shape of the matrix
})

# by walk/bicycle: assume 15km/h

link11=link10 %>% mutate(FFtime=linklen/15*60/10^3) # %>%  # fftime (minutes)
nodes=data.frame(node10$ID,st_coordinates(node10))
names(nodes)=c("Node","X","Y")

sgr.bike <- makegraph(df = link11[,c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(), 
                      directed = TRUE,
                      capacity = 10^4,
                      # capacity = link10$cap/KK,
                      alpha = alpha,
                      beta = beta,
                      coords = nodes)

system.time({ # 0.01  
  dists1.bike<-get_distance_matrix(sgr.bike,
                                   from=zones.o,
                                   to=zones.d,
                                   algorithm = "mch") # because of the rectangular shape of the matrix
})


ptd3.wide=pivot_wider(ptd3,names_from = code,values_from = trips)

ptd3.wide[which(ptd3.wide$grd.x==50301491 & ptd3.wide$grd.y==50301491),]

dist1.df=as.data.frame.table(dists1.bike) %>% 
  left_join(as.data.frame.table(dists1.road),by=c("Var1","Var2")) %>% 
  left_join(as.data.frame.table(dists0.rail),by=c("Var1","Var2")) %>% 
  mutate(grd.x=str_extract(Var1, "\\d+$") |> as.integer()) %>% 
  mutate(grd.y=str_extract(Var2, "\\d+$") |> as.integer()) %>% 
  dplyr::select(O=grd.x,D=grd.y,T.bike=Freq.x,T.car=Freq.y,T.rail=Freq)

MCdata=ptd3.wide %>% dplyr::select(O.pt=O,D.pt=D,O=grd.x,D=grd.y,Q.bike="1",Q.car="2",Q.rail="3") %>% 
  left_join(dist1.df,by=c("O","D"))

MCdata.cl= MCdata %>% filter(!is.na(T.bike),!is.na(T.car),!is.na(T.rail)) %>% 
  # dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))
  dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))
# 1:active,2:private,3:public

save(MCdata.cl,file="data/MCdata.cl.xdr")

# E:/WorkDir01/prog/R/2025/2025_LUTI_Fukuoka_Ooi/Fukuoka_Rosenka01.R
load("data/PT_bnd3.6668.xdr") # PT_bnd3.6668 kouji-chika + todoufuken-chika-chousa

# assume 3% of land price for annual land rent, 12.5 sqm for parking lot: yen/day
PKP=PT_bnd3.6668 %>% mutate(ptz=as.numeric(PT_bnd3.6668$B5NO), parking=PT_bnd3.6668$avgPrice*0.03/365*12.5) %>% 
  select(ptz,parking) %>% st_drop_geometry()
save(PKP,file="data/PKP.xdr")
head(PKP)

MCdata.cl2=MCdata.cl %>% 
  left_join(PKP,by=c("O.pt"="ptz")) %>% 
  left_join(PKP,by=c("D.pt"="ptz"))
save(MCdata.cl2,file="data/MCdata.cl2.xdr")

#####


# mode choice model estimation
#####
rm(list=ls())
gc();gc();

library(tidyverse)
library(mlogit)

load("data/MCdata.cl.xdr") # MCdata.cl
# sum(MCdata.cl$Q.bike)+sum(MCdata.cl$Q.car)+sum(MCdata.cl$Q.rail)

# MCdata.cl.sam=sample_n(MCdata.cl,size=100)



loglikf=function(para){
  V.bike=para[1]*MCdata.cl$T.bike
  V.car=para[1]*MCdata.cl$T.car+para[2]
  V.rail=para[1]*MCdata.cl$T.rail+para[3]
  
  LL=MCdata.cl$Q.bike*V.bike+MCdata.cl$Q.car*V.car+MCdata.cl$Q.rail*V.rail-
    (MCdata.cl$Q.bike+MCdata.cl$Q.car+MCdata.cl$Q.rail)*log(exp(V.bike)+exp(V.car)+exp(V.rail))
  return(-sum(LL))
}

para0=c(-0.001,0.01,0.01)

loglikf(para0)

system.time({
  zz=optim(par=para0,fn=loglikf,method="BFGS",hessian=T)
})

# 推定パラメータ
coef <- zz$par

# 観測情報行列（= NLL のヘッセ行列）
H <- zz$hessian

# 条件チェック：正定値かどうか（BFGSでも数値的に崩れることはあります）
eigs <- eigen(H, symmetric = TRUE)$values
if (any(eigs <= 0)) {
  warning("Hessian is not positive definite at the optimum. SEs may be unreliable.")
}

# 分散共分散行列（逆行列）
vcov <- tryCatch(solve(H), error = function(e) MASS::ginv(H))  # 失敗時は一般化逆行列で代替

# 標準誤差
se <- sqrt(diag(vcov))

# 参考
# 2) optimHess() でヘッセを再計算（より安定する場合あり）
{
  H2 <- optimHess(par = zz$par, fn = loglikf)  # 最適点で数値的にヘッセを再計算
  vcov2 <- tryCatch(solve(H2), error = function(e) MASS::ginv(H2))
  se2 <- sqrt(diag(vcov2))
}

# まとめ
result <- data.frame(
  param = c("beta_time","const_car","const_rail"),
  estimate = coef,
  std_error = se,
  z = coef / se,
  p_value = 2 * pnorm(abs(coef / se), lower.tail = FALSE)
)

result

# --- 2. Null model（定数項のみ）の対数尤度 ---
# 時間変数（T.*）の係数を 0 にし、ASC（定数）だけにする
loglikf_null <- function(para_null){
  V.bike = 0
  V.car  = para_null[1]
  V.rail = para_null[2]
  
  LL = MCdata.cl$Q.bike*V.bike + MCdata.cl$Q.car*V.car + MCdata.cl$Q.rail*V.rail -
    (MCdata.cl$Q.bike + MCdata.cl$Q.car + MCdata.cl$Q.rail) *
    log(exp(V.bike) + exp(V.car) + exp(V.rail))
  
  return(-sum(LL))  # NLL
}

# 初期値（ASC のみ）
para0_null <- c(0, 0)

zz_null <- optim(par = para0_null, fn = loglikf_null, method = "BFGS")

LL_null <- -zz_null$value  # NLL の符号反転 → LL

# --- 3. McFadden's Pseudo R2 ---
pseudo_R2 <- 1 - (-zz$value / LL_null)

pseudo_R2


# representability



estF=function(para){
  V.bike=para[1]*MCdata.cl$T.bike
  V.car=para[1]*MCdata.cl$T.car+para[2]
  V.rail=para[1]*MCdata.cl$T.rail+para[3]
  
  den=exp(V.bike)+exp(V.car)+exp(V.rail)
  P.bike=exp(V.bike)/den
  P.car=exp(V.car)/den
  P.rail=exp(V.rail)/den
  
  # return(list(P.bike=P.bike,P.car=P.car,P.rail=P.rail))
  return(data.frame(P.bike=P.bike,P.car=P.car,P.rail=P.rail))
}

MCdata.cl
ODD=apply(MCdata.cl[,c("Q.bike","Q.car","Q.rail")],1,sum)
estP=estF(coef)
estQ=estP*ODD
# apply(estQ,1,sum)
plot(MCdata.cl$Q.bike,estQ[,1])
plot(MCdata.cl$Q.car,estQ[,2])
plot(MCdata.cl$Q.rail,estQ[,3])
abline(0,1,col="red")

# estimation error in destination aggregates
tdf=MCdata.cl %>% select(O.pt,D.pt,Q.bike,Q.car,Q.rail) %>% 
  mutate(Qe.bike=estQ[,1],Qe.car=estQ[,2],Qe.rail=estQ[,3]) %>% 
  mutate(dQ.bike=Qe.bike-Q.bike,dQ.car=Qe.car-Q.car,dQ.rail=Qe.rail-Q.rail)

df.agg.d=tdf %>% select(D.pt,dQ.bike,dQ.car,dQ.rail) %>% group_by(D.pt) %>% 
  summarise(dQ.bike=sum(dQ.bike),dQ.car=sum(dQ.car),dQ.rail=sum(dQ.rail)) %>% ungroup()
head(df.agg.d)
load("data/PT_bnd3.xdr") # PT_bnd3

PT_bnd4=PT_bnd3 %>% mutate(zc=as.numeric(B5NO)) %>% left_join(df.agg.d,by=c("zc"="D.pt"))

library(sf)
library(dplyr)
library(ggplot2)

# 例: shp (sf), df (data.frame) を region_id で結合
# map_df <- shp %>%
#   left_join(df, by = "region_id")

# 差分の絶対値の最大を使って、左右対称のレンジに（色のバイアスを避ける）
diff=PT_bnd4$dQ.car
diff=PT_bnd4$dQ.rail
max_abs <- max(abs(diff), na.rm = TRUE)

p <- ggplot(PT_bnd4) +
  geom_sf(aes(fill = diff), color = "grey60", size = 0.1) +   # 枠線は淡色で細く
  coord_sf(datum = NA) +                                      # 座標軸オフ
  scale_fill_gradient2(
    low = "#2b6cb0",      # 青（負）
    mid = "white",        # 白（ゼロ）
    high = "#c53030",     # 赤（正）
    midpoint = 0,         # 0 を中心
    limits = c(-max_abs, max_abs),  # 対称レンジ
    # limits = c(-20000, 20000),  # 対称レンジ
    oob = scales::squish,           # 範囲外は端に丸める
    name = "差分"                    # 凡例タイトル
  ) +
  labs(
    title = "差分コロプレス（正=赤, 負=青）",
    subtitle = "ゼロ中心の発色（scale_fill_gradient2, midpoint=0）",
    caption = "Source: your data"
  ) +
  theme_void(base_family = "") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p

# 中心部で自動車の集中交通量が過大評価：駐車場の料金などが考慮されていない．

#####

# mode choice model estimation with parking cost
#####
rm(list=ls())
gc();gc();

library(tidyverse)
library(mlogit)

load("data/MCdata.cl.xdr") # MCdata.cl
load("data/MCdata.cl2.xdr") # MCdata.cl2
# sum(MCdata.cl$Q.bike)+sum(MCdata.cl$Q.car)+sum(MCdata.cl$Q.rail)

# MCdata.cl.sam=sample_n(MCdata.cl,size=100)



loglikf=function(para){
  V.bike=para[1]*MCdata.cl2$T.bike
  V.car=para[1]*MCdata.cl2$T.car+para[2]+para[4]*(MCdata.cl2$parking.x+MCdata.cl2$parking.y)/2 # cost par trip, parking fee is for two trips
  V.rail=para[1]*MCdata.cl2$T.rail+para[3]
  
  LL=MCdata.cl$Q.bike*V.bike+MCdata.cl$Q.car*V.car+MCdata.cl$Q.rail*V.rail-
    (MCdata.cl$Q.bike+MCdata.cl$Q.car+MCdata.cl$Q.rail)*log(exp(V.bike)+exp(V.car)+exp(V.rail))
  return(-sum(LL))
}

para0=c(-0.001,0.001,0.001,-0.0001)

loglikf(para0)

system.time({
  zz=optim(par=para0,fn=loglikf,method="BFGS",hessian=T)
})

# 推定パラメータ
coef <- zz$par

# 観測情報行列（= NLL のヘッセ行列）
H <- zz$hessian

# 条件チェック：正定値かどうか（BFGSでも数値的に崩れることはあります）
eigs <- eigen(H, symmetric = TRUE)$values
if (any(eigs <= 0)) {
  warning("Hessian is not positive definite at the optimum. SEs may be unreliable.")
}

# 分散共分散行列（逆行列）
vcov <- tryCatch(solve(H), error = function(e) MASS::ginv(H))  # 失敗時は一般化逆行列で代替

# 標準誤差
se <- sqrt(diag(vcov))

# 参考
# 2) optimHess() でヘッセを再計算（より安定する場合あり）
{
  H2 <- optimHess(par = zz$par, fn = loglikf)  # 最適点で数値的にヘッセを再計算
  vcov2 <- tryCatch(solve(H2), error = function(e) MASS::ginv(H2))
  se2 <- sqrt(diag(vcov2))
}

# まとめ
result <- data.frame(
  param = c("beta_time","const_car","const_rail","beta_parking"),
  estimate = coef,
  std_error = se,
  z = coef / se,
  p_value = 2 * pnorm(abs(coef / se), lower.tail = FALSE)
)

# result
save(result,file="data/result.mc02.xdr")

# --- 2. Null model（定数項のみ）の対数尤度 ---
# 時間変数（T.*）の係数を 0 にし、ASC（定数）だけにする
loglikf_null <- function(para_null){
  V.bike = 0
  V.car  = para_null[1]
  V.rail = para_null[2]
  
  LL = MCdata.cl$Q.bike*V.bike + MCdata.cl$Q.car*V.car + MCdata.cl$Q.rail*V.rail -
    (MCdata.cl$Q.bike + MCdata.cl$Q.car + MCdata.cl$Q.rail) *
    log(exp(V.bike) + exp(V.car) + exp(V.rail))
  
  return(-sum(LL))  # NLL
}

# 初期値（ASC のみ）
para0_null <- c(0, 0)

zz_null <- optim(par = para0_null, fn = loglikf_null, method = "BFGS")

LL_null <- -zz_null$value  # NLL の符号反転 → LL

# --- 3. McFadden's Pseudo R2 ---
# 0.2〜0.4：実務でかなり良い
# 0.1〜0.2：普通
# 0.05 以下：モデル仕様の改善を検討
pseudo_R2 <- 1 - (-zz$value / LL_null)

pseudo_R2


# representability


estF=function(para){
  V.bike=para[1]*MCdata.cl$T.bike
  V.car=para[1]*MCdata.cl2$T.car+para[2]+para[4]*(MCdata.cl2$parking.x+MCdata.cl2$parking.y)
  V.rail=para[1]*MCdata.cl$T.rail+para[3]
  
  den=exp(V.bike)+exp(V.car)+exp(V.rail)
  P.bike=exp(V.bike)/den
  P.car=exp(V.car)/den
  P.rail=exp(V.rail)/den
  
  # return(list(P.bike=P.bike,P.car=P.car,P.rail=P.rail))
  return(data.frame(P.bike=P.bike,P.car=P.car,P.rail=P.rail))
}

# MCdata.cl2
ODD=apply(MCdata.cl2[,c("Q.bike","Q.car","Q.rail")],1,sum)
estP=estF(coef)
estQ=estP*ODD
# apply(estQ,1,sum)
plot(MCdata.cl2$Q.bike,estQ[,1])
plot(MCdata.cl2$Q.car,estQ[,2])
plot(MCdata.cl2$Q.rail,estQ[,3])
abline(0,1,col="red")

# estimation error in destination aggregates
tdf=MCdata.cl %>% select(O.pt,D.pt,Q.bike,Q.car,Q.rail) %>% 
  mutate(Qe.bike=estQ[,1],Qe.car=estQ[,2],Qe.rail=estQ[,3]) %>% 
  mutate(dQ.bike=Qe.bike-Q.bike,dQ.car=Qe.car-Q.car,dQ.rail=Qe.rail-Q.rail)

df.agg.d=tdf %>% select(D.pt,dQ.bike,dQ.car,dQ.rail) %>% group_by(D.pt) %>% 
  summarise(dQ.bike=sum(dQ.bike),dQ.car=sum(dQ.car),dQ.rail=sum(dQ.rail)) %>% ungroup()
head(df.agg.d)
load("data/PT_bnd3.xdr") # PT_bnd3

PT_bnd4=PT_bnd3 %>% mutate(zc=as.numeric(B5NO)) %>% left_join(df.agg.d,by=c("zc"="D.pt"))

library(sf)
library(dplyr)
library(ggplot2)

# 例: shp (sf), df (data.frame) を region_id で結合
# map_df <- shp %>%
#   left_join(df, by = "region_id")

# 差分の絶対値の最大を使って、左右対称のレンジに（色のバイアスを避ける）
diff=PT_bnd4$dQ.car
diff=PT_bnd4$dQ.rail
max_abs <- max(abs(diff), na.rm = TRUE)

p <- ggplot(PT_bnd4) +
  geom_sf(aes(fill = diff), color = "grey60", size = 0.1) +   # 枠線は淡色で細く
  coord_sf(datum = NA) +                                      # 座標軸オフ
  scale_fill_gradient2(
    low = "#2b6cb0",      # 青（負）
    mid = "white",        # 白（ゼロ）
    high = "#c53030",     # 赤（正）
    midpoint = 0,         # 0 を中心
    limits = c(-max_abs, max_abs),  # 対称レンジ
    # limits = c(-20000, 20000),  # 対称レンジ
    oob = scales::squish,           # 範囲外は端に丸める
    name = "差分"                    # 凡例タイトル
  ) +
  labs(
    title = "差分コロプレス（正=赤, 負=青）",
    subtitle = "ゼロ中心の発色（scale_fill_gradient2, midpoint=0）",
    caption = "Source: your data"
  ) +
  theme_void(base_family = "") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p

#####