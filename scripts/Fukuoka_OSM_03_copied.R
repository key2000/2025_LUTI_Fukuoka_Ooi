# extract network data from OSM
#road network#####
rm(list=ls())
gc();gc();

library(osmextract)
library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
library(zoo)
library(units)

# 対象の都市雇用圏のデータを抽出
MEA <- st_read("data/UEA/MEAmap3.shp")
# plot(MEA)
UEA_Fukuoka <- MEA %>%
  filter(mea_mea == "40130") # %>% dplyr::select(mea_mea, geometry)
plot(UEA_Fukuoka$geometry)
st_crs(UEA_Fukuoka)
UEA_Fukuoka.wgs84=st_transform(UEA_Fukuoka,4326)
# save(UEA_Fukuoka.wgs84,file="data/UEA/UEA_Fukuoka.wgs84.xdr")
## OSM
tstr="E:/WorkDir01/prog/R/2025/2025_GlobalUrbanModel/data/OSM/"
fl=list.files(tstr,pattern=".pbf")

tI=grep("kyushu",fl)
system.time({ # 36.88  
  pbf.lines=oe_read(paste0(tstr,fl[tI]),layer="lines",force_vectortranslate=T)
})
# save(pbf.lines,file="data/osm/kyushu.pbf.lines.xdr")
load("data/osm/kyushu.pbf.lines.xdr") # pbf.lines

pbf.highway=pbf.lines %>% filter(!is.na(highway))

ttype=c("motorway","motorway_link","trunk","trunk_link","primary","primary_link","secondary","secondary_link","tertiary","tertiary_link")
pbf.highway2=pbf.highway %>% filter(highway%in%ttype)
# plot(pbf.highway2$geometry)
# st_crs(pbf.highway2)

## Fukuoka UEA
# via web-mercator
UEA_Fukuoka.wgs84.buf=UEA_Fukuoka.wgs84 %>% st_transform(3857) %>% st_buffer(100) %>% st_transform(4326)
# UEA_Fukuoka.wgs84.buf.100=UEA_Fukuoka.wgs84 %>% st_transform(3857) %>% st_buffer(100) %>% st_transform(4326)
# UEA_Fukuoka.wgs84.buf.1k=UEA_Fukuoka.wgs84 %>% st_transform(3857) %>% st_buffer(1000) %>% st_transform(4326)
# save(UEA_Fukuoka.wgs84.buf.100,file="data/UEA_Fukuoka.wgs84.buf.100.xdr")
# save(UEA_Fukuoka.wgs84.buf.1k,file="data/UEA_Fukuoka.wgs84.buf.1k.xdr")
# st_write(UEA_Fukuoka.wgs84.buf,dsn="data/UEA/UEA_Fukuoka.wgs84.buf.gpkg")
# save(UEA_Fukuoka.wgs84.buf,file="data/UEA/UEA_Fukuoka.wgs84.buf.xdr")
tI=st_intersects(UEA_Fukuoka.wgs84.buf,pbf.highway2) %>% unlist()
Fukuoka.osm=pbf.highway2[tI,]
# plot(Fukuoka.osm$geometry)

system.time({ # 5.16  
  lanetag=Fukuoka.osm$other_tags %>% 
    strsplit(",") %>% 
    lapply(function(x)return(grep("\"lanes\"=>",x) %>% x[.] %>% 
                               strsplit("=>") %>% unlist() %>% 
                               .[2] %>% gsub("\"","",.)
    ))
  
  speedtag=Fukuoka.osm$other_tags %>% 
    strsplit(",") %>% 
    lapply(function(x)return(grep("\"maxspeed\"=>",x) %>% x[.] %>% 
                               strsplit("=>") %>% unlist() %>% 
                               .[2] %>% gsub("\"","",.)
    ))
  
  onewaytag=Fukuoka.osm$other_tags %>% 
    strsplit(",") %>% 
    lapply(function(x)return(grep("\"oneway\"=>",x) %>% x[.] %>% 
                               strsplit("=>") %>% unlist() %>% 
                               .[2] %>% gsub("\"","",.)
    ))
})
is.na(lanetag)=lengths(lanetag)==0
is.na(speedtag)=lengths(speedtag)==0
is.na(onewaytag)=lengths(onewaytag)==0

Fukuoka.osm2=Fukuoka.osm %>% mutate(nlane=unlist(lanetag),maxspeed=unlist(speedtag),oneway=unlist(onewaytag))
# https://lemulus.me/column/epsg-list-gis
Fukuoka.osm2=st_transform(Fukuoka.osm2,6689) # JGD2011 / UTM zone 52N

# save
Fukuoka.osm2=Fukuoka.osm2 %>% select(osm_id,name,highway,nlane,maxspeed,oneway)
save(Fukuoka.osm2,file="data/osm/Fukuoka.osm2.xdr")
load("data/osm/Fukuoka.osm2.xdr") #Fukuoka.osm2
st_write(Fukuoka.osm2,dsn="data/osm/Fukuoka.osm2.gpkg",delete_dsn = T)

# cast line to point
system.time({ # 9.31 
  df=Fukuoka.osm2 %>% dplyr::select(osm_id)
  pt <- st_cast(df, "POINT")
})

pt_line=st_coordinates(Fukuoka.osm2 %>% dplyr::select(osm_id))

pt_cd=st_coordinates(pt) %>% as.data.frame() # coordinates
n_pt=pt_cd %>% group_by(X,Y) %>% summarise(n=n()) %>% ungroup() %>% ungroup() # group by same coordinates
tI=which(n_pt$n>1) # nodes appeared more than equal twice: end points are neglected
n_pt2=n_pt[tI,] # crossing points
n_pt3=n_pt2 %>% mutate(ID=1:nrow(n_pt2)) # put tentative ID

# convert to sf from nodes coordinates
# node.sf=st_as_sf(n_pt3,coords=c("X","Y"),crs=6691)
# save(node.sf,file="data/node.sf.xdr")
# st_write(node.sf,dsn="data/shape01/kansai.highway.nw.node.gpkg")

pt_cd2=pt_cd %>% left_join(n_pt3,by=c("X","Y")) # crossing point ID
# head(pt)
# head(pt_cd2)

pt2=mutate(pt,n=pt_cd2$n,ID=pt_cd2$ID) # add to the original point data
# head(pt2)
pt3=pt2 %>% na.omit() %>% 
  left_join(Fukuoka.osm2 %>% st_drop_geometry(),by="osm_id") # connecting points among different links


# number of crossing points for each linestring
system.time({ # 0.25 
  line_n=pt3 %>% st_drop_geometry() %>% group_by(osm_id) %>% summarise(n=n())
})


tI=which(line_n$n>2) # lines that has crossing points more than 2: i.e. cross with the other lines at middle of the line
pt3.newseg=pt3 %>% filter(osm_id%in%line_n$osm_id[tI]) # lines with more than 2 nodes
pt3.orgseg=pt3 %>% filter(!osm_id%in%line_n$osm_id[tI]) # lines with 2 nodes

# create segmented lines with more than 2 nodes
n.newseg=pt3.newseg %>% st_drop_geometry() %>% select(osm_id) %>% group_by(osm_id) %>% summarise(n=n())
seg1=seg2=rep(NA,nrow(pt3.newseg))
system.time({ # 1.35 
  for(ii in 1:nrow(n.newseg)){ # ii=1
    tI=which(pt3.newseg$osm_id==n.newseg$osm_id[ii])
    seg1[tI[-length(tI)]]=1:(length(tI)-1)
    seg2[tI[2:length(tI)]]=1:(length(tI)-1)
  }
})
pt3.newseg2=rbind(pt3.newseg %>% mutate(osm_id=paste0(osm_id,"_",seg1)) %>% .[-which(is.na(seg1)),],
                  pt3.newseg %>% mutate(osm_id=paste0(osm_id,"_",seg2)) %>% .[-which(is.na(seg2)),]) %>% 
  arrange(osm_id)

system.time({ # 51.26   
  link3 = rbind(pt3.orgseg,pt3.newseg2) %>% 
    group_by(osm_id) %>% 
    summarize(ID1=first(ID),ID2=last(ID),name=first(name),highway=first(highway),nlane=first(nlane),maxspeed =first(maxspeed),oneway=first(oneway)) %>%
    filter(st_geometry_type(.) == "MULTIPOINT") %>%
    st_cast("LINESTRING")
})

save(link3,file="data/osm/link3.Fukuoka.highway.xdr")
# load("data/osm/link3.Fukuoka.highway.xdr")
# plot(link3$geometry)
st_write(link3,dsn="data/osm/link3.Fukuoka.highway.gpkg",delete_dsn=T)

# check the node name duplication: based on the coordinates
# points3=st_cast(link3,"MULTIPOINT")
# points3$geometry[[1]] %>% st_coordinates()
# link3$geometry[[1]] %>% st_coordinates()
system.time({ # 14.28 
  checkNodes.L=as.list(rep(NA,nrow(link3)))
  for(ii in 1:nrow(link3)){ # ii=1
    cd=link3$geometry[[ii]] %>% st_coordinates()
    checkNodes.L[[ii]]=data.frame(ID=c(link3$ID1[ii],link3$ID2[ii]),X=cd[,1],Y=cd[,2])
  }
})

checkNodes=do.call(rbind,checkNodes.L)

aa=checkNodes %>% group_by(X,Y) %>% summarise(nn=length(unique(ID))) # 同じ座標なのに異なるノード番号
bb=checkNodes %>% group_by(ID) %>% summarise(nnx=length(unique(X)),nny=length(unique(Y))) # 同じIDなのに異なる座標


nodes.Fukuoka.highway=rbind(pt3.orgseg,pt3.newseg2) %>% dplyr::select(ID,geometry) %>% distinct(ID,.keep_all = T)
# save(nodes.kansai.highway,file="data/nodes.kansai.highway.xdr")

nodes2=c(link3$ID1,link3$ID2) %>% unique() %>% sort()
nodes.Fukuoka.highway=nodes.Fukuoka.highway %>% filter(nodes.Fukuoka.highway$ID%in%nodes2)
save(nodes.Fukuoka.highway,file="data/osm/nodes.Fukuoka.highway.xdr")
load("data/osm/nodes.Fukuoka.highway.xdr") # nodes.Fukuoka.highway
plot(nodes.Fukuoka.highway$geometry,add=T)
st_write(nodes.Fukuoka.highway,dsn="data/osm/nodes.Fukuoka.highway.gpkg",delete_dsn=T)

system.time({ # 9.17 
  # 座標抽出（L1 = 元の行番号）
  coords <- st_coordinates(link3)  # matrix with X, Y, L1
  
  # 属性をデータフレーム化して L1 と join
  attrs <- link3 %>%
    st_drop_geometry() %>%
    mutate(L1 = row_number())
  
  # 座標データに属性を join して sf POINT に変換
  points_sf <- coords %>%
    as_tibble() %>%
    left_join(attrs, by = "L1") %>%
    group_by(L1) %>%
    mutate(seq_id = row_number()) %>%  # ← 各LINESTRING内の順番
    ungroup() %>%
    mutate(geometry = st_sfc(map2(X, Y, ~st_point(c(.x, .y))), crs = st_crs(link3))) %>%
    st_as_sf()
})

# ?map2

# start point and end point of each line
system.time({ # 2.85 
  pt_start=points_sf %>% 
    st_drop_geometry() %>% 
    arrange(osm_id, seq_id) %>% 
    group_by(osm_id) %>% 
    summarise(X=first(X),Y=first(Y))
  
  pt_end=points_sf %>% 
    st_drop_geometry() %>% 
    arrange(osm_id, seq_id) %>% 
    group_by(osm_id) %>% 
    summarise(X=last(X),Y=last(Y))
})
pt_se=rbind(pt_start,pt_end) # start and end points

pt_cd2=points_sf %>% 
  st_drop_geometry() %>% 
  left_join(pt_se,by=c("X","Y"),multiple="first") # add osm_id if the point is shared by start and end point (not middle points)

tmpM=pt_cd2 %>% select(osm_id.x,osm_id.y) %>% na.omit() # crossing point only
ptn=tmpM %>% group_by(osm_id.x) %>% summarise(n=n()) # number of crossing points (if line which connect only at end point, ptn=2)
tI=which(ptn$n>2) # 中間点で交点を持つリンク(osm_id）

pt_cd3=pt_cd2 #%>% mutate(osm_id_new=osm_id.x)
pt_cd3$osm_id.y[which(!is.na(pt_cd3$osm_id.y))]=pt_cd3$osm_id.x[which(!is.na(pt_cd3$osm_id.y))]

if(length(tI)>0){
  tmp.L=as.list(rep(NA,length(tI)))
  system.time({ # 331.28    
    for(ii in 1:length(tI)){ # ii=1
      tI2=which(pt_cd3$osm_id.x==ptn$osm_id.x[tI[ii]])
      # pt_cd3[tI2,]
      tmp.tbl=pt_cd3[tI2,]
      tmp.tbl$osm_id.y[nrow(tmp.tbl)]=NA
      
      tI3=which(!is.na(tmp.tbl$osm_id.y))
      
      tmp.tbl1=tmp.tbl[tI3,]
      tmp.tbl2=tmp.tbl[tI3[-1],]
      tmp.tbl=tmp.tbl[-tI3,]
      
      osm_id_sep1=paste0(ptn$osm_id.x[tI[ii]],"_",formatC(1:length(tI3),width=3,flag="0"))
      tmp.tbl1$osm_id.y=osm_id_sep1
      tmp.tbl2$osm_id.y=osm_id_sep1[1:(length(tI3)-1)]
      
      tmp.tbl=rbind(tmp.tbl,tmp.tbl1,tmp.tbl2) %>% arrange(seq_id,osm_id.y)
      tmp.L[[ii]]=tmp.tbl
      pt_cd3=pt_cd3[-tI2,]
    }
  })
  tpt_cd3=do.call(rbind,tmp.L)
  pt_cd4=rbind(pt_cd3,tpt_cd3)
} else {
  pt_cd4=pt_cd3
  
}

pt_cd4$osm_id.y=na.locf(pt_cd4$osm_id.y,)

# ?na.locf

system.time({ # 9.92 
  link4 = st_as_sf(pt_cd4,coords=c("X","Y"),crs=st_crs(link3)) %>% 
    arrange(osm_id.y, seq_id) %>%
    group_by(osm_id.y) %>% 
    summarize(osm_id.x=first(osm_id.x),name=first(name),highway=first(highway),nlane=first(nlane),maxspeed=first(maxspeed),oneway=first(oneway),
              do_union=F,geometry=st_cast(st_combine(geometry), "LINESTRING")) %>%
    st_as_sf()
})

link5=link4 %>% # left_join(kansai.pbf.highway4.UTM54N %>% st_drop_geometry(),by=c("osm_id.x"="osm_id")) %>% # merge()
  mutate(linklen=st_length(link4) %>% drop_units())
save(link5,file="data/osm/link5.xdr")
st_write(link5,dsn="data/osm/link5.gpkg",delete_dsn = T)
load("data/osm/link5.xdr") # link5
plot(link5$geometry)

system.time({ # 9.17 
  # 座標抽出（L1 = 元の行番号）
  coords <- st_coordinates(link5)  # matrix with X, Y, L1
  
  # 属性をデータフレーム化して L1 と join
  attrs <- link5 %>%
    st_drop_geometry() %>%
    mutate(L1 = row_number())
  
  # 座標データに属性を join して sf POINT に変換
  points_sf <- coords %>%
    as_tibble() %>%
    left_join(attrs, by = "L1") %>%
    group_by(L1) %>%
    mutate(seq_id = row_number()) %>%  # ← 各LINESTRING内の順番
    ungroup() %>%
    mutate(geometry = st_sfc(map2(X, Y, ~st_point(c(.x, .y))), crs = st_crs(link5))) %>%
    st_as_sf()
})


# pick up end points
tI1=which(points_sf$seq_id==1)
tI2=points_sf %>% 
  mutate(row_num=row_number()) %>% 
  group_by(osm_id.y) %>% 
  filter(seq_id == max(seq_id)) %>%
  pull(row_num)

ep.sf=points_sf[c(tI1,tI2),] %>% arrange(osm_id.y,seq_id)
# give id to nodes
ep.unique.sf=ep.sf %>% distinct(X,Y,.keep_all = T) %>% mutate(nodeID=paste0("N_",formatC(1:nrow(.),width=5,flag="0")))
ep.sf2=ep.sf %>% left_join(ep.unique.sf %>% st_drop_geometry() %>% dplyr::select(X,Y,nodeID),by=c("X","Y")) %>% 
  rename(linkID=osm_id.y)

# simplified links (just connect endpoints)
system.time({ # 9.98    
  link6 = ep.sf2 %>% 
    group_by(linkID) %>% 
    summarize(ID1=first(nodeID),ID2=last(nodeID),name=first(name),highway=first(highway),nlane=first(nlane),maxspeed=first(maxspeed),oneway=first(oneway),linklen=sum(linklen,na.rm = T),
              do_union=F,geometry=st_cast(st_combine(geometry), "LINESTRING")) %>%
    st_as_sf()
})

# remove unconnected links
library(igraph)

# igraphオブジェクトに変換
g <- graph_from_edgelist(link6[,c("ID1","ID2")] %>% st_drop_geometry() %>% as.matrix(), directed = FALSE)
# ?graph_from_edgelist
E(g)$name=link6$linkID

# components(g) で連結成分を取得
comp <- igraph::components(g)
# comp$no      # 連結成分の数
# comp$membership  # 各エッジがどのグループに属するか
# comp$csize
# tI=which(comp$membership==1)
# length(tI)
# dim(link6)

# エッジごとの成分判定
edge_membership <- apply(as_edgelist(g), 1, function(e) {
  comp$membership[e[1]]  # 片方の頂点の成分を採用（両方同じはず）
})

tI=which(edge_membership==1)

link7=link6[tI,]
plot(link7$geometry)

save(link7,file="data/osm/link7.xdr")
st_write(link7,dsn="data/osm/link7.gpkg",delete_dsn = T)

# node data
node7=ep.sf2 %>% select(nodeID) %>% .[!duplicated(.$nodeID),]
# node7=node7[!duplicated(node7$nodeID),]

linknode=unique(c(link7$ID1,link7$ID2)) %>% sort()
tI=which(node7$nodeID%in%linknode)
# length(tI)
# dim(node7)
node7=node7[tI,]
save(node7,file="data/osm/node7.xdr")
load("data/osm/node7.xdr") # node7
st_write(node7,dsn="data/osm/node7.gpkg",delete_dsn = T)


#####

# Fukuoka mesh data
#####
library(dplyr)
library(sf)

load("data/UEA/UEA_Fukuoka.wgs84.buf.xdr") # UEA_Fukuoka.wgs84.buf
mesh0=st_read("data/1kmメッシュ福岡境界データ/MESH05030.shp")
UEA_Fukuoka.buf.uea=st_transform(UEA_Fukuoka.wgs84.buf,6689)
mesh0.uea=st_transform(mesh0,6689)
tI=st_intersects(UEA_Fukuoka.buf.uea,mesh0.uea) %>% unlist()
mesh.Fukuoka.uea=mesh0.uea[tI,]
plot(mesh.Fukuoka.uea$geometry)
plot(UEA_Fukuoka.buf.uea,add=T,col=NA,border="blue")
plot(mesh.Fukuoka.uea$geometry,add=T,col=NA,border="blue")

st_cast

UEA_Fukuoka.buf.uea.sep=st_cast(UEA_Fukuoka.buf.uea,"POLYGON")
UEA_Fukuoka.buf.uea.sep=UEA_Fukuoka.buf.uea.sep %>% select(mea_mea) %>% mutate(area=st_area(UEA_Fukuoka.buf.uea.sep$geometry))
which.max(UEA_Fukuoka.buf.uea.sep$area)
UEA_Fukuoka.buf.uea.max=UEA_Fukuoka.buf.uea.sep[which.max(UEA_Fukuoka.buf.uea.sep$area),]
plot(UEA_Fukuoka.buf.uea.max$geometry,add=T,col=NA,border="red")
tI=st_intersects(UEA_Fukuoka.buf.uea.max,mesh0.uea) %>% unlist()
mesh.Fukuoka.uea2=mesh0.uea[tI,]
plot(mesh.Fukuoka.uea2$geometry,add=T,col=NA,border="blue")

save(mesh.Fukuoka.uea2,file="data/mesh.Fukuoka.uea2.xdr")


#####

# connect centroid of mesh to osm road network
#####
rm(list=ls())
gc();gc();

library(dplyr)
library(sf)
library(units)
library(purrr)
library(units)
library(data.table)

load("data/osm/link7.xdr") # link7
load("data/osm/node7.xdr") # node7
# load("data/mesh.Fukuoka.uea2.xdr") # mesh.Fukuoka.uea2
mesh.Fukuoka.uea2.0=st_read("data/bnd_mesh_wL10_filterd/bnd_mesh_wL10_filterd.shp")
# st_crs(link7)
# st_crs(mesh.Fukuoka.uea2)
mesh.Fukuoka.uea2=st_transform(mesh.Fukuoka.uea2.0,st_crs(link7))

# centeroid of mesh
mesh.Fukuoka.uea2.cnt=st_centroid(mesh.Fukuoka.uea2)

system.time({ # 2.27  
  distm=st_distance(mesh.Fukuoka.uea2.cnt,node7)
})

tI=apply(distm,1,which.min)
aa1=node7[tI,] %>% dplyr::select(nodeID) %>% mutate(linkID=paste0("acl_",formatC(1:length(tI),width=3,flag="0"))) #%>% rename(ID=osm_id.y)
aa2=mesh.Fukuoka.uea2.cnt %>% mutate(MeshCode=paste0("mc_",KEY_CODE),linkID=paste0("acl_",formatC(1:length(tI),width=3,flag="0"))) %>% rename(nodeID=MeshCode) %>% select(nodeID,linkID,geometry)
aa3=rbind(aa1,aa2) %>% arrange(linkID) %>% select(linkID,nodeID,geometry)

system.time({ # 1.78  
  ac_link = aa3 %>% 
    group_by(linkID) %>% 
    summarize(ID1=first(nodeID),ID2=last(nodeID),name=as.character(NA),highway="access",nlane=2,maxspeed=30,oneway=as.character(NA),
              do_union=F,geometry=st_cast(st_combine(geometry), "LINESTRING")) %>%
    # filter(st_geometry_type(.) == "MULTIPOINT") %>%
    # st_cast("LINESTRING")
    st_as_sf()
})
ac_link2=ac_link %>% mutate(linklen=st_length(ac_link) %>% drop_units())
# ac_link=ac_link %>% rename(osm_id=linkID) %>% mutate()
# link7=link6 %>% mutate(ID1=as.character(ID1),ID2=as.character(ID2),nlane=as.numeric(nlane),maxspeed=as.numeric(maxspeed))
# link7=link6 %>% mutate(nlane=as.numeric(nlane),maxspeed=as.numeric(maxspeed))

link8=link7 %>% mutate(nlane=as.numeric(nlane),maxspeed=as.numeric(maxspeed)) %>% rbind(ac_link2)
# save(link8,file="data/osm/link8.xdr")
# st_write(link8,dsn="data/osm/link8.gpkg",delete_dsn = T)
# load("data/osm/link8.xdr") # link8
plot(link8$geometry,add=T)

nodes8=rbind(
  node7 %>% dplyr::select(ID=nodeID),
  mesh.Fukuoka.uea2.cnt %>% mutate(ID=paste0("mc_",KEY_CODE)) %>% dplyr::select(ID)
)

# save(nodes8,file="data/osm/nodes8.xdr")
# st_write(nodes8,dsn="data/osm/nodes8.gpkg",delete_dsn = T)
# load("data/osm/nodes8.xdr") # nodes8

# estimate nlane, maxsped, oneway based on road type
tmpM=link8 %>% st_drop_geometry() %>% group_by(highway,nlane,maxspeed) %>% summarise(n=n())
tmpM2=tmpM %>% na.omit() %>% as.data.table()
tmpM3=tmpM2[tmpM2[, .I[which.max(n)], by = highway]$V1]

for(ii in 1:nrow(tmpM3)){
  tI=which(link8$highway==tmpM3$highway[ii] & is.na(link8$nlane))
  link8$nlane[tI]=tmpM3$nlane[ii]
  tI=which(link8$highway==tmpM3$highway[ii] & is.na(link8$maxspeed))
  link8$maxspeed[tI]=tmpM3$maxspeed[ii]
  link8$oneway[which(is.na(link8$oneway))]="no"
}

# tI=which(duplicated(link8[,c("ID1","ID2")]))
# tI=which(duplicated(link8[,c("ID1","ID2")]) | duplicated(link8[,c("ID1","ID2")], fromLast = TRUE))
# link8[tI,] %>% arrange("ID1","ID2")

# 出発地と到着地が重複しているリンクを削除
link9 <- link8 %>%
  group_by(ID1, ID2) %>%
  slice_max(linklen, n = 1, with_ties = FALSE) %>%
  ungroup()
# tI=which(duplicated(link9[,c("ID1","ID2")]) | duplicated(link9[,c("ID1","ID2")], fromLast = TRUE))
# tI=which(link9$ID1==link9$ID2) # integer(0)
# # link9[tI,]
# link9=link9[-tI,]

# plot(link9$geometry)
save(link9,file="data/osm/link9.xdr")
st_write(link9,dsn="data/osm/link9.gpkg",delete_dsn = T)

nodes9=nodes8
save(nodes9,file="data/osm/nodes9.xdr")
st_write(nodes9,dsn="data/osm/nodes9.gpkg",delete_dsn = T)
# load("data/osm/nodes9.xdr") # nodes9

# which(nodes9$ID=="N_00069")

#####


# before making reverse link, intermediate node should be removed to make simplified network
# remove intermediate node only connect to two links (OK)
#####
rm(list=ls())
gc();gc();

library(dplyr)
library(sf)
library(units)
library(tidyr)
library(data.table)
library(tidygraph)
library(igraph)

load("data/osm/link9.xdr") # link9
load("data/osm/nodes9.xdr") # nodes9

# estimate nlane, maxsped, oneway based on road type
tmpM=link9 %>% st_drop_geometry() %>% group_by(highway,nlane,maxspeed) %>% summarise(n=n())
tmpM2=tmpM %>% na.omit() %>% as.data.table
tmpM3=tmpM2[tmpM2[, .I[which.max(n)], by = highway]$V1]

hwyn=tmpM$highway %>% unique() %>% sort()
dhwyn=setdiff(hwyn,tmpM3$highway)
tmp.df=data.frame(highway=dhwyn,nlane=1,maxspeed=40,n=0)

tmpM3=rbind(tmpM3,tmp.df) %>% arrange(highway)


for(ii in 1:nrow(tmpM3)){
  tI=which(link9$highway==tmpM3$highway[ii] & is.na(link9$nlane))
  link9$nlane[tI]=tmpM3$nlane[ii]
  tI=which(link9$highway==tmpM3$highway[ii] & is.na(link9$maxspeed))
  link9$maxspeed[tI]=tmpM3$maxspeed[ii]
  link9$oneway[which(is.na(link9$oneway))]="no"
}
# tI=which(is.na(link9$maxspeed))

{
  nodes.df=data.frame(node=c(link9$ID1,link9$ID2)) %>% group_by(node) %>% summarise(n=n())
  tg.nodes=nodes.df$node[which(nodes.df$n==2)]
  
  tg.link=link9 %>% filter(ID1%in%tg.nodes | ID2%in%tg.nodes)
  
  # rest.link=sub_dt2[!sub_dt2$osm_id%in%tg.link$osm_id,]
  rest.link=link9[!link9$linkID%in%tg.link$linkID,]
  
  tg.link.df=tg.link %>% st_drop_geometry() %>% select(linkID,ID1,ID2,maxspeed,linklen) %>% mutate(maxspeed=as.numeric(maxspeed))
  for(ii in 1:length(tg.nodes)){ # ii=1
    tI1=which(tg.link.df$ID1==tg.nodes[ii])
    tI2=which(tg.link.df$ID2==tg.nodes[ii])
    if(length(tI1)>0 & length(tI2)>0){
      mspd=(tg.link.df$maxspeed[tI1]*tg.link.df$linklen[tI1]+tg.link.df$maxspeed[tI2]*tg.link.df$linklen[tI2])/(tg.link.df$linklen[tI1]+tg.link.df$linklen[tI2])
      slen=tg.link.df$linklen[tI1]+tg.link.df$linklen[tI2]
      # tg.link.df=rbind(tg.link.df,
      #                  data.frame(osm_id=tg.link.df$osm_id[tI1],ID1=tg.link.df$ID1[tI2],ID2=tg.link.df$ID2[tI1],maxspeed=mspd,length=slen))
      tg.link.df[nrow(tg.link.df)+1,]=list(tg.link.df$linkID[tI1],tg.link.df$ID1[tI2],tg.link.df$ID2[tI1],mspd,slen)
      tg.link.df=tg.link.df[-c(tI1,tI2),]
    }
  }
  
  tg.link2 <- merge(tg.link.df, nodes9, by.x = "ID1", by.y = "ID", all.x = TRUE) %>% st_sf %>% select(linkID)
  tg.link3 <- merge(tg.link.df, nodes9, by.x = "ID2", by.y = "ID", all.x = TRUE) %>% st_sf %>% select(linkID)
  
  tg.link4=rbind(tg.link2,tg.link3) %>% 
    group_by(linkID) %>% 
    summarize(linkID=first(linkID),
              do_union=F,geometry=st_cast(st_combine(geometry), "LINESTRING")) %>%
    st_as_sf()
  
  
  tg.link5=merge(tg.link4, tg.link.df, by = "linkID", all.x = TRUE) %>% 
    merge(tg.link %>% st_drop_geometry() %>% dplyr::select(linkID,name,highway,nlane,oneway),by = "linkID")
  
  tg.link6=rbind(tg.link5,rest.link)
  simplified_net=tg.link6
}


# plot(simplified_net$geometry)
save(simplified_net,file="data/osm/simplified_net.xdr") # still strange for some links
st_write(simplified_net,dsn="data/osm/simplified_net.gpkg",delete_dsn = T)

linknode=unique(c(simplified_net$ID1,simplified_net$ID2)) %>% sort()
tI=which(nodes9$ID%in%linknode)
# length(tI)
# dim(node7)
nodes9=nodes9[tI,]
simplified_node=nodes9
save(simplified_node,file="data/osm/simplified_node.xdr")
st_write(simplified_node,dsn="data/osm/simplified_net_node.gpkg",delete_dsn = T)

#####


# すべて一方通行リンクとする．
#####
rm(list=ls())
gc();gc();

library(dplyr)
library(sf)

load("data/osm/simplified_net.xdr") # simplified_net
load("data/osm/simplified_node.xdr") # simplified_node

link9=simplified_net
node9=simplified_node

# which(is.na(link9$maxspeed))

# 本当は一方通行なのに，両側通行としているリンクが存在する．
# 逆向きのリンクを作成すると重複していることになる．
oneway0=link9[link9$oneway=="no",]
check.df=rbind(oneway0[,c("ID1","ID2")] %>% st_drop_geometry(),oneway0 %>% rename(ID1=ID2,ID2=ID1) %>% .[,c("ID2","ID1")] %>% st_drop_geometry())
check.n=check.df %>% group_by(ID1,ID2) %>% summarise(n=n())
# unique(check.n$n)
tI=which(check.n$n==2)
link9$oneway[which(link9$ID1%in%check.n$ID1[tI] &link9$ID2%in%check.n$ID2[tI])]="yes" # 両方向存在するリンクは一方通行とする

oneway0=link9[link9$oneway=="no",]

# LINESTRING を反転する関数
reverse_linestring <- function(x) {
  st_linestring(st_coordinates(x)[nrow(st_coordinates(x)):1, 1:2])
}
# 反転適用（ジオメトリ列に適用）
oneway.rev.ln <- st_sfc(lapply(st_geometry(oneway0), reverse_linestring), crs = st_crs(oneway0))
# sf として再構成
oneway.rev.sf <- st_set_geometry(oneway0, oneway.rev.ln) %>% rename(ID1=ID2,ID2=ID1)

link9=rbind(
  link9[link9$oneway=="yes",],
  # link8[link8$oneway=="no",],
  # link8[link8$oneway=="no",] %>% rename(ID1=ID2,ID2=ID1)
  oneway0,
  oneway.rev.sf
)
# which(is.na(link9$maxspeed))

# 重複するリンクは削除
link10 <- link9 %>%
  group_by(ID1, ID2) %>%
  slice_max(nlane, n = 1, with_ties = FALSE) %>%
  ungroup()

# which(is.na(link10$maxspeed))

# dim(link9)
# dim(link10)
# aa=link10 %>% st_drop_geometry() %>% group_by(ID1,ID2) %>% summarise(n=n())
# which(aa$n>1) %>% aa[.,]
# tI=which(aa$n>1)
# dup_all <- duplicated(link10[, c("ID1","ID2")] %>% st_drop_geometry()) | duplicated(link10[, c("ID1","ID2")] %>% st_drop_geometry(), fromLast = TRUE)
# dup.ind <- which(dup_all)
# link10[dup.ind,]

save(link10,file="data/osm/link10.xdr")
st_write(link10,dsn="data/osm/link10.gpkg",delete_dsn = T)


linknode=unique(c(link10$ID1,link10$ID2)) %>% sort()
tI=which(node9$ID%in%linknode)
# length(tI)
# dim(node9)
node10=node9[tI,]
save(node10,file="data/osm/node10.xdr")
st_write(node10,dsn="data/osm/node10.gpkg",delete_dsn = T)


#####


# traffic assignment
#####
rm(list=ls())
gc();gc();

library(cppRouting)
library(dplyr)
library(sf)
library(igraph)

# source("tntp.R")
load("data/osm/link10.xdr") # link10
load("data/osm/node10.xdr") # node10

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
tM=expand.grid(node10$ID[tI],node10$ID[tI])
# head(tM)
trips=data.frame(from=tM[,1],to=tM[,2],demand=5)
dim(trips)
zones=node10$ID[tI]
# city certers
tI=which(node10$ID%in%paste0("mc_",c(50303303,50303385,50305465,50302165,50302401)))
centers=node10$ID[tI]

sgr <- makegraph(df = link10[,c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(), 
                 directed = TRUE,
                 # capacity = 10^4,
                 capacity = link10$cap/KK,
                 alpha = alpha,
                 beta = beta,
                 coords = nodes)
save(sgr,file="sgr.road.xdr")

# estimate OD travel time under user zero traffic 
system.time({ # 0.13 
  dists0<-get_distance_matrix(sgr,
                              from=zones,
                              to=centers,
                              algorithm = "mch") # because of the rectangular shape of the matrix
})




## codes for traffic assignment
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

# estimated link trafic 
estM.df=traffic$data %>% left_join(link10 %>% 
                                     st_drop_geometry() %>% 
                                     dplyr::select(highway,linkID,ID1,ID2,maxspeed,linklen),
                                   by=c("from"="ID1","to"="ID2"))

# merge(cap.df, by.x = "highway", by.y = "hwy", all.x = TRUE)
dim(estM.df)




#####



# rail network
#####
rm(list=ls())
gc();gc();

library(osmextract)
library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
library(zoo)
library(units)

# 対象の都市雇用圏バッファ(m)
load("data/UEA_Fukuoka.wgs84.buf.100.xdr") # UEA_Fukuoka.wgs84.buf.100
load("data/UEA_Fukuoka.wgs84.buf.1k.xdr") # UEA_Fukuoka.wgs84.buf.1k

## OSM
load("data/osm/kyushu.pbf.lines.xdr") # pbf.lines

pbf.other=pbf.lines %>% filter(is.na(highway))

tI=st_intersects(UEA_Fukuoka.wgs84.buf.1k,pbf.other) %>% unlist()
Fukuoka.other.osm=pbf.other[tI,]
st_write(Fukuoka.other.osm,dsn="data/osm/Fukuoka.other.osm.gpkg",delete_dsn = T)

railwaytagind=Fukuoka.other.osm$other_tags %>% 
  strsplit(",") %>% 
  lapply(function(x)return(grep("\"railway\"=>",x) %>% x[.] %>% 
                             strsplit("=>") %>% unlist() %>% 
                             .[2] %>% length(.)
  )) %>% unlist()

Fukuoka.rail=Fukuoka.other.osm[which(railwaytagind==1),]
plot(Fukuoka.rail$geometry)
# st_write(Fukuoka.rail)
st_write(Fukuoka.rail,dsn="data/osm/Fukuoka.rail0.gpkg",delete_dsn = T)
# Fukuoka.rail=st_read("data/osm/Fukuoka.rail0.gpkg")

## OSM
tstr="E:/WorkDir01/prog/R/2025/2025_GlobalUrbanModel/data/OSM/"
fl=list.files(tstr,pattern=".pbf")
tI=grep("kyushu",fl)
system.time({ # 4.67   
  pbf.points=oe_read(paste0(tstr,fl[tI]),layer="points",force_vectortranslate=T)
})
pbf.points.other=pbf.points %>% filter(is.na(highway))

# within the fukuoka geographical buffer
tI=st_intersects(UEA_Fukuoka.wgs84.buf.1k,pbf.points.other) %>% unlist()
Fukuoka.other.osm.points=pbf.points.other[tI,]

# railway
# rail.stop.tag=Fukuoka.other.osm.points$other_tags %>%
#   strsplit(",") %>% 
#   lapply(function(x)return(grep("\"railway\"=>",x) %>% x[.] %>% 
#                              strsplit("=>") %>% unlist() %>% 
#                              .[2] %>% gsub("\"","",.)
#   ))
# tI=lapply(rail.stop.tag,function(x)return(length(x))) %>% unlist() %>% {which(.>0)}
# Fukuoka.other.osm.points.railway=Fukuoka.other.osm.points[tI,] %>% mutate(tag=rail.stop.tag[tI] %>% unlist())
# Fukuoka.other.osm.points.railway$tag %>% unique()
# tI2=which(Fukuoka.other.osm.points.railway$tag=="station") # %>% length()
# tI3=which(Fukuoka.other.osm.points.railway$tag=="stop") # %>% length()

# public transport
rail.station.tag=Fukuoka.other.osm.points$other_tags %>%
  strsplit(",") %>% 
  lapply(function(x)return(grep("\"public_transport\"=>",x) %>% x[.] %>% 
                             strsplit("=>") %>% unlist() %>% 
                             .[2] %>% gsub("\"","",.)
  ))
tI=lapply(rail.station.tag,function(x)return(length(x))) %>% unlist() %>% {which(.>0)}
Fukuoka.other.osm.points.publicTransport=Fukuoka.other.osm.points[tI,] %>% mutate(tag=rail.station.tag[tI] %>% unlist())
# Fukuoka.other.osm.points.publicTransport$tag %>% unique()
tI1=which(Fukuoka.other.osm.points.publicTransport$tag=="station") # %>% length()
# Fukuoka.other.osm.points.publicTransport[tI1,]

plot(Fukuoka.rail$geometry,col="blue",lwd=2)
plot(Fukuoka.other.osm.points.publicTransport$geometry[tI1],pch=20,col="red",cex=2,add=T)
# plot(Fukuoka.other.osm.points.railway$geometry[tI2],pch=20,col="purple",cex=2,add=T)
# plot(Fukuoka.other.osm.points.railway$geometry[tI2],pch=20,col="purple",cex=2,add=T)
# Fukuoka.other.osm.points.railway[tI2,]
# plot(Fukuoka.other.osm.points.railway$geometry[tI3],pch=20,col="green",cex=2,add=T)
Fukuoka.stations=Fukuoka.other.osm.points.publicTransport[tI1,]

# Fukuoka.other.osm.points.railway[tI2,]
# stop1=Fukuoka.other.osm.points.publicTransport$osm_id[tI1]
# stop2=Fukuoka.other.osm.points.railway$osm_id[tI2]
# stop3=Fukuoka.other.osm.points.railway$osm_id[tI3]
# tI4=setdiff(stop1,stop2)
# Fukuoka.other.osm.points.publicTransport[which(Fukuoka.other.osm.points.publicTransport$osm_id%in%tI4),]
save(Fukuoka.stations,file="data/Fukuoka.stations.xdr")
# load("data/Fukuoka.stations.xdr") # Fukuoka.stations
st_write(Fukuoka.stations,dsn="data/osm/Fukuoka.stations.gpkg",delete_dsn = T)


rail.station.tag2=Fukuoka.stations$other_tags %>%
  strsplit(",") %>% 
  lapply(function(x)return(grep("\"railway\"=>",x) %>% x[.] %>% 
                             strsplit("=>") %>% unlist() %>% 
                             .[2] %>% gsub("\"","",.)
  ))
tI=lapply(rail.station.tag2,function(x)return(length(x))) %>% unlist() %>% {which(.>0)}
Fukuoka.stations2=Fukuoka.stations[tI,] %>% mutate(tag=rail.station.tag2[tI] %>% unlist())
# Fukuoka.other.osm.points.publicTransport$tag %>% unique()
tI1=which(Fukuoka.stations2$tag=="station") # %>% length()
Fukuoka.stations2=Fukuoka.stations2[tI1,]

save(Fukuoka.stations2,file="data/Fukuoka.stations2.xdr")
st_write(Fukuoka.stations2,dsn="data/osm/Fukuoka.stations2.gpkg",delete_dsn = T)


# railway tag
fukuoka.railway.tag=Fukuoka.rail$other_tags %>%
  strsplit(",") %>% 
  lapply(function(x)return(grep("\"railway\"=>",x) %>% x[.] %>% 
                             strsplit("=>") %>% unlist() %>% 
                             .[2] %>% gsub("\"","",.)
  ))
# fukuoka.railway.tag %>% unlist() %>% unique()
# lapply(fukuoka.railway.tag,function(x)return(length(x))) %>% unlist() %>% {which(.==0)}
# lapply(fukuoka.railway.tag,function(x)return(length(x))) %>% unlist() %>% {which(.>1)}
fukuoka.railway.tag=fukuoka.railway.tag %>% unlist()

tI5=which(fukuoka.railway.tag%in%c("subway","rail","monorail"))

fukuoka.railway=Fukuoka.rail[tI5,] %>% mutate(type=fukuoka.railway.tag[tI5])
save(fukuoka.railway,file="data/fukuoka.railway.xdr")
st_write(fukuoka.railway,dsn="data/osm/fukuoka.railway.gpkg",delete_dsn = T)


# rail tag: exlude usage except main and branch, (yard should be excluded)
# fukuoka.railway.usage.tag=fukuoka.railway$other_tags %>%
#   strsplit(",") %>% 
#   lapply(function(x)return(grep("\"usage\"=>",x) %>% x[.] %>% 
#                              strsplit("=>") %>% unlist() %>% 
#                              .[2] %>% gsub("\"","",.)
#   ))
# fukuoka.railway.usage.tag %>% unlist() %>% unique()
# # fukuoka.railway.usage.main.ind=lapply(fukuoka.railway.usage.tag,function(x)return(ifelse(x%in%c("main"),1,0))) %>% unlist()
# fukuoka.railway.usage.main.ind=lapply(fukuoka.railway.usage.tag,function(x){
#   y=ifelse(length(x)==1,x,0)
#   return(ifelse(y%in%c("main","branch"),1,0))
#   }) %>% unlist()
# tI6=which(fukuoka.railway.usage.main.ind==0 & fukuoka.railway$type=="rail")
# fukuoka.railway[tI6,]
# fukuoka.railway2=fukuoka.railway[-tI6,]
# all the usage are main or branch

# rail tag: exlude service
fukuoka.railway.service.tag=fukuoka.railway$other_tags %>%
  strsplit(",") %>%
  lapply(function(x)return(grep("\"service\"=>",x) %>% x[.] %>%
                             strsplit("=>") %>% unlist() %>%
                             .[2] %>% gsub("\"","",.)
  ))
fukuoka.railway.service.tag %>% unlist() %>% unique()

fukuoka.railway.service.select.ind=lapply(fukuoka.railway.service.tag,function(x){
  y=ifelse(length(x)==1,x,0)
  return(ifelse(y%in%c("siding","yard","spur","crossover"),1,0))
}) %>% unlist()

fukuoka.railway.service.tag[1:10]
fukuoka.railway.service.tag[[3]] %>% length()
fukuoka.railway.service.tag[[3]]%in%c("siding","yard","spur","crossover")
head(fukuoka.railway.service.select.ind)

tI6=which(fukuoka.railway.service.select.ind==1)
# fukuoka.railway[tI6,] %>% head()
fukuoka.railway2=fukuoka.railway[-tI6,]

# length(fukuoka.railway.usage.main.ind)
# length(fukuoka.railway$type)
# service"=>"yard"
# "usage"=>"main"
save(fukuoka.railway2,file="data/fukuoka.railway2.xdr")
load("data/fukuoka.railway2.xdr") # fukuoka.railway2
st_write(fukuoka.railway2,dsn="data/osm/fukuoka.railway2.gpkg",delete_dsn = T)

#####



# rail network model: connect station and railway, station and zone centroid
#rail network#####
rm(list=ls())
gc();gc();

library(sf)
library(dplyr)
library(units)
library(lwgeom)

load("data/fukuoka.railway2.xdr") # fukuoka.railway2
load("data/Fukuoka.stations2.xdr") # Fukuoka.stations2

# st_write(fukuoka.railway2,dsn="data/osm/fukuoka.railway2.gpkg",delete_dsn = T)

fukuoka.railway2.3857=st_transform(fukuoka.railway2,3857)
Fukuoka.stations2.3857=st_transform(Fukuoka.stations2,3857)

# 必要なら: sf_use_s2(FALSE)

# 入力: stations (POINT sf), lines (LINESTRING/MULTILINESTRING sf) は同じ投影CRS（m）
stations <- Fukuoka.stations2.3857
lines    <- fukuoka.railway2.3857

save(stations,file="data/stations_3857.xdr")
load("data/stations_3857.xdr") # stations

# 1) 駅ごとに「150m以内のラインの行番号」を取得（リスト列）
# st_is_within_distance は x と y の空間関係で、x各要素ごとにyのヒットを返します
within150_list <- st_is_within_distance(stations, lines, dist = 150)

# 2) （空の駅を除外）駅インデックスとラインインデックスの全ペアを展開
sta_idx <- rep(seq_len(nrow(stations)), lengths(within150_list))  # 駅の行番号を繰り返し
lin_idx <- unlist(within150_list)                                  # 対応するラインの行番号

# もし 150m以内に1本もない駅がある場合、上記は空になります（その駅は無視されます）

# 3) 駅とラインを同じ長さにして 1:1 ペアで最近傍線を作る（pairwise=TRUE）
#    駅は sta_idx に従って複製、ラインは lin_idx で抽出
stations_rep <- stations[sta_idx, ]             # 複製済み 駅 sf
lines_rep    <- lines[lin_idx, ]                # 候補ライン sf（同じ長さ）

B_L <- lapply(1:nrow(stations_rep), function(ii) { # i=unique(B_sf$line_id)[2]; i=which(lines$osm_id==23322477)
  verts <- st_cast(lines_rep$geometry[ii], "POINT") # 既存の頂点だけが並ぶ sf
  # 3) p と各頂点の距離を計算して最短を選ぶ（補間なし）
  d <- st_distance(stations_rep$geometry[ii], verts, by_element = FALSE)  # ベクトル（長さ = 頂点数）
  j <- which.min(d)
  nearest_vertex <- verts[j, ]  %>% st_as_sf                # ← これが「既存頂点」で最も近い点
})

B_all <- do.call(rbind, B_L) # %>% st_as_sf
B_sf <- st_as_sf(
  data.frame(station_id = stations_rep$osm_id, line_id = lines_rep$osm_id),
  geometry = B_all$x, crs = st_crs(lines)
)

# ★ 各ペア (p1[i], p2[i]) を LINESTRING(sfg) にする
lines_acc <- Map(
  function(a, b) {
    # 2点を結合 → LINESTRING にキャスト（戻り値は sfg_LINESTRING）
    st_cast(st_combine(c(a, b)), "LINESTRING")
  },
  stations_rep$geometry, B_all$x
)
lines_acc_sfc <- st_sfc(unlist(lines_acc, recursive = FALSE), crs = st_crs(stations_rep))
lines_acc_sf  <- st_sf(
  data.frame(station_id = stations_rep$osm_id, line_id = lines_rep$osm_id),
  geometry = lines_acc_sfc
)

# 1) ラインごとに B を束ねる（line_idでグループ）
split_list <- lapply(unique(lines_acc_sf$line_id), function(line_id) { # i=unique(B_sf$line_id)[2]; line_id=which(lines$osm_id==23322477); line_id=23322477
  
  Li <- lines[which(lines$osm_id==line_id), ]                  # この1行のライン（SFの元属性付き）
  Bi <- B_sf[B_sf$line_id == line_id, ]    # このライン上のBたち（複数可）
  
  # plot(Li$geometry)
  # plot(Bi$geometry,add=T,col="blue",pch=20,cex=2)
  # plot(stations$geometry[which(stations$osm_id%in%Bi$station_id)],add=T,col="red",pch=1,cex=3)
  # check distance : this distance must be exactly zero
  
  # 複数ポイントで一括分割（bladeはst_combine）
  gc  <- st_split(Li, st_combine(Bi))
  out <- st_collection_extract(gc, "LINESTRING")
  
  # plot(out$geometry,add=T,col=1:nrow(out),lwd=2)
  # plot(out$geometry[7],add=T,col="green",lwd=2)
  # label_pts <- st_line_sample(st_geometry(out), sample = 0.5) |> st_cast("POINT")
  # 
  # text(x = st_coordinates(label_pts)[,1],
  #      y = st_coordinates(label_pts)[,2],
  #      labels =1:nrow(out))
  
  # plot(out$geometry[1],add=T,col="red")
  # plot(out$geometry[2],add=T,col="green")
  
  # 元属性を分割片にコピー（全列保持）
  cbind(st_drop_geometry(Li)[rep(1, nrow(out)), , drop = FALSE],
        st_as_sf(out))
})

lines_parts <- do.call(rbind, split_list) %>% st_as_sf

# lines_parts %>% filter(osm_id==23322477)

# combine with original links. lines_parts is only nearest links to the stations
sepid=lines_parts$osm_id %>% unique()
lines.rest=lines[-which(lines$osm_id%in%sepid),] %>% select(osm_id,name,z_order,type)

lines_parts2=lines_parts %>% select(osm_id,name,z_order,type) %>% group_by(osm_id) %>% 
  mutate(idx=row_number(),
         # osm_id = if_else(n() > 1, paste0(osm_id, "_", idx), osm_id)
         osm_id = if (n() > 1) paste0(osm_id, "_", idx) else osm_id
  ) %>% ungroup() %>% 
  select(-idx)

lines2=rbind(lines.rest,lines_parts2) # %>% mutate(length=st_length(.$geometry) %>% drop_units())

# tmp.nodes=st_coordinates(lines2$geometry) %>%
#   as.data.frame() %>% 
#   group_by(X,Y) %>% summarise(n=n()) %>% ungroup() %>% ungroup() # group by same coordinates
# # dim(tmp.nodes)
# lines2=lines2 %>% mutate()

save(lines2,file="data/lines2.xdr")
st_write(lines2,dsn="data/osm/lines2.gpkg",delete_dsn = T)

# 23322477: 福岡市地下鉄七隈線
# lines_acc_sf %>% filter(line_id==23322477)
# ii=which(lines$osm_id==23322477)
# which(B_sf$line_id==ii)
# tI=grep("23322477",lines2$osm_id)

# add access lines from station to railway lines

# use station id with suffix acc_
lines_acc_sf2=lines_acc_sf %>% mutate(osm_id=paste0("acc_",formatC(seq(1,nrow(lines_acc_sf)),width=5,flag="0")),
                                      name=paste0("st_",stations$name[sta_idx]),
                                      z_order=0,
                                      type="access") %>% 
  select(osm_id,name,z_order,type)

lines3=rbind(lines2,lines_acc_sf2) %>% mutate(length=st_length(.$geometry) %>% drop_units())
save(lines3,file="data/lines3.xdr")
st_write(lines3,dsn="data/osm/lines3.gpkg",delete_dsn = T)
load("data/lines3.xdr") # lines3

tI=grep("acc_",lines3$osm_id)
lines3[tI,]

length(unique(lines3$osm_id))
nrow(lines3)

# cast line to point
system.time({ # 9.31 
  df=lines3 %>% select(osm_id)
  pt <- st_cast(df, "POINT")
})
# pt_line=st_coordinates(lines3 %>% select(osm_id))
pt_cd=st_coordinates(pt) %>% as.data.frame() # coordinates
n_pt=pt_cd %>% group_by(X,Y) %>% summarise(n=n()) %>% ungroup() %>% ungroup() # group by same coordinates

# n_pt %>% group_by(n) %>% summarise(nn=n())
tI=which(n_pt$n>1) # nodes appeared more than equal twice: end points are neglected
n_pt2=n_pt[tI,] # crossing points
n_pt3=n_pt2 %>% mutate(ID=1:nrow(n_pt2)) # put tentative ID

# convert to sf from nodes coordinates
pt_cd2=pt_cd %>% left_join(n_pt3,by=c("X","Y")) # crossing point ID
# head(pt)
# head(pt_cd2)

pt2=mutate(pt,n=pt_cd2$n,ID=pt_cd2$ID) # add to the original point data
# head(pt2)
pt3=pt2 %>% na.omit() %>% 
  left_join(lines3 %>% st_drop_geometry(),by="osm_id") # connecting points among different links

fukuoka.railway.simplified=pt3 %>% 
  group_by(osm_id) %>% 
  summarize(ID1=first(ID),ID2=last(ID),name=first(name),z_order=first(z_order),type=first(type),length=first(length)) %>%
  filter(st_geometry_type(.) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

# tI=grep("acc_",fukuoka.railway.simplified$osm_id)
# fukuoka.railway.simplified[tI,]

plot(fukuoka.railway.simplified$geometry)
plot(stations$geometry,add=T,col="red",pch=20)

# fukuoka.railway.simplified=link3
save(fukuoka.railway.simplified,file="data/fukuoka.railway.simplified.xdr")
st_write(fukuoka.railway.simplified,dsn="data/osm/fukuoka.railway.simplified.gpkg",delete_dsn=T)

if(F){
  seg_AB_all   <- st_nearest_points(stations_rep, lines_rep, pairwise = TRUE)
  
  # 前提：p は POINT sf（1行）、lines は LINESTRING/MULTILINESTRING sf
  #       CRS は p と lines で一致させておく（投影座標なら距離はメートル）
  # 1) まず p に最も近いラインを特定
  i <- st_nearest_feature(p, lines)     # p に最も近い行番号
  
  # 2) そのラインを頂点（POINT）に分解
  verts <- st_cast(lines[i, ], "POINT") # 既存の頂点だけが並ぶ sf
  
  # 3) p と各頂点の距離を計算して最短を選ぶ（補間なし）
  d <- st_distance(p, verts, by_element = FALSE)  # ベクトル（長さ = 頂点数）
  j <- which.min(d)
  
  nearest_vertex <- verts[j, ]                  # ← これが「既存頂点」で最も近い点
  st_coordinates(nearest_vertex)                # 座標を取り出す
  
  # → 駅Aから対応ライン上Bへの直線（2点LINESTRING）が全ペア分できます
  # 仕様は sf の st_nearest_points の説明どおりです。[2](https://www.rdocumentation.org/packages/sf/versions/1.0-23/topics/st_nearest_points)
  
  # 23322477: 福岡市地下鉄七隈線
  # lines_rep %>% filter(osm_id==23322477)
  
  # （任意）長過ぎるものの除外：st_is_within_distance で既に150m以内ですが再確認したい場合のみ
  # len_all   <- drop_units(st_length(seg_AB_all))
  # keep_all  <- which(len_all <= 150)
  # seg_AB_all <- seg_AB_all[keep_all]
  # sta_idx    <- sta_idx[keep_all]
  # lin_idx    <- lin_idx[keep_all]
  
  # 4) B 点の抽出（A,B,A,B,... の偶数番目がB）
  pts_AB_all <- st_cast(seg_AB_all, "POINT")
  B_all      <- pts_AB_all[seq(2, length(seg_AB_all) * 2, by = 2)]
  A_all      <- pts_AB_all[seq(1, length(seg_AB_all) * 2, by = 2)]
  
  # ?st_line_project
  # library(nngeo)
  # # B_on <- st_snap_points(Bi, Li, max_dist = 200)  # 許容距離は適宜（m）
  # B_on <- nngeo::st_snap_points(B_all, lines_rep, max_dist = 200)  # 許容距離は適宜（m）
  # B_on <- sf::st_snap(B_all, st_geometry(lines_rep), tolerance = 200) # 全然違う場所にスナップされるどうしようもない．
  B_on <- sf::st_snap(B_all, st_geometry(lines_rep), tolerance = 0.5) # できた．着地点からの距離なので，近くで十分．# osmid=23322477についてはできていなかった．誤差発生
  
  # ii=which(lines_rep$osm_id==23322477)[1]
  # # lines_rep[ii,]
  # plot(lines_rep$geometry[ii])
  # plot(B_all[ii],add=T,col="red")
  
  # --- 2) 座標精度の丸め（グリッドに吸着） ---
  # 精度値は“座標の単位”（投影座標なら m）。1e-9 〜 1e-7 くらいで調整。
  # prec <- 1e-9
  # B_all_round   <- st_set_precision(B_on, precision = prec)   |> st_round()
  # lines_rep_round <- st_set_precision(lines_rep, precision = prec) |> st_round()
  # packageVersion("sf") 
  
  # 投影座標（m）であることが前提
  prec <- 1e-9  # 例：1e-9 m グリッドに丸める（必要に応じて調整）
  # 1) 座標の丸め（sf 1.0-23では st_snap_to_grid を使用）
  B_all_round     <- sf::st_snap_to_grid(B_on,      size = prec)
  lines_rep_round <- sf::st_snap_to_grid(lines_rep, size = prec)
  
  # ?st_round
  
  # 以降は “丸め済み” を使う：
  B_sf <- st_as_sf(
    data.frame(station_id = sta_idx, line_id = lin_idx),
    geometry = B_all_round, crs = st_crs(lines_rep_round)
  )
  
  # LINESTRING の作成も丸め済みで
  lines_acc <- Map(
    function(a, b) { st_cast(st_combine(c(a, b)), "LINESTRING") },
    A_all, B_all_round
  )
  lines_acc_sfc <- st_sfc(unlist(lines_acc, recursive = FALSE), crs = st_crs(lines_rep_round))
  lines_acc_sf  <- st_sf(
    data.frame(station_id = stations$osm_id[sta_idx], line_id = lines$osm_id[lin_idx]),
    geometry = lines_acc_sfc
  )
  
  # 
  system.time({ # 1.49 
    for(ii in 1:length(B_all)){ # ii=1, ii=which(lines_rep$osm_id==23322477)[1]
      # 線上位置（通過距離）→ 線上点を内挿
      loc<- sf::st_line_project(lines_rep$geometry[ii], B_all[ii]) # 距離（座標単位）
      # Bi_on_sfc <- sf::st_line_interpolate(lines_rep$geometry[ii], loc) # 線上点を生成
      # Bi_on     <- st_as_sf(Bi[, setdiff(names(Bi), attr(Bi, "sf_column"))],
      #                       geometry = Bi_on_sfc, crs = st_crs(Li))
      B_all[ii] <- sf::st_line_interpolate(lines_rep$geometry[ii], loc) # 線上点を生成
    }
  })
  # ?st_snap
  # B_all=B_on
  
  # access link from station to rail lines
  
  # 1) ペアごとに結合 → LINESTRING にキャスト
  # lines_acc <- mapply(function(a, b) {
  #   st_cast(st_combine(c(a, b)), "LINESTRING")
  # }, A_all, B_all, SIMPLIFY = FALSE)
  
  
  # ★ 各ペア (p1[i], p2[i]) を LINESTRING(sfg) にする
  lines_acc <- Map(
    function(a, b) {
      # 2点を結合 → LINESTRING にキャスト（戻り値は sfg_LINESTRING）
      st_cast(st_combine(c(a, b)), "LINESTRING")
    },
    A_all, B_all
  )
  
  # 2) sf オブジェクトにまとめる
  # lines_sf <- st_sfc(lines_acc, crs = st_crs(lines)) # error
  
  # ★ do.call で sfg を展開して sfc を作る
  # lines_sfc <- do.call(st_sfc, c(lines_acc, crs = st_crs(lines)))  # p1 と同じ CRS を使用
  # または:
  lines_acc_sfc <- st_sfc(unlist(lines_acc, recursive = FALSE), crs = st_crs(lines))
  
  lines_acc_sf <- st_sf(
    # data.frame(id = seq_along(lines_sfc)), 
    data.frame(station_id = stations$osm_id[sta_idx], line_id = lines$osm_id[lin_idx]),
    geometry = lines_acc_sfc
  )
  
  # 23322477: 福岡市地下鉄七隈線
  lines_acc_sf %>% filter(line_id==23322477)
  plot(lines$geometry[which(lines$osm_id==23322477)])
  plot(lines_acc_sf$geometry[which(lines_acc_sf$line_id==23322477)],add=T,col="red",lwd=3)
  plot(stations$geometry[which(stations$osm_id%in%lines_acc_sf$station_id[which(lines_acc_sf$line_id==23322477)])],add=T,col="blue",cex=2,pch=1)
  # stations[which(stations$osm_id%in%lines_acc_sf$station_id[which(lines_acc_sf$line_id==23322477)]),]
  
  # plot(lines_rep$geometry)
  # plot(B_on,add=T,col="blue",pch=20,cex=2)
  # plot(B_all,add=T,col="red",pch=20,cex=2)
  
  # geom_L=st_geometry(lines_rep)
  # # Bi を Li に沿った正確な「線上の点」に置き換える
  # # 複数点をペアで処理
  # loc_list <- st_line_project(geom_L, st_geometry(B_all))  # Li上の位置(0..1の測線比)
  # B_on_sfc <- st_line_sample(geom_L, sample = loc_list)      # 線上のポイントに再生成
  # B_on <- st_as_sf(Bi[, setdiff(names(Bi), attr(Bi, "sf_column"))], 
  #                  geometry = B_on_sfc, crs = st_crs(Li))
  
  
  # 5) 「どのライン上のBか」をsfで保持（駅IDとラインIDを属性に）
  B_sf <- st_as_sf(
    data.frame(station_id = sta_idx, line_id = lin_idx),
    geometry = B_all,
    crs = st_crs(lines)
  )
  
  # dim(lines_acc_sf)
  # dim(B_sf)
  
  # 23322477: 福岡市地下鉄七隈線
  # lines_acc_sf %>% filter(line_id==23322477)
  ii=which(lines$osm_id==23322477)
  which(B_sf$line_id==ii)
  
  # ?st_line_project
  i=which(lines$osm_id==23322477)
  Li <- lines[ i, ]                  # この1行のライン（SFの元属性付き）
  Bi <- B_sf[B_sf$line_id == i, ]    # このライン上のBたち（複数可）Bi_on <- sf::st_snap(Bi, st_geometry(Li), tolerance = 0.5) # できた．着地点からの距離なので，近くで十分．
  d <- drop_units(st_distance(Bi_on, st_geometry(Li), by_element = FALSE))
  summary(d)
  
  plot(Li$geometry)
  plot(Bi$geometry,add=T,col="blue",pch=20,cex=2)
  # check distance : this distance must be exactly zero
  
  # 複数ポイントで一括分割（bladeはst_combine）
  gc  <- st_split(Li, st_combine(Bi))
  out <- st_collection_extract(gc, "LINESTRING")
  
  
  
  
  # ii=1
  # plot(lines$geometry[B_sf$line_id[which(B_sf$station_id==ii)]])
  # plot(stations$geometry[ii],add=T,col="red",pch=20)
  # plot(st_geometry(B_sf),col="blue",add=T,pch=20)
  # stations[ii,]
  
  # 6) （任意）プロット確認
  plot(st_geometry(lines), col = 'grey')
  plot(st_geometry(seg_AB_all), col = 'blue', add = TRUE)  # 駅→Bの接続線
  plot(st_geometry(B_sf),col="red",add=T)
  
  
  # library(lwgeom)  # 推奨（複数点で分割を安定化）
  # 1) ラインごとに B を束ねる（line_idでグループ）
  split_list <- lapply(unique(B_sf$line_id), function(i) { # i=unique(B_sf$line_id)[2]; i=which(lines$osm_id==23322477)
    Li <- lines[ i, ]                  # この1行のライン（SFの元属性付き）
    Bi <- B_sf[B_sf$line_id == i, ]    # このライン上のBたち（複数可）
    
    # plot(Li$geometry)
    # plot(Bi$geometry,add=T,col="blue",pch=20,cex=2)
    # check distance : this distance must be exactly zero
    
    
    # 複数ポイントで一括分割（bladeはst_combine）
    gc  <- st_split(Li, st_combine(Bi))
    out <- st_collection_extract(gc, "LINESTRING")
    
    # plot(out$geometry[1],add=T,col="red")
    # plot(out$geometry[2],add=T,col="green")
    
    # 元属性を分割片にコピー（全列保持）
    cbind(st_drop_geometry(Li)[rep(1, nrow(out)), , drop = FALSE],
          st_as_sf(out))
  })
  
  lines_parts <- do.call(rbind, split_list) %>% st_as_sf
  
  # lines_parts$geometry[1]
  # plot(lines_parts$geometry[1])
  # plot(B_sf$geometry[1],add=T,pch=20)
  
  # ii=1
  # plot(lines$geometry[B_sf$line_id[which(B_sf$station_id==ii)]])
  # plot(stations$geometry[ii],add=T,col="red",pch=20)
  # plot(st_geometry(B_sf),col="blue",add=T,pch=20)
  
  # 確認（分割片を重ね描き）
  plot(st_geometry(lines), col = 'grey')
  plot(st_geometry(lines_parts), col = 'dodgerblue', lwd = 2, add = TRUE)
  plot(st_geometry(B_sf),col="red",add=T)
  
  
  # nodes:
  # link_nodes=st_coordinates(lines3$geometry) %>% as.data.frame() %>% group_by(X,Y) %>% summarise(n=n()) %>% ungroup()
  # link_nodes %>% group_by(n) %>% summarise(nn=n())
  # stations[sta_idx,]
  
  # cast line to point
  system.time({ # 9.31 
    df=lines3 %>% select(osm_id)
    pt <- st_cast(df, "POINT")
  })
  # pt_line=st_coordinates(lines3 %>% select(osm_id))
  pt_cd=st_coordinates(pt) %>% as.data.frame() # coordinates
  n_pt=pt_cd %>% group_by(X,Y) %>% summarise(n=n()) %>% ungroup() %>% ungroup() # group by same coordinates
  
  tI=which(n_pt$n>1) # nodes appeared more than equal twice: end points are neglected
  n_pt2=n_pt[tI,] # crossing points
  n_pt3=n_pt2 %>% mutate(ID=1:nrow(n_pt2)) # put tentative ID
  
  # convert to sf from nodes coordinates
  pt_cd2=pt_cd %>% left_join(n_pt3,by=c("X","Y")) # crossing point ID
  # head(pt)
  # head(pt_cd2)
  
  pt2=mutate(pt,n=pt_cd2$n,ID=pt_cd2$ID) # add to the original point data
  # head(pt2)
  pt3=pt2 %>% na.omit() %>% 
    left_join(lines3 %>% st_drop_geometry(),by="osm_id") # connecting points among different links
  
  
  # number of crossing points for each linestring
  # system.time({ # 0.25 
  #   line_n=pt3 %>% st_drop_geometry() %>% group_by(osm_id) %>% summarise(n=n())
  # })
  
  # assume all lines are independent, even if the lines are crossing, trains cannot transfer to the other line.
  # tI=which(line_n$n>2) # lines that has crossing points more than 2: i.e. cross with the other lines at middle of the line
  # pt3.newseg=pt3 %>% filter(osm_id%in%line_n$osm_id[tI]) # lines with more than 2 nodes
  # pt3.orgseg=pt3 %>% filter(!osm_id%in%line_n$osm_id[tI]) # lines with 2 nodes
  pt3.orgseg=pt3
  
  # create segmented lines with more than 2 nodes
  # n.newseg=pt3.newseg %>% st_drop_geometry() %>% select(osm_id) %>% group_by(osm_id) %>% summarise(n=n())
  # seg1=seg2=rep(NA,nrow(pt3.newseg))
  # system.time({ # 1.35 
  #   for(ii in 1:nrow(n.newseg)){ # ii=1
  #     tI=which(pt3.newseg$osm_id==n.newseg$osm_id[ii])
  #     seg1[tI[-length(tI)]]=1:(length(tI)-1)
  #     seg2[tI[2:length(tI)]]=1:(length(tI)-1)
  #   }
  # })
  # pt3.newseg2=rbind(pt3.newseg %>% mutate(osm_id=paste0(osm_id,"_",seg1)) %>% .[-which(is.na(seg1)),],
  #                   pt3.newseg %>% mutate(osm_id=paste0(osm_id,"_",seg2)) %>% .[-which(is.na(seg2)),]) %>% 
  #   arrange(osm_id)
  
  system.time({ # 51.26   
    link3 = rbind(pt3.orgseg,pt3.newseg2) %>% 
      group_by(osm_id) %>% 
      summarize(ID1=first(ID),ID2=last(ID),name=first(name),highway=first(highway),nlane=first(nlane),maxspeed =first(maxspeed),oneway=first(oneway)) %>%
      filter(st_geometry_type(.) == "MULTIPOINT") %>%
      st_cast("LINESTRING")
  })
  
  link3=pt3.orgseg %>% 
    group_by(osm_id) %>% 
    summarize(ID1=first(ID),ID2=last(ID),name=first(name),z_order=first(z_order),type=first(type)) %>%
    filter(st_geometry_type(.) == "MULTIPOINT") %>%
    st_cast("LINESTRING")
  
  plot(link3$geometry)
  plot(stations$geometry,add=T,col="red",pch=20)
  
  fukuoka.railway.simplified=link3
  save(fukuoka.railway.simplified,file="data/fukuoka.railway.simplified.xdr")
  st_write(fukuoka.railway.simplified,dsn="data/osm/fukuoka.railway.simplified.gpkg",delete_dsn=T)
  
  # load("data/fukuoka.railway2.xdr") # fukuoka.railway2
  # load("data/Fukuoka.stations2.xdr") # Fukuoka.stations2
  
  
  save(link3,file="data/osm/link3.Fukuoka.highway.xdr")
  
  # load("data/osm/link3.Fukuoka.highway.xdr")
  # plot(link3$geometry)
  st_write(link3,dsn="data/osm/link3.Fukuoka.highway.gpkg",delete_dsn=T)
  
}


if(F){
  aa=lines_parts %>% group_by(osm_id) %>% summarise(n=n())
  aa$n %>% unique()
  aa %>% group_by(n) %>% summarise(nn=n())
  
  # # まず geometry type を確認
  # unique(st_geometry_type(fukuoka.railway2.3857))
  # # LINESTRING / MULTILINESTRING のみを残す
  # is_line <- st_geometry_type(L_m) %in% c("LINESTRING", "MULTILINESTRING")
  # L_line  <- L_m[is_line, ]    # ← これを分割対象に
  L_line_ls <- st_cast(fukuoka.railway2.3857, "LINESTRING")
  
  # A と L（単一フィーチャ）から最近傍の接続線を作る
  seg_AB <- st_nearest_points(Fukuoka.stations2.3857, fukuoka.railway2.3857)  # LINESTRING（A→B の直線）
  
  len_AB=st_length(seg_AB) %>% drop_units()
  tI=which(len_AB<150)
  seg_AB.LT150=seg_AB[tI,]
  
  # plot(fukuoka.railway2.3857$geometry)
  # plot(seg_AB.LT150,add=T,col="red")
  
  # A→B の接続線から B を抽出（奇数がA, 偶数がB）
  pts_AB_LT150 <- st_cast(seg_AB.LT150, "POINT")
  B_list <- pts_AB_LT150[seq(2, length(seg_AB.LT150) * 2, by = 2)]
  # B と元ライン L_m の対応を data.frame に
  # key <- data.frame(line_id = tI)    # 元ラインの行インデックス
  # "元ラインの行番号" と "B 点" を持つ sf を作成
  key_df <- data.frame(line_id = tI)  # tI は seg_AB.LT150 に対応する L_m の行番号
  B_sf <- st_as_sf(key_df, geometry = B_list, crs = st_crs(fukuoka.railway2.3857))
  
  
  
  # library(lwgeom)  # 複数点分割の安定性向上に推奨（環境によって必要）
  # 参考：複数点を渡す場合、sf では y を st_combine したほうが安定との指摘あり。[3](https://github.com/r-spatial/lwgeom/issues/94)
  # L_m を LINESTRING のみに整えたものを使う（上の L_line_ls）
  # 元の行番号と一致するように、事前に行インデックス列を付けるのが安全
  L_line_ls$line_id <- as.integer(rownames(L_line_ls))  # 既存の rownames を ID に
  
  # 分割（ラインごと）
  split_list <- lapply(unique(B_sf$line_id), function(i) { # i=unique(B_sf$line_id)[1]
    Li <- L_line_ls[L_line_ls$line_id == i, ]  # 対象ライン（sf）
    Bi <- B_sf[B_sf$line_id == i, ]            # このラインに属する分割点群
    
    plot(Bi$geometry)
    plot(Li$geometry,add=T,col="blue")
    plot(Li$geometry,col="blue")
    plot(Bi$geometry,add=T)
    
    # 複数点で一括分割（blade を st_combine して渡す）
    gc  <- st_split(Li, st_combine(Bi))
    out <- st_collection_extract(gc, "LINESTRING")
    
    # ★ 元ラインの属性を保持（cbind で付け直す）
    attrs <- st_drop_geometry(Li)              # 元ラインの属性（data.frame）
    # 分割片すべてに、元の属性をコピー（必要に応じて一部のみ）
    out_attr <- cbind(attrs[rep(1, nrow(out)), , drop = FALSE], 
                      st_as_sf(out))
    out_attr
  })
  
  # すべて結合
  lines_parts <- do.call(rbind, split_list)
  
  # 確認
  lines_parts
  
  # この接続線を POINT にキャストし、2番目の点が B
  pts_AB_L=lapply(seg_AB.LT150,function(x)return(st_cast(x,"POINT") %>% {.[2]}))
  pts_AB_L=lapply(seg_AB.LT150,function(x)return(st_cast(x,"POINT")))
  
  seg_AB.LT150[1] %>% st_cast("POINT") %>% .[2,]
  
  warnings()
  pts_AB <- st_cast(seg_AB.LT150, "POINT")
  BB <- pts_AB[,2]  # B は L_m 上の最も近い点
  
}



#####


# rail network model: connect zone centroid
#####
rm(list=ls())
gc();gc();

library(sf)
library(dplyr)
library(purrr) # map2
library(igraph)
# library(units)
# library(lwgeom)

load("data/fukuoka.railway.simplified.xdr") #fukuoka.railway.simplified
load("data/stations_3857.xdr") # stations

mesh.Fukuoka.uea2.0=st_read("data/bnd_mesh_wL10_filterd/bnd_mesh_wL10_filterd.shp")
mesh.Fukuoka.uea2=st_transform(mesh.Fukuoka.uea2.0,st_crs(fukuoka.railway.simplified))

# centeroid of mesh
mesh.Fukuoka.uea2.cnt=st_centroid(mesh.Fukuoka.uea2)

# check isolation of network
# fukuoka.railway.simplified
# link10=rbind(rail_network,rail_network %>% rename(ID1=ID2,ID2=ID1)) %>% left_join(lkspd,by="type") %>% mutate(FFtime=length/speed*60/10^3,cap=10^5) #%>%  # fftime (minutes)
link10=fukuoka.railway.simplified
# link10=fukuoka.railway.simplified2
ig <- graph_from_data_frame(
  d = link10[,c("ID1", "ID2", "length")] %>% st_drop_geometry(),
  directed = TRUE,
  vertices = NULL
)
# is_connected(ig) # 連結判定
# components(ig)$no # 連結成分の数を知りたい場合
# components(ig)$csize # 成分ごとのサイズを確認
# どのノードがどの成分か知りたい
comp <- components(ig, mode = "weak")  # or "strong"
aa=data.frame(
  node = V(ig)$name,
  component = comp$membership
)
aa %>% group_by(component) %>% summarise(n=n())
# isolate nodes
isonodes=aa %>% filter(component!=1) %>% .$node
# remove isolate network nodes
fukuoka.railway.simplified=fukuoka.railway.simplified[-c(which(fukuoka.railway.simplified$ID1%in%isonodes),
                                                         which(fukuoka.railway.simplified$ID2%in%isonodes)),]

# node data of network
tmp.nodes=st_coordinates(fukuoka.railway.simplified) %>% 
  as.data.frame() %>%
  group_by(X,Y) %>% summarise(n=n()) %>% ungroup() %>% ungroup() # group by same coordinates
# # dim(tmp.nodes)

# station nodes
st.nodes=stations %>% st_coordinates() %>% 
  as.data.frame() %>% mutate(osm_id=stations$osm_id,name=stations$name)

tmp.nodes2=tmp.nodes %>% left_join(st.nodes,by=c("X","Y"))

st.nodes2=tmp.nodes2 %>% filter(!is.na(tmp.nodes2$osm_id))
st.nodes2.sf=st.nodes2 %>% 
  mutate(geometry = st_sfc(map2(X, Y, ~st_point(c(.x, .y))), crs = st_crs(stations))) %>%
  st_as_sf()

# rewrite network node ids
lk.nodes=tmp.nodes2 %>% filter(is.na(tmp.nodes2$osm_id))
lk.nodes$osm_id=paste0("NN_",formatC(seq(1:nrow(lk.nodes)),width=5,flag="0"))
lk.nodes.sf=lk.nodes %>% 
  mutate(geometry = st_sfc(map2(X, Y, ~st_point(c(.x, .y))), crs = st_crs(fukuoka.railway.simplified))) %>%
  st_as_sf()

nodes2=rbind(st.nodes2,lk.nodes)

# rewrite network node ids
tmp.nodes=st_coordinates(fukuoka.railway.simplified) %>% as.data.frame() %>% 
  group_by(L1) %>% 
  summarize(X1=first(X),Y1=first(Y),X2=last(X),Y2=last(Y)) %>% 
  left_join(nodes2 %>% dplyr::select(X,Y,osm_id),by=c("X1"="X","Y1"="Y")) %>% 
  left_join(nodes2 %>% dplyr::select(X,Y,osm_id),by=c("X2"="X","Y2"="Y"))

fukuoka.railway.simplified2=fukuoka.railway.simplified %>% 
  mutate(ID1=tmp.nodes$osm_id.x,ID2=tmp.nodes$osm_id.y)
# which(is.na(fukuoka.railway.simplified2$ID2))
# tmp.nodes %>% group_by(L1) %>% summarise(n=n()) %>% group_by(n) %>% summarise(nn=n())

system.time({ # 2.27  
  distm=st_distance(mesh.Fukuoka.uea2.cnt,st.nodes2.sf)
})
# dim(distm)
# which(is.na(distm))

tI=apply(distm,1,which.min)
aa1=st.nodes2.sf[tI,] %>% dplyr::select(nodeID=osm_id) %>% mutate(linkID=paste0("acl_",formatC(1:length(tI),width=4,flag="0"))) #%>% rename(ID=osm_id.y)
aa2=mesh.Fukuoka.uea2.cnt %>% mutate(MeshCode=paste0("mc_",KEY_CODE),linkID=paste0("acl_",formatC(1:length(tI),width=4,flag="0"))) %>% rename(nodeID=MeshCode) %>% dplyr::select(nodeID,linkID,geometry)
aa3=rbind(aa1,aa2) %>% arrange(linkID) %>% dplyr::select(linkID,nodeID,geometry)

system.time({ # 1.78  
  ac_link = aa3 %>% 
    group_by(linkID) %>% 
    summarize(osm_id=first(linkID), ID1=first(nodeID),ID2=last(nodeID),name=as.character(NA),z_order=0,type="access",geometry=st_cast(st_combine(geometry), "LINESTRING")) %>%
    mutate(length=st_length(geometry)) %>% dplyr::select(-linkID)
  # filter(st_geometry_type(.) == "MULTIPOINT") %>%
  # st_cast("LINESTRING")
  # st_as_sf()
})

rail_network=rbind(fukuoka.railway.simplified2,ac_link)

# plot(rail_network$geometry)
save(rail_network,file="data/osm/rail_network.xdr")
st_write(rail_network,dsn="data/osm/rail_network.gpkg",delete_dsn = T)

# nodes
rail_network.nodes=rbind(aa2 %>% dplyr::select(nodeID) %>% mutate(type="mesh"),
                         st.nodes2.sf %>% dplyr::select(nodeID=osm_id) %>% mutate(type="station"),
                         lk.nodes.sf %>% dplyr::select(nodeID=osm_id) %>% mutate(type="network"))
save(rail_network.nodes,file="data/osm/rail_network.nodes.xdr")
st_write(rail_network.nodes,dsn="data/osm/rail_network.nodes.gpkg",delete_dsn = T)


#####


# rail network assignment: estimate travel time
#####
rm(list=ls())
gc();gc();

library(cppRouting)
library(dplyr)
library(sf)
library(igraph)

# source("tntp.R")
# load("data/osm/link10.xdr") # link10
# load("data/osm/node10.xdr") # node10

load("data/osm/rail_network.xdr") # rail_network
load("data/osm/rail_network.nodes.xdr") # rail_network.nodes

# capacity, alpha, beta, free.flow.time

# capacity: pcu/hour
# https://www.nilim.go.jp/lab/bcg/siryou/tnn/tnn0317pdf/ks0317005.pdf
# cap.df=data.frame(hwy=unique(link10$highway) %>% sort(),
#                   cap=c(10^4,2500,1700,2500,1700,1500,1500,1250,1250,2500,1700))
# 1500pcu/lane/hour
# 日換算係数の考え方
# https://www.nilim.go.jp/lab/bcg/siryou/tnn/tnn0317pdf/ks0317006.pdf
# KK=0.1
# capacity pcu/day
# cap.df$cap/KK

# http://library.jsce.or.jp/jsce/open/00037/2002/695-0091.pdf
# alpha=0.15;beta=4 #BPR
# alpha=0.96;beta=1.2 # mizokami/matsui, 1989

# https://bin.t.u-tokyo.ac.jp/kaken/pdf/traffic_assign%20tutrial%20for%20C.pdf
# alpha=0.48;beta=2.82

#https://www.jstage.jst.go.jp/article/thagis/17/2/17_203/_pdf
# alpha=0.48;beta=2.89 # yamada/matsui (1998)

# rail_network$type %>% unique()
# rail_network$length

#link speed km/h
lkspd=data.frame(type=c("rail","subway","access"),speed=c(60,40,5))
# in case of using bus when access length is long, the speed of access should be faster.

# link10=rail_network %>% left_join(lkspd,by="type") %>% mutate(FFtime=length/speed*60/10^3,cap=10^5) #%>%  # fftime (minutes)
link10=rbind(rail_network,rail_network %>% rename(ID1=ID2,ID2=ID1)) %>% left_join(lkspd,by="type") %>% mutate(FFtime=length/speed*60/10^3,cap=10^5) #%>%  # fftime (minutes)
# merge(cap.df, by.x = "highway", by.y = "hwy", all.x = TRUE)
dim(link10)
# which(is.na(link10$FFtime))

node10=data.frame(rail_network.nodes$nodeID,st_coordinates(rail_network.nodes))
names(node10)=c("Node","X","Y")

# E:/WorkDir01/prog/R/2024/2024_TAP/cppRouting01.R




work_zone<-c(50303302,50303206,50303395,50303208,50302364,50302356,50303344,50303393,50302390,50302290,50302347,50302329,50305475,50305318,50304377,50302422,50301491,50302176)
tI=which(node10$Node%in%paste0("mc_",work_zone))
centers=node10$Node[tI]

sgr <- makegraph(df = link10[,c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(), 
                 directed = T,
                 capacity = 10^5,
                 # capacity = link10$cap/KK,
                 # alpha = alpha,
                 # beta = beta,
                 alpha = 0.1,
                 beta = 1,
                 coords = node10)
save(sgr,file="sgr.rail.xdr")
# sgr$data
# ?makegraph
#　孤立判定
{
  library(igraph)
  # makegraph() に使った edges をそのまま使う
  ig <- graph_from_data_frame(
    d = link10[,c("ID1", "ID2", "FFtime")] %>% st_drop_geometry(),
    directed = TRUE,
    vertices = NULL
  )
  
  is_connected(ig) # 連結判定
  components(ig)$no # 連結成分の数を知りたい場合
  components(ig)$csize # 成分ごとのサイズを確認
  
  # どのノードがどの成分か知りたい
  comp <- components(ig, mode = "weak")  # or "strong"
  data.frame(
    node = V(ig)$name,
    component = comp$membership
  )
  
  
  # 完全孤立ノード
  isolated_nodes <- V(ig)$name[degree(ig) == 0]
  isolated_nodes
  # 連結成分による孤立判定
  wc <- components(ig, mode = "weak")
  isolated_weak <- V(ig)$name[wc$csize[wc$membership] == 1]
  isolated_weak
  
  # Strongly connected（強連結孤立）
  sc <- components(ig, mode = "strong")
  isolated_strong <- V(ig)$name[sc$csize[sc$membership] == 1]
  isolated_strong
  
  # 起点から到達不能ノード＝ルーティング不能
  tI=grep("mc_",node10$Node)
  zones=node10$Node[tI]
  dist<-get_distance_matrix(sgr,
                            from=zones,
                            to=centers,
                            algorithm = "mch") # because of the rectangular shape of the matrix
  unreachable <- names(dist)[!is.finite(dist)]
  unreachable
  which(is.na(dist))
  dim(dist)
  zones[397]
  dist[397,]
  which(link10$ID1==zones[397]) %>% link10[.,]
  
}

# 混雑による速度低下は考慮しないので，igraphで十分
# estimate OD travel time under user zero traffic 
tI=grep("mc_",node10$Node)
zones=node10$Node[tI]
system.time({ # 0.13 
  dists0<-get_distance_matrix(sgr,
                              from=zones,
                              to=centers,
                              algorithm = "mch") # because of the rectangular shape of the matrix
})
# apply(dists0,2,min,na.rm=T)
# which(is.na(dists0))

save(dists0,file="data/railway_dist.xdr")

load("data/railway_dist.xdr") # dists0, minutes per one way
rownames(dists0)
colnames(dists0)

#####


