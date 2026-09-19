# 住宅・土地統計調査（e-Stat）の家賃データを、先生がくれたarea.df.xdr/hhn.df.xdrと
# 同じ形式（mun列 + 年ごとの列、市区町村コード×調査年の横持ち）に変換してdata/rent.df.xdrに保存する。
#
# 対象表の例：123-3-2「住宅の建て方、世帯人員別住宅の１か月当たり家賃－全国、都道府県、市区町村」
# https://www.e-stat.go.jp/index.php/dbview?sid=0004021513
#
# 使い方：
#   1. e-Statから年ごとにCSV（DB形式・縦持ち）をダウンロードし、data/raw/住宅土地統計調査_家賃/ 以下に置く
#      （ファイル名は自由。下のrent_csv_filesで年と紐付ける。取得した表番号・日付は同フォルダのREADME.mdに記録）
#   2. まずStep 0（列名の確認）だけ実行し、実際の列名を確認する
#   3. 実際の列名に合わせてStep 1（抽出・整形）内のdplyr::filterの列名を修正する
#      （年によって列構成が違う可能性がある）
#   4. Step 1〜2を通しで実行し、data/rent.df.xdrが生成されることを確認する
#
# 依存：dplyr, tidyr, purrr（readrは使わない。この環境のRに入っていないためベースRのread.csvで読む）

library(dplyr)
library(tidyr)

yyv1 <- c(1993, 2003, 2008, 2013, 2018, 2023)

# 年 → CSVファイルパスの対応（表がない年・未取得の年はファイルが無いだけでよい＝その年はrent.dfでNAになる）。
# 2023は e-Stat のダウンロード名のまま（FEH_...csv）。表番号・取得日は
# data/raw/住宅土地統計調査_家賃/README.md に記録している。
rent_csv_files <- list(
  `1993` = "data/raw/住宅土地統計調査_家賃/rent_1993.csv",
  `2003` = "data/raw/住宅土地統計調査_家賃/rent_2003.csv",
  `2008` = "data/raw/住宅土地統計調査_家賃/rent_2008.csv",
  `2013` = "data/raw/住宅土地統計調査_家賃/rent_2013.csv",
  `2018` = "data/raw/住宅土地統計調査_家賃/rent_2018.csv",
  `2023` = "data/raw/住宅土地統計調査_家賃/FEH_00200522_260919172902.csv"
)


#### Step 0：列名を確認 ####
# e-Stat DB形式CSV（縦持ち、1行1セル）。2023年分はShift-JIS（CP932）だったので、
# read.csv()にはfileEncoding="CP932"を指定する（UTF-8扱いだと文字化けする）。
# 列：tab_code, 表章項目, cat01_code, 住宅の建て方, cat02_code, 世帯人員,
#     cat03_code, 住宅の家賃の平均, area_code, 全国、都道府県、市区町村,
#     time_code, 時間軸, unit, value, annotation
# 「住宅の家賃の平均」という軸が別にあり、1=家賃０円を含む／2=家賃０円を含まない の2値。
sample_path <- rent_csv_files[["2023"]]
if (file.exists(sample_path)) {
  sample_csv <- read.csv(sample_path, fileEncoding = "CP932", colClasses = "character")
  print(names(sample_csv))
  print(head(sample_csv, 20))
} else {
  cat("先に", sample_path, "をダウンロードして配置してください\n")
}


#### Step 1：抽出・整形 ####
# 1年分のCSVから「市区町村コード」「2人世帯の家賃」を取り出す関数。
# 家賃は「家賃０円を含まない」（cat03_code == "2"）を使う。０円は給与住宅（社宅など）で、
# モデルの市場家賃r_bar_iと比べる対象ではないため。
# ★2018年以前のCSVは列構成が違う可能性がある（要Step 0での確認）。
extract_rent_one_year <- function(path) {
  if (!file.exists(path)) return(NULL)

  df <- read.csv(path, fileEncoding = "CP932", colClasses = "character")

  df %>%
    dplyr::filter(
      世帯人員 == "２人",                  # 全角。area.df/hhn.dfは2人世帯換算なので揃える
      住宅の建て方 == "総数",              # area.df/hhn.dfが「総数」相当（推定）なので揃える
      cat03_code == "2"                    # 家賃０円を含まない
    ) %>%
    dplyr::transmute(
      mun  = area_code,                    # 既に5桁の文字列（area.df/hhn.dfと同じ形式）
      rent = suppressWarnings(as.numeric(value))  # "-"（秘匿・該当なし）はNAになる
    )
}

# 全年分をまとめて横持ちに変換
rent_long <- purrr::imap_dfr(rent_csv_files, function(path, year) {
  one_year <- extract_rent_one_year(path)
  if (is.null(one_year)) return(NULL)
  one_year %>% mutate(year = as.integer(year))
})

rent.df <- rent_long %>%
  tidyr::pivot_wider(names_from = year, values_from = rent, names_prefix = "rent_") %>%
  dplyr::arrange(mun)

# area.df/hhn.dfと同じ列順（mun, rent_1993, ..., rent_2023）に揃える
rent_year_cols <- paste0("rent_", yyv1)
missing_cols <- setdiff(rent_year_cols, names(rent.df))
for (col in missing_cols) rent.df[[col]] <- NA_real_  # データがない年はNA列を追加
rent.df <- rent.df %>% dplyr::select(mun, dplyr::all_of(rent_year_cols))

print(head(rent.df))
cat("行数:", nrow(rent.df), "\n")


#### Step 2：保存 ####
save(rent.df, file = "data/rent.df.xdr")
cat("saved data/rent.df.xdr\n")

# 確認：area.df.xdrと同じ使い方ができるか
load("data/rent.df.xdr")
tI_rent <- grep("^40", rent.df$mun)
print(rent.df[tI_rent, ])
