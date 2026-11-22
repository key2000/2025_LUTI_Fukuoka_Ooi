# 較正08（基準年2021・中心地32・所要時間で集約）のGAを回し、そのままscn0とシナリオB/Cまで進める
# 実行（リポジトリルート）：Rscript project_log/base-year-revision/09_base2021-inputs/run_ga_08.R
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(luti_model_setup = "base2021_E10000_time", luti_run_ga = TRUE, luti_t0 = Sys.time())
source("scripts/Fukuoka_LUTI_model_260617.R")
