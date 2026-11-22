# 較正09：08と同じ設定（基準年2021・中心地32・所要時間）で、ペナルティを直し、初期集団に既存の解を入れてGAを回し直す
# 実行（リポジトリルート）：Rscript project_log/calibration/09_ga-base2021-penalty-fix/run_ga_09.R
ld <- function(d) { e <- new.env(); load(file.path("project_log/calibration", d, "ga_result.RData"), envir = e); e$calib_result }
c07 <- ld("07_ga-housing-survey-target")
c08 <- ld("08_ga-base2021-E10000-time")
sugg <- rbind(
  sol07       = c(c07$params, c07$k_i),
  par08_ki07  = c(c08$params, c07$k_i),
  x0_ki01     = c(0.3, 0.2, 0.6, 0.1, 2.0, rep(0.1, length(c07$k_i)))
)
rm(ld, c07, c08)
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(luti_model_setup = "base2021_E10000_time", luti_calib_trial = "09_ga-base2021-penalty-fix",
        luti_run_ga = TRUE, luti_ga_maxiter = 100, luti_ga_suggestions = unname(sugg), luti_t0 = Sys.time())
rm(sugg)
source("scripts/Fukuoka_LUTI_model_260617.R")
