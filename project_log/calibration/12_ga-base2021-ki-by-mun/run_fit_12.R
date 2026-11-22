# 較正12（種1）の解でLUTIを収束まで回し、モデル適合度検証（R²）まで実行して止める。GAとシナリオは回さない
# 実行（リポジトリルート）：Rscript project_log/calibration/12_ga-base2021-ki-by-mun/run_fit_12.R
.ctl <- new.env()
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(luti_model_setup = "base2021_E10000_time",
        luti_calib_trial = "12_ga-base2021-ki-by-mun/seed1", luti_t0 = Sys.time())
.ctl$src <- "scripts/Fukuoka_LUTI_model_260617.R"
.ctl$ex  <- parse(.ctl$src, keep.source = TRUE)
.ctl$end <- grep("^print\\(fit_summary\\)", readLines(.ctl$src))[1]
for (.i_ctl in seq_along(.ctl$ex)) {
  eval(.ctl$ex[[.i_ctl]], envir = globalenv())
  if (attr(.ctl$ex, "srcref")[[.i_ctl]][1] == .ctl$end) break  # 適合度の表を出したら終了
}
write.csv(fit_summary, "project_log/calibration/12_ga-base2021-ki-by-mun/seed1/fit_summary.csv", row.names = FALSE)
