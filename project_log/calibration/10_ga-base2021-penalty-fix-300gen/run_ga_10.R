# 較正10：09の最終世代（50個）を初期集団にして、同じ設定でGAを300世代続ける。GAの後のscn0・シナリオは回さない
# 実行（リポジトリルート）：Rscript project_log/calibration/10_ga-base2021-penalty-fix-300gen/run_ga_10.R
.ctl <- new.env()
local({
  e <- new.env(); load("project_log/calibration/09_ga-base2021-penalty-fix/ga_result.RData", envir = e)
  .ctl$sugg <- e$calib_result$ga_result@population
})
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(luti_model_setup = "base2021_E10000_time", luti_calib_trial = "10_ga-base2021-penalty-fix-300gen",
        luti_run_ga = TRUE, luti_ga_maxiter = 300, luti_ga_suggestions = unname(.ctl$sugg), luti_t0 = Sys.time())
.ctl$src <- "scripts/Fukuoka_LUTI_model_260617.R"
.ctl$ex  <- parse(.ctl$src, keep.source = TRUE)
.ctl$end <- grep("^if \\(run_ga_852dim\\) \\{", readLines(.ctl$src))[1]
for (.i_ctl in seq_along(.ctl$ex)) {
  eval(.ctl$ex[[.i_ctl]], envir = globalenv())
  if (attr(.ctl$ex, "srcref")[[.i_ctl]][1] == .ctl$end) break  # GAのブロックまでで終了
}
