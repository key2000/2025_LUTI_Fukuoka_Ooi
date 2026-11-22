# 較正11：初期集団＝乱数49個＋基準の点1個（x0、k_i一律0.1）。前の試行の結果は使わない。300世代。GAの後は回さない
# 実行（リポジトリルート）：Rscript project_log/calibration/11_ga-base2021-random-init/run_ga_11.R <乱数の種>
.ctl <- new.env()
.ctl$seed <- as.integer(commandArgs(trailingOnly = TRUE)[1])
.ctl$sugg <- matrix(c(0.3, 0.2, 0.6, 0.1, 2.0, rep(0.1, 847)), nrow = 1)
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(luti_model_setup = "base2021_E10000_time",
        luti_calib_trial = file.path("11_ga-base2021-random-init", paste0("seed", .ctl$seed)),
        luti_seed = .ctl$seed, luti_run_ga = TRUE, luti_ga_maxiter = 300,
        luti_ga_suggestions = .ctl$sugg, luti_t0 = Sys.time())
.ctl$src <- "scripts/Fukuoka_LUTI_model_260617.R"
.ctl$ex  <- parse(.ctl$src, keep.source = TRUE)
.ctl$end <- grep("^if \\(run_ga_852dim\\) \\{", readLines(.ctl$src))[1]
for (.i_ctl in seq_along(.ctl$ex)) {
  eval(.ctl$ex[[.i_ctl]], envir = globalenv())
  if (attr(.ctl$ex, "srcref")[[.i_ctl]][1] == .ctl$end) break  # GAのブロックまでで終了
}
