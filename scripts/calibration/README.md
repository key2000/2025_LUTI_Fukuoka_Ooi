# scripts/calibration/

`project_log/calibration/NN_.../` の各較正試行に対応するコードを、
`scripts/Fukuoka_LUTI_model_260617.R` 本体の `if(FALSE)` ブロックから分割したもの。
較正まわりのどのパターンが「生きている」かを、数千行の `if(FALSE)` を読まずに
短いパイプラインとして追えるようにするための場所。

## 対応表

| project_log/calibration/ の試行 | このディレクトリのファイル | 状態 |
|---|---|---|
| `01_baseline-5param-ga` | `ga_baseline_5var.R` | **現在ライブ**（本体スクリプトの `active_calibration` の既定値） |
| `02_ga-800dim` | （なし） | この試行のga()/fitness関数のコードは今回の分割より前に既に本体スクリプトから失われている。`project_log/calibration/02_ga-800dim/meta.md` と `ga_result.RData` のみが記録として残る |
| `03_ki-backward-calc` → `04_ki-outlier-crop-kriging` → `05_ki-relaxation` | `ki_estimation.R` | 1つながりの試行（代替案ではなく積み上げ）。`calibrate_ki_relaxation()`で`active_calibration`から選べる（5変数は01を暫定借用、下記参照） |
| `07_ga-housing-survey-target` | （なし） | 本体スクリプトの「GA較正（住宅・土地統計調査ベース…）####」セクションに戻した（対話実行で1つずつ確認したいため関数化をやめた）。データは揃い単発テストまで実施済み、GA本番は未実行（`run_ga_852dim <- FALSE`） |

要約：
- `01` → `ga_baseline_5var.R`
- `07` → このディレクトリには無い。本体スクリプトの「GA較正（住宅・土地統計調査ベース…）####」セクション
- `03` → `04` → `05` → `ki_estimation.R`の`calibrate_ki_relaxation()`（k_iはこの系譜の推定値、
  5変数は01を暫定借用）
- `02` → 抽出できるコードが残っていない（今回の分割より前に本体スクリプトから失われた。`meta.md` と `ga_result.RData` キャッシュのみ参照可能）

## 方針

このディレクトリは較正まわりだけを対象にしている。機能ごとの切り出しは、
実際に複数パターンが並行して試されている場合にのみ行う方針で、
現状この条件を満たすのは較正だけなので `scripts/calibration/` のみが存在する。

## 本体スクリプトでの切り替え方

`scripts/Fukuoka_LUTI_model_260617.R` の`#GA####`跡地（旧L.735付近）にある
`active_calibration <- "01_baseline-5param-ga"` の1行を書き換えるだけで、
本体パイプラインが使う較正結果（`alpha_a`等5変数 + `k_i`）を切り替えられる。

- 実体は`load_calibration_result()`が`project_log/calibration/<試行>/ga_result.RData`内の
  `calib_result`を読み込むだけで、GAの再実行はしない（重いので）。
- **各`calibrate_*()`は必ず`calib_result <- list(params = <5変数named vector>, k_i = <長さnz_res
  のベクトル>, fitness = ..., ga_result = ...)`という同じ形式で保存する**（k_iを固定値にする
  試行でも`rep(0.1, nz_res)`のような完全なベクトルとして持たせる）。この形式さえ守れば、
  本体スクリプト側の`load_calibration_result()`はパターンごとの分岐を一切持たずに済む。
  新しい較正パターンを追加した時も、このローダーは変更不要。
- 選べるのは3つ:
  - `"01_baseline-5param-ga"` — `ga_result.RData`あり、すぐ使える。
  - `"05_ki-relaxation"` — `ki_estimation.R`の`calibrate_ki_relaxation()`を1度実行して
    キャッシュを作る必要がある（未実行の場合、選ぶとエラーで止まる）。k_iは03-05の解析的
    推定値、5変数は**01を暫定的に借用**しているだけで同時較正した結果ではない点に注意
    （`calib_result$fitness`はNA、`calib_result$note`にその旨を記載）。
  - `"07_ga-housing-survey-target"` — 本体スクリプトのGAセクションで`run_ga_852dim <- TRUE`にして
    実行するとキャッシュが作られる。`ga_result.RData`がまだ無い＝選ぶとエラーで止まる。
- `01_baseline-5param-ga/ga_result.RData`は元々`best_params`/`ga_result_calib`という
  旧形式で保存されていた実際のGA結果を、この形式に合わせて1回だけ変換し直したもの
  （GAは再実行していない。値は変わっていない）。
- `estimate_ki_analytical()`（k_iだけを返す下請け関数）と`calibrate_ki_relaxation()`
  （01の5変数と組み合わせて`calib_result`として保存する、`active_calibration`から選べる方）
  は別関数。k_iの計算だけ使いたい場合は前者を直接呼んでもよい。
