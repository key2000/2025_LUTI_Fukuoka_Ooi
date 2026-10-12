# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要（2025_LUTI_Fukuoka_Ooi）

福岡都市圏を対象にしたLUTI（土地利用交通統合）モデル。R言語（Positron/RStudio想定）で構築。
コード・コメントは基本的に日本語。

ビルド・lint・テストの仕組みはない（`.Rproj` があるのみ）。スクリプトはPositron/RStudioで
リポジトリルートをカレントディレクトリにして、上から順番に通しで実行する運用（`Rscript`でも可）。
セクション単位でのつまみ食い実行は前提にしない。

## 全体像

福岡都市圏の1つの都市モデル（土地利用モデル＋交通モデル）を完成させることが目的で、
パラメータ較正・モデル構造の改良・シナリオ分析など、複数のテーマを並行して進めている。
試行・プロット・方針メモなど、まだ論文に採用すると決めていないものは全て `project_log/` にテーマフォルダ単位で記録し、
ユーザーが論文に載せると決めたものだけを `paper/` に置く運用にしている（次々節「記録先の振り分け」、
詳細は各フォルダの README/メモを参照）。

### モデルの流れ（メインスクリプトを読む前の見取り図）

1. **データ読込・前処理** — 1kmメッシュ（`KEY_CODE`）を居住ゾーン（`nz_res`）・従業ゾーン（`nz_work`）とし、
   所要時間行列 `dists0`、給料 `omega_j`、居住可能面積 `G0_i` などを `data/*.xdr` から `load()` する。
2. **土地利用モデル** — `caluculate_model_state(v_j_vec)` が与えられた効用水準から家賃・床面積・立地確率
   （Cobb-Douglas型、`alpha_a` `gamma_0` `gamma_1` `theta_H` `theta_L` `k_i` がパラメータ）を返す。
   `gapf_vj` を `nleqslv` で解いて均衡効用 `v_equilibrium` を求める。
3. **交通** — 手段選択（自転車/自動車/鉄道のロジット、係数 `para`）と道路網への配分（`assign_traffic`、
   グラフ `sgr`）。配分結果の混雑で `dists0.road` が更新され、`c_ij` → `disposable_income_ij` を通じて
   土地利用側に戻る。この土地利用⇄交通の反復（MSAで平滑化）がLUTIループ。
4. **較正結果の読込** — パラメータ（5変数）と `k_i` は `active_calibration` の1行で切り替える（次節）。
   そのパラメータでLUTIループを再実行して現状（scn0）を得る。
5. **シナリオ分析** — `run_scenario()` にシナリオ別の雇用分布・賃金行列を渡して再実行し、現状との差分を
   マップ化する（B: 雇用一律再配置、C: CBD距離加重再配置）。

## scripts/ — Rスクリプト

- **`scripts/Fukuoka_LUTI_model_260617.R`** — 現在のメインスクリプト（約2700行）。データ読込→道路網→
  鉄道網→土地利用モデル→較正結果読込→LUTIループ→実データ比較→シナリオ分析・比較マップまで含む。
  `####` 見出しでフェーズが区切られているので、目的の処理を探すときはまず
  `grep -n "####" scripts/Fukuoka_LUTI_model_260617.R` で目次を出すとよい。
  過去の試行錯誤コードは `if(FALSE){...}` / `if(F){...}` で無効化されたまま残されている箇所がある
  （削除はされていないが実行はされない。`#LUTI ####` 直後の最初のループも `if(F)` 内）。
  実行時のカレントディレクトリはリポジトリルート想定（`project_log/...`・`data/...` へのパスはすべてルート相対）。
- **`scripts/calibration/`** — 較正まわり（GA・k_i解析的推定）をメインスクリプトの `if(FALSE)` から切り出したもの。
  `calibrate_*()` 関数として定義されており、メインスクリプトから `source()` はされない
  （較正を再実行したいときだけ手動で読み込んで呼ぶ）。**どのファイルがどの試行に対応するか・
  現在ライブか・実行可能かは `scripts/calibration/README.md` の対応表を必ず先に読むこと。**
- **`scripts/build_rent_df_from_estat.R`** — e-Stat（住宅・土地統計調査）の家賃CSVを `data/rent.df.xdr` に変換する。
  `07_ga-housing-survey-target` の前提データ。手順はスクリプト冒頭コメント（Step 0〜2）参照。
- **`scripts/Fukuoka_LUmodel_251207.R`** — メインスクリプトの旧版。参照用に残っているが現行ではない。
- **`scripts/Fukuoka_OSM_03_copied.R`** — OSMから道路ネットワークを抽出する処理。
- **`scripts/Fukuoka_modeChoide_260309_copied.R`** — 交通手段選択モデルの推定。

`project_log/calibration/` からのpptx自動生成スクリプト（`generate_slides.R` / `generate_slides_05_detail.R`）は
廃止済み。スライド化はもう行わない方針。

`_copied` 接尾辞や日付入りファイル名は過去の手動バージョン管理の名残で、統一された命名規則ではない。
新しいファイルを作る際は接尾辞に頼らず、役割が分かる名前にする。

### 較正結果の切り替え（`active_calibration`）

メインスクリプトの `active_calibration <- "01_baseline-5param-ga"`（約L.750）を書き換えるだけで、
モデルが使うパラメータ（`alpha_a`, `gamma_0`, `gamma_1`, `theta_H`, `theta_L`）と `k_i` を切り替えられる。
`load_calibration_result()` は `project_log/calibration/<試行>/ga_result.RData` の `calib_result`
（`list(params = 5変数のnamed vector, k_i = 長さnz_resのベクトル, fitness, ...)`）を読むだけでGAは再実行しない。
新しい較正パターンを追加するときも、この `calib_result` 形式で保存すればローダーは変更不要。
キャッシュが無い試行（`07` 等）を選ぶとエラーで止まる。

### 地図プロットの規約

地図（差分マップ・`make_map()` 系の関数・`ggsave()` でのPNG保存）を新規作成・修正するときは、
スキル `map-plot-conventions`（`.claude/skills/map-plot-conventions/`）を参照する。
差分マップのスケールは固定値ではなく `sym_limits()` で決め、`theme_void()` の地図は背景を白で明示する。

## project_log/ — 較正試行・改良テーマの記録（source of truth）

`project_log/<テーマ>/` がテーマフォルダ。Git管理下（`*.RData` と `.DS_Store` のみ除外）。
記録の形式・手順はテーマごとにフォルダ内のREADME/メモに書いてあるので、記録を追加する前に必ず読む。

- **`calibration/`** — GA較正・k_i逆算などパラメータ較正の試行記録。`NN_短い名前/` が1試行。
  追加手順・`meta.md` テンプレート・`ga_result.RData`（Git管理外のローカルキャッシュ）の扱いは
  `project_log/calibration/README.md`。
- **`scenario-analysis/`** — シナリオ分析（交通量・速度・鉄道・土地利用のマップ）。NNフォルダは使わず、
  `plots/` 内のサブフォルダに固定ファイル名で上書きする。詳細は `plots/note.md`。
- **`paper-direction/`** — 論文としてまとめる方向性、指導教員とのやり取り、次のTODO（`notes.md`、日付順に追記）、
  第3章・第4章の素材の下書き（`ch3_ch4_materials.md`、未確定・要確認を含む）。
  モデル改良の動機（例: Stone-Geary型効用関数の検討）もここ。作業の前提となる方針を確認したいときに読む。
  詳細は `paper-direction/README.md`。

上記のどれにも当てはまらない改良（フォルダ構成の見直し、スクリプト分割など）を計画する場合は、
`project_log/` の下に新しいテーマフォルダを作り、同じ「テーマフォルダ > NN連番」の構造で記録する。

## 記録先の振り分け（`project_log/` と `paper/`）

- **`project_log/`** — まだ論文に採用すると決めていない全て。試行錯誤、途中経過、不採用の案、未確定の方針、TODO、
  先生とのやり取り、素材の下書き。コードや設定を変更したら、その内容・結果もここに記録する。
- **`paper/`** — ユーザーが「論文に載せる」と決めた内容だけ（式・パラメータ・データ・図・数値）。
  形式・ファイル構成は `paper/README.md`。

守ること：
- **`paper/` への書き込みは、ユーザーが論文への採用を明言したときだけ。** 採用の判定はユーザー本人で、
  Claudeが「採用してよさそう」と判断して書くことはしない。「先生がOKと言った」「結果が良かった」だけでも
  採用にはならない。迷ったら `project_log/` に書き、採用するか尋ねる。
- 採用が決まったら、スキル `promote-to-paper`（`.claude/skills/promote-to-paper/`）で昇格する。
  昇格すると、元の試行の `meta.md` に `paper: adopted`（と `paper_ref:`）が記録される。未採用は `paper: none`。
  `verdict:`（試行の中での採用／不採用／進行中）とは別の項目で、`verdict: adopted` でも `paper: none` のことがある。
- `paper/**` へのファイル書き込み（Edit/Write ツール）は、`.claude/settings.json` の `permissions.ask`
  （`Edit(/paper/**)`）で毎回確認が出る（承認が採用の確認を兼ねる）。この設定は Bash 経由の書き換え
  （`sed -i`、リダイレクトなど）は止めないので、Bash で `paper/` を書き換えることも同じルールに従い、
  ユーザーの採用明言なしには行わない。

## data/ — 完全にGit管理外（.gitignore で `data/` を丸ごと除外）

約7.3GB。cloneしただけでは存在しない。主な構成：

- `data/raw/` — 生データ。政府統計・GISデータなど。フォルダ名が日本語・英語・半角スペース・
  括弧混在（例: `UEA2015 csv`, `小地域(基本単位区)`）なので、シェルやRからパス操作する際は
  必ずクォートする。
- `data/processed/` — 中間生成物。
- `data/osm/` — OSM由来の道路ネットワークデータ（`link3`〜`link10`, `node7`〜`node10` のように
  処理段階を通し番号で表しており、番号の意味はスクリプト側のコメントに依存する）。
- QGISの `*.sr.lock` / `*.rd.lock` ファイルが散在することがあるが、単なる異常終了時の残骸で
  実データではない。

## Git運用上の注意

- `.gitignore` は `data/`、`*.RData`、`.DS_Store` 等を除外している。新しく生成物や一時ファイルを
  置く場合は、既存の除外パターンで拾えるか確認してから追加する。
- ルートに一時ファイル（`Rplots.pdf` など、Rのデフォルトグラフィックデバイス出力）や
  PowerPointのロックファイル（`~$*.pptx`）が残っていることがあるが、これらは成果物ではない。
