# scenario-analysis/

シナリオ別（現状 / シナリオB: 雇用一律再配置 / シナリオC: CBD距離加重再配置）の比較マップ。

## フォルダ構成（較正パターン別）

どの較正結果で回したかを区別するため、出力は `active_calibration`（メインスクリプトで選ぶ較正パターン名、
`project_log/calibration/` のフォルダ名と同じ）ごとのフォルダに分ける。

```
scenario-analysis/
├── README.md                     ← このファイル
├── plots/study_area_map.png      ← 論文用の分析対象範囲図（較正に依存しないため共通）
└── runs/
    ├── <active_calibration>/     ← 例: 07_ga-housing-survey-target/
    │   ├── run_info.md           ← 実行記録（自動生成）
    │   ├── summary_indicators.md ← 総合指標の表（便益・CO2・地代収入・宅地面積など、自動生成）
    │   └── plots/{road,landuse,rail}/*.png
    └── legacy_unknown-calibration/  ← 2026-10-13 以前の出力（較正状態不明）
```

- `scripts/Fukuoka_LUTI_model_260617.R` の `#シナリオ結果の保存先（較正パターン別）####` セクションで
  `scn_plot_dir <- runs/<active_calibration>/plots` を決め、以降の `ggsave()` はすべてここに保存する。
  `active_calibration` を切り替えて再実行すれば、別フォルダに出力される（他の較正の結果は上書きしない）。
- 同じ `active_calibration` で再実行した場合は、そのフォルダ内を固定ファイル名で上書きする。
  過去版はGit履歴（`git log -- project_log/scenario-analysis/runs/<active_calibration>/`）を辿る。
- `run_info.md` は実行のたびに自動で上書きされる。実行日時・`active_calibration`・git commit
  （`scripts/` に未コミット変更があればその旨）・5パラメータ・k_iの要約・各シナリオの収束状況を記録する。
  `summary_indicators.md` も同様に実行ごとに上書きされる（`#総合指標の表（シナリオ別）####` セクション）。
  結果の考察などは手で書き足さず、このREADMEか `paper-direction/notes.md` に書く（自動上書きで消えるため）。

以下の各節のファイル名は `runs/<active_calibration>/plots/` からの相対パス。

## 交通量・速度マップ（`road/`）

- `road/scenario_flow_speed_maps.png` — 3シナリオ（現状/B/C）を横並びにした比較図（太さ=交通量、色=速度）
- `road/scn0_flow_speed_map.png` / `scn1_flow_speed_map.png` / `scn2_flow_speed_map.png` — シナリオ別単体図
- `road/scenario_flow_diff_maps.png` — シナリオB・Cの現状からの交通量差分（太さ=変化量の絶対値、色=増減）を横並びにした比較図
- `road/scenario_speed_diff_maps.png` — シナリオB・Cの現状からの速度差分（太さ=変化量の絶対値、色=増減）を横並びにした比較図
- `road/scn1_vs_scn0_flow_diff_map.png` / `scn2_vs_scn0_flow_diff_map.png` — シナリオ別 交通量差分の単体図
- `road/scn1_vs_scn0_speed_diff_map.png` / `scn2_vs_scn0_speed_diff_map.png` — シナリオ別 速度差分の単体図
- `road/*_major.png` / `road/*_major_mesh.png` — 上記の「主要道路のみ」版、および1kmメッシュ背景付き版

`scripts/Fukuoka_LUTI_model_260617.R` の `#シナリオ別 交通量・速度マップ####` および
`#シナリオ別 交通量・速度 差分マップ（シナリオB/C − 現状）####` セクションで生成される
（`runs/<active_calibration>/plots/road/` に `ggsave()` で保存）。差分マップは色が発散配色（青=減少、赤=増加）、
太さが変化量の絶対値（大きく変化したリンクほど太い）。

## 土地利用系の差分マップ（`landuse/`）

`#シナリオ比較####` セクションで生成される、ゾーン(KEY_CODE)単位のシナリオ間差分マップ。
`landuse/scenario_{household,rent,floor_area,residential_land_ratio,building_height}_diff_maps.png`
（横並び比較図）と、末尾`_diverging`が付く発散配色版がある。スケールは`sym_limits()`方針
（CLAUDE.md参照）で両シナリオ共通の対称スケールに揃えている。

## 交通量の単位（pcu/日）について

pcu = Passenger Car Unit（乗用車換算台数）。車種ごとの道路容量への影響度を乗用車換算した単位で、
道路交通工学で容量を表す標準的な単位。`link10$cap`（道路種別ごとの容量、`scripts/Fukuoka_LUTI_model_260617.R`
のcap.df）はpcu/hour表記（国総研資料に基づく）だが、`capacity = link10$cap/KK`（`KK=0.1`、いわゆるK値＝
ピーク時/日交通量の比率）により日単位相当に換算して交通配分している。需要側（`l_i_j × P.car`）も
「1世帯1日1往復通勤」想定の日単位の量のため、`flow`（`road/` の交通量マップが示す値）は
pcu/hourではなく**pcu/日相当**。

## 鉄道リンク別利用者数マップ（`rail/`）

- `rail/scenario_rail_ridership_residence_maps.png` / `scenario_rail_ridership_workplace_maps.png` —
  ゾーン(KEY_CODE)別の鉄道利用世帯数（居住地/従業地）マップ、3シナリオ横並び
- `rail/scn{0,1,2}_rail_ridership_{residence,workplace}_map.png` — シナリオ別単体図
- `rail/scenario_rail_ridership_{residence,workplace}_diff_maps.png` — シナリオB・Cの差分マップ
- `rail/scenario_rail_flow_maps.png` — 3シナリオを横並びにした鉄道リンク別利用世帯数マップ（太さ・色とも利用世帯数/日）
- `rail/scn0_rail_flow_map.png` / `scn1_rail_flow_map.png` / `scn2_rail_flow_map.png` — シナリオ別単体図
- `rail/scenario_rail_flow_diff_maps.png` — シナリオB・Cの現状からの鉄道利用世帯数差分を横並びにした比較図
- `rail/scn1_vs_scn0_rail_flow_diff_map.png` / `scn2_vs_scn0_rail_flow_diff_map.png` — シナリオ別 差分単体図
- `rail/scenario_rail_flow_maps_mesh.png` / `scn0_rail_flow_map_mesh.png` 等 — 上記の1kmメッシュ背景付き版
- `rail/scenario_rail_flow_diff_maps_mesh.png` / `scn1_vs_scn0_rail_flow_diff_map_mesh.png` 等 — 差分マップの1kmメッシュ背景付き版

`scripts/Fukuoka_LUTI_model_260617.R` の `#鉄道 リンク別利用者数マップ####`、
`#鉄道 リンク別利用者数 差分マップ（シナリオB/C − 現状）####`、および
`#鉄道 リンク別利用者数マップ・差分マップ（1kmメッシュ背景・試作）####` セクションで生成される。
1kmメッシュ背景版は、道路の「主要道路のみ・1kmメッシュ背景」版と同じパターンで
`key_code_sf`の境界線（`gray60`）を背景に重ねたもの。

道路と異なり鉄道配分は混雑関数を持たない（BPR型の速度低下を考慮しない固定容量=10^5の
プレースホルダ）ため、「太さ=交通量・色=速度」という道路と同じ組合せの地図は作れない。
速度に相当する情報がないため、太さ・色とも「利用世帯数（`l_i_j × P.rail` の配分結果、
**世帯/日**。道路のpcu/日とは単位が異なる）」の単一指標のみを地図化している。

鉄道網データはメッシュ重心と最寄駅・駅と線形頂点を結ぶ合成リンク（`type=="access"`）を含むが、
これは実在の軌道ではないため地図描画からは除外している（`type %in% c("rail","subway")` で
フィルタ）。

配分結果（`rail_link_flow`）はリンクごとに方向別（ID1→ID2 と ID2→ID1）の値を持つが、
物理的な同一区間として1本の線で描画するため、往復方向の利用世帯数を合計した値
（`flow = flow_fwd + flow_rev`）を使っている点が道路の交通量マップ（方向別にそのまま描画）と異なる。

鉄道網データ自体は `scripts/Fukuoka_OSM_03_copied.R` の「rail network」節（OSM抽出→駅接続→
メッシュ重心接続）で構築し、`data/osm/rail_network.xdr` / `data/osm/rail_network.nodes.xdr` に
保存している。`data/` はGit管理外のため、このリポジトリをcloneしただけでは存在せず、
上記スクリプトの再実行が必要。

### 差分マップが博多駅周辺で支配的になる点について

`rail/scenario_rail_flow_diff_maps.png` は差分の絶対値最大でスケールを揃えているため
（CLAUDE.md記載の`sym_limits()`方針）、博多駅周辺（複数路線が収束するハブ）のリンクで
シナリオ間の差分が突出して大きく、それ以外の区間はほぼ白（変化なし）に見える。
これはコードの不具合ではなく、放射状の鉄道網では末端の変化が積算されてハブ付近の
リンクに集中するという実際の構造を反映したもの（絶対値マップでも同じ場所が最大流動量）。
末端側の変化を見たい場合は、現状は個別にデータを絞り込んで確認する必要がある
（道路の「主要道路のみ」版のような専用フィルタ版は今のところ未整備）。

また、鉄道網トポロジー構築（`scripts/Fukuoka_OSM_03_copied.R`の駅接続・線形分割処理）は
上下線が別ジオメトリのOSMデータに由来し、簡略化後も同一ノードペア（ID1, ID2）に
複数の物理セグメントが対応するケースが少数残っている（2600リンク中42ペア、約1.6%）。
このケースでは`rail_network`側で同じ場所に同じflow値の線が重なって描画されるため
見た目には影響しないが、リンク数を数える集計をする場合は注意。

## 旧出力（`runs/legacy_unknown-calibration/`）

2026-10-13 に較正パターン別フォルダへ移行する前に `plots/` 直下に上書き保存されていたもの。
どの較正状態で生成したかは記録が残っていない（2026-08-24 時点の旧メモでは「k_i を固定値0.1」で
実行したとされていたが、その後再生成されたかは不明）。比較用に残しているだけで、参照する場合は注意。
