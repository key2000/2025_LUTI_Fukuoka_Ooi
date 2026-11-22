# 賃構 都道府県別第1表（福岡）から、産業大分類C〜Rと産業計の「きまって支給する現金給与額」（男女計・企業規模計10人以上、千円/月、H12セル）を取り出す
# 実行（リポジトリルート）: python3 -I project_log/base-year-revision/06_wage-by-year/extract_wage_by_year.py
import zipfile, re, csv, os
BASE = "data/raw/賃金構造基本統計調査　福岡　産業別"
FILES = {2009: "2009_H21_newmethod/fukuoka_kochi_table1_2009_H21_newmethod.xlsx",
         2014: "2014_H26_newmethod/fukuoka_kochi_table1_2014_H26_newmethod.xlsx",
         2021: "2021_R3/fukuoka_kochi_table1_2021_R3.xlsx",
         2022: "(1-10-1-20)aa2n11.xlsx"}
FW = "ＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲ"
def read_book(path):
    z = zipfile.ZipFile(path)
    ss = re.findall(r"<si>(.*?)</si>", z.read("xl/sharedStrings.xml").decode(), re.S)
    ss = ["".join(re.findall(r"<t[^>]*>([^<]*)</t>", re.sub(r"<rPh.*?</rPh>", "", s, flags=re.S))) for s in ss]
    names = re.findall(r"<sheet [^>]*name=\"([^\"]*)\"", z.read("xl/workbook.xml").decode())
    return z, ss, names
def cell(z, ss, i, ref):
    x = z.read(f"xl/worksheets/sheet{i}.xml").decode()
    m = re.search(r"<c r=\"" + ref + r"\"([^>]*?)(?:/>|>(.*?)</c>)", x, re.S)
    v = re.search(r"<v>(.*?)</v>", m.group(2) or "")
    return ss[int(v.group(1))] if 't="s"' in m.group(1) else v.group(1)
rows = []
for year, f in FILES.items():
    z, ss, names = read_book(os.path.join(BASE, f))
    title = cell(z, ss, 1, "D1")
    for i, n in enumerate(names, 1):
        if "福岡" not in n: continue
        m = re.search(r"福岡県?\)?(産業計|([" + FW + r"])(?![0-9０-９]))", n)
        if not m: continue
        assert "きまって" in cell(z, ss, i, "H9")
        ind = "ALL" if m.group(1) == "産業計" else "CDEFGHIJKLMNOPQR"[FW.index(m.group(2))]
        rows.append({"year": year, "Industry_id": ind, "avg_wage": round(float(cell(z, ss, i, "H12")), 1),
                     "source": title, "sheet": n})
out = "data/raw/賃金構造基本統計調査　福岡　産業別/Fukuoka_avg_wage_by_year.csv"
with open(out, "w", newline="", encoding="utf-8") as fo:
    w = csv.DictWriter(fo, fieldnames=["year", "Industry_id", "avg_wage", "source", "sheet"]); w.writeheader(); w.writerows(rows)
for y in FILES:
    r = [x for x in rows if x["year"] == y]
    print(y, len(r), " ".join(f'{x["Industry_id"]}={x["avg_wage"]}' for x in r))
