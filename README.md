# Feature extraction

本目录为 ELSA 特征提取与衰弱（frailty）合并分析相关代码与数据。

## 目录结构

- **script/**  
  - `elsa_data.Rmd`：从 ELSA 原始数据构建 2010–2014（wave 5–7）特征数据，输出 `result/final_elsa.csv`  
  - `merge_frailty_elsa.R`：将 frailty 指标按 id 合并到 final_elsa，输出 `result/final_elsa_with_frailty.csv`  
  - `export_elsa_frailty.R`：从 frailty 的 ELSA.dta 导出 id 与 frailty，输出 `result/ELSA_frailty_id_only.csv`

- **result/**  
  运行上述脚本后生成的 CSV 结果（如 `final_elsa.csv`、`final_elsa_with_frailty.csv`）。

- **frailty/**  
  衰弱指标相关代码与数据。合并脚本会读取 `frailty/data/ELSA.dta`（若路径不同，可在脚本中通过 `CODE_ROOT` 或默认路径调整）。

- **social-determinants-frailty/**  
  社会决定因素与衰弱跨国分析的项目代码与数据（ELSA/HRS/CHARLS），包含分析、可视化和原始数据说明等。详见该目录下的 README。

## 运行顺序建议

1. 确保 ELSA 原始数据已放在 `social-determinants-frailty/data/raw/elsa/`（参见 `social-determinants-frailty/data/raw/README.md`）。  
2. 在 R 中打开并运行 `script/elsa_data.Rmd`，生成 `result/final_elsa.csv`。  
3. 若有 `frailty/data/ELSA.dta`，可运行 `script/merge_frailty_elsa.R` 得到 `result/final_elsa_with_frailty.csv`，或运行 `script/export_elsa_frailty.R` 导出仅含 id 与 frailty 的 CSV。

## 环境与路径

- 默认以本目录（Feature-extraction）为工作根目录；脚本中通过 `CODE_ROOT` 环境变量或默认值 `D:/study/2026-大三下/Feature extraction/Feature-extraction` 指定，可按本机路径修改。  
- R 依赖见 `script/elsa_data.Rmd` 开头及 `social-determinants-frailty` 内说明。
