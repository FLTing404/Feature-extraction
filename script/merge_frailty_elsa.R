# =============================================================================
# 将 code\frailty\data\ELSA.dta 中的 frailty 按 id 合并到 code\result\final_elsa.csv
# final_elsa 为 ELSA 2010–2014 wave 5–7 数据；frailty 表为一人一行，按 id 合并后
# 每个 wave 行会带上该 id 的 frailty 值。结果保存到 code\result\final_elsa_with_frailty.csv
# =============================================================================

library(haven)
library(dplyr)

# 以 code 为根目录（与 script、result、frailty 同级）；可按本机路径修改
code_root <- Sys.getenv("CODE_ROOT", unset = "D:/study/2026-大三下/Feature extraction/Feature-extraction")

path_final   <- file.path(code_root, "result", "final_elsa.csv")
path_frailty <- file.path(code_root, "frailty", "data", "ELSA.dta")
path_out     <- file.path(code_root, "result", "final_elsa_with_frailty.csv")

if (!file.exists(path_final)) stop("未找到 final_elsa.csv，请先运行 elsa_data.Rmd 生成 result/final_elsa.csv。")
if (!file.exists(path_frailty)) stop("未找到 frailty 的 ELSA.dta。")

# 读入 final_elsa（ELSA 2010–2014 wave 5–7）
final_elsa <- read.csv(path_final, stringsAsFactors = FALSE)

# 读入 frailty 项目的 ELSA.dta，只保留 id 与 frailty，并改名为 frailty_project 表示「原版」
frailty_dta <- read_dta(path_frailty) %>%
  select(idauniq, frailty_project = frailty)

# 按 id 合并：final_elsa 的 id 对应 frailty 表的 idauniq，原表所有列不变，只新增 frailty_project
merged <- final_elsa %>%
  left_join(frailty_dta, by = c("id" = "idauniq"))

# 写出到 result
dir.create(dirname(path_out), showWarnings = FALSE, recursive = TRUE)
write.csv(merged, path_out, row.names = FALSE, na = "")

message("已生成: ", path_out)
message("合并行数: ", nrow(merged), "；带 frailty_project（原版）非 NA 的行数: ", sum(!is.na(merged$frailty_project)))
