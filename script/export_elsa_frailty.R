# =============================================================================
# 直接从 code\frailty\data\ELSA.dta 导出 id（idauniq）和 frailty 两列到 CSV
# 不做任何合并或转换，便于查看 .dta 里的原版数据
# =============================================================================

library(haven)
library(dplyr)

code_root <- Sys.getenv("CODE_ROOT", unset = "D:/study/2026-大三下/Feature extraction/Feature-extraction")
path_dta  <- file.path(code_root, "frailty", "data", "ELSA.dta")
path_out  <- file.path(code_root, "result", "ELSA_frailty_id_only.csv")

if (!file.exists(path_dta)) stop("未找到 ELSA.dta: ", path_dta)

d <- read_dta(path_dta) %>%
  select(idauniq, frailty)

dir.create(dirname(path_out), showWarnings = FALSE, recursive = TRUE)
write.csv(d, path_out, row.names = FALSE, na = "")

message("已导出: ", path_out)
message("行数: ", nrow(d))
message("frailty 唯一值个数: ", length(unique(na.omit(d$frailty))))
