# ELSA Frailty 要点汇总

## 1. 缺少的五个变量

- **raccnofeat**、**ahown**、**alone**、**adltot_e**、**adltotm_e**
- 在 h_elsa_g2 里若整列缺失，会导出为 `"none"`

---

## 2. Frailty 的定义（方法）
code 里没有 frailty 的处理过程，是在别处处理好后直接打包成 .dta，只能通过 id 去匹配已算好的 frailty。
这是论文里提到的01是如何计算的方法
- **方法**：Frailty Index (FI)
- **计算**：deficits / total items
- **范围**：0–1
- **二分类 cutoff**：FI ≥ 0.25 → 衰弱 = 1，否则 = 0

---

## 3. final_elsa_with_frailty.csv 是怎么来的

- **第一步**：`elsa_data.Rmd` 用 h_elsa_g2 + wave_4/5、life_history 做 long → 纳入排除 → 基线/随访 → 得到 **result/final_elsa.csv**（三波 2010/2012/2014，含 fi 等）。
- **第二步**：`merge_frailty_elsa.R` 读入 final_elsa.csv 与 **code\\frailty\\data\\ELSA.dta**，按 **id** 左连接，把 .dta 里的 frailty 以列名 **frailty_project** 并入，写出 **result/final_elsa_with_frailty.csv**。

---

## 4. 论文设计与可用的 frailty 数据
code 里的 frailty **每人只有一个值**（不按年份/wave 区分，相当于 2010–2014 整段只给一个），而 final_elsa 有三个 wave（2010、2012、2014），合并后同一人的三行会重复同一个 frailty。
- **设计**：**baseline → 4 年 follow-up**  
  - 用 **baseline 的社会决定因素** 预测 **4 年后的 FI**。  
  - 例如：2010（baseline）→ 2014（FI）；2012（baseline）→ 2016（FI）。
- **当前能提取到的数据**：**2010–2014 年的 frailty**（对应 final_elsa 的 wave 5–7：2010、2012、2014）。
- **含义**：在现有数据下，可做 **2010 baseline → 2014 的 FI** 这一组 4 年随访；若没有 2016 波次，则无法做 2012 → 2016 的 4 年随访。

