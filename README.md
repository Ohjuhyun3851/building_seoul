# building_seoul

FAC <- read.csv("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/0523_그래프과제/0523.csv",
                header = T,
                sep = ",",
                quote = "|",
                dec = ".",
                fill = T,
                colClasses = c("UFID"="character", "GRND_FLR"="character", "UGRND_FLR"="character",
                               "PNU"="character", "ARCHAREA"="character", "TOTALAREA"="character",
                               "PLATAREA"="character", "HEIGHT"="character", "STRCT_CD"="character",
                               "USABILITY"="character", "BC_RAT"="character", "VL_RAT"="character",
                               "BLDRGST_PK"="character", "USEAPR_DAY"="character", "REGIST_DAY"="character",
                               "GB_CD"="character", "VIOL_BD_YN" = "character", "GEOIDN"="character",
                               "BD_MGT_SN"="character", "SGG_OID"="character", "COL_ADM_SE"="character",
                               "Shape_Length"="character", "Shape_Area"="character", "YEAR"="character"),
                fileEncoding = "UTF-8")


Building <- read.table("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/#DB_FILE/MID_TL_SPBD_BULD_11_202504.txt",
                       header = T,
                       sep = ",",
                       colClasses = c("BDTYP_CD"="character", "BD_MGT_SN"="character",
                                      "EMD_CD"="character", "LI_CD"="character", "LNBR_MNNM"="character", "LNBR_SLNO"="character",
                                      "MNTN_YN"="character", "SIG_CD"="character",
                                      "Shape_Length"="character", "Shape_Area"="character"),
                       fileEncoding = "UTF-8")


LandUse <- read.table("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/#DB_FILE/MID_ABPD_LAND_USE_STS_11_202503.txt",
                      header = T,
                      sep = "|",
                      colClasses = c("ADM_SECT_CD"="character", "LAND_LOC_CD"="character",
                                     "LEDG_GBN"="character", "BOBN"="character", "BUBN"="character",
                                     "MNG_NO"="character", "REM"="character", "COL_ADM_SECT_CD"="character"),
                      fileEncoding = "CP949")



#FAC 편집

#USEAPR_DAY열에서 건축승인년도만 추출(앞에서 4자리)
fac <- FAC
fac$USEAPR_YEAR <- substr(fac$USEAPR_DAY, 1, 4)

#건축물 나이 계산(numeric으로 바꾸기)
fac$USEAPR_YEAR <- as.numeric(fac$USEAPR_YEAR)
fac$year <- 2025-fac$USEAPR_YEAR



#건축물 용도 중 공동주택만 추출
fac_02000 <-fac[grepl("02000", fac$USABILITY),]

#grepl()로 구조열에서 철근콘크리트, 철골콘크리트, 철골철근콘크리트, 강구조 추출/ fac_02000_STR로 저장
fac_02000_STR <-fac_02000[grepl("17|21|22|26|41|40|42|49|30|31|32|33|39|43", fac_02000$STRCT_CD),]

#건축물 나이 30 이상인 행만 추출(NA도 제거)
fac_02000_STR_30 <- fac_02000_STR[!is.na(fac_02000_STR$year) & fac_02000_STR$year >= 30, ]



#공동주택 중 위의 구조 제외한 구조 추출
fac_02000_NOTSTR <-fac_02000[!grepl("17|21|22|26|41|40|42|49|30|31|32|33|39|43", fac_02000$STRCT_CD),]

#건축물 나이 20 이상인 행만 추출(NA도 제거)
fac_02000_NOTSTR_20 <- fac_02000_NOTSTR[!is.na(fac_02000_NOTSTR$year) & fac_02000_NOTSTR$year >= 20, ]


#fac_02000_STR_30, fac_NOT02000_STR_20두 데이터프레임 합치기
fac_merge <- rbind(fac_02000_STR_30, fac_02000_NOTSTR_20)

#건물관리번호, 건물 나이만 추출
fac_OLD <- fac_merge[, c(19, 26)]

#중복 행 제거
fac_OLD <- fac_OLD[!duplicated(fac_OLD), ]




#Building 파일 편집

#BLD로 Building 재지정
BLD <- Building

#산지번 데이터 타입을 숫자로
BLD$MNTN_YN <- as.numeric(Building$MNTN_YN)

#BLD에서 산지번 +1
BLD$MNTN_YN <- BLD$MNTN_YN + 1


#BLD에서 PNU 합치기-> 열 이름 PNU 지정
install.packages("tidyr")
library(tidyr)
BLD1 <- unite(BLD, c(10, 4, 6, 9, 7, 8), col = "PNU", sep = "")

#중복 행 제거
BLD <- BLD[!duplicated(BLD), ]




#BD_MGT_SN기준으로 BLD와 fac_OLD 데이터 leftjoin
install.packages("dplyr")
library(dplyr)
fac_OLD_FINISH <- left_join(BLD, fac_OLD, by = "BD_MGT_SN")




#density plot

# 1. SIG_CD → 자치구명 매핑
sig_cd_to_name <- c(
  "11110" = "종로구", "11140" = "중구", "11170" = "용산구", "11200" = "성동구", "11215" = "광진구",
  "11230" = "동대문구", "11260" = "중랑구", "11290" = "성북구", "11305" = "강북구", "11320" = "도봉구",
  "11350" = "노원구", "11380" = "은평구", "11410" = "서대문구", "11440" = "마포구", "11470" = "양천구",
  "11500" = "강서구", "11530" = "구로구", "11545" = "금천구", "11560" = "영등포구", "11590" = "동작구",
  "11620" = "관악구", "11650" = "서초구", "11680" = "강남구", "11710" = "송파구", "11740" = "강동구"
)

# 2. 노후 건축물 필터링 및 GU 생성
BLD_seoul_old <- fac_OLD_FINISH %>%
  filter(!is.na(year), year >= 20) %>%
  mutate(GU = sig_cd_to_name[as.character(SIG_CD)])

# 3. 밀도그래프
library(ggplot2)
ggplot(BLD_seoul_old, aes(x = year, fill = GU)) +
  geom_density(alpha = 0.4, color = "black") +
  scale_x_continuous(breaks = seq(20, max(BLD_seoul_old$year, na.rm = TRUE), by = 20)) +  # 20년 단위 눈금
  labs(title = "서울시 자치구별 공동주택 노후건축물 연한 밀도분포", fill = "자치구") +
  theme_minimal()

# 4. 밀도 그래프 (x축 20년 단위 + fill 적용 + facet)
ggplot(BLD_seoul_old, aes(x = year, fill = GU)) +
  geom_density(alpha = 0.4, color = "black") +
  facet_wrap(~ GU, scales = "free_y") +
  labs(title = "서울시 자치구별 공동주택 노후건축물 연한 밀도분포") +
  theme_minimal() +
  theme(legend.position = "none")  # 범례는 facet에 이름 있으므로 생략




#scatter plot
#노원구를 대상으로 건축연도별 건물 층수 확인

#노원구 추출
NW <- fac_OLD_FINISH %>% 
  filter(SIG_CD == 11380)


# 결측값 및 이상치 제거
NW_clean <- NW %>%
  filter(!is.na(year) & !is.na(GRO_FLO_CO)) %>%
  filter(GRO_FLO_CO > 0 & GRO_FLO_CO < 100)

# 산점도 + 추세선
ggplot(data = NW_clean, aes(x = year, y = GRO_FLO_CO)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", se = T, color = "red", size = 1) +
  labs(title = "서울시 노원구 건축연한에 따른 건물 층수 및 추세선") +
  theme_minimal()




#violin plot

#LandUse 편집

#grepl()로 하위 용도지역에 해당하는 행 추출 -> LU 데이터프레임으로 지정
LU <- LandUse[grepl("UQA11[1-2]|UQA12[1-3]|UQA130|UQA2[1-4]0|UQA3[1-3]0|UQA4[1-3]0|UQB[1-3]00|UQ[C-D]001", LandUse$MNG_NO),]

#용도지역 파일에서 PNU 합치기(5열 합치기, tidyr라이브러리 켜기)
library(tidyr)
LU <- unite(LU, 1:5, col = "PNU", sep = "")

#LU에서 용도지역 6자리만 추출 -> SPA로 열 추가
LU$SPA<- substr(LU$MNG_NO, 21, 26)

#중복되는 행 제거
LU <- LU[!duplicated(LU), ]

#LU에서 PNU, SPA만 추출
LU <- LU[, c(1, 5)]


#앞서 법령에 따라 공동주택 노후건축물 데이터를 rbind한 fac_merge에서 PNU, 건물 나이만 추출
fac_OLD_PNU <- fac_merge[, c(4, 26)]

#중복되는 행 제거
fac_OLD_PNU <- fac_OLD_PNU[!duplicated(fac_OLD_PNU), ]


#LU, fac_OLD_PNU두 데이터 결합
library(dplyr)
fac_LU_OLD_FINISH <- left_join(LU, fac_OLD_PNU, by = "PNU", relationship = "many-to-many")

#중복되는 행 제거
fac_LU_OLD_FINISH <- fac_LU_OLD_FINISH[!duplicated(fac_LU_OLD_FINISH), ]




# 결측치 제거
fac_LU_OLD_FINISH_clean <- fac_LU_OLD_FINISH %>%
  filter(!is.na(SPA) & !is.na(year))

# 예: df는 SPA 코드를 포함한 데이터프레임이라고 가정
fac_LU_OLD_FINISH_clean <- fac_LU_OLD_FINISH_clean %>%
  mutate(SPA_name = case_when(
    SPA == "UQA111" ~ "제1종 전용주거",
    SPA == "UQA112" ~ "제2종 전용주거",
    SPA == "UQA121" ~ "제1종 일반주거",
    SPA == "UQA122" ~ "제2종 일반주거",
    SPA == "UQA123" ~ "제3종 일반주거",
    SPA == "UQA130" ~ "준주거",
    SPA == "UQA220" ~ "일반상업",
    SPA == "UQA230" ~ "근린상업",
    SPA == "UQA330" ~ "준공업",
    SPA == "UQA420" ~ "생산녹지",
    SPA == "UQA430" ~ "자연녹지",
    TRUE ~ "기타"
  ))

# 바이올린 플롯
library(ggplot2)
ggplot(fac_LU_OLD_FINISH_clean, aes(x = SPA_name, y = year, fill = SPA)) +
  geom_violin(trim = FALSE, color = "black") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "서울시 용도지역별 건축연한 분포") +
  theme_minimal() +
  theme(legend.position = "none")



