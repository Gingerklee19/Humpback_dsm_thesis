# 03.dsm.models.R

# Load required packages
library(dsm)
library(knitr)

# Load the data
load(1.filtered_segs_obs.Rdata)
load(detfunc.Rdata)

# Creating the correct col names to create a dsm

# Sample Label
segs_f <- dplyr::rename(segs_f, Sample.Label = seg_id)
obs_f <- dplyr::rename(obs_f, Sample.Label = seg_id)

# Object
obs_f <- obs_f %>% 
  mutate(object = 1:length(Id))

# Size
obs_f <- dplyr::rename(obs_f, size = BestNumber)

# Ensuring the data is in the right format
segs_f <- segs_f %>% mutate(Effort = Effort %>% as.numeric()) %>% filter(Effort>100)
obs_f <- obs_f %>% filter(distance <= 3000)

obs_f$size<- as.numeric(obs_f$size)
obs_f <- obs_f %>% filter(!is.na(size))

# ==========================================
# Running the DSMs, tweedie family
# ==========================================

# Base DSMs - no covariates added
dsm.xy.tw <- dsm(count ~ s(x, y), ds_hr, segs_f, obs_f, family = tw(), method = "REML")
vis.gam(dsm.xy.tw, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)

dsm.xy.hn.tw <- dsm(count ~ s(x, y), ds_hn, segs_f, obs_f, family = tw(), method = "REML")
vis.gam(dsm.xy.hn.tw, plot.type = "contour", view = c("x", "y"), asp = 1, type = "response", contour.col = "black", n.grid = 100)

# DSMs with month as a factor
dsm.xy.fmonth.tw <- dsm(count ~ s(x, y) + month.x, ds_hr, segs_f, obs_f, family = tw(), method = "REML")
dsm.xy.fmonth.tw.hn <- dsm(count ~ s(x, y) + month.x, ds_hn, segs_f, obs_f, family = tw(), method = "REML")

# DSMs with month as a smooth
dsm.xy.smonth.tw <- dsm(count ~ s(x, y) + s(month.n, k = 5), ds_hr, segs_f, obs_f, family = tw(), method = "REML")
dsm.xy.smonth.tw.hn <- dsm(count ~ s(x, y) + s(month.n, k = 5), ds_hn, segs_f, obs_f, family = tw(), method = "REML")

# DSMs with day of year
dsm.xy.d.tw <- dsm(count ~ s(x, y) + s(d, k = 5), ds_hr, segs_f, obs_f, family = tw(), method = "REML")
dsm.xy.d.tw.hn <- dsm(count ~ s(x, y) + s(d, k = 5), ds_hn, segs_f, obs_f, family = tw(), method = "REML")

# DSMs with week
# binning days into week
segs_f <- segs_f %>%
  mutate(d_week = floor((d - 1) / 7) + 1)

# Smooth
dsm.xy.sdweek.tw <- dsm(count ~ s(x, y) + s(d_week, k = 5), ds_hr, segs_f, obs_f, family = tw(), method = "REML")
dsm.xy.sdweek.tw.hn <- dsm(count ~ s(x, y) + s(d_week, k = 5), ds_hn, segs_f, obs_f, family = tw(), method = "REML")

# Factor
dsm.xy.dweek.tw <- dsm(count ~ s(x, y) + as.factor(d_week), ds_hr, segs_f, obs_f, family = tw(), method = "REML")
dsm.xy.dweek.tw.hn <- dsm(count ~ s(x, y) + as.factor(d_week), ds_hn, segs_f, obs_f, family = tw(), method = "REML")

# DSms with Season
segs_f$Season <- factor(segs_f$Season, levels = c("22/23", "23/24", "24/25"))

dsm.xy.Season.tw <- dsm(count ~ s(x, y) + Season, ds_hr, segs_f, obs_f, family = tw(), method = "REML")
dsm.xy.Season.tw.hn <- dsm(count ~ s(x, y) + Season, ds_hn, segs_f, obs_f, family = tw(), method = "REML")

# Making a comparison table of the DSMs 
# Comparing Deviance explained
tweedie_dsm_model <- data.frame(
  "Model name" = c("dsm.xy.tw", "dsm.xy.hn.tw", "dsm.xy.fmonth.tw", "dsm.xy.fmonth.tw.hn",
                   "dsm.xy.smonth.tw", "dsm.xy.smonth.tw.hn", "dsm.xy.d.tw", "dsm.xy.d.tw.hn", 
                   "dsm.xy.sdweek.tw", "dsm.xy.sdweek.tw.hn", "dsm.xy.dweek.tw", "dsm.xy.dweek.tw.hn",
                   "dsm.xy.Season.tw", "dsm.xy.Season.tw.hn"),
  "Description" = c("base model, tweedie",
                    "half normal, tweedie",
                    "fmonth, tweedie",
                    "fmonth, half normal, tweedie",
                    "smooth of month, tweedie",
                    "smooth of month, half normal, tweedie",
                    "day of year, tweedie",
                    "day of year, half normal, tweedie",
                    "smooth weekly",
                    "smooth weekly, hn",
                    "weekly, tweedie",
                    "weekly, half normal. tweedie",
                    "Season, tweedie",
                    "Season, half normal, tweedie"),
  "Deviance explained" = unlist(lapply(
    list(dsm.xy.tw, dsm.xy.hn.tw, dsm.xy.fmonth.tw, dsm.xy.fmonth.tw.hn,
         dsm.xy.smonth.tw, dsm.xy.smonth.tw.hn, dsm.xy.d.tw, dsm.xy.d.tw.hn, dsm.xy.sdweek.tw, 
         dsm.xy.sdweek.tw.hn, dsm.xy.dweek.tw, dsm.xy.dweek.tw.hn, dsm.xy.Season.tw, dsm.xy.Season.tw.hn),
    function(x) paste0(round(summary(x)$dev.expl * 100, 2), "%")
  ))
)

kable(tweedie_dsm_model, col.names=c("Model name", "Description", "Deviance explained"))

# Comaring AIC scores
AIC(dsm.xy.tw, dsm.xy.hn.tw, dsm.xy.fmonth.tw, dsm.xy.fmonth.tw.hn,
    dsm.xy.smonth.tw, dsm.xy.smonth.tw.hn, dsm.xy.d.tw, dsm.xy.d.tw.hn, dsm.xy.sdweek.tw, 
    dsm.xy.sdweek.tw.hn, dsm.xy.dweek.tw, dsm.xy.dweek.tw.hn, dsm.xy.Season.tw, dsm.xy.Season.tw.hn)


# ===========================================
# Running the DSMs, negative binomial family
# ===========================================

# Base DSMs
dsm.xy.nb <- dsm(count ~ s(x, y), ds_hr, segs_f, obs_f, family = nb(), method = "REML")
vis.gam(dsm.xy.nb, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)

dsm.xy.hn.nb <- dsm(count ~ s(x, y), ds_hn, segs_f, obs_f, family = nb(), method = "REML")
vis.gam(dsm.xy.hn.nb, plot.type = "contour", view = c("x", "y"), asp = 1, type = "response", contour.col = "black", n.grid = 100)

# DSMs with month as a factor
dsm.xy.fmonth.nb <- dsm(count ~ s(x, y) + month.x, ds_hr, segs_f, obs_f, family = nb(), method = "REML")
dsm.xy.fmonth.nb.hn <- dsm(count ~ s(x, y) + month.x, ds_hn, segs_f, obs_f, family = nb(), method = "REML")

# DSMs with month as a smooth
dsm.xy.smonth.nb <- dsm(count ~ s(x, y) + s(month.n, k = 5), ds_hr, segs_f, obs_f, family = nb(), method = "REML")
dsm.xy.smonth.nb.hn <- dsm(count ~ s(x, y) + s(month.n, k = 5), ds_hn, segs_f, obs_f, family = nb(), method = "REML")

# DSMs with day of year
dsm.xy.d.nb <- dsm(count ~ s(x, y) + s(d, k = 5), ds_hr, segs_f, obs_f, family = nb(), method = "REML")
dsm.xy.d.nb.hn <- dsm(count ~ s(x, y) + s(d, k = 5), ds_hn, segs_f, obs_f, family = nb(), method = "REML")

# DSMs with week as a smooth
dsm.xy.sdweek.nb <- dsm(count ~ s(x, y) + s(d_week, k = 5), ds_hr, segs_f, obs_f, family = nb(), method = "REML")
dsm.xy.sdweek.nb.hn <- dsm(count ~ s(x, y) + s(d_week, k = 5), ds_hn, segs_f, obs_f, family = nb(), method = "REML")

# DSMs with week as a factor
dsm.xy.dweek.nb <- dsm(count ~ s(x, y) + as.factor(d_week), ds_hr, segs_f, obs_f, family = nb(), method = "REML")
dsm.xy.dweek.nb.hn <- dsm(count ~ s(x, y) + as.factor(d_week), ds_hn, segs_f, obs_f, family = nb(), method = "REML")

# DSMs with Season
dsm.xy.Season.nb <- dsm(count ~ s(x, y) + Season, ds_hr, segs_f, obs_f, family = nb(), method = "REML")
dsm.xy.Season.nb.hn <- dsm(count ~ s(x, y) + Season, ds_hn, segs_f, obs_f, family = nb(), method = "REML")

# Making a comparison table
# Comparing deviance explained
negbinom_dsm_models <- data.frame(
  "Model name" = c("dsm.xy.nb", "dsm.xy.hn.nb", "dsm.xy.fmonth.nb", "dsm.xy.fmonth.nb.hn",
                   "dsm.xy.smonth.nb", "dsm.xy.smonth.nb.hn", "dsm.xy.d.nb", "dsm.xy.d.nb.hn",
                   "dsm.xy.sdweek.nb", "dsm.xy.sdweek.nb.hn", "dsm.xy.dweek.nb", "dsm.xy.dweek.nb.hn",
                   "dsm.xy.Season.nb", "dsm.xy.Season.nb.hn"),
  "Description" = c("base model, negative binomial",
                    "half normal, negative binomial",
                    "fmonth, negative binomial",
                    "fmonth, half normal, negative binomial",
                    "smooth of month, negative binomial",
                    "smooth of month, half normal, negative binomial",
                    "day of year, negative binomial",
                    "day of year, half normal, negative binomial",
                    "smooth weekly, nb",
                    "smooth weekly, hn, nb",
                    "weekly, nb",
                    "weekly, half normal. nb",
                    "Season, nb",
                    "Season, half normal, nb"),
  "Deviance explained" = unlist(lapply(
    list(dsm.xy.nb, dsm.xy.hn.nb, dsm.xy.fmonth.nb, dsm.xy.fmonth.nb.hn,
         dsm.xy.smonth.nb, dsm.xy.smonth.nb.hn, dsm.xy.d.nb, dsm.xy.d.nb.hn, 
         dsm.xy.sdweek.nb, dsm.xy.sdweek.nb.hn, dsm.xy.dweek.nb, dsm.xy.dweek.nb.hn,
                   dsm.xy.Season.nb, dsm.xy.Season.nb.hn),
    function(x) paste0(round(summary(x)$dev.expl * 100, 2), "%")
  ))
)

kable(negbinom_dsm_models, col.names=c("Model name", "Description", "Deviance explained"))

# Comparing AIC scores
AIC(dsm.xy.nb, dsm.xy.hn.nb, dsm.xy.fmonth.nb, dsm.xy.fmonth.nb.hn,
    dsm.xy.smonth.nb, dsm.xy.smonth.nb.hn, dsm.xy.d.nb, dsm.xy.d.nb.hn, 
    dsm.xy.sdweek.nb, dsm.xy.sdweek.nb.hn, dsm.xy.dweek.nb, dsm.xy.dweek.nb.hn,
    dsm.xy.Season.nb, dsm.xy.Season.nb.hn)
