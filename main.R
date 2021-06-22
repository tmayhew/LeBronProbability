library(tidyverse)
library(rvest)
library(MASS)
library(formattable)
select = dplyr::select

"game_log = NULL
for (yr in 2004:2021){
  log = read_html(paste0('https://www.basketball-reference.com/players/j/jamesle01/gamelog/', yr))
  log_ = html_table(log, fill = T)[[8]]
  log_new = cbind.data.frame(log_, yr)
  game_log = rbind.data.frame(game_log, log_new)
  print(yr)
}

game_log = game_log %>% mutate(FG = as.numeric(FG), PTS = as.numeric(PTS), 
                               TRB = as.numeric(TRB), AST = as.numeric(AST))
game_log = game_log %>% filter(is.na(FG) == F)
game_log %>% write.csv('LeBronGameLog.csv')"

complete_log = NULL
df = read.csv("LeBronGameLog.csv")[,-1] %>% select(PTS, TRB, AST, everything())
df_ = df %>% filter(PTS == 27 & AST == 7 | PTS == 27 & TRB == 7 | TRB == 7 & AST == 7) %>% 
  mutate(type = ifelse(PTS == 27 & AST == 7, "PTS & AST", ifelse(PTS == 27 & TRB == 7, "PTS & TRB", "TRB & AST"))) %>% 
  mutate(offby = ifelse(type == "PTS & AST", TRB-7, ifelse(type == "PTS & TRB", AST-7, PTS-27))) %>% 
  mutate(statline = paste0(PTS, "/", TRB, "/", AST))
df_ = df_ %>% mutate(tm = ifelse(yr %in% c(seq(2003,2010), seq(2015,2018)), "CLE", ifelse(yr %in% 2011:2014, "MIA", "LAL")))
df_counts = df_ %>% group_by(statline) %>% summarize(.groups = "drop", count = n())
df_ = df_ %>% left_join(df_counts, by = "statline") %>% mutate(statline = paste0(statline, " (", count, ")"))

df_ %>% distinct(statline, .keep_all = T) %>% ggplot(aes(x = type, y = offby, label = statline, color = tm)) + geom_text(size = 3) + geom_hline(yintercept = 0) +
  scale_y_continuous("Proximity to LEBRON", breaks = seq(-15,25,5)) + theme_bw() +
  scale_x_discrete("Values of LEBRON") + scale_color_manual(values = c("#860038", "#552583", "black")) + guides(color = F) +
  ggtitle("Illusive LEBRON: Games where LeBron almost had 27/7/7")
ggsave("images/plot1.png", width = 7.75, height = 5.5)

pts_summary = c(mean(df$PTS), sd(df$PTS))
ast_summary = c(mean(df$AST), sd(df$AST))
trb_summary = c(mean(df$TRB), sd(df$TRB))
summary1 = data.frame(pts_summary, trb_summary, ast_summary) %>% set_names(c("PTS", "TRB", "AST"))
rownames(summary1) = c("Mean", "Std. Dev.")
formattable(summary1)

ptsdis = data.frame(pts = seq(-.5,max(df$PTS)+.5,1)) %>% mutate(lower_prob = pnorm(q = pts, mean = pts_summary[1], sd = pts_summary[2]))
ptsdis$pts_actual = seq(-1,61)
ptsdis$diff = 0
for (i in 1:nrow(ptsdis)){if (i == 1){ptsdis$diff[i] = 0} else{ptsdis$diff[i] = ptsdis$lower_prob[i] - ptsdis$lower_prob[i-1]}}
ptsdis = ptsdis %>% mutate(games = diff*nrow(df)) %>% filter(pts_actual >= 0) %>% transmute(PTS = pts_actual, est_games = games)
ptsdis = ptsdis %>% left_join(df$PTS %>% table() %>% data.frame() %>% set_names(c("PTS", "games")) %>% mutate(PTS = as.numeric(as.character(PTS))))
ptsdis = ptsdis %>% mutate(games = ifelse(is.na(games), 0, games))
ptsdis %>% mutate(is = ifelse(PTS == 27, "a", "b")) %>% ggplot(aes(x = PTS, y = est_games)) + geom_line() + geom_bar(alpha = I(7/8), aes(y = games, fill = is), stat = "identity") + 
  scale_y_continuous("Number of Games", breaks = seq(0,75,5)) + scale_x_continuous(breaks = c(seq(0,60,10), 27)) +
  theme_bw() + scale_fill_manual(values = c("#de0f00", "grey50")) + guides(fill = F) + geom_point(alpha = I(7/8)) +
  ggtitle("Points Breakdown", "Observed versus Discrete Normal Distribution")
ggsave("images/plot2.png", width = 7.75, height = 5.5)

#ptsdis %>% gather("type", "value", -PTS) %>% mutate(is = ifelse(PTS == 27, "a", "b"), type = ifelse(type == "est_games", "Normal Distribution", "Observed Games")) %>% filter(PTS >= 22 & PTS <= 32) %>% 
#  ggplot(aes(x = PTS, y = value, fill = type, alpha = is)) + 
#  geom_bar(width = I(2/3), stat = "identity", position = "dodge") + scale_x_continuous("Points", breaks = seq(22,32,1)) + 
#  scale_y_continuous("Number of Games", breaks = seq(0,75,5)) + scale_fill_manual("", values = c("grey50", "#de0f00")) + theme_bw() +
#  scale_alpha_manual(values = c(1, .6)) + guides(alpha = F) + theme(legend.position = "top")

astdis = data.frame(ast = seq(-.5,max(df$AST)+.5,1)) %>% mutate(lower_prob = pnorm(q = ast, mean = ast_summary[1], sd = ast_summary[2]))
astdis$ast_actual = seq(-1,max(df$AST))
astdis$diff = 0
for (i in 1:nrow(astdis)){if (i == 1){astdis$diff[i] = 0} else{astdis$diff[i] = astdis$lower_prob[i] - astdis$lower_prob[i-1]}}
astdis = astdis %>% mutate(games = diff*nrow(df)) %>% filter(ast_actual >= 0) %>% transmute(AST = ast_actual, est_games = games)
astdis = astdis %>% left_join(df$AST %>% table() %>% data.frame() %>% set_names(c("AST", "games")) %>% mutate(AST = as.numeric(as.character(AST))))
astdis = astdis %>% mutate(games = ifelse(is.na(games), 0, games))
astdis %>% mutate(is = ifelse(AST == 7, "a", "b")) %>% ggplot(aes(x = AST, y = est_games)) + geom_line() + geom_bar(alpha = I(7/8), aes(y = games, fill = is), stat = "identity") + 
  scale_y_continuous("Number of Games", breaks = seq(0,200,10)) + scale_x_continuous(breaks = c(7, seq(0,20,5))) +
  theme_bw() + scale_fill_manual(values = c("#de0f00", "grey50")) + guides(fill = F) + geom_point(alpha = I(7/8)) +
  ggtitle("Assists Breakdown", "Observed versus Discrete Normal Distribution")
ggsave("images/plot3.png", width = 7.75, height = 5.5)

#astdis %>% gather("type", "value", -AST) %>% mutate(is = ifelse(AST == 7, "a", "b"), type = ifelse(type == "est_games", "Normal Distribution", "Observed Games")) %>% filter(AST >= 4 & AST <= 11) %>% 
#  ggplot(aes(x = AST, y = value, fill = type, alpha = is)) + 
#  geom_bar(width = I(2/3), stat = "identity", position = "dodge") + scale_x_continuous("Assists", breaks = seq(4,11,1)) + 
#  scale_y_continuous("Number of Games", breaks = seq(0,200,10)) + scale_fill_manual("", values = c("grey50", "#de0f00")) + theme_bw() +
#  scale_alpha_manual(values = c(1, .6)) + guides(alpha = F) + theme(legend.position = "top")

trbdis = data.frame(trb = seq(-.5,max(df$TRB)+.5,1)) %>% mutate(lower_prob = pnorm(q = trb, mean = trb_summary[1], sd = trb_summary[2]))
trbdis$trb_actual = seq(-1,max(df$TRB))
trbdis$diff = 0
for (i in 1:nrow(trbdis)){if (i == 1){trbdis$diff[i] = 0} else{trbdis$diff[i] = trbdis$lower_prob[i] - trbdis$lower_prob[i-1]}}
trbdis = trbdis %>% mutate(games = diff*nrow(df)) %>% filter(trb_actual >= 0) %>% transmute(TRB = trb_actual, est_games = games)
trbdis = trbdis %>% left_join(df$TRB %>% table() %>% data.frame() %>% set_names(c("TRB", "games")) %>% mutate(TRB = as.numeric(as.character(TRB))))
trbdis = trbdis %>% mutate(games = ifelse(is.na(games), 0, games))
trbdis %>% mutate(is = ifelse(TRB == 7, "a", "b")) %>% ggplot(aes(x = TRB, y = est_games)) + geom_line() + geom_bar(alpha = I(7/8), aes(y = games, fill = is), stat = "identity") + 
  scale_y_continuous("Number of Games", breaks = seq(0,200,10)) + scale_x_continuous(breaks = c(7, seq(0,20,5))) +
  theme_bw() + scale_fill_manual(values = c("#de0f00", "grey50")) + guides(fill = F) + geom_point(alpha = I(7/8)) +
  ggtitle("Rebounds Breakdown", "Observed versus Discrete Normal Distribution")
ggsave("images/plot4.png", width = 7.75, height = 5.5)

#trbdis %>% gather("type", "value", -TRB) %>% mutate(is = ifelse(TRB == 7, "a", "b"), type = ifelse(type == "est_games", "Normal Distribution", "Observed Games")) %>% filter(TRB >= 4 & TRB <= 11) %>% 
#  ggplot(aes(x = TRB, y = value, fill = type, alpha = is)) + 
#  geom_bar(width = I(2/3), stat = "identity", position = "dodge") + scale_x_continuous("Assists", breaks = seq(4,11,1)) + 
#  scale_y_continuous("Number of Games", breaks = seq(0,200,10)) + scale_fill_manual("", values = c("grey50", "#de0f00")) + theme_bw() +
#  scale_alpha_manual(values = c(1, .6)) + guides(alpha = F) + theme(legend.position = "top")

## PROBABILITY ESTIMATION
pts_summary = c(mean(df$PTS), sd(df$PTS))
ast_summary = c(mean(df$AST), sd(df$AST))
trb_summary = c(mean(df$TRB), sd(df$TRB))

# POINTS DISCRETE ANALYSIS
(pnorm(q = 27.5, mean = pts_summary[1], sd = pts_summary[2]))-(pnorm(q = 26.5, mean = pts_summary[1], sd = pts_summary[2]))
# (Pr(PTS = 27) = 0.05119192)
0.05119192*nrow(df)
# (Ex(PTS = 27) = 67.06142)
length(which(df$PTS == 27))
# True Value: 72

# ASSISTS DISCRETE ANALYSIS
(pnorm(q = 7.5, mean = ast_summary[1], sd = ast_summary[2]))-(pnorm(q = 6.5, mean = ast_summary[1], sd = ast_summary[2]))
# (Pr(AST = 7) = 0.131482)
0.131482*nrow(df)
# (Ex(AST = 7) = 172.2414)
length(which(df$AST == 7))
# True Value: 191

# REBOUNDS DISCRETE ANALYSIS
(pnorm(q = 7.5, mean = trb_summary[1], sd = trb_summary[2]))-(pnorm(q = 6.5, mean = trb_summary[1], sd = trb_summary[2]))
# (Pr(TRB = 7) = 0.1301082)
0.1301082*nrow(df)
# (Ex(AST = 7) = 170.4417)
length(which(df$TRB == 7))
# True Value: 184

# Calculating Pr(NEVER LEBRON) (assuming independence)
(1-((72/nrow(df))*(191/nrow(df))*(184/nrow(df))))^(nrow(df))
complete_log = complete_log %>% rbind.data.frame(cbind.data.frame(type = "Calculation (Independent Probabilities)", est = (1-((72/nrow(df))*(191/nrow(df))*(184/nrow(df))))^(nrow(df))))
# So the probability that these three would never line up is 0.228706

# Approximation 1: Re-Sampling with Replacement (Assuming Independence)
g = nrow(df)
occ = NULL
for (i in 1:1000){
  pts = sample(x = df$PTS, size = g, replace = T)
  reb = sample(x = df$TRB, size = g, replace = T)
  ast = sample(x = df$AST, size = g, replace = T)
  sim = data.frame(pts, reb, ast)
  occ_ = sim %>% filter(pts == 27, reb == 7, ast == 7) %>% nrow()
  occ = c(occ, occ_)
  print(i)
}

occ %>% table() %>% as.data.frame() %>% mutate(Freq = Freq/sum(Freq), reality = ifelse(. == 0, "Y", "N")) %>% 
  ggplot(aes(x = ., y = Freq, fill = reality)) + geom_bar(stat = "identity", width = I(1/2)) +
  scale_x_discrete("Occurrences of LEBRON") +
  scale_y_continuous("Percent of Simulations", breaks = seq(0,.35,.05)) +
  theme_bw() + scale_fill_manual(values = c("grey60", "black")) + 
  theme(legend.position = "none") + 
  ggtitle("Results of Approximation", "Independent Re-Sampling, Career")
ggsave("images/plot5.png", width = 7.75, height = 5.5)
mean(occ == 0)
complete_log = complete_log %>% rbind.data.frame(cbind.data.frame(type = "Simulation (Independent Re-Sampling, Career)", est = mean(occ == 0)))

# Approximation 2: Re-Sampling with Replacement (by Season)
occ = NULL
years = df %>% group_by(yr) %>% summarize(.groups = "drop", count = n())
for (i in 1:1000){
  for (j in 1:nrow(years)){
    pts = sample(x = df$PTS[which(df$yr == years$yr[j])], size = years$count[j], replace = T)
    reb = sample(x = df$TRB[which(df$yr == years$yr[j])], size = years$count[j], replace = T)
    ast = sample(x = df$AST[which(df$yr == years$yr[j])], size = years$count[j], replace = T)
    sim = data.frame(pts, reb, ast)
    occurs = sim %>% filter(pts == 27, reb == 7, ast == 7) %>% nrow()
    occ = rbind.data.frame(occ, data.frame(sim = i, szn = years$yr[j], occurs))
  }
  print(i)
}

sims = occ %>% group_by(sim) %>% summarise(.groups = "drop", occurs = sum(occurs))
table(sims$occurs) %>% as.data.frame() %>% mutate(Freq = Freq/sum(Freq), reality = ifelse(Var1 == 0, "Y", "N")) %>%
  ggplot(aes(x = Var1, y = Freq, fill = reality)) + geom_bar(stat = "identity", width = I(1/2)) +
  scale_x_discrete("Occurrences of LEBRON") +
  scale_y_continuous("Percent of Simulations", breaks = seq(0,.5,.05)) +
  theme_bw() + scale_fill_manual(values = c("grey60", "black")) + 
  theme(legend.position = "none") + 
  ggtitle("Results of Approximation", "Independent Re-Sampling, Individual Seasons")
ggsave("images/plot6.png", width = 7.75, height = 5.5)
mean(sims$occurs == 0)
complete_log = complete_log %>% rbind.data.frame(cbind.data.frame(type = "Simulation (Independent Re-Sampling, Season)", est = mean(sims$occurs == 0)))

byszn = occ %>% group_by(szn)
table(byszn$szn, byszn$occurs) %>% as.data.frame() %>% set_names(nm = c("Year", "Value", "Occurrences")) %>% mutate(tm = ifelse(Year %in% c(seq(2003,2010), seq(2015,2018)), "CLE", ifelse(Year %in% 2011:2014, "MIA", "LAL"))) %>% 
  filter(Value == 0) %>% mutate(Occurrences = 1-(Occurrences/1000)) %>% 
  ggplot(aes(x = Year, y = Occurrences, fill = tm)) + geom_bar(stat = "identity") + 
  scale_y_continuous("Probability of LEBRON", limits = c(0,.20)) + theme_bw() + scale_x_discrete("") +
  scale_fill_manual(values = c("#860038", "#552583", "black")) +
  guides(fill = F) + ggtitle("Which seasons had the highest LEBRON probability?", "Independent Re-Sampling")
ggsave("images/plot7.png", width = 7.75, height = 5.5)

# Approximation 3: Drawing from Normal Distribution (Career)
mu_pts = mean(df$PTS); sd_pts = sd(df$PTS)
mu_ast = mean(df$AST); sd_ast = sd(df$AST)
mu_trb = mean(df$TRB); sd_trb = sd(df$TRB)
g = nrow(df)
occ = NULL
for (i in 1:1000){
  pts = round(rnorm(n = g, mean = mean(df$PTS), sd = sd(df$PTS)))
  reb = round(rnorm(n = g, mean = mean(df$TRB), sd = sd(df$TRB)))
  ast = round(rnorm(n = g, mean = mean(df$AST), sd = sd(df$AST)))
  sim = data.frame(pts, reb, ast)
  occ_ = sim %>% filter(pts == 27, reb == 7, ast == 7) %>% nrow()
  occ = c(occ, occ_)
  print(i)
}
sim = data.frame(PTS = rnorm(n = 2e5, mean = mean(df$PTS), sd = sd(df$PTS)),
           TRB = rnorm(n = 2e5, mean = mean(df$TRB), sd = sd(df$TRB)),
           AST = rnorm(n = 2e5, mean = mean(df$AST), sd = sd(df$AST))) 
cplot1a = sim %>% ggplot(aes(x = PTS, y = TRB)) + geom_hex(alpha = I(1/2)) + scale_fill_viridis_c() + theme_minimal() + guides(fill = F) +
  scale_x_continuous(breaks = seq(0,60,5)) + scale_y_continuous(breaks = seq(0,20,5))
cplot2a = sim %>% ggplot(aes(x = PTS, y = AST)) + geom_hex(alpha = I(1/2)) + scale_fill_viridis_c() + theme_minimal() + guides(fill = F) +
  scale_x_continuous(breaks = seq(0,60,5)) + scale_y_continuous(breaks = seq(0,20,5))
cplot3a = sim %>% ggplot(aes(x = TRB, y = AST)) + geom_hex(alpha = I(1/2)) + scale_fill_viridis_c() + theme_minimal() + guides(fill = F) +
  scale_x_continuous(breaks = seq(0,60,5)) + scale_y_continuous(breaks = seq(0,20,5))

occ %>% table() %>% as.data.frame() %>% mutate(Freq = Freq/sum(Freq), reality = ifelse(. == 0, "Y", "N")) %>% 
  ggplot(aes(x = ., y = Freq, fill = reality)) + geom_bar(stat = "identity", width = I(1/2)) +
  scale_x_discrete("Occurrences of LEBRON") +
  scale_y_continuous("Percent of Simulations", breaks = seq(0,.35,.05)) +
  theme_bw() + scale_fill_manual(values = c("grey60", "black")) + 
  theme(legend.position = "none") + 
  ggtitle("Results of Approximation", "Independent Draws from Normal Distribution, Career")
ggsave("images/plot8.png", width = 7.75, height = 5.5)
mean(occ == 0)
complete_log = complete_log %>% rbind.data.frame(cbind.data.frame(type = "Simulation (Independent Normal Draws, Career)", est = mean(occ == 0)))

# Approximation 4: Drawing from Multivariate Normal (career) (assumes dependence)
corrplot::corrplot.mixed(cor(df[,c("PTS", "TRB", "AST")]), lower.col = "black", number.cex = .7)
pl1 = df %>% ggplot(aes(x = PTS, y = TRB)) + geom_smooth(method = "lm", formula = 'y ~ x', se = F) + geom_point(alpha = I(1/3)) + theme_bw()
pl2 = df %>% ggplot(aes(x = PTS, y = AST)) + geom_smooth(method = "lm", formula = 'y ~ x', se = F) + geom_point(alpha = I(1/3)) + theme_bw()
pl3 = df %>% ggplot(aes(x = TRB, y = AST)) + geom_smooth(method = "lm", formula = 'y ~ x', se = F) + geom_point(alpha = I(1/3)) + theme_bw()
pl4 = df %>% select(PTS, TRB, AST) %>% gather("Type", "Value") %>% ggplot(aes(x = Type, y = Value, fill = Type)) + geom_violin() + coord_flip() + guides(fill = F) + scale_x_discrete("") + scale_y_continuous("") + theme_bw()
ggpubr::ggarrange(pl1, pl2, pl3, pl4, nrow = 2, ncol = 2)
ggsave("images/plot9.png", width = 7.75, height = 5.5)
# note: these plots show that points/rebounds/assists are not independent.

### 
df = read.csv("LeBronGameLog.csv")[,-1]
b_df = df[,c("PTS", "TRB", "AST")]
mu_0 = colMeans(b_df); sigma_0 = cov(b_df)
sample = mvrnorm(n = 2e5, mu = mu_0, Sigma = sigma_0)
sample_mvrnorm = sample %>% as.data.frame()

ptsast = b_df %>% table() %>% data.frame() %>% select(PTS, AST, Freq) %>% filter(Freq > 0) %>% arrange(desc(Freq)) %>% group_by(PTS, AST) %>% summarize(.groups = "drop", count = sum(Freq)) %>% arrange(desc(count)) %>% mutate(PTS = as.numeric(PTS), AST = as.numeric(AST))
ptstrb = b_df %>% table() %>% data.frame() %>% select(PTS, TRB, Freq) %>% filter(Freq > 0) %>% arrange(desc(Freq)) %>% group_by(PTS, TRB) %>% summarize(.groups = "drop", count = sum(Freq)) %>% arrange(desc(count)) %>% mutate(PTS = as.numeric(PTS), TRB = as.numeric(TRB))
trbast = b_df %>% table() %>% data.frame() %>% select(AST, TRB, Freq) %>% filter(Freq > 0) %>% arrange(desc(Freq)) %>% group_by(AST, TRB) %>% summarize(.groups = "drop", count = sum(Freq)) %>% arrange(desc(count)) %>% mutate(AST = as.numeric(AST), TRB = as.numeric(TRB))

cplot1b = sample_mvrnorm %>% ggplot(aes(x = PTS, y = TRB)) + geom_hex(alpha = I(1/3)) + scale_fill_viridis_c() + theme_minimal() +
  geom_point(data = ptsast, aes(x = PTS, y = AST, alpha = count))+ guides(fill = F) + scale_x_continuous(breaks = seq(0,60,5)) + 
  scale_y_continuous(breaks = seq(0,20,5)) + scale_alpha_continuous("Number of Games") + theme(legend.position = "none")
cplot2b = sample_mvrnorm %>% ggplot(aes(x = PTS, y = AST)) + geom_hex(alpha = I(1/3)) + scale_fill_viridis_c() + theme_minimal() +
  geom_point(data = ptsast, aes(x = PTS, y = AST, alpha = count)) + guides(fill = F) + scale_x_continuous(breaks = seq(0,60,5)) + 
  scale_y_continuous(breaks = seq(0,20,5)) + scale_alpha_continuous("Number of Games") + theme(legend.position = "none")
cplot3b = sample_mvrnorm %>% ggplot(aes(x = TRB, y = AST)) + geom_hex(alpha = I(1/3)) + scale_fill_viridis_c() + theme_minimal() +
  scale_x_continuous(breaks = seq(0,20,5)) + scale_y_continuous(breaks = seq(0,20,5)) +
  geom_point(data = trbast, aes(x = TRB, y = AST, alpha = count)) + scale_alpha_continuous("Number of Games") + 
  guides(fill = F) + theme(legend.position = "none")
ggpubr::ggarrange(cplot1a, cplot1b, nrow = 1, ncol = 2)
ggsave("images/plot10.png", width = 7.75, height = 4)
ggpubr::ggarrange(cplot2a, cplot2b, nrow = 1, ncol = 2)
ggsave("images/plot11.png", width = 7.75, height = 4)
ggpubr::ggarrange(cplot3a, cplot3b, nrow = 1, ncol = 2)
ggsave("images/plot12.png", width = 7.75, height = 4)
  
career = mvrnorm(n = nrow(b_df), mu = mu_0, Sigma = sigma_0) %>% as.data.frame()
career = career %>% transmute(estPTS = round(PTS), estTRB = round(TRB), estAST = round(AST)) %>% cbind.data.frame(b_df)

career %>% ggplot() + geom_density(aes(x = estPTS), fill = "grey35", alpha = I(1/4)) + 
  geom_histogram(alpha = I(1/2), aes(x = PTS, y = ..density..), binwidth = 2, fill = "steelblue") + 
  theme_bw() + scale_x_continuous("Points", breaks = seq(0,60,7.5))
career %>% ggplot() + geom_density(aes(x = estTRB), fill = "grey35", alpha = I(1/4)) + 
  geom_histogram(alpha = I(1/2), aes(x = TRB, y = ..density..), binwidth = 1, fill = "steelblue") + 
  theme_bw() + scale_x_continuous("Rebounds", breaks = seq(0,60,2.5))
career %>% ggplot() + geom_density(aes(x = estAST), fill = "grey35", alpha = I(1/4)) + 
  geom_histogram(alpha = I(1/2), aes(x = AST, y = ..density..), binwidth = 1, fill = "steelblue") + 
  theme_bw() + scale_x_continuous("Assists", breaks = seq(0,60,2.5))

occurrences = c()
for (i in 1:1000){
  sim_career = mvrnorm(n = 1310, mu = mu_0, Sigma = sigma_0) %>% as.data.frame() %>% transmute(estPTS = round(PTS), estTRB = round(TRB), estAST = round(AST))
  sim_career[which((sim_career$estAST == 7 & sim_career$estPTS == 27 & sim_career$estTRB == 7)),]
  occurrences = c(occurrences, length(which((sim_career$estAST == 7 & sim_career$estPTS == 27 & sim_career$estTRB == 7))))
  print(i)
}
data.frame(occurrences) %>% group_by(occurrences) %>% summarize(.groups = "drop", count = n()) %>% mutate(prop = count/sum(count), ind = ifelse(occurrences == 0, "y", "n"), occurrences = factor(occurrences)) %>% 
  ggplot(aes(x = occurrences, y = prop, fill = ind)) + geom_bar(stat = "identity", width = I(1/2)) + guides(fill = F) + 
  scale_x_discrete("Occurrences of LEBRON") +
  scale_y_continuous("Percent of Simulations", breaks = seq(0,.4,.05)) +
  theme_bw() + scale_fill_manual(values = c("grey60", "black")) + 
  theme(legend.position = "none") + 
  ggtitle("Results of Approximation", "Multivariate Draws from Normal Distribution, Career")
ggsave("images/plot13.png", width = 7.75, height = 5.5)
mean(occurrences == 0)
complete_log = complete_log %>% rbind.data.frame(cbind.data.frame(type = "Simulation (Multivariate Normal Draws, Career)", est = mean(occurrences == 0)))

# Approximation 5: Drawing from Multivariate Normal (by season) (assumes dependence)
corr_df = NULL
years = df %>% group_by(yr) %>% summarize(.groups = "drop", count = n())
for (j in 1:nrow(years)){
  df_ = df %>% filter(yr == years$yr[j])
  b_df = df_[,c("PTS", "TRB", "AST")]
  corr_df = rbind.data.frame(corr_df, data.frame(yr = years$yr[j], PTSTRB = cor(b_df)[1,2], PTSAST = cor(b_df)[1,3], TRBAST = cor(b_df)[2,3]))
}
corr_df %>% gather("stat", "Correlation", -yr) %>% 
  mutate(stat = ifelse(stat == "PTSTRB", "PTS & TRB",ifelse(stat == "PTSAST", "PTS & AST", "TRB & AST")), 
         abs = ifelse(Correlation < 0, "neg", "pos")) %>% 
  ggplot(aes(x = yr, y = Correlation, fill = abs)) + 
  geom_bar(stat = "identity") + facet_grid(stat ~ .) +
  guides(fill = F) + scale_x_continuous("Year", breaks = seq(2004,2020,2)) +
  geom_hline(yintercept = 0, color = "grey35")

occ = NULL
for (i in 1:1000){
  for (j in 1:nrow(years)){
    df_ = df %>% filter(yr == years$yr[j])
    b_df = df_[,c("PTS", "TRB", "AST")]
    mu_0 = colMeans(b_df); sigma_0 = cov(b_df)
    sample = mvrnorm(n = nrow(b_df), mu = mu_0, Sigma = sigma_0) %>% as.data.frame()
    sample = sample %>% mutate(PTS = round(PTS), TRB = round(TRB), AST = round(AST))
    occurs = sample %>% filter(PTS == 27, TRB == 7, AST == 7) %>% nrow()
    occ = rbind.data.frame(occ, data.frame(sim = i, szn = years$yr[j], occurs))
  }
  print(i)
}

sims = occ %>% group_by(sim) %>% summarise(.groups = "drop", occurs = sum(occurs))
table(sims$occurs) %>% as.data.frame() %>% mutate(Freq = Freq/sum(Freq), reality = ifelse(Var1 == 0, "Y", "N")) %>%
  ggplot(aes(x = Var1, y = Freq, fill = reality)) + geom_bar(stat = "identity", width = I(1/2)) +
  scale_x_discrete("Occurrences of LEBRON") +
  scale_y_continuous("Percent of Simulations", breaks = seq(0,.5,.05)) +
  theme_bw() + scale_fill_manual(values = c("grey60", "black")) + 
  theme(legend.position = "none") + 
  ggtitle("Results of Approximation", "Multivariate Normal Sampling, Individual Seasons")
ggsave("images/plot14.png", width = 7.75, height = 5.5)
mean(sims$occurs == 0)
complete_log = complete_log %>% rbind.data.frame(cbind.data.frame(type = "Simulation (Multivariate Normal Draws, Season)", est = mean(sims$occurs == 0)))

byszn = occ %>% group_by(szn)
table(byszn$szn, byszn$occurs) %>% as.data.frame() %>% set_names(nm = c("Year", "Value", "Occurrences")) %>% mutate(tm = ifelse(Year %in% c(seq(2003,2010), seq(2015,2018)), "CLE", ifelse(Year %in% 2011:2014, "MIA", "LAL"))) %>% 
  filter(Value == 0) %>% mutate(Occurrences = 1-(Occurrences/1000)) %>% 
  ggplot(aes(x = Year, y = Occurrences, fill = tm)) + geom_bar(stat = "identity") + 
  scale_y_continuous("Probability that at least one LEBRON occurred", limits = c(0,.125)) + theme_bw() +
  scale_fill_manual(values = c("#860038", "#552583", "black")) + scale_x_discrete("") +
  guides(fill = F) + ggtitle("Which seasons had the highest LEBRON probability?", "Multivariate Normal Draws")
ggsave("images/plot15.png", width = 7.75, height = 5.5)

complete_log %>% distinct(type, .keep_all = T) %>% mutate(a = 1-est) %>% gather("this", "that", -type) %>% 
  ggplot(aes(x = type, y = that, fill = this)) + geom_bar(alpha = I(4/5), stat = "identity") + 
  coord_flip() + guides(fill = F) + scale_fill_manual(values = c("grey70", "red2")) +
  theme_bw() + scale_x_discrete("") + scale_y_continuous("Approximate Probability of no LEBRON")
complete_log = complete_log %>% set_names(c("Approximation Type", "Probability"))
formattable::formattable(complete_log)



