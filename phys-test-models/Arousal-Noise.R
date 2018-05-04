require(ggplot2)
require(reshape2)

get_arousal_noise = function(id)
{
  theme_update(plot.title = element_text(hjust = 0.5))
  ArousalANS.mat = read.csv(paste0("C:/Users/Phys-Cog/ACT-R_Phi/arous-ans-log", id, ".txt"), header=FALSE)
  CECArousal.mat = read.csv(paste0("C:/Users/Phys-Cog/ACT-R_Phi/CEC-Arous", id, ".txt"))
  CECArousal.mat = cbind(matrix(1:1798, 1798, 1), CECArousal.mat)
  CECArousalANS.mat = CECArousal.mat
  colnames(CECArousalANS.mat)[1] = "Time"
  CECArousalANS.mat = cbind(CECArousalANS.mat, ArousalANS.mat[1:1798,3])
  colnames(CECArousalANS.mat)[6] = "Noise"
  CECArousalANSLong.mat = melt(CECArousalANS.mat, id="Time")
  CECArousalANSLong.plot = ggplot(as.data.frame(CECArousalANSLong.mat))
  fac_names = c(
    `f.Cortisol.` = "f(cortisol)",
    `g.Epinephrine.` = "g(epinephrine)",
    `h.CRH.` = "h(crh)",
    `Arousal` = "arousal",
    `Noise` = "noise"
  )
  CECArousalANSLong.plot + geom_line(aes(x=Time, y=value, color=variable), size=0.5) +
    labs(y="Normalized Effect (1 is a \"normal\" level)", x="Time (s)", title="Effects of stress physiology on arousal") +
    facet_grid(variable~., scales = "free_y", labeller = as_labeller(fac_names))
  
}