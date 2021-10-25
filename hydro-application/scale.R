pal11 <- colorRampPalette(c("green3","lightgray","indianred4"))

pdf("figures/scale-app.pdf", width = 1, height = 3)
par(mar=c(.1,2,.1,.1))
plot(cbind(1,seq(0,1,length.out = 11)), col=pal11(100)[seq(51,100,length.out=11)], pch=19, cex=2, yaxt="n", xaxt="n")
axis(2,at = seq(0,1,.5))

dev.off()


