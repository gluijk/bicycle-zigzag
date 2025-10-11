# Calculating the effort saved by zigzagging uphill on a bicycle
# www.overfitting.net
# https://www.overfitting.net/2025/10/subiendo-cuestas-en-bicicleta.html

library(Cairo)

# Fine grid of values
RESOL=10
minalpha=0; maxalpha=30
alpha <- seq(from=minalpha, to=maxalpha, length.out=(maxalpha-minalpha+1)*RESOL)

mintheta=0; maxtheta=90
theta <- seq(from=mintheta, to=maxtheta, length.out=(maxtheta-mintheta+1)*RESOL)

# Convert to radians
alpha_rad <- alpha * pi / 180
theta_rad <- theta * pi / 180

# Build matrix: rows = alpha, cols = theta
M <- outer(alpha_rad, theta_rad, function(a, t) sin(a) * cos(t))
# Add row and column names
rownames(M) <- alpha      # alpha values
colnames(M) <- theta      # theta values

# Write data
# write.csv2(M, "bike.csv")

NCOL=10
CairoPNG("heatmap.png", width=1920, height=1080)
    # Plot using original increasing alpha
    image(theta, alpha, t(M),
          # col = gray.colors(NCOL, start = 1, end = 0.25),
          col = cm.colors(NCOL),
          xlab = "Deviation from straight path up (º)",
          ylab = "Slope of terrain (º)",
          main = "Effort per unit distance: dW/ds ~ Δh/Δs = sin(Slope) * cos(Deviation)",
          cex.main = 2.5, cex.lab = 1.5,
          asp=1, axes = FALSE)
    abline(h=c(minalpha, maxalpha), v=c(mintheta, maxtheta))

    # Tick marks every 10°
    ticksalpha <- seq(minalpha, maxalpha, by = 10)
    tickstheta <- seq(mintheta, maxtheta, by = 10)
    axis(1, at = tickstheta, labels = tickstheta, cex.axis = 2)
    axis(2, at = ticksalpha, labels = ticksalpha, cex.axis = 2)
    
    contour(theta, alpha, t(M),
            add = TRUE,
            levels = seq(min(M), max(M), length.out=NCOL+1),
            drawlabels = TRUE,  # set to FALSE to hide numbers
            labelfmt = "%.3f",
            lwd = 1.2, labcex = 1.2, col = 'black')
    
    # Example 1: angle to get slope (effort) halved, for a max slope of dh/ds=0.25
    value_1_4=asin(1/4)  # slope to get dh/ds=0.25
    print(paste0("Slope at which dh/ds=0.25: ", round(value_1_4*180/pi,1), "º"))
    i=as.integer(which.min(abs(M[,1]-value_1_4)))
    vec=M[i,]
    j=as.integer(which.min(abs(vec-max(vec)/2)))
    print(paste0("Must be around 0.25/2=0.125: ", M[i,j]))
    abline(h=value_1_4*180/pi, v=(j-1)/(length(theta)-1)*maxtheta,
           col='red', lty='dotted')
    print(paste0("You must ride ", round((j-1)/(length(vec)-1)*maxtheta,1),
                 "º deviated from straight line"))
    
    # Example 2: slope (effort) reduction in % for a Deviatio of 45º,
    # for a max slope of dh/ds=0.25
    print(paste0("Effort reduction for 45º of Deviation: ",
                 round((1-vec[round(45/maxtheta*length(vec))]/max(vec))*100,1),
                 "%"))
dev.off()


# Example 1 and 2 plots
plot(theta, vec, type='l', xlim=c(mintheta,maxtheta), ylim=c(0,max(vec)),
     main='Δh/Δs reduction (%) a function of Deviation',
     xlab='Deviation from straight path up (º)',
     ylab='dW/ds ~ Δh/Δs (%)',
     axes=FALSE)
box()
axis(1, at = tickstheta, labels = tickstheta, cex.axis = 1)
axis(2, at = seq(0, max(vec), length.out=11),
     labels = seq(0,100, length.out=11), cex.axis = 1)
# Example 1
abline(h=M[i,j], v=round((j-1)/(length(vec)-1)*maxtheta),
       col='red', lty='dotted')
# Example 2
abline(h=vec[round(45/maxtheta*length(vec))], v=45, col='red', lty='dotted')
       