#Code for solving Problem 1 is included in job 1.1, 1.2 and 1.3.
#Code for solving Problem 2 is included in job 2.

w3_chen <- function(job = 1.1){
  
  # Problem 1.b
  if (job == 1.1){
    par(mar = c(3,2,3,1))
    Ngrid = 30
    fxy = matrix(nrow = Ngrid, ncol = Ngrid)
    x = seq(from = -4, to = 2, length = Ngrid)
    y = seq(from = -3, to = 2, length = Ngrid)
    for(i in 1:Ngrid)
    for(j in 1:Ngrid)
        fxy[i, j] = (x[i]+1)^2+y[j]-x[i]+(x[i]-y[j])^2
    
    persp(x, y, fxy, theta = 320, phi = 20, r = 1, ticktype = "detailed",
          main = "Illustration of the function")
  }
  
    # Problem 1.c 
  if(job == 1.2){
    par(mar = c(4,4,5,1), mgp = c(2, 1, 0))
    Ngrid = 30
    fxy = matrix(nrow = Ngrid, ncol = Ngrid)
    x = seq(from = -4, to = 2, length = Ngrid)
    y = seq(from = -3, to = 2, length = Ngrid)
    for(i in 1:Ngrid)
      for(j in 1:Ngrid)
        fxy[i, j] = (x[i]+1)^2+y[j]-x[i]+(x[i]-y[j])^2
    
    contour(x, y, fxy,
            main = "Illustration of the contour of the function",
            xlab = "x", ylab = "y")
    points(-1, -1.5, pch = 17, col = 2)
    text(-1, -1.5, "Gradient = 0\nat (-1, -1.5)", pos = 4, cex = 0.7)
  }
  
    
  # Problem 1.d
  # Modification of w3(job == 1.1)
  # trans3d(x, y, z, pmat) -- Projection of 3-dimensional to 2-dimensional points using a 4x4 viewing transformation matrix.
  if(job == 1.3){
    expQ=function(x,y)	(x+1)^2+y-x+(x-y)^2
    
    Ngrid=40
    x=y=seq(from=-4,to=5,length=Ngrid)
    fexpM=matrix(ncol=Ngrid,nrow=Ngrid)
    for(i in 1:Ngrid)
      for(j in 1:Ngrid)
        fexpM[i,j]=expQ(x[i],y[j])
    
    for(lev in seq(from=10,to=0,by=-1))
    {
      par(mar = c(2,2,4,1))
      op=persp(x,y,fexpM,theta=-35,phi=5,r=10,ticktype="detailed", #adjusted for better camera angle.
               main = "Illustration of the contour on the surface",
               zlab = "f(x,y)")	 
      outl = contourLines(x,y,fexpM,levels=lev)
      x.out = outl[[1]]$x; y.out=outl[[1]]$y; nl=length(x.out)
      lines(trans3d(x.out, y.out, rep(0,nl), pmat=op),col=3,lwd=2)
      lines(trans3d(x.out, y.out, rep(lev,nl), pmat=op),col=2,lwd=2)
      Sys.sleep(.1)
    }
  }
  
  # Problem 2
  if(job == 2){
    x0 = 0; y0 = 0
    eps=0.0001
    for(k in 1:100)
    {
      fxy = (x0+1)^2 + y0 - x0 + (x0-y0)^2
      dfdx = 4*x0 -2*y0 + 1
      dfdy = 2*y0 - 2*x0 + 1
      xy=c(x0,y0); g=c(dfdx,dfdy)
      for(IL in 0:10)
      {
        xyL=xy-g/2^IL
        fxyIL=(xyL[1]+1)^2 + xyL[2] - xyL[1] + (xyL[1]-xyL[2])^2
        IL.try=IL
        if(fxyIL<fxy) break
      }
        
      if(max(abs(xy-xyL))<eps) break
      x0=xyL[1]; y0=xyL[2]	
      print(paste("x0=",xyL[1],"y0=",xyL[2],"f(x0,y0)=", fxyIL), sep="")
    }
  }
  
}


  