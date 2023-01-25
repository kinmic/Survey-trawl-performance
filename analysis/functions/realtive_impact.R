# This function detrmines how swell is striking a vessel
# Vessell bearing should be compass bearing ( 0 - 360), as integer
# Swell bearing should be compass bearing ( 0 - 360), as integer
# Some effort is made to wquantify


relative_impact<- function(vessel_bearing,swell_direction){
  port_range<-30:120
  stb_range <- 240:330
  d_z<-as.list(vessel_bearing+port_range,
               vessel_bearing+stb_range)
  #Port
  if(vessel_bearing < 180){
    if(swell_direction %in% d_z[1]){
      print("starboard")
    } else if (!swell_direction %in% d_z[1]){
      print("stern")
    
    }
  else {
    if (swell_direction %in% d_z[2]){
      print("port")
    } else(!swell_direction %in% d_z[2])
    print("bow")
   }
  }
 }



relative_impact(90,90)

  
  
  
  #Startboard
  
  
}
vb<-1
x<- vb + 30:120
print(x)

y<-90

if(y)

  y %in% x
