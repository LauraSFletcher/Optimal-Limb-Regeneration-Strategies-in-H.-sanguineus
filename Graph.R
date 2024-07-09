###############
#### Graph ####
###############

#dev.off() #Run this to reset the graphics device (graphs aren't showing up, figure margins too large, etc.).

graph <- function (c,p,i) {
  #Set custom color palettes. 
  color0 <- colorRampPalette(c("violet","red","pink","orange","yellow","white","green","blue","purple","brown","black","gray"))(12)
  color1 <- colorRampPalette(c("violet","red","pink","orange","yellow","white","green","blue","purple","brown","black"))(11)
  color2 <- colorRampPalette(c("violet","red","pink","orange","yellow","white","green","blue","purple","brown"))(10)
  color3 <- colorRampPalette(c("violet","red","pink","orange","yellow","white","green","blue","purple"))(9) 
  color4 <- colorRampPalette(c("violet","red","pink","orange","yellow","white","green","blue"))(8) 
  color5 <- colorRampPalette(c("violet","red","pink","orange","yellow","white","green"))(7)
  color6 <- colorRampPalette(c("violet","red","pink","orange","yellow","white"))(6)
  color7 <- colorRampPalette(c("violet","red","pink","orange","yellow"))(5) 
  color8 <- colorRampPalette(c("violet","red","pink","orange"))(4) 
  color9 <- colorRampPalette(c("violet","red","pink"))(3)
  color10 <- colorRampPalette(c("violet","red"))(2)
  color11 <- colorRampPalette(c("violet"))(1)
  
  #key:
  #strategy 1 = red (all reproduction)
  #strategy 2 = pink (2/3 reproduction, 1/3 growth)
  #strategy 3 = orange (2/3 reproduction, 1/3 regeneration)
  #strategy 4 = yellow (1/3 reproduction, 2/3 growth)
  #strategy 5 = white (1/3 reproduction, 2/3 regeneration)
  #strategy 6 = green (1/3 reproduction, 1/3 growth, 1/3 regeneration)
  #strategy 7 = blue (all growth)
  #strategy 8 = purple (all regeneration)
  #strategy 9 = brown (2/3 growth, 1/3 regeneration)
  #strategy 10 = black (1/3 growth, 2/3 regeneration)
  #no optimal strategy (0 fecundity for all strategies) = violet

  for (c in Cstate) {
    for (p in Pstate) {
      for (i in injury) { #Each individual graph will represent strategies for each combination of injury, consumption, and energy states.
        heatmap (
          beststrategy [,,c,i,p],
          Rowv = NA,
          Colv = NA,
          scale = "none", 
          col = #Assign correct color pallet based on minimum and maximum values in this section of the array. 
            if (max(beststrategy[,,c,i,p], na.rm = T)==11) {color0} 
          else if (max(beststrategy[,,c,i,p], na.rm = T)==10) {color1} 
          else if (max(beststrategy[,,c,i,p], na.rm = T)==9) {color2} 
          else if (max(beststrategy[,,c,i,p], na.rm = T)==8) {color3}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==7) {color4}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==6) {color5}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==5) {color6}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==4) {color7}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==3) {color8}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==2) {color9}
          else if (max(beststrategy[,,c,i,p], na.rm = T)==1){color10}
          else {color11},
          xlab = "Body Size (1-12)",
          ylab = "Month",
          #Designate state variable values for each graph. 
          main = if (c == 1) {
            if (p == 1) {
              if (i == 1) {"0.5x Consumption, Energy = 1, Injured"}
              else if (i == 2) {"0.5x Consumption, Energy = 1, 1/4 Regenerated"}
              else if (i == 3) {"0.5x Consumption, Energy = 1, 1/2 Regenerated"}
              else if (i == 4) {"0.5x Consumption, Energy = 1, 3/4 Regenerated"}
              else {"0.5x Consumption, Energy = 1, Uninjured"}
            }
            else if (p == 2) {
              if (i == 1) {"0.5x Consumption, Energy = 2, Injured"}
              else if (i == 2) {"0.5x Consumption, Energy = 2, 1/4 Regenerated"}
              else if (i == 3) {"0.5x Consumption, Energy = 2, 1/2 Regenerated"}
              else if (i == 4) {"0.5x Consumption, Energy = 2, 3/4 Regenerated"}
              else {"0.5x Consumption, Energy = 2, Uninjured"}
            }
            else if (p == 3) {
              if (i == 1) {"0.5x Consumption, Energy = 3, Injured"}
              else if (i == 2) {"0.5x Consumption, Energy = 3, 1/4 Regenerated"}
              else if (i == 3) {"0.5x Consumption, Energy = 3, 1/2 Regenerated"}
              else if (i == 4) {"0.5x Consumption, Energy = 3, 3/4 Regenerated"}
              else {"0.5x Consumption, Energy = 3, Uninjured"}
            }
            else if (p == 4) {
              if (i == 1) {"0.5x Consumption, Energy = 4, Injured"}
              else if (i == 2) {"0.5x Consumption, Energy = 4, 1/4 Regenerated"}
              else if (i == 3) {"0.5x Consumption, Energy = 4, 1/2 Regenerated"}
              else if (i == 4) {"0.5x Consumption, Energy = 4, 3/4 Regenerated"}
              else {"0.5x Consumption, Energy = 4, Uninjured"}
            }
          }
          else if (c == 2) {
            if (p == 1) {
              if (i == 1) {"1x Consumption, Energy = 1, Injured"}
              else if (i == 2) {"1x Consumption, Energy = 1, 1/4 Regenerated"}
              else if (i == 3) {"1x Consumption, Energy = 1, 1/2 Regenerated"}
              else if (i == 4) {"1x Consumption, Energy = 1, 3/4 Regenerated"}
              else {"1x Consumption, Energy = 1, Uninjured"}
            }
            else if (p == 2) {
              if (i == 1) {"1x Consumption, Energy = 2, Injured"}
              else if (i == 2) {"1x Consumption, Energy = 2, 1/4 Regenerated"}
              else if (i == 3) {"1x Consumption, Energy = 2, 1/2 Regenerated"}
              else if (i == 4) {"1x Consumption, Energy = 2, 3/4 Regenerated"}
              else {"1x Consumption, Energy = 2, Uninjured"}
            }
            else if (p == 3) {
              if (i == 1) {"1x Consumption, Energy = 3, Injured"}
              else if (i == 2) {"1x Consumption, Energy = 3, 1/4 Regenerated"}
              else if (i == 3) {"1x Consumption, Energy = 3, 1/2 Regenerated"}
              else if (i == 4) {"1x Consumption, Energy = 3, 3/4 Regenerated"}
              else {"1x Consumption, Energy = 3, Uninjured"}
            }
            else if (p == 4) {
              if (i == 1) {"1x Consumption, Energy = 4, Injured"}
              else if (i == 2) {"1x Consumption, Energy = 4, 1/4 Regenerated"}
              else if (i == 3) {"1x Consumption, Energy = 4, 1/2 Regenerated"}
              else if (i == 4) {"1x Consumption, Energy = 4, 3/4 Regenerated"}
              else {"1x Consumption, Energy = 4, Uninjured"}
            }
          }
          else { #c = 3
            if (p == 1) {
              if (i == 1) {"2x Consumption, Energy = 1, Injured"}
              else if (i == 2) {"2x Consumption, Energy = 1, 1/4 Regenerated"}
              else if (i == 3) {"2x Consumption, Energy = 1, 1/2 Regenerated"}
              else if (i == 4) {"2x Consumption, Energy = 1, 3/4 Regenerated"}
              else {"2x Consumption, Energy = 1, Uninjured"}
            }
            else if (p == 2) {
              if (i == 1) {"2x Consumption, Energy = 2, Injured"}
              else if (i == 2) {"2x Consumption, Energy = 2, 1/4 Regenerated"}
              else if (i == 3) {"2x Consumption, Energy = 2, 1/2 Regenerated"}
              else if (i == 4) {"2x Consumption, Energy = 2, 3/4 Regenerated"}
              else {"2x Consumption, Energy = 2, Uninjured"}
            }
            else if (p == 3) {
              if (i == 1) {"2x Consumption, Energy = 3, Injured"}
              else if (i == 2) {"2x Consumption, Energy = 3, 1/4 Regenerated"}
              else if (i == 3) {"2x Consumption, Energy = 3, 1/2 Regenerated"}
              else if (i == 4) {"2x Consumption, Energy = 3, 3/4 Regenerated"}
              else {"2x Consumption, Energy = 3, Uninjured"}
            }
            else if (p == 4) {
              if (i == 1) {"2x Consumption, Energy = 4, Injured"}
              else if (i == 2) {"2x Consumption, Energy = 4, 1/4 Regenerated"}
              else if (i == 3) {"2x Consumption, Energy = 4, 1/2 Regenerated"}
              else if (i == 4) {"2x Consumption, Energy = 4, 3/4 Regenerated"}
              else {"2x Consumption, Energy = 4, Uninjured"}
            }
          }
        )
      }
    }
  }
}



