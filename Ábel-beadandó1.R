#4. Hozz létre egy fgv-t, amelyik data.frame-t vár bemenetként, illetve két oszlop (változó) nevet 
#és kimenetként a két változóra készít egy scatterplot-t, amit kirajzol. 
#Opcionálisan lehessen tetszõleges "title"-t hozzáadni, illetve képként menteni is az eredményt.


ess= read.spss("ESS6_HUN_autotranslate.sav",
                rownames= F,
                Stringsasfactors= T,
                tolower= F,
                as.data.frame= T,
                reencode= T)
ess= data.frame (ess)


#csak a plot
fgv = function (m,c,d){
  if (is.data.frame(m)){
     if (is.numeric(c)){
       if (is.numeric(d)){
      plot (x=c, y=d,type="p")
       }   
    }
  }
}


fgv1 (mtcars,mtcars$hp,mtcars$drat,"remek","zsír")

#az egész egybe
fgv1 = function (m,c,d,t,k) { 
  png (filename =k)
  if (is.data.frame(m)) {
    if (is.numeric(c)){
      if (is.numeric(d)){
        plot (x=c, y=d,type="p",main=t)
      }
    }
  }
   dev.off()
        plot (x=c, y=d,type="p",main=t)
}




