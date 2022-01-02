#4. Hozz létre egy fgv-t, amelyik data.frame-t vár bemenetként, illetve két oszlop (változó) nevet 
#és kimenetként a két változóra készít egy scatterplot-t, amit kirajzol. 
#Opcionálisan lehessen tetszõleges "title"-t hozzáadni, illetve képként menteni is az eredményt.


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

#javított: ha még nincsen kész plot, akkor nem tud plotot kiadni, csak ha azonnal 
#menteni is akarod. Ha már van kinnt egy plot, akkor tudja mentés nélkül is kiírni.
#vagyis az op="no" legelsőre nem működik.De ha már van egy megjelenített plot, akkor már az is megy.

#őszintén szólva lövésem sincs, hogy ezt hogyan tudnám megoldani; azt írta, hogy a dev.off()-al van valami probléma,
#de ha azt máshova rakom, akkor meg mindig valami más romlik el.
#A factorizálással is próbálkoztam, de sehogy sem akart belemenni. Erre:
ifelse(is.numeric(c),
    c1 <- c,
    c1 <-as.factor(c)
  )

ifelse(is.numeric(d),
       d1 <- d,
       d1 <-as.factor(d)
)  
plot (x=c1, y=d1,type="p",main=t)
#azt írta, hogy a plot nem találja a c1 objektumot. Az se segített, ha mindenhonnan levettem az egyest, és mindig önmagát írtam felül.
#szóval jelen pillanatban ez a végleges változat:

fgv2 = function (m,c,d,t,op="no",k) {
  
  if (!op =="no") {
    
    if (is.data.frame(m)) {
      if("c" %in% colnames(m)){
        if("d" %in% colnames(m)){
          plot (x=as.numeric(c), y=as.numeric(d),type="p",main=t)
        }
      }
    }
    png (filename =k)
    
    print(plot (x=as.numeric(c), y=as.numeric(d),type="p",main=t))
  }
  
  else (op =="no") 
  
  if (is.data.frame(m)) {
    if("c" %in% colnames(m)){
      if("d" %in% colnames(m)){
        plot (x=as.numeric(c), y=as.numeric(d),type="p",main=t)
      }
    }
  }
  dev.off()
  print(plot (x=as.numeric(c), y=as.numeric(d),type="p",main=t))
}

fgv2(mtcars,mtcars$disp,mtcars$hp,"teszt","n","teszt")
#a 42. sori else-hez se az Rstudió, se a sima R szerint nem kell "{". Mármint hogy nem fogadták el azzal, nekem csak így futott le.

