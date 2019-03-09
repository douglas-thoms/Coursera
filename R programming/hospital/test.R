median<-function(x){
        
        odd.even<-length(x)%%2 
        
        if (x == 6){    
                print("got here") 
        } else {
                (sort(x)[ceiling(length(x)/2)])
        }
        print(x)
} 
