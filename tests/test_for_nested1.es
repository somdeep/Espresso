class Test {

int main(int a) {
    int x;
    int y;
	for(x =0; x <10;x = x +1) {
	    x = x +2;

	    for(y = 7; y < 20 ; y = y+2){
	    	y = y + 1 ;
			if(x ==2 && y==8)
			    print_int(x+y);
	    }
	} 
    return 0;
   }
}