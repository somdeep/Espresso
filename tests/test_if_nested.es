class Test {

int main(int a) {
    bool x;
    x = true;
    bool y;
    y = true;
    int z;
    z = 1;
    if(x){
    	z = z+2;    
    	if (y) {
    		z = z+3;
    	}
    }
    print_int(z);
    return 0;
   }
}