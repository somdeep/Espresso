class Test {

  int main(int y) {
     int [10]arr;
     int i;
     i = 0;
     for (i=0;i<10;i=i+1){
         arr[i]=i;
     }
     int sum;
     sum = 0;
     foreach(int c: arr){
     sum = sum + c;
     }
     print_int(sum);     
     return sum;
   }
}
