class Work {

      int work_field;
}

class Test {
  int x;

  int getSum(int n) {
      int i;
      int sum;
      for (i = n; i > -1 ; i = i - 1) {
      	  sum = sum + i;
      }
      return sum;
  }
  
  int main(int y) {
     x = 10;
     x = getSum(x);
//     print(x);
     return 0;
   }
}
