class Work {

      int work_field;
      int get_work_from_work() {
      	  return 0;
      }

}

class Test {
  int x;

      int get_work() {
      	  return 0;
      }


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
     class Work w;
     x = getSum(x) + get_work() + w.get_work_from_work();
     print_int(x);
//     print(x);
     return 0;
   }
}
