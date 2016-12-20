class Work {

      int work_field;
      class Test test_obj;
      int mem_func(char c) {
      	  int x ;
	        x = 5;
	     return x;
      }
}

class Test {
  int x;

  int main(int y) {
     class Work w;
     w.work_field =  w.mem_func('a');
	   print_int(w.work_field);
     return 0;
   }
}
