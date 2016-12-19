class Work {

  int work_field;
	work_field = 2;//error
  int get_work_from_work() {
    return work_field;
  }

}

class Test {
 
  int main() {
     x = 10;
     class Work w;
     x = x + w.get_work_from_work();
     print_int(x);
     return 0;
   }
}
