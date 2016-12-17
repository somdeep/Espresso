class Work {
    int work_field;
}

class Test {
  int x;
  int main(int y) {
    class Work wObj;
	wObj.work_field=5;
    class Work [5]wArr;
    wArr[4] = wObj;
    print_int(wArr[4].work_field);     
     return 0;
   }
}