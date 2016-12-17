class Test {
  //int x;
  int main(int y) {
    int [10]arr;
    arr[5] = 3;
    int x;
    x = arr[5]; //error
    arr[5] = arr[5] + 10*x-1; //error
    arr[5] = 10*x-1;
    print_int(arr[5]);
    return 0;
   }
}
