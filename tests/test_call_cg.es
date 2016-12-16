class Test {

      int get(int x) {
      	  int y;
	  y = 10 + x;
      	  return y * y;
      }

      int main() {
      	  int x;
	  x = get(2);
	  print_int(x);
	  if (x == 144) {
	     print_string("got value 144");
	  } else {
	    print_string("f*cked up!");
	  }
	  return 0;
      }

}

