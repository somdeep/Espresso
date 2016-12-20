
class int_array_list {
      // datastructure used to represent the arraylist
      int [20] arr;

      // maintains the number of elements currently stored in the arraylist
      int count;

      // maintains the actual size reserved for the arraylist
      int size;

      // constructor - call this method explicitly before invoking any other methods
      void init(class int_array_list self, int [10] array) {
      	   self.count = 0;
	   self.size = 20;
	   self.arr = array;
      }

      // add an element to the array list
      void add(class int_array_list self, int val) {
      	   if (self.count == self.size) {
	      // resize array -  (since we used 'int' for the arraytype declaration in the parser, we are forced to use a constant here :(
	      int [1024] new_arr;

	      // copy all elements
	      int i;
	      for(i = 0; i < self.count; i = i + 1) {
	      	    new_arr[i] = self.arr[i];
	      }
	      
	      // switch to the new array
	      self.arr = new_arr;
	   }	   
	   
	   // at this point, we know that there is space to hold at least one value
	   self.arr[self.count] = val;
	   self.count = self.count + 1;
      }

      // remove the element val from the array list - removes only the first occurence
      int remove(class int_array_list self, int val) {
      	   int i;
	   int pos;
	   int removed_val;

	   pos = -1;
	   for(i = 0; i < self.count; i = i + 1) {
	   	 if (self.arr[i] == val) {
		    pos = i;
		    break;
		 }
	   }

	   if (pos == -1) {
	      return -1;
	   }
	   removed_val = self.arr[pos];
	   
      	   // fill the every slot with the value from the next adjacent slot
	   for(i = pos; i < self.count; i = i + 1) {
	   	 self.arr[i] = self.arr[i + 1];
	   }

	   self.count = self.count - 1;
	   return removed_val;
      }
      
}


class Test {

      int main() {
      	  class int_array_list al;
	  int i;
	  int count;
	  count = 5;
	  int [5] arr;
	  
	  al.init(al, arr);
	  for(i = 0; i < count; i = i + 1) {
	  	al.add(al, i);	
	  }

	  print_string("******** printing from array list ************\n");
	  for(i = 0; i < count; i = i + 1) {
	  	print_int(al.arr[i]);
		print_string(" ");
	  }
	  print_string("\n******** removing elements from array list **************\n");

	  for(i = 0; i < count; i = i + 1) {
	  	int removed_val;
		removed_val = al.remove(al, i);
		if (removed_val == i) {
		   print_int(removed_val);
		   print_string(" ");
		} else {
		  print_string("error removing: ");
		  print_int(i);
		  print_string("\n");
		}
	  }
	  
	  print_string("\n");
	  
	  return 0;
      }
}