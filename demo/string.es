class string_buffer {

      char[10] arr;
      int len;
      int size;
      
      void init(class string_buffer self, String str) {
      	   self.arr = str;
	   self.len = 0;
	   char c;
	   c = 'a' - 'a';
	   while(self.arr[self.len] != c) {
	   	self.len = self.len + 1;
	   }
	   self.size = self.len;
      }

      int get_length(class string_buffer self) {
      	  return self.len;
      }

      void append(class string_buffer self, char ch) {
      	   if (self.size == self.len) {
	      // increase buffer size
	      char[1024] new_arr;
	      int i;
	      for(i = 0; i < self.len; i = i + 1) {
	      	    new_arr[i] = self.arr[i];
	      }
	      new_arr[i] = new_arr[i] - new_arr[i];
	      self.arr = new_arr;
	   }

	   self.arr[self.len] = ch;
	   self.len = self.len + 1;
      }

      void print_string_buffer(class string_buffer self) {
      	   int i;
	   for(i = 0; i < self.len; i = i + 1) {
	   	 print_char(self.arr[i]);
	   }
	   print_string("\n");
      }

      void concat(class string_buffer self, class string_buffer st) {
      	  
	   int i;
	   for(i = 0; i < st.get_length(st); i = i + 1) {
	   	 append(self, st.arr[i]);
	   }
      }

}

class Test {

      int main() {
      	  class string_buffer sb;
	  class string_buffer sb2;
	  String str;
	  String str2;
	  str = "hell";
	  str2 = "world!";

	  sb.init(sb, str);
      	  print_int(sb.get_length(sb));
	  print_string("\n");
	  sb.append(sb, 'o');
	  sb.print_string_buffer(sb);

	  sb2.init(sb2, str2);
	  sb.concat(sb, sb2);
	  sb.print_string_buffer(sb);
	  
      	  return 0;
      }
}