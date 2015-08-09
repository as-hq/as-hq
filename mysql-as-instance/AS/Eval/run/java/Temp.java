
import com.bloombergapi.examples.*;

import java.util.ArrayList;
import java.util.Arrays; 

public class Temp { //rather hacky but end parentheses given by Lang.hs

public static String pprint(Object o){
	if ((o instanceof Float) || (o instanceof Integer))
		return o.toString();
	else if ((o instanceof String))
		return "'" + (o.toString()) + "'";
	else if (o instanceof ArrayList<?>)
		return Arrays.toString(((ArrayList)o).toArray());
	else 
		return "undefined";
}
int a = 1 + 1;
int b = 1 + 1;
a + b + 2.0
public static void main(String[] args) throws Exception{Object x = ; System.out.println(pprint(x));}}