
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


public static void main(String[] args) throws Exception{Object x = RunIntradayTickDataRequest.RunExample("IBM US Equity"); System.out.println(pprint(x));}}