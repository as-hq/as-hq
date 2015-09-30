//------------------------------------------------------------------------------
// <copyright project="BEmu_maven" file="/BEmu_maven/bemu/src/main/java/com/bloomberglp/blpapi/IndentType.java" company="Jordan Robinson">
//     Copyright (c) 2013 Jordan Robinson. All rights reserved.
//
//     The use of this software is governed by the Microsoft Public License
//     which is included with this distribution.
// </copyright>
//------------------------------------------------------------------------------

package com.bloomberglp.blpapi;

import java.lang.StringBuilder;

public class IndentType
{
	public static String TAB = "    "; //four spaces
	
	public static String Indent(int count)
	{
		StringBuilder result = new StringBuilder();
		for(int i = 0; i < count; i++)
		{
			result.append(IndentType.TAB);
		}
		return result.toString();
	}
	
	public static String Tab()
	{
		return IndentType.TAB;
	}
}
