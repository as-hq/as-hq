//------------------------------------------------------------------------------
// <copyright project="BEmu_maven" file="/BEmu_maven/bemu/src/main/java/com/bloomberglp/blpapi/ReferenceElementErrorInfo.java" company="Jordan Robinson">
//     Copyright (c) 2013 Jordan Robinson. All rights reserved.
//
//     The use of this software is governed by the Microsoft Public License
//     which is included with this distribution.
// </copyright>
//------------------------------------------------------------------------------

package com.bloomberglp.blpapi;

import com.bloomberglp.blpapi.Element;
import com.bloomberglp.blpapi.Name;
import com.bloomberglp.blpapi.SchemaTypeDefinition;

public class ReferenceElementErrorInfo extends Element
{
    private final ReferenceElementString _source, _category, _message, _subCategory;
    private final ReferenceElementInt _code;

    ReferenceElementErrorInfo()
    {
        String sourceGibberish = RandomDataGenerator.randomString(5).toLowerCase();
        
        this._source = new ReferenceElementString("source", String.format("%s::%s%s", RandomDataGenerator.randomInt(99), sourceGibberish, RandomDataGenerator.randomInt(99)));
        this._code = new ReferenceElementInt("code", RandomDataGenerator.randomInt(99));
        this._category = new ReferenceElementString("category", "BAD_FLD");
        this._message = new ReferenceElementString("message", "Field not valid");
        this._subCategory = new ReferenceElementString("subcategory", "INVALID_FIELD");
    }
    
    public SchemaTypeDefinition typeDefinition() throws Exception
    {
    	return new SchemaTypeDefinition(this.datatype(), new Name("ErrorInfo"));
    }
	
    public Name name() throws Exception
    {
    	return new Name("errorInfo");
    }
    
    public int numValues()
    {
    	return 1;
    }
    
    public int numElements()
    {
    	return 5;
    }
    
    public String getElementAsString(String name) throws Exception
    {
    	return this.getElement(name).getValueAsString();
    }
    
    public int getElementAsInt32(String name) throws Exception
    {
    	return this.getElement(name).getValueAsInt32();
    }
    
    public Element getElement(String name) throws Exception
    {
    	if(name.equals("source"))
    		return this._source;
    	
    	else if(name.equals("code"))
    		return this._code;
    	
    	else if(name.equals("category"))
    		return this._category;
    	
    	else if(name.equals("message"))
    		return this._message;
    	
    	else if(name.equals("subcategory"))
    		return this._subCategory;
    	
    	else
    		return super.getElement(name);
    }
    
    public boolean hasElement(String name)
    {
    	return this.hasElement(name, false);
    }
    
    public boolean hasElement(String name, boolean excludeNullElements)
    {
    	return name.equals("source") ||
    			name.equals("code") ||
    			name.equals("category") ||
    			name.equals("message") ||
    			name.equals("subcategory");
    } 
    
    protected StringBuilder prettyPrint(int tabIndent) throws Exception
    {
        String tabs = com.bloomberglp.blpapi.IndentType.Indent(tabIndent);
        StringBuilder result = new StringBuilder();
        
        result.append(String.format("%s%s = {%s", tabs, this.name().toString(), System.getProperty("line.separator")));
        result.append(this._source.prettyPrint(tabIndent + 1));
        result.append(this._code.prettyPrint(tabIndent + 1));
        result.append(this._category.prettyPrint(tabIndent + 1));
        result.append(this._message.prettyPrint(tabIndent + 1));
        result.append(this._subCategory.prettyPrint(tabIndent + 1));
        result.append(String.format("%s}%s", tabs, System.getProperty("line.separator")));

        return result;
    }
    
    public boolean isComplexType()
    {
    	return true;
    }
    
    public boolean isArray()
    {
    	return false;
    }
    
    public boolean isNull()
    {
    	return false;
    }

}
