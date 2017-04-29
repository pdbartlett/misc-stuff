/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Enum.cs
 *	Description:	Enumerated types
 */

using System;
using System.Collections;

namespace Pdbartlett.Whatbi
{
	public interface IEnumeratedType
	{
		ICollection	Names	{ get; }
		ICollection	Values	{ get; }
		
		object LookupName(object oValue);
		object LookupValue(object oName);
	}
	
	public class DynamicEnumeration : IEnumeratedType
	{
		private Hashtable m_namedValues;
		
		public DynamicEnumeration(Hashtable namedValues)
		{
			if (namedValues == null)
				throw new ArgumentNullException();
			
			if (namedValues.Count == 0)
				throw new ArgumentException();
			
			m_namedValues = namedValues;
		}
		
		public ICollection Names	{ get { return m_namedValues.Values; } }
		public ICollection Values	{ get { return m_namedValues.Keys; } }
		
		public object LookupName(object oValue)
		{
			return m_namedValues[oValue];
		}
		
		public object LookupValue(object oName)
		{
			if (oName == null)
				throw new ArgumentNullException();
			
			foreach (DictionaryEntry de in m_namedValues)
			{
				if (de.Value == oName)
					return de.Key;
			}
			
			return null;
		}
	}
	
	public class ClrEnumeration : IEnumeratedType
	{
		private Type m_type;
		
		public ClrEnumeration(Type type)
		{
			if (!type.IsSubclassOf(typeof(Enum)))
				throw new ArgumentException();
			
			m_type = type;
		}
		
		public ICollection Names	{ get { return Enum.GetNames(m_type); } }
		public ICollection Values	{ get { return Enum.GetValues(m_type); } }
		
		public object LookupName(object oValue)
		{
			return Enum.GetName(m_type, oValue);
		}
		
		public object LookupValue(object oName)
		{
			if (oName == null)
				throw new ArgumentNullException();
			
			string name = oName.ToString();
			return Enum.IsDefined(m_type, name) ? Enum.Parse(m_type, name) : null;
		}
	}
}
