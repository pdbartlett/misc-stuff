/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests\Enum.cs
 *	Description:	Tests of enumerated types
 */

using System;
using System.Collections;

#if USE_MBUNIT
using MbUnit.Core.Framework;
using MbUnit.Framework;
#else
using NUnit.Framework;
#endif

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Examples;

namespace Pdbartlett.Whatbi.Tests
{
	public abstract class BaseEnumTest : BaseTest
	{
		private		IEnumeratedType	m_enum;
		protected	Hashtable		m_dataMap;
		
		protected abstract IEnumeratedType CreateEnumeratedType(); 
	
		[SetUp] public void Init()
		{
			m_dataMap = new Hashtable();
			foreach (object o in Enum.GetValues(typeof(TestEnum)))
			{
				m_dataMap.Add(o, Enum.GetName(typeof(TestEnum), o));
			}
			
			m_enum = CreateEnumeratedType();
		}
		
		[Test] public void Names()
		{
			AssertCollectionsEqual(m_dataMap.Values, m_enum.Names, false);
		}
		
		[Test] public void Values()
		{
			AssertCollectionsEqual(m_dataMap.Keys, m_enum.Values, false);
		}
		
		[Test] public void LookupName()
		{
			foreach (DictionaryEntry de in m_dataMap)
			{
				Assert.AreEqual(de.Value, m_enum.LookupName(de.Key));
			}
		}
		
		[Test, ExpectedException(typeof(ArgumentNullException))] public void InvalidNameNull()
		{
			Assert.IsNull(m_enum.LookupValue(null));
		}
		
		[Test] public void InvalidNameEmpty()
		{
			Assert.IsNull(m_enum.LookupValue(""));
		}
		
		[Test] public void InvalidNameDoesntExist()
		{
			Assert.IsNull(m_enum.LookupValue("DoesntExist"));
		}
		
		[Test] public void LookupValue()
		{
			foreach (DictionaryEntry de in m_dataMap)
			{
				Assert.AreEqual(de.Key, m_enum.LookupValue(de.Value));
			}
		}
		
		[Test, ExpectedException(typeof(ArgumentNullException))] public void InvalidValueNull()
		{
			Assert.IsNull(m_enum.LookupName(null));
		}
		
		[Test] public void InvalidValueNegative()
		{
			Assert.IsNull(m_enum.LookupName(-1));
		}
		
		[Test] public void InvalidValueTooBig()
		{
			Assert.IsNull(m_enum.LookupName(m_dataMap.Count));
		}
	}
	
	[TestFixture] public class DynamicEnumTest : BaseEnumTest
	{
		protected override IEnumeratedType CreateEnumeratedType()
		{
			return new DynamicEnumeration(m_dataMap);
		}
		
		[Test, ExpectedException(typeof(ArgumentNullException))] public void InvalidCtorNull()
		{
			IEnumeratedType test = new DynamicEnumeration(null);
		}
		
		[Test, ExpectedException(typeof(ArgumentException))] public void InvalidCtorEmpty()
		{
			IEnumeratedType test = new DynamicEnumeration(new Hashtable());
		}
	}
	
	[TestFixture] public class ClrEnumTest : BaseEnumTest
	{
		protected override IEnumeratedType CreateEnumeratedType()
		{
			return new ClrEnumeration(typeof(TestEnum));
		}
		
		[Test, ExpectedException(typeof(ArgumentException))] public void InvalidCtorNotEnumInt32()
		{
			IEnumeratedType test = new ClrEnumeration(typeof(Int32));
		}
		
		[Test, ExpectedException(typeof(ArgumentException))] public void InvalidCtorNotEnumString()
		{
			IEnumeratedType test = new ClrEnumeration(typeof(String));
		}
	}
}
