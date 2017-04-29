/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/TestBase.cs
 *	Description:	Base class for tests
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
	public class BaseTest
	{
		// members
		protected Caveman[] TestData = new Caveman[]
			{	new Caveman("Fred",		"Quarryman")
			,	new Caveman("Wilma",	"Cavewife")
			,	new Caveman("Barney",	"Quarryman")
			,	new Caveman("Betty",	"Cavewife")
			};
		
		// helpers
		protected void AssertCollectionsEqual(ICollection lhs, ICollection rhs, bool checkDistinct)
		{
			Assert.AreEqual(lhs.Count, rhs.Count);
			foreach (object o1 in rhs)
			{
				bool FoundEquivalent	= false;
				bool FoundDistinct		= false;
				
				foreach (object o2 in lhs)
				{
					if (o1.Equals(o2))
					{
						FoundEquivalent = true;
						
						if (checkDistinct && !Object.ReferenceEquals(o1, o2))
						{
							FoundDistinct = true;
							break;
						}
					}
				}
				
				if (!FoundEquivalent)
					Assert.Fail("Could not find equivalent object in resultset: " + o1.ToString());
				
				if (checkDistinct && !FoundDistinct)
					Assert.Fail("Found same object in resultset: " + o1.ToString());
			}
		}
	}
}
