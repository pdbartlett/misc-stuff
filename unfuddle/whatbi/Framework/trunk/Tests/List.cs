/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/List.cs
 *	Description:	Test of simple, read-only list datasource
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
	[TestFixture] public class ListTest : DataSourceBaseTest
	{
		// initialisation/destruction

		protected override IDataSource CreateDataSource()
		{
			return new ListDataSource(TestData);
		}
		
		// tests
		
		[Test] public override void ValidGets()
		{
			for (int i = 1; i <= TestData.Length; ++i)
			{
				GetHelper(i, TestData[i-1]);
			}
		}
		
		[Test] public override void InvalidGets()
		{
			GetHelper(0, null);
			GetHelper(TestData.Length + 1, null);
		}
		
	}
}

