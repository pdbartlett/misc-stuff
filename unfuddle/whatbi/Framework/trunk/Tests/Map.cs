/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/ReadOnlyMap.cs
 *	Description:	Test of simple, map-based datasource
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
	[TestFixture] public class MapTest : DataSourceBaseTest
	{
		// initialisation/destruction

		protected override IDataSource CreateDataSource()
		{
			return new MapDataSource(TestData);
		}
	}
	
	[TestFixture] public class UpdatableMapTest : UpdatableDataSourceBaseTest 
	{		
		// initialisation/destruction

		protected override IDataSource CreateDataSource()
		{
			return new UpdatableMap(TestData);
		}
	}
}

