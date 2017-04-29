/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/Xml.cs
 *	Description:	Test of simple, XML-based datasource
 */

using System;
using System.Collections;
using System.IO;
using System.Xml.Serialization;

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
    [TestFixture]
    public class XmlDataSourceTest : UpdatableDataSourceBaseTest
    {
        // members
        private const string m_fileName = "TestData.xml";

        // initialisation/destruction

        protected override IDataSource CreateDataSource()
        {
            File.Delete(m_fileName);
			XmlUtility.SerializeObjectToFile(TestData, m_fileName);
						
            return new SerializedXmlDataSource(typeof(Caveman), typeof(Caveman[]), m_fileName, FileShare.None);
        }
	}
	
	[TestFixture]
	public class CachedXmlDataSourceTest : XmlDataSourceTest
	{
		// initialisation/destruction
		protected override IDataSource CreateDataSource()
		{
			SerializedXmlDataSource ds = (SerializedXmlDataSource)base.CreateDataSource();
			return new CachingBulkFileAdapter(ds);
		}
	}
}

