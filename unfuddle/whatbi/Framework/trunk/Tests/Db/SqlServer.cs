/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/Db/SqlServer.cs
 *	Description:	Test of simple, database-based datasource using MS SQL Server
 */

using System;

#if USE_MBUNIT
using MbUnit.Core.Framework;
using MbUnit.Framework;
#else
using NUnit.Framework;
#endif

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Tests;

namespace Pdbartlett.Whatbi.DbTests
{
    [TestFixture] public class SqlServerTest : DatabaseTest
    {
        public SqlServerTest() : base(DatabaseType.MsSqlServer, @"(local)\Test", "WhatbiTest")
        {
        }

        [TestFixtureTearDown] public void FixtureCleanUp()
        {
            ResetData();
        }
    }
}
