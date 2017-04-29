/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/Db/Database.cs
 *	Description:	Base classes for tests of simple, database-based datasource with various back-ends
 */

using System;
using System.Collections;
using System.Data;

#if USE_MBUNIT
using MbUnit.Core.Framework;
using MbUnit.Framework;
#else
using NUnit.Framework;
#endif

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Examples;
using Pdbartlett.Whatbi.Tests;

namespace Pdbartlett.Whatbi.DbTests
{
    public abstract class DatabaseTest : UpdatableDataSourceBaseTest
    {
        // members

        private DatabaseType m_dbType;
        private string m_server;
        private string m_database;
        private Exception m_resetException;

        // constructor
        public DatabaseTest(DatabaseType dbType, string server, string database)
        {
            m_dbType = dbType;
            m_server = server;
            m_database = database;
        }

        // helpers

        protected override IDataSource CreateDataSource()
        {
            if (m_resetException == null)
            {
                try
                {
                    ResetData();
                }
                catch (Exception ex)
                {
                    m_resetException = ex;
                }
            }

            if (m_resetException != null)
            {
                // cannot connect to database to reset data, so ignore the calling test case
                Assert.Ignore("Database connection failed: " + m_resetException.Message);
                return null;
            }

            return new DatabaseDataSource(m_dbType, m_server, m_database);
        }

        public void ResetData()
        {
            IDbObjectFactory dbof = DatabaseDataSourceHelper.GetFactory(m_dbType, m_server, m_database);

            using (IDbConnection conn = dbof.GetConnection())
            {
                conn.Open();

                using (IDbCommand delAll = dbof.GetCommand())
                {
                    delAll.Connection = conn;
                    delAll.CommandType = CommandType.Text;
                    delAll.CommandText = "DELETE FROM Cavemen";
                    delAll.ExecuteNonQuery();
                }

                using (IDbCommand insertOne = dbof.GetCommand())
                {
                    insertOne.Connection = conn;
                    insertOne.CommandType = CommandType.Text;
                    insertOne.CommandText = "INSERT INTO Cavemen([Name], Job) VALUES (@name, @job)";

                    insertOne.Parameters.Add(dbof.GetParameter("@name", DbType.AnsiString, 64, null));
                    insertOne.Parameters.Add(dbof.GetParameter("@job", DbType.AnsiString, 64, null));

                    foreach (Caveman c in TestData)
                    {
                        ((IDataParameter)insertOne.Parameters["@name"]).Value = c.Name;
                        ((IDataParameter)insertOne.Parameters["@job"]).Value = c.Job;
                        insertOne.ExecuteNonQuery();
                    }
                }
            }
        }
    }
}
