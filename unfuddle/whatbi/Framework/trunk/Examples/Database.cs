/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Examples/Database.cs
 *	Description:	Example database-based datasources
 */

using System;
using System.Collections;
using System.Data;
using System.Data.SqlClient;

using Pdbartlett.Whatbi;

namespace Pdbartlett.Whatbi.Examples
{
    public class DatabaseDataSource : DatabaseDataSourceHelper
    {
        public DatabaseDataSource(DatabaseType type, string server, string database)
        {
            IDbObjectFactory factory =  GetFactory(type, server, database);
            Factory = factory;

            IORMapping mapper = new CavemanMapping(factory);
            Mapper = mapper;
        }
    }

    public class CavemanMapping : ObjectTableMapping
    {
        private const int MAX_NAME_LENGTH	= 64;
        private const int MAX_JOB_LENGTH	= 64;

        public static object CavemanGetNameHelper(object o)
        {
            return ((Caveman)o).Name;
        }

        public static void CavemanSetNameHelper(object o, object v)
        {
            ((Caveman)o).Name = (String)v;
        }

        public static object CavemanGetJobHelper(object o)
        {
            return ((Caveman)o).Job;
        }

        public static void CavemanSetJobHelper(object o, object v)
        {
            ((Caveman)o).Job = (String)v;
        }

        public CavemanMapping(IDbObjectFactory factory) : base("Cavemen", "Name", factory)
        {
            Add(new PropertyColumnMapping(
                    new PropertyGetter(CavemanGetNameHelper), new PropertySetter(CavemanSetNameHelper), "Name", DbType.AnsiString, MAX_NAME_LENGTH)
                );
            Add(new PropertyColumnMapping(
                    new PropertyGetter(CavemanGetJobHelper), new PropertySetter(CavemanSetJobHelper), "Job", DbType.AnsiString, MAX_JOB_LENGTH)
                );
        }

        protected override object CreateObject()
        {
            return new Caveman();
        }

        protected override void ValidateObject(object obj)
        {
            CheckArgumentNotNull(obj);

            Caveman c = obj as Caveman;

            CheckStringValue(c.Name, MAX_NAME_LENGTH);
            CheckStringValue(c.Job, MAX_NAME_LENGTH);
        }

        protected override void ValidateKey(object key)
        {
            CheckArgumentNotNull(key);
            String sKey = key.ToString();
            CheckStringValue(sKey, MAX_NAME_LENGTH);
        }

        private void CheckArgumentNotNull(object o)
        {
            if (o == null)
                throw new ArgumentNullException();
        }

        private void CheckStringValue(string s, int maxLen)
        {
            if (s == null || s.Length == 0 || s.Length > maxLen)
                throw new ArgumentException();
        }
    }
}
