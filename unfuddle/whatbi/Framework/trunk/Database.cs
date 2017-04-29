/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Database.cs
 *	Description:	Database-based datasources
 */

using System;
using System.Collections;
using System.Data;
using System.Data.SqlClient;
using System.IO;

using Finisar.SQLite;

namespace Pdbartlett.Whatbi
{
    public enum DatabaseType
    {
        MsSqlServer,
        SQLite
    }

    public interface IDbObjectFactory
    {
        IDbConnection	GetConnection();
        IDbCommand		GetCommand();
        IDataParameter	GetParameter(string name, DbType type, int length, object oValue);
    }

    public abstract class DatabaseDataSourceHelper : DataSourceHelper, IUpdatableDataSource
    {
        private IDbObjectFactory	m_factory;
        private IORMapping			m_mapper;

        protected IDbObjectFactory Factory
        {
            get { if (m_factory == null)	throw new BaseException("Factory not set");	return m_factory; }
            set { if (value == null)		throw new ArgumentNullException();			    m_factory = value; }
        }

        protected IORMapping Mapper
        {
            get { if (m_mapper == null)    throw new BaseException("Mapper not set");    return m_mapper; }
            set { if (value == null)		throw new ArgumentNullException();			    m_mapper = value; }
        }

        protected DatabaseDataSourceHelper()
        {
        }

        protected DatabaseDataSourceHelper(IDbObjectFactory factory, IORMapping mapper)
        {
            Factory = factory;
            Mapper = mapper;
        }

        public override int Count
        {
            get { return (int)ExecuteSqlScalar(Mapper.GetCountCmd()); }
        }

        protected override ICollection InternalGetAll(out bool IsCopy)
        {
            IsCopy = true;
            return ExecuteSqlQuery(Mapper.GetAllCmd());
        }

        public void Insert(object obj)
        {
            ExecuteSqlCmd(Mapper.GetInsertCmd(obj));
        }

        public void Update(object obj)
        {
            ExecuteSqlCmd(Mapper.GetUpdateCmd(obj));
        }

        public void Delete(object key)
        {
            ExecuteSqlCmd(Mapper.GetDeleteCmd(key));
        }

        public override ICollection Query(IPredicate pred)
        {
            ICollection results = QueryAsSql(pred);
            return results == null ? base.Query(pred) : results;
        }

        private ICollection QueryAsSql(object query)
        {
            ISqlConvertible sqlQuery = query as ISqlConvertible;

            if (sqlQuery != null)
            {
                try
                {
                    IDbCommand cmd = Mapper.GetQueryCmd(sqlQuery.ConvertToSql(Mapper));
                    return ExecuteSqlQuery(cmd);
                }
                catch (Exception) {}
            }

            return null;
        }

        private ICollection ExecuteSqlQuery(IDbCommand cmd)
        {
            using (cmd)
            {
                ArrayList data = new ArrayList();
                using (IDbConnection conn = Factory.GetConnection())
                {
                    conn.Open();
                    cmd.Connection = conn;
                    using (IDataReader reader = cmd.ExecuteReader())
                    {
                        while (reader.Read())
                            data.Add(Mapper.ExtractObjectFromDataReader(reader));
                    }
                }
                return data;
            }
        }

        private void ExecuteSqlCmd(IDbCommand cmd)
        {
            using (cmd)
            {
                using (IDbConnection conn = Factory.GetConnection())
                {
                    conn.Open();
                    cmd.Connection = conn;

                    int cRows = cmd.ExecuteNonQuery();

                    switch (cRows)
                    {
                        case 0:
                            throw new BaseException("SQL command failed: " + cmd.CommandText);
                        case 1:
                            return;
                        default:
                            throw new BaseException("Unexpected results from SQL command '" + cmd.CommandText + "': " + cRows.ToString() + " rows affected.");
                    }
                }
            }
        }

        private object ExecuteSqlScalar(IDbCommand cmd)
        {
            using (cmd)
            {
                using (IDbConnection conn = Factory.GetConnection())
                {
                    conn.Open();
                    cmd.Connection = conn;
                    return cmd.ExecuteScalar();
                }
            }
        }

        public static IDbObjectFactory GetFactory(DatabaseType type, string server, string database)
        {
            if (!Enum.IsDefined(typeof(DatabaseType), type))
                throw new ArgumentException("type");

            switch(type)
            {
            case DatabaseType.MsSqlServer:
                return new SqlServerFactory(server, database);
            case DatabaseType.SQLite:
                return new SQLiteFactory(server, database);
            default:
                throw new BaseException("Unsupported database type: " + type);
            }
        }
    }

    public class SqlServerFactory : IDbObjectFactory
    {
        private string m_connectionString;

        public SqlServerFactory(string server, string database)
        {
            m_connectionString = String.Format("Server={0};Database={1};Integrated Security=SSPI", server, database);
        }

        public IDbConnection GetConnection()
        {
            return new SqlConnection(m_connectionString);
        }

        public IDbCommand GetCommand()
        {
            return new SqlCommand();
        }

        public IDataParameter GetParameter(string name, DbType type, int len, object oValue)
        {
            SqlParameter param = new SqlParameter(name, ConvertType(type), len);
            param.Value = oValue;
            return param;
        }

        private static SqlDbType ConvertType(DbType type)
        {
            switch(type)
            {
                case DbType.String:
                    return SqlDbType.NVarChar;
                case DbType.AnsiString:
                    return SqlDbType.VarChar;
                default:
                    throw new ArgumentException("Unsupport database type");
            }
        }
    }

    public class SQLiteFactory : IDbObjectFactory
    {
        private string m_connectionString;

        public SQLiteFactory(string server, string database)
        {
            if (server != null && server.Length > 0)
                throw new ArgumentException("Sqlite only supports local connections - server should not be specified");

            bool create = !File.Exists(database);

            m_connectionString = String.Format("Data Source={0};New={1};Compress=True", database, create);
        }

        public IDbConnection GetConnection()
        {
            return new SQLiteConnection(m_connectionString);
        }

        public IDbCommand GetCommand()
        {
            return new SQLiteCommand();
        }

        public IDataParameter GetParameter(string name, DbType type, int len, object oValue)
        {
            return new SQLiteParameter(name, type);
        }
    }
}
