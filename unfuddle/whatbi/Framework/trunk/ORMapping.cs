/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		ORMapping.cs
 *	Description:	Various classes to enable object-relational (OR) mapping
 */

using System;
using System.Collections;
using System.Data;
using System.Text;

namespace Pdbartlett.Whatbi
{
    public interface IORMapping
    {
        IDbCommand	GetCountCmd();
        IDbCommand	GetAllCmd();
        IDbCommand	GetQueryCmd(string query);
        IDbCommand	GetInsertCmd(object obj);
        IDbCommand	GetUpdateCmd(object obj);
        IDbCommand	GetDeleteCmd(object key);

        object		ExtractObjectFromDataReader(IDataReader reader);

        string		GetKeyColumn();
        string		GetColumnForProperty(string property);
    }

    public delegate void PropertySetter(object o, object v);
    public delegate object PropertyGetter(object o);

    public class PropertyColumnMapping
    {
        private PropertyGetter	m_getter;
        private PropertySetter	m_setter;
        private string			m_columnName;
        private DbType		    m_columnType;
        private int				m_columnLength;

        public PropertyColumnMapping(PropertyGetter getter, PropertySetter setter, string columnName, DbType columnType, int columnLength)
        {
            m_getter		= getter;
            m_setter		= setter;
            m_columnName	= columnName;
            m_columnType	= columnType;
            m_columnLength	= columnLength;
        }

        public object GetValue(object o)
        {
            return m_getter(o);
        }

        public void SetValue(object o, object v)
        {
            m_setter(o, v);
        }

        public string		ColumnName		{ get { return m_columnName;	} }
        public DbType	    ColumnType		{ get { return m_columnType;	} }
        public int			ColumnLength	{ get { return m_columnLength;	} }
    }

    public abstract class ObjectTableMapping : IORMapping
    {
        private string				m_tableName;
        private string				m_keyColumn;
        private string				m_columnList;
        private Hashtable			m_items	= new Hashtable();
        private IDbObjectFactory	m_factory;

        public ObjectTableMapping(string tableName, string keyColumn, IDbObjectFactory factory)
        {
            m_tableName	= tableName;
            m_keyColumn	= keyColumn;
            m_factory	= factory;
        }

        public void Add(PropertyColumnMapping item)
        {
            m_items.Add(item.ColumnName, item);

            if (m_columnList == null)
                m_columnList = item.ColumnName;
            else
                m_columnList += (", " + item.ColumnName);
        }

        public IDbCommand GetCountCmd()
        {
            IDbCommand cmd = m_factory.GetCommand();
            cmd.CommandText = String.Format("SELECT COUNT(1) FROM {0}", m_tableName);
            return cmd;
        }

        public IDbCommand GetAllCmd()
        {
            IDbCommand cmd = m_factory.GetCommand();
            cmd.CommandText = String.Format("SELECT {0} FROM {1}", m_columnList, m_tableName);
            return cmd;
        }

        public IDbCommand GetQueryCmd(string query)
        {
            IDbCommand cmd = GetAllCmd();
            cmd.CommandText += " WHERE " + query;
            return cmd;
        }

        public IDbCommand GetInsertCmd(object obj)
        {
            ValidateObject(obj);

            IDbCommand cmd = m_factory.GetCommand();

            StringBuilder sInsert = new StringBuilder();
            sInsert.Append("INSERT INTO ");
            sInsert.Append(m_tableName);
            sInsert.Append("(");

            StringBuilder sValues = new StringBuilder();
            sValues.Append(" VALUES (");

            int i = 0;

            foreach (PropertyColumnMapping mapping in m_items.Values)
            {
                if (++i > 1)
                {
                    sInsert.Append(", ");
                    sValues.Append(", ");
                }

                string sParamName = "@p" + i.ToString();

                sInsert.Append(mapping.ColumnName);
                sValues.Append(sParamName);

                cmd.Parameters.Add(m_factory.GetParameter(sParamName, mapping.ColumnType, mapping.ColumnLength, mapping.GetValue(obj)));
            }

            sInsert.Append(")");
            sValues.Append(")");

            sInsert.Append(sValues);

            cmd.CommandText = sInsert.ToString();

            return cmd;
        }

        public IDbCommand GetUpdateCmd(object obj)
        {
            ValidateObject(obj);

            IDbCommand cmd = m_factory.GetCommand();

            StringBuilder sUpdate = new StringBuilder();
            sUpdate.Append("UPDATE ");
            sUpdate.Append(m_tableName);
            sUpdate.Append(" SET ");

            string sKeyColumn = m_keyColumn;
            int i = 0;

            foreach (PropertyColumnMapping mapping in m_items.Values)
            {
                if (mapping.ColumnName == sKeyColumn)
                {
                    cmd.Parameters.Add(m_factory.GetParameter("@key", mapping.ColumnType, mapping.ColumnLength, mapping.GetValue(obj)));
                }
                else
                {
                    if (++i > 1)
                        sUpdate.Append(", ");

                    string sParamName = "@p" + i.ToString();

                    sUpdate.Append(mapping.ColumnName);
                    sUpdate.Append(" = ");
                    sUpdate.Append(sParamName);

                    cmd.Parameters.Add(m_factory.GetParameter(sParamName, mapping.ColumnType, mapping.ColumnLength, mapping.GetValue(obj)));
                }
            }

            sUpdate.Append(" WHERE ");
            sUpdate.Append(sKeyColumn);
            sUpdate.Append(" = @key");

            cmd.CommandText = sUpdate.ToString();

            return cmd;
        }

        public IDbCommand GetDeleteCmd(object key)
        {
            ValidateKey(key);

            string sCol = m_keyColumn;
            PropertyColumnMapping mapping = (PropertyColumnMapping)m_items[sCol];

            IDbCommand cmd = m_factory.GetCommand();
            cmd.CommandText = "DELETE FROM " + m_tableName + " WHERE " + sCol + " = @key";
            cmd.Parameters.Add(m_factory.GetParameter("@key", mapping.ColumnType, mapping.ColumnLength, key));

            return cmd;
        }

        public object ExtractObjectFromDataReader(IDataReader reader)
        {
            object o = CreateObject();

            int i = 0;
            foreach (PropertyColumnMapping mapping in m_items.Values)
            {
                mapping.SetValue(o, reader.GetValue(i++));
            }

            return o;
        }

        public string GetKeyColumn()
        {
            return m_keyColumn;
        }

        public string GetColumnForProperty(string property)
        {
            if (property == null)
                throw new ArgumentNullException();

            PropertyColumnMapping mapping = (PropertyColumnMapping)m_items[property];
            if (mapping == null)
                throw new ArgumentException();

            return mapping.ColumnName;
        }

        protected abstract void		ValidateKey(object key);
        protected abstract void		ValidateObject(object obj);
        protected abstract object	CreateObject();
    }
}