/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Xml.cs
 *	Description:	Classes for XML-based datasources
 */

using System;
using System.Collections;
using System.IO;
using System.Xml.Serialization;

namespace Pdbartlett.Whatbi
{
	public class SerializedXmlDataSource : DataSourceHelper, IUpdatableDataSource, IFileBulkDataSource, IDisposable
	{
		private const int INDEX_NOT_FOUND = -1;
		
		private Type			m_itemType;
        private Type			m_collType;
		private Type[]			m_extraTypes;
        private string			m_filePath;
		private bool			m_isNew;
		private FileShare		m_sharing;
		private FileStream		m_stream;
		private StreamReader	m_reader;
		private StreamWriter	m_writer;

		public SerializedXmlDataSource(Type itemType, Type collType, string filePath, FileShare sharing)
			: this(itemType, collType, null, filePath, sharing)
		{
		}

		public SerializedXmlDataSource(Type itemType, Type collType, Type[] extraTypes, string filePath, FileShare sharing)
		{
			m_itemType		= itemType;
            m_collType		= collType;
			m_extraTypes	= extraTypes;
            m_filePath		= filePath;
			m_sharing		= sharing;
			m_isNew			= !File.Exists(filePath);
			m_stream		= File.Open(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, sharing);
			m_reader		= new StreamReader(m_stream);
			m_writer		= new StreamWriter(m_stream);
        }

		~SerializedXmlDataSource()
		{
			Dispose(false);
		}

		public void Dispose()
		{
			Dispose(true);
		}

		protected void Dispose(bool bDisposing)
		{
			if (m_stream != null)
				m_stream.Close();

			if (bDisposing)
				GC.SuppressFinalize(this);
		}
		
		public string		Path	{ get { return m_filePath;				} }
		public FileAccess 	Access	{ get { return FileAccess.ReadWrite;	} } // must be, for updatable data source
		public FileShare	Sharing	{ get { return m_sharing;				} }

		protected override ICollection InternalGetAll(out bool IsCopy)
		{
			IsCopy = true;
			
			if (m_isNew)
				return new ArrayList();	// not ideal as it's not of type collType, but should do for now

			ICollection data = (ICollection)XmlUtility.DeserializeObject(m_collType, m_extraTypes, m_reader);

			m_stream.Seek(0, SeekOrigin.Begin); // rewind stream, ready for next IO operation

			return data;
        }

        public void Insert(object obj)
        {
			if (obj == null)
                throw new ArgumentNullException();

            if (obj.GetType() != m_itemType)
                throw new ArgumentException();

			object key = Utility.GetKey(obj);
            ArrayList data = new ArrayList(InternalGetAll());

			if (FindIndexForKey(data, key) != INDEX_NOT_FOUND)
				throw new ArgumentException("An item with the same key already exists");
			
            data.Add(obj);
			SerializeArrayList(data);
		}

        public void Update(object obj)
        {
			if (obj == null)
                throw new ArgumentNullException();

            if (obj.GetType() != m_itemType)
                throw new ArgumentException();
			
			ArrayList data = new ArrayList(InternalGetAll());
			int index = FindIndexForKey(data, Utility.GetKey(obj));
			
			if (index == INDEX_NOT_FOUND)
				throw new ArgumentException("Object not found");
			
			data[index] = obj;			
			SerializeArrayList(data);
        }

        public void Delete(object key)
        {
			if (key == null)
				throw new ArgumentNullException();

			ArrayList data = new ArrayList(InternalGetAll());
			int index = FindIndexForKey(data, key);
			
			if (index == INDEX_NOT_FOUND)
				throw new ArgumentException("Key not found");
			
			data.RemoveAt(index);			
			SerializeArrayList(data);
		}
		
		public void UpdateAll(ICollection data)
		{
			SerializeArrayList(Utility.MakeArrayList(data));
		}
		
		private int FindIndexForKey(ArrayList list, object key)
		{
			for (int i = 0; i < list.Count; ++i)
			{
				if (key.Equals(Utility.GetKey(list[i])))
					return i;
			}
			
			return INDEX_NOT_FOUND;
		}
		
		private void SerializeArrayList(ArrayList data)
		{
			Array arr = data.ToArray(m_itemType);
			
			XmlUtility.SerializeObject(arr, m_collType, m_extraTypes, m_writer);

			m_writer.Flush();
			m_stream.SetLength(m_stream.Seek(0, SeekOrigin.Current));	// truncate any "old" data
			m_stream.Seek(0, SeekOrigin.Begin);							// rewind stream, ready for next IO operation

			m_isNew = false;											// data is now available for reading
		}
    }
}
