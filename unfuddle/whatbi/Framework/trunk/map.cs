/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Map.cs
 *	Description:	Map-based datasources
 */

using System;
using System.Collections;

namespace Pdbartlett.Whatbi
{
	[Serializable]	
	public class MapDataSource : DataSourceHelper
	{
		protected Hashtable m_data = new Hashtable();
		
		protected override ICollection InternalGetAll(out bool IsCopy)
		{
			IsCopy = false;
			return m_data.Values;
		}

		protected void InsertHelper(object obj, bool checkKey)
		{
			object key = Utility.GetKey(obj);
			if (checkKey && m_data.ContainsKey(key))
				return;

			DoInsert(key, obj);
		}

		protected void DoInsert(object key, object obj)
		{
			m_data.Add(key, obj);
		}
		
		public MapDataSource(ICollection coll)
		{
			if (coll == null)
				return;

			foreach (object o in coll)
			{
				InsertHelper(o, false);
			}
		}
		
		public override object Get(object key)
		{
			return Utility.GetReadOnly(m_data[key]);
		}
	}
	
	[Serializable]	
	public class UpdatableMap : MapDataSource, IUpdatableDataSource
	{
		public UpdatableMap(ICollection coll) : base(coll) {}
		
		public void Insert(object obj)
		{
			InsertHelper(obj, false);
		}
		
		public void Update(object obj)
		{
			object key = Utility.GetKey(obj);
			CheckKey(key);
			m_data[key] = obj;
		}
		
		public void Delete(object key)
		{
			CheckKey(key);
			m_data.Remove(key);
		}
		
		private void CheckKey(object key)
		{
			if (!m_data.ContainsKey(key))
				throw new ArgumentException();
		}
	}
}
