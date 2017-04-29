/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		List.cs
 *	Description:	List-based datasources
 */

using System;
using System.Collections;

namespace Pdbartlett.Whatbi
{
	public class ListDataSource : DataSourceHelper
	{
		private Object[] m_data;
		
		protected override ICollection InternalGetAll(out bool IsCopy)
		{
			IsCopy = false;
			return m_data;
		}
		
		public ListDataSource(ICollection data)
		{
			m_data = new Object[data.Count];
			data.CopyTo(m_data, 0);
		}
		
		public override Object Get(Object key)
		{
			if (key == null)
				throw new ArgumentNullException();
			
			Int32 i;
			if (!Utility.TryParseInt32(key.ToString(), out i))
				return null;

			if (i > 0 && i <= m_data.Length)
				return Utility.GetReadOnly(m_data[i-1]);
			
			return null;
		}
	}
}
