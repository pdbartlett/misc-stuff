/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		Detail.cs
 *	Description:	Class describing a custom detail
 */

using System;
using System.Collections;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Custom;

namespace Pdbartlett.MembershipLog
{
	public class Detail : ICloneable, IGetKey
	{
		private string	m_name	= "";
		private object	m_value = "";
		
		public string	Name	{ get { return m_name; }	set { m_name = value; } }
		public object	Value	{ get { return m_value; }	set { m_value = value; } }
		
		public object Clone()
		{
			Detail d = (Detail)MemberwiseClone();
			
			d.Name	= (String)Name.Clone();
			d.Value	= ((ICloneable)Value).Clone();
			
			return d;
		}
		
		public object GetKey()
		{
			return Name;
		}
	}
	
	public class DetailCollection : ListAndUpdatableDataSource
	{
		public DetailCollection() : this(null) {}
		public DetailCollection(ICollection coll) : base(new UpdatableSortDecorator(new UpdatableMap(coll), new SortSpec("Name"))) {}
		
		public override int Add(object o)
		{
			if (o == null)
				throw new ArgumentNullException();
			
			if (!(o is Detail))
				throw new ArgumentException();
			
			return base.Add(o);
		}
	}
}
