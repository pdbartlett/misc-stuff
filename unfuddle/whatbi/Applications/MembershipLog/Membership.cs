/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		Membership.cs
 *	Description:	Class describing a period of membership
 */

using System;
using System.Collections;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Custom;

namespace Pdbartlett.MembershipLog
{
	public class Membership : ICloneable, IGetKey
	{
		private DateTime	m_startDate;
		private DateTime	m_endDate;
		private Decimal		m_cost;
		private string		m_type			= "";
		
		public Membership()
		{
			m_startDate = DateTime.Today;
			m_endDate = new DateTime(m_startDate.Year, 12, 31);
		}
		
		public DateTime StartDate
		{
			get { return m_startDate; }
			set { m_startDate = value; }
		}
		
		public DateTime EndDate
		{
			get { return m_endDate; }
			set { m_endDate = value; }
		}
		
		public string MembershipType
		{
			get { return m_type; }
			set { m_type = value; }
		}
		
		public Decimal Cost
		{
			get { return m_cost; }
			set { m_cost = value; }
		}
		
		public object Clone()
		{
			return MemberwiseClone();
		}
		
		public object GetKey()
		{
			return m_endDate.ToString("u");
		}
	}

	public class MembershipCollection : ListAndUpdatableDataSource
	{
		public MembershipCollection() : this(null) {}
		public MembershipCollection(ICollection coll) : base(new UpdatableSortDecorator(new UpdatableMap(coll), new SortSpec("StartDate", false))) {}
		
		public override int Add(object o)
		{
			if (o == null)
				throw new ArgumentNullException();
			
			if (!(o is Membership))
				throw new ArgumentException();
			
			return base.Add(o);
		}
	}
}