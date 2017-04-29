/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		Payment.cs
 *	Description:	Class describing a payment
 */

using System;
using System.Collections;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Custom;

namespace Pdbartlett.MembershipLog
{
	public class Payment : ICloneable, IGetKey    
	{
		private DateTime	m_date;
		private Decimal		m_amount;
		private string		m_description = "";
		
		public DateTime	PaymentDate	{ get { return m_date; }		set { m_date = value; }			}
		public Decimal	Amount		{ get { return m_amount; }		set { m_amount = value; }		}
		public string	Description	{ get { return m_description; }	set { m_description = value; }	}
		
		public Payment()
		{
			m_date = DateTime.Today;
		}
		
		public object Clone()
		{
			return MemberwiseClone();
		}
		
		public object GetKey()
		{
			return String.Format("{0}:{1}:{2}", m_date.ToString("u"), m_amount.ToString(), m_description);
		}
	}
	
	public class PaymentCollection : ListAndUpdatableDataSource
	{
		public PaymentCollection() : this(null) {}
		public PaymentCollection(ICollection coll) : base(new UpdatableSortDecorator(new UpdatableMap(coll), new SortSpec("PaymentDate", false))) {}
		
		public override int Add(object o)
		{
			if (o == null)
				throw new ArgumentNullException();
			
			if (!(o is Payment))
				throw new ArgumentException();
			
			return base.Add(o);
		}
	}
}
