/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		ListAdapter.cs
 *	Description:	Adapts datasources to conform to IList}
 */

using System;
using System.Collections;

using Pdbartlett.Whatbi;

namespace Pdbartlett.Whatbi
{
	[Serializable]	
	public class ListAdapter : IList
	{
		// Member variables
		
		private IDataSource				m_adaptee;
		private IUpdatableDataSource	m_updatable;
		
		// Accessor properties
		
		protected IDataSource Adaptee { get { return m_adaptee; } }
		
		protected IUpdatableDataSource Updatable
		{
			get
			{
				if (IsReadOnly)
					throw new NotSupportedException();
				
				return m_updatable;
			}
		}
		
		// Constructor
		
		public ListAdapter(IDataSource adaptee)
		{
			m_adaptee	= adaptee;
			m_updatable	= adaptee as IUpdatableDataSource;
		}
		
		// IList properties

		public virtual int		Count			{ get { return Adaptee.Count; } }
		public virtual bool		IsFixedSize		{ get { return false; } }
		public virtual bool		IsReadOnly		{ get { return false; } }
		public virtual bool		IsSynchronized	{ get { return false; } }
		public virtual object	SyncRoot		{ get { throw new NotImplementedException(); } }
		
		public virtual object this[int index]
		{
			get
			{
				return (GetAsList())[index];
			}
			
			set
			{
				throw new NotImplementedException();
			}
		}
		
		// IList methods
		
		public virtual int Add(object o)
		{
			Updatable.Insert(o);
			return IndexOf(o);
		}
		
		public virtual void Clear()
		{
			throw new NotImplementedException();
		}
		
		public virtual bool Contains(object o)
		{
			ICollection results = Adaptee.Query(Expr.Equal(Expr.Self(), Expr.Literal(o)));
			return (results.Count > 0);
		}
		
		public virtual void CopyTo(Array array, int offset)
		{
			Adaptee.GetAll().CopyTo(array, offset);
		}
		
		public virtual IEnumerator GetEnumerator()
		{
			return GetAsList().GetEnumerator();
		}
		
		public virtual int IndexOf(object o)
		{
			return GetAsList().IndexOf(o);
		}
		
		public virtual void Insert(int index, object o)
		{
			throw new NotImplementedException();
		}
		
		public virtual void Remove(object o)
		{
			Updatable.Delete(Utility.GetKey(o));
		}
		
		public virtual void RemoveAt(int index)
		{
			object o = this[index];
			Remove(o);
		}
		
		// Additional methods
		
		public virtual void AddRange(ICollection coll)
		{
			foreach (object o in coll)
				Add(o);
		}
		
		// Helpers
		
		private IList GetAsList()
		{
			return Utility.MakeList(Adaptee.GetAll());
		}
	}
	
	[Serializable]	
	public class ListAndDataSource : ListAdapter, IDataSource
	{
		public ListAndDataSource(IDataSource ds) : base(ds) {}
		
		// IDataSource methods
		
		public object		Get(object key)			{ return Adaptee.Get(key);		}
		public ICollection	GetAll()				{ return Adaptee.GetAll();		}
		public ICollection	Query(IPredicate query)	{ return Adaptee.Query(query);	}
	}

	[Serializable]	
	public class ListAndUpdatableDataSource : ListAndDataSource, IUpdatableDataSource
	{
		public ListAndUpdatableDataSource(IUpdatableDataSource ds) : base(ds) {}
		
		// IUpdatableDataSource methods
		
		public void			Insert(object obj)		{ Updatable.Insert(obj); 		}
		public void			Update(object obj)		{ Updatable.Update(obj); 		}
		public void			Delete(object key)		{ Updatable.Delete(key); 		}
	}
}
