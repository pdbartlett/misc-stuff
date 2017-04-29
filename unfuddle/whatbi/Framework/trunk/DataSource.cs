/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		DataSource.cs
 *	Description:	Generic datasource interfaces and helper classes
 */

using System;
using System.Collections;
using System.IO;

namespace Pdbartlett.Whatbi
{
	public interface IDataSource
	{
		int Count { get; }
		object Get(object key);
		ICollection GetAll();
		ICollection Query(IPredicate pred);
	}
	
	public interface IUpdatableDataSource : IDataSource
	{
		void Insert(object obj);
		void Update(object obj);
		void Delete(object key);
	}
	
	public interface IBulkDataSource
	{
		ICollection GetAll();
		void UpdateAll(ICollection data);
	}
	
	public interface IFileBased
	{
		string		Path	{ get; }
		FileAccess	Access	{ get; }
		FileShare	Sharing	{ get; }
	}
	
	public interface IFileBulkDataSource : IFileBased, IBulkDataSource {}
	
	[Serializable]	
	public abstract class DataSourceHelper : IDataSource
	{
		public virtual int Count
		{
			get { return InternalGetAll().Count; }
		}
		
		public virtual object Get(object key)
		{
			if (key == null)
				throw new ArgumentNullException();
			
			IPredicate query = Expr.Equal(Expr.Key(), Expr.Literal(key));
			ICollection results = Query(query);

			foreach (object o in results)
				return o;	// should only be one

			return null;
		}
		
		public virtual ICollection GetAll()
		{
			bool IsCopy;
			ICollection coll = InternalGetAll(out IsCopy);
			
			return IsCopy ? coll : (ICollection)Utility.GetReadOnly(InternalGetAll());
		}
		
		public virtual ICollection Query(IPredicate pred)
		{
			if (pred == null)
				throw new ArgumentException("Predicate must not be null");
			
			ArrayList results = new ArrayList();
			bool IsCopy;
			
			foreach (object o in InternalGetAll(out IsCopy))
			{
				if (pred.Evaluate(o))
					results.Add(IsCopy ? o : Utility.GetReadOnly(o));
			}
			
			return results;
		}

		protected ICollection InternalGetAll()
		{
			bool dummy;
			return InternalGetAll(out dummy);
		}
		
		protected abstract ICollection InternalGetAll(out bool IsCopy);
	}
}
