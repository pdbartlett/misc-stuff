using System;
using System.Collections;
using System.Collections.Generic;

namespace Pdbartlett.Test
{
	class Program
	{
		static void Main(string[] args)
		{
			Check(new SimpleDataSource());
			Check(new SaferDataSource<string, string>());
		}

		static void Check(ISimplifiedDataSource ds)
		{
			ds.Add("Fred", "Flintstone");
			ds.Add("Barney", "Rubble");

			if (ds.Get("Fred").ToString() != "Flintstone")
				throw new ApplicationException("Test for Fred failed");

			if (ds.Get("Barney").ToString() != "Rubble")
				throw new ApplicationException("Test for Barney failed");

			if (ds.GetAll().Count != 2)
				throw new ApplicationException("Wrong count from GetAll()");
		}
	}

	interface ISimplifiedDataSource
	{
		void Add(object key, object value);
		object Get(object key);
		ICollection GetAll();
	}

	interface ISimplifiedDataSource<K, V>
	{
		V Get(K key);
		void Add(K key, V value);
		ICollection<V> GetAll();
	}

	class SimpleDataSource : ISimplifiedDataSource
	{
		private Hashtable data = new Hashtable();

		public void Add(object key, object value)
		{
			data[key] = value;
		}
		
		public object Get(object key)
		{
			return data[key];
		}

		public ICollection GetAll()
		{
			return data.Values;
		}
	}

	class SaferDataSource<K, V> : DataSourceHelper<K, V>
	{
		private Dictionary<K, V> data = new Dictionary<K, V>();

		public override void Add(K key, V value)
		{
			data[key] = value;
		}

		public override V Get(K key)
		{
			return data[key];
		}

		public override ICollection<V> GetAll()
		{
			return data.Values;
		}
	}

	abstract class DataSourceHelper<K, V> : ISimplifiedDataSource<K, V>, ISimplifiedDataSource
	{
		public abstract V Get(K key);
		public abstract void Add(K key, V value);
		public abstract ICollection<V> GetAll();

		public void Add(object key, object value)
		{
			if (key.GetType() != typeof(K))
				throw new ArgumentException("Key is of wrong type");

			if (value.GetType() != typeof(V))
				throw new ArgumentException("Value is of wrong type");

			Add((K)key, (V)value);
		}

		public object Get(object key)
		{
			if (key.GetType() != typeof(K))
				throw new ArgumentException("Key is of wrong type");

			return Get((K)key);
		}

		ICollection ISimplifiedDataSource.GetAll()
		{
			return (ICollection)GetAll();
		}
	}
}
