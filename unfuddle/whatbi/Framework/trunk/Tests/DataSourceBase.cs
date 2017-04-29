/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/DataSourceBase.cs
 *	Description:	Base class for helping test datasource
 */

using System;
using System.Collections;

#if USE_MBUNIT
using MbUnit.Core.Framework;
using MbUnit.Framework;
#else
using NUnit.Framework;
#endif

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Examples;

namespace Pdbartlett.Whatbi.Tests
{
	public abstract class DataSourceBaseTest : BaseTest
	{
		// members
		
		protected IDataSource TestSource;
		
		// initialisation/destruction
		
		[SetUp] public void Init()
		{
			TestSource = CreateDataSource();
		}

		[TearDown] public void Tidy()
		{
			IDisposable disp = TestSource as IDisposable;
			if (disp != null)
				disp.Dispose();
		}
		
		protected abstract IDataSource CreateDataSource();
		
		// common tests
		
		[Test] public void Count()
		{
			Assert.AreEqual(TestSource.Count, TestData.Length);
		}
		
		[Test] public virtual void ValidGets()
		{
			foreach (Caveman c in TestData)
			{
				GetHelper(Utility.GetKey(c), c);
			}
		}
		
		[Test] public virtual void InvalidGets()
		{
			GetHelper("Dino", null);
		}
		
		[Test, ExpectedException(typeof(ArgumentNullException))] public void NullGet()
		{
			GetHelper(null, null);
		}
		
		[Test] public void GetAll()
		{
			ICollection all = TestSource.GetAll();
			AssertCollectionsEqual(TestSource.GetAll(), TestData, true);
		}
		
		[Test] public void QueryForFred()
		{
			CheckForOneName(TestSource.Query(new FredTester()), "Fred");
		}
		
		[Test] public void QueryByNameField()
		{
			foreach (Caveman c in TestData)
			{
				IPredicate query = Expr.Equal(Expr.GetProperty("Name"), Expr.Literal(c.Name));
				CheckForOneName(TestSource.Query(query), c.Name);
			}
		}
		
		[Test] public void QueryByNameWithQuoteTest()
		{
			IPredicate query = Expr.Equal(Expr.GetProperty("Name"), Expr.Literal("Connor O'Connor"));
			CheckQueryCount(query, 0);
		}
		
		[Test] public void QueryByNameAndJobFields()
		{
			foreach (Caveman c in TestData)
			{
				IPredicate query = Expr.And
					(	Expr.Equal(Expr.GetProperty("Name"),	Expr.Literal(c.Name))
					,	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal(c.Job))
					);
				CheckForOneName(TestSource.Query(query), c.Name);
			}
		}
		
		[Test] public void QueryByNameAndWrongJobFields()
		{
			foreach (Caveman c in TestData)
			{
				IPredicate query = Expr.And
					(	Expr.Equal(Expr.GetProperty("Name"),	Expr.Literal(c.Name))
					,	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Analyst/Programmer"))
					);
			CheckQueryCount(query, 0);
			}
		}
		
		[Test] public void QueryByMultipleJobs1()
		{
			IPredicate query = Expr.Or
				(	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Quarryman"))
				,	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Analyst/Programmer"))
				);
			CheckQueryCount(query, 2);
		}
		
		[Test] public void QueryByMultipleJobs2()
		{
			IPredicate query = Expr.Or
				(	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Cavewife"))
				,	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Analyst/Programmer"))
				);
			CheckQueryCount(query, 2);
		}
		
		[Test] public void QueryByMultipleJobs3()
		{
			IPredicate query = Expr.Or
				(	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Quarryman"))
				,	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Cavewife"))
				);
			CheckQueryCount(query, 4);
		}
		
		[Test] public void QueryByMultipleJobs4()
		{
			IPredicate query = Expr.Or
				(	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Architect"))
				,	Expr.Equal(Expr.GetProperty("Job"),	Expr.Literal("Analyst/Programmer"))
				);
			CheckQueryCount(query, 0);
		}
		
		[Test] public void QueryByNameNotFred1()
		{
			IPredicate query = Expr.NotEqual(Expr.GetProperty("Name"), Expr.Literal("Fred"));
			CheckQueryCount(query, 3);
		}
		
		[Test] public void QueryByNameNotFred2()
		{
			IPredicate query = Expr.Not(Expr.Equal(Expr.GetProperty("Name"), Expr.Literal("Fred")));
			CheckQueryCount(query, 3);
		}
		
		[Test] public void AlphaBeforeFredExclusive()
		{
			IPredicate query = Expr.LessThan(Expr.GetProperty("Name"), Expr.Literal("Fred"));
			CheckQueryCount(query, 2);
		}
		
		[Test] public void AlphaBeforeFredInclusive()
		{
			IPredicate query = Expr.LessThanOrEqual(Expr.GetProperty("Name"), Expr.Literal("Fred"));
			CheckQueryCount(query, 3);
		}
		
		[Test] public void AlphaAfterFredExclusive()
		{
			IPredicate query = Expr.GreaterThan(Expr.GetProperty("Name"), Expr.Literal("Fred"));
			CheckQueryCount(query, 1);
		}
		
		[Test] public void AlphaAfterFredInclusive()
		{
			IPredicate query = Expr.GreaterThanOrEqual(Expr.GetProperty("Name"), Expr.Literal("Fred"));
			CheckQueryCount(query, 2);
		}
		
		[Test] public void SelfQuery()
		{
			foreach (Caveman c in TestData)
			{
				IPredicate query = Expr.Equal(Expr.Self(), Expr.Literal(c));
				CheckQueryCount(query, 1);
			}
		}
		
		// helpers
		
		protected IUpdatableDataSource UpdatableSource
		{
			get { return (IUpdatableDataSource)TestSource; }
		}
		
		protected void GetHelper(object key, object test)
		{
			object obj = TestSource.Get(key);
			Assert.AreEqual(test, obj);
			if (test != null)
				Assert.IsFalse(Object.ReferenceEquals(test, obj));
		}
		
		private void CheckQueryCount(IPredicate query, int expected)
		{
			ICollection results = TestSource.Query(query);
			Assert.AreEqual(results.Count, expected);
		}
		
		static private void CheckForOneName(ICollection coll, string name)
		{
			Assert.AreEqual(coll.Count, 1);
			foreach(object o in coll)
			{
				Caveman c = o as Caveman;
				Assert.IsNotNull(c);
				Assert.AreEqual(c.Name, name);
			}
		}
		
		static private bool IsCavemanCalledFred(object o)
		{
			Caveman c = o as Caveman;
			return c != null && c.Name == "Fred";
		}
		
		private class FredTester : IPredicate
		{
			public bool Evaluate(object o)
			{
				return IsCavemanCalledFred(o);
			}
		}
	}
	
	public abstract class UpdatableDataSourceBaseTest : DataSourceBaseTest
	{
		// common tests
		
		[Test] public void InsertTest()
		{
			Caveman ToInsert = new Caveman("Bam-bam", "Baby");
			UpdatableSource.Insert(ToInsert);
			GetHelper("Bam-bam", ToInsert);
		}
		
		[Test] public void ExistingInsert()
		{
			try
			{
				Caveman NewVersion = new Caveman("Fred", "Farmer");
				UpdatableSource.Insert(NewVersion);
			}
			catch (Exception)
			{
				// For now any exception will do...
				return;
			}
			
			Assert.Fail("Should have thrown an exception");
		}
		
		[Test] public void UpdateTest()
		{
			Caveman NewVersion = new Caveman("Fred", "Foreman");
			UpdatableSource.Update(NewVersion);
			GetHelper("Fred", NewVersion);
		}
		
		[Test] public void NonExistentUpdate()
		{
			try
			{
				Caveman NewVersion = new Caveman("Dino", "Pet");
				UpdatableSource.Update(NewVersion);
			}
			catch (Exception)
			{
				// For now any exception will do...
				return;
			}
			
			Assert.Fail("Should have thrown an exception");
		}
		
		[Test] public void DeleteTest()
		{
			UpdatableSource.Delete("Fred");
			GetHelper("Fred", null);
		}
		
		[Test] public void NonExistentDelete()
		{
			try
			{
				UpdatableSource.Delete("Dino");
			}
			catch (Exception)
			{
				// For now any exception will do...
				return;
			}
			
			Assert.Fail("Should have thrown an exception");
		}
	}
}
